/*
** Lua parser (source code -> bytecode).
** Copyright (C) 2005-2009 Mike Pall. See Copyright Notice in luajit.h
**
** Major portions taken verbatim or adapted from the Lua interpreter.
** Copyright (C) 1994-2008 Lua.org, PUC-Rio. See Copyright Notice in lua.h
*/

#define lj_parse_c
#define LUA_CORE

#include "lj_obj.h"
#include "lj_gc.h"
#include "lj_err.h"
#include "lj_str.h"
#include "lj_tab.h"
#include "lj_func.h"
#include "lj_state.h"
#include "lj_bc.h"
#include "lj_lex.h"
#include "lj_parse.h"
#include "lj_vm.h"
#include "lj_vmevent.h"

/* -- Parser structures and definitions ----------------------------------- */

/* Expression kinds. */
typedef enum {
  /* Constant expressions must be first and in this order: */
  VKNIL,
  VKFALSE,
  VKTRUE,
  VKSTR,	/* sval = string value */
  VKNUM,	/* nval = numerical value */
  VKLAST = VKNUM,
  /* Non-constant expressions follow: */
  VLOCAL,	/* info = local register */
  VUPVAL,	/* info = upvalue index */
  VGLOBAL,	/* sval = string value */
  VINDEXED,	/* info = table register, aux = index reg/byte/string const */
  VJMP,		/* info = instruction PC */
  VRELOCABLE,	/* info = instruction PC */
  VNONRELOC,	/* info = result register */
  VCALL,	/* info = instruction PC, aux = base */
  VVOID
} ExpKind;

/* Expression descriptor. */
typedef struct ExpDesc {
  union {
    struct { uint32_t info, aux; } s;
    TValue nval;
    GCstr *sval;
  } u;
  ExpKind k;
  BCPos t;  /* true condition exit list */
  BCPos f;  /* false condition exit list */
} ExpDesc;

/* Tests for expression types */
#define isK(e)		((uint32_t)((e)->k) <= VKLAST)
#define isnumK(e)	((e)->k == VKNUM)
#define isstrK(e)	((e)->k == VKSTR)
#define expnumV(e)	check_exp(isnumK((e)), numV(&(e)->u.nval))

#define hasjumps(e)	((e)->t != (e)->f)
#define isKexp(e)	(isK(e) && !hasjumps(e))
#define isnumKexp(e)	(isnumK(e) && !hasjumps(e))

#define priKk(k)	check_exp((k) <= VKTRUE, (k) - VKNIL)
#define priK(e)		priKk((e)->k)

/* Per-function linked list of blocks. */
typedef struct FuncBlock {
  struct FuncBlock *previous;  /* chain */
  BCPos breaklist;  /* list of jumps out of this loop */
  uint8_t nactvar;  /* # active locals outside the breakable structure */
  uint8_t upval;  /* true if some variable in the block is an upvalue */
  uint8_t isbreakable;  /* true if `block' is a loop */
} FuncBlock;

typedef struct UpValDesc {
  uint8_t k;
  uint8_t info;
} UpValDesc;

/* Per-function state. */
typedef struct FuncState {
  GCproto *pt;  /* current function header */
  GCtab *kt;  /* table to find (and reuse) elements in `k' */
  struct FuncState *prev;  /* enclosing function */
  struct LexState *ls;  /* lexical state */
  struct lua_State *L;  /* copy of the Lua state */
  struct FuncBlock *bl;  /* chain of current blocks */
  BCPos pc;  /* next bytecode position */
  BCPos lasttarget;  /* PC of last jump target */
  BCPos jpc;  /* list of pending jumps to PC */
  BCReg freereg;  /* first free register */
  BCReg nkn, nkgc;  /* number of lua_Number/GCobj constants */
  uint16_t nlocvars;  /* number of elements in `locvars' */
  uint8_t nactvar;  /* number of active local variables */
  uint8_t nuv;  /* number of upvalues */
  UpValDesc upvalues[LJ_MAX_UPVAL];  /* upvalues */
  uint16_t actvar[LJ_MAX_LOCVAR];  /* declared-variable stack */
} FuncState;

/* Binary and unary operators. ORDER OPR */
typedef enum BinOpr {
  OPR_ADD, OPR_SUB, OPR_MUL, OPR_DIV, OPR_MOD, OPR_POW,  /* ORDER ARITH */
  OPR_CONCAT,
  OPR_NE, OPR_EQ,
  OPR_LT, OPR_GE, OPR_LE, OPR_GT,
  OPR_AND, OPR_OR,
  OPR_NOBINOPR
} BinOpr;

LJ_STATIC_ASSERT((int)BC_ISGE-(int)BC_ISLT == (int)OPR_GE-(int)OPR_LT);
LJ_STATIC_ASSERT((int)BC_ISLE-(int)BC_ISLT == (int)OPR_LE-(int)OPR_LT);
LJ_STATIC_ASSERT((int)BC_ISGT-(int)BC_ISLT == (int)OPR_GT-(int)OPR_LT);
LJ_STATIC_ASSERT((int)BC_SUBVV-(int)BC_ADDVV == (int)OPR_SUB-(int)OPR_ADD);
LJ_STATIC_ASSERT((int)BC_MULVV-(int)BC_ADDVV == (int)OPR_MUL-(int)OPR_ADD);
LJ_STATIC_ASSERT((int)BC_DIVVV-(int)BC_ADDVV == (int)OPR_DIV-(int)OPR_ADD);
LJ_STATIC_ASSERT((int)BC_MODVV-(int)BC_ADDVV == (int)OPR_MOD-(int)OPR_ADD);

typedef enum UnOpr { OPR_MINUS, OPR_NOT, OPR_LEN, OPR_NOUNOPR } UnOpr;

/* -- Error handling ------------------------------------------------------ */

LJ_NORET LJ_NOINLINE static void err_syntax(LexState *ls, ErrMsg em)
{
  lj_lex_error(ls, ls->token, em);
}

LJ_NORET LJ_NOINLINE static void err_token(LexState *ls, LexToken token)
{
  lj_lex_error(ls, ls->token, LJ_ERR_XTOKEN, lj_lex_token2str(ls, token));
}

LJ_NORET static void err_limit(FuncState *fs, uint32_t limit, const char *what)
{
  if (fs->pt->linedefined == 0)
    lj_lex_error(fs->ls, 0, LJ_ERR_XLIMM, limit, what);
  else
    lj_lex_error(fs->ls, 0, LJ_ERR_XLIMF, fs->pt->linedefined, limit, what);
}

#define checklimit(fs, v, l, m)		if ((v) >= (l)) err_limit(fs, l, m)
#define checklimitgt(fs, v, l, m)	if ((v) > (l)) err_limit(fs, l, m)
#define checkcond(ls, c, em)		{ if (!(c)) err_syntax(ls, em); }

/* -- Code emitter: branches ---------------------------------------------- */

static BCPos getjump(FuncState *fs, BCPos pc)
{
  ptrdiff_t delta = bc_j(fs->pt->bc[pc]);
  if ((BCPos)delta == NO_JMP)
    return NO_JMP;
  else
    return (BCPos)(((ptrdiff_t)pc+1)+delta);
}

static int need_value(FuncState *fs, BCPos list)
{
  for (; list != NO_JMP; list = getjump(fs, list)) {
    BCOp op = bc_op(fs->pt->bc[list >= 1 ? list-1 : list]);
    if (!(op == BC_ISTC || op == BC_ISFC)) return 1;
  }
  return 0;  /* Not found. */
}

static int patchtestreg(FuncState *fs, BCPos pc, BCReg reg)
{
  BCIns *i = &fs->pt->bc[pc >= 1 ? pc-1 : pc];
  BCOp op = bc_op(*i);
  if (!(op == BC_ISTC || op == BC_ISFC))
    return 0;  /* cannot patch other instructions */
  if (reg != NO_REG && reg != bc_d(*i)) {
    setbc_a(i, reg);
  } else {  /* no register to put value or register already has the value */
    setbc_op(i, op+(BC_IST-BC_ISTC));
    setbc_a(i, 0);
  }
  return 1;
}

static void removevalues(FuncState *fs, BCPos list)
{
  for (; list != NO_JMP; list = getjump(fs, list))
    patchtestreg(fs, list, NO_REG);
}

static void fixjump(FuncState *fs, BCPos pc, BCPos dest)
{
  BCIns *jmp = &fs->pt->bc[pc];
  BCPos offset = dest-(pc+1)+BCBIAS_J;
  lua_assert(dest != NO_JMP);
  if (offset > BCMAX_D)
    err_syntax(fs->ls, LJ_ERR_XJUMP);
  setbc_d(jmp, offset);
}

static void concatjumps(FuncState *fs, BCPos *l1, BCPos l2)
{
  if (l2 == NO_JMP) return;
  else if (*l1 == NO_JMP) {
    *l1 = l2;
  } else {
    BCPos list = *l1;
    BCPos next;
    while ((next = getjump(fs, list)) != NO_JMP)  /* find last element */
      list = next;
    fixjump(fs, list, l2);
  }
}

static void patchlistaux(FuncState *fs, BCPos list, BCPos vtarget,
			 BCReg reg, BCPos dtarget)
{
  while (list != NO_JMP) {
    BCPos next = getjump(fs, list);
    if (patchtestreg(fs, list, reg))
      fixjump(fs, list, vtarget);
    else
      fixjump(fs, list, dtarget);  /* jump to default target */
    list = next;
  }
}

static void patchtohere(FuncState *fs, BCPos list)
{
  fs->lasttarget = fs->pc;
  concatjumps(fs, &fs->jpc, list);
}

static void patchlist(FuncState *fs, BCPos list, BCPos target)
{
  if (target == fs->pc) {
    patchtohere(fs, list);
  } else {
    lua_assert(target < fs->pc);
    patchlistaux(fs, list, target, NO_REG, target);
  }
}

/* -- Code emitter: instructions ------------------------------------------ */

static BCPos emitINS(FuncState *fs, BCIns i)
{
  GCproto *pt;
  patchlistaux(fs, fs->jpc, fs->pc, NO_REG, fs->pc);
  fs->jpc = NO_JMP;
  pt = fs->pt;
  if (LJ_UNLIKELY(fs->pc >= pt->sizebc)) {
    checklimit(fs, fs->pc, LJ_MAX_BCINS, "bytecode instructions");
    lj_mem_growvec(fs->L, pt->bc, pt->sizebc, LJ_MAX_BCINS, BCIns);
    lj_mem_growvec(fs->L, pt->lineinfo, pt->sizelineinfo, LJ_MAX_BCINS, BCLine);
  }
  pt->bc[fs->pc] = i;
  pt->lineinfo[fs->pc] = fs->ls->lastline;
  return fs->pc++;
}

#define emitABC(fs, o, a, b, c)	emitINS(fs, BCINS_ABC(o, a, b, c))
#define emitAD(fs, o, a, d)	emitINS(fs, BCINS_AD(o, a, d))
#define emitAJ(fs, o, a, j)	emitINS(fs, BCINS_AJ(o, a, j))

#define bcptr(fs, e)		(&(fs)->pt->bc[(e)->u.s.info])

static BCPos emit_jump(FuncState *fs)
{
  BCPos jpc = fs->jpc;  /* save list of jumps to here */
  BCPos j = fs->pc - 1;
  fs->jpc = NO_JMP;
  if ((int32_t)j >= (int32_t)fs->lasttarget && bc_op(fs->pt->bc[j]) == BC_UCLO)
    setbc_j(&fs->pt->bc[j], NO_JMP);
  else
    j = emitAJ(fs, BC_JMP, fs->freereg, NO_JMP);
  concatjumps(fs, &j, jpc);  /* keep them on hold */
  return j;
}

/* -- Code emitter: constants --------------------------------------------- */

static BCReg numK(FuncState *fs, ExpDesc *e)
{
  lua_State *L = fs->L;
  TValue *val;
  lua_assert(isnumK(e));
  val = lj_tab_set(L, fs->kt, &e->u.nval);
  if (tvisnum(val))
    return val->u32.lo;
  val->u64 = fs->nkn;
  return fs->nkn++;
}

static BCReg gcK(FuncState *fs, GCobj *gc, int itype)
{
  lua_State *L = fs->L;
  TValue o, *val;
  setgcV(L, &o, &gc->gch, itype);
  val = lj_tab_set(L, fs->kt, &o);
  if (tvisnum(val))
    return val->u32.lo;
  val->u64 = fs->nkgc;
  return fs->nkgc++;
}

static BCReg strK(FuncState *fs, ExpDesc *e)
{
  lua_assert(isstrK(e) || e->k == VGLOBAL);
  return gcK(fs, obj2gco(e->u.sval), LJ_TSTR);
}

GCstr *lj_parse_keepstr(LexState *ls, const char *str, size_t len)
{
  lua_State *L = ls->L;
  GCstr *s = lj_str_new(L, str, len);
  TValue *tv = lj_tab_setstr(L, ls->fs->kt, s);
  if (tvisnil(tv)) setboolV(tv, 1);  /* Anchor string to avoid GC. */
  return s;
}

static void keep_token(LexState *ls)
{
  if (ls->token == TK_name || ls->token == TK_string) {
    TValue *tv = lj_tab_setstr(ls->L, ls->fs->kt, strV(&ls->tokenval));
    if (tvisnil(tv)) setboolV(tv, 1);  /* Anchor string to avoid GC. */
  }
}

static void nilK(FuncState *fs, BCReg from, BCReg n)
{
  BCIns *pr;
  if (fs->pc > fs->lasttarget) {  /* no jumps to current position? */
    BCReg pfrom, pto;
    pr = &fs->pt->bc[fs->pc-1];
    pfrom = bc_a(*pr);
    switch (bc_op(*pr)) {
    case BC_KPRI:
      if (bc_d(*pr) != ~LJ_TNIL) break;
      if (from == pfrom) {
	if (n == 1) return;
      } else if (from == pfrom+1) {
	from = pfrom;
	n++;
      } else {
	break;
      }
      fs->pc--;
      break;
    case BC_KNIL:
      pto = bc_d(*pr);
      if (pfrom <= from && from <= pto+1) {  /* can connect both? */
	if (from+n-1 > pto)
	  setbc_d(pr, from+n-1);
	return;
      }
      break;
    default:
      break;
    }
  }
  emitINS(fs, n == 1 ? BCINS_AD(BC_KPRI, from, priKk(VKNIL))
		     : BCINS_AD(BC_KNIL, from, from+n-1));
}

/* -- Code emitter: registers --------------------------------------------- */

static void checkframe(FuncState *fs, BCReg n)
{
  BCReg sz = fs->freereg + n;
  if (sz > fs->pt->framesize) {
    if (sz >= LJ_MAX_SLOTS)
      err_syntax(fs->ls, LJ_ERR_XSLOTS);
    fs->pt->framesize = cast_byte(sz);
  }
}

static void reserveregs(FuncState *fs, BCReg n)
{
  checkframe(fs, n);
  fs->freereg += n;
}

static void freereg(FuncState *fs, BCReg reg)
{
  if (reg >= fs->nactvar) {
    fs->freereg--;
    lua_assert(reg == fs->freereg);
  }
}

static void freeexp(FuncState *fs, ExpDesc *e)
{
  if (e->k == VNONRELOC)
    freereg(fs, e->u.s.info);
}

/* -- Code emitter: expressions ------------------------------------------- */

static void dischargevars(FuncState *fs, ExpDesc *e)
{
  BCIns ins;
  switch (e->k) {
  case VUPVAL:
    ins = BCINS_AD(BC_UGET, 0, e->u.s.info);
    break;
  case VGLOBAL:
    ins = BCINS_AD(BC_GGET, 0, strK(fs, e));
    break;
  case VINDEXED: {
    /* TGET[VSB] key = reg, string const or byte const */
    BCReg rc = e->u.s.aux;
    if ((int32_t)rc < 0) {
      ins = BCINS_ABC(BC_TGETS, 0, e->u.s.info, ~rc);
    } else if (rc > BCMAX_C) {
      ins = BCINS_ABC(BC_TGETB, 0, e->u.s.info, rc-(BCMAX_C+1));
    } else {
      freereg(fs, rc);
      ins = BCINS_ABC(BC_TGETV, 0, e->u.s.info, rc);
    }
    freereg(fs, e->u.s.info);
    break;
    }
  case VCALL:
    e->u.s.info = e->u.s.aux;
    /* fallthrough */
  case VLOCAL:
    e->k = VNONRELOC;
    /* fallthrough */
  default:
    return;
  }
  e->u.s.info = emitINS(fs, ins);
  e->k = VRELOCABLE;
}

static void discharge2reg(FuncState *fs, ExpDesc *e, BCReg reg)
{
  BCIns ins;
  dischargevars(fs, e);
  switch (e->k) {
  case VKNIL: case VKFALSE:  case VKTRUE:
    ins = BCINS_AD(BC_KPRI, reg, priK(e));
    break;
  case VKSTR:
    ins = BCINS_AD(BC_KSTR, reg, strK(fs, e));
    break;
  case VKNUM: {
    lua_Number n = expnumV(e);
    int32_t k = lj_num2int(n);
    if (checki16(k) && n == cast_num(k))
      ins = BCINS_AD(BC_KSHORT, reg, (BCReg)(uint16_t)k);
    else
      ins = BCINS_AD(BC_KNUM, reg, numK(fs, e));
    break;
    }
  case VRELOCABLE:
    setbc_a(bcptr(fs, e), reg);
    goto noins;
  case VNONRELOC:
    if (reg == e->u.s.info)
      goto noins;
    ins = BCINS_AD(BC_MOV, reg, e->u.s.info);
    break;
  default:
    lua_assert(e->k == VVOID || e->k == VJMP);
    return;  /* nothing to do... */
  }
  emitINS(fs, ins);
noins:
  e->u.s.info = reg;
  e->k = VNONRELOC;
}

static void exp2reg(FuncState *fs, ExpDesc *e, BCReg reg)
{
  discharge2reg(fs, e, reg);
  if (e->k == VJMP)
    concatjumps(fs, &e->t, e->u.s.info);  /* put this jump in `t' list */
  if (hasjumps(e)) {
    BCPos final;  /* position after whole expression */
    BCPos p_f = NO_JMP;  /* position of an eventual LOAD false */
    BCPos p_t = NO_JMP;  /* position of an eventual LOAD true */
    if (need_value(fs, e->t) || need_value(fs, e->f)) {
      BCPos fj = (e->k == VJMP) ? NO_JMP : emit_jump(fs);
      p_f = emitAD(fs, BC_KPRI, reg, priKk(VKFALSE));
      emitAJ(fs, BC_JMP, fs->freereg, 1);
      p_t = emitAD(fs, BC_KPRI, reg, priKk(VKTRUE));
      patchtohere(fs, fj);
    }
    final = fs->pc;
    fs->lasttarget = final;
    patchlistaux(fs, e->f, final, reg, p_f);
    patchlistaux(fs, e->t, final, reg, p_t);
  }
  e->f = e->t = NO_JMP;
  e->u.s.info = reg;
  e->k = VNONRELOC;
}

static void exp2nextreg(FuncState *fs, ExpDesc *e)
{
  dischargevars(fs, e);
  freeexp(fs, e);
  reserveregs(fs, 1);
  exp2reg(fs, e, fs->freereg - 1);
}

static BCReg exp2anyreg(FuncState *fs, ExpDesc *e)
{
  dischargevars(fs, e);
  if (e->k == VNONRELOC) {
    if (!hasjumps(e)) return e->u.s.info;  /* exp is already in a register */
    if (e->u.s.info >= fs->nactvar) {  /* reg. is not a local? */
      exp2reg(fs, e, e->u.s.info);  /* put value on it */
      return e->u.s.info;
    }
  }
  exp2nextreg(fs, e);  /* default */
  return e->u.s.info;
}

static void exp2val(FuncState *fs, ExpDesc *e)
{
  if (hasjumps(e))
    exp2anyreg(fs, e);
  else
    dischargevars(fs, e);
}

static void storevar(FuncState *fs, ExpDesc *var, ExpDesc *e)
{
  BCIns ins;
  switch (var->k) {
  case VLOCAL:
    freeexp(fs, e);
    exp2reg(fs, e, var->u.s.info);
    return;
  case VUPVAL:
    exp2val(fs, e);
    switch (e->k) {
    case VKNIL: case VKFALSE: case VKTRUE:
      ins = BCINS_AD(BC_USETP, var->u.s.info, priK(e));
      break;
    case VKSTR:
      ins = BCINS_AD(BC_USETS, var->u.s.info, strK(fs, e));
      break;
    case VKNUM:
      ins = BCINS_AD(BC_USETN, var->u.s.info, numK(fs, e));
      break;
    default:
      ins = BCINS_AD(BC_USETV, var->u.s.info, exp2anyreg(fs, e));
      break;
    }
    break;
  case VGLOBAL: {
    BCReg ra = exp2anyreg(fs, e);
    ins = BCINS_AD(BC_GSET, ra, strK(fs, var));
    break;
    }
  case VINDEXED: {
    /* TSET[VSB] key = reg, string const or byte const */
    BCReg ra = exp2anyreg(fs, e);
    BCReg rc = var->u.s.aux;
    if ((int32_t)rc < 0) {
      ins = BCINS_ABC(BC_TSETS, ra, var->u.s.info, ~rc);
    } else if (rc > BCMAX_C) {
      ins = BCINS_ABC(BC_TSETB, ra, var->u.s.info, rc-(BCMAX_C+1));
    } else {
      /* Free late alloced key reg to avoid assert on free of value reg. */
      /* This can only happen when called from constructor(). */
      lua_assert(e->k != VNONRELOC || ra < fs->nactvar ||
		 rc < ra || (freereg(fs, rc),1));
      ins = BCINS_ABC(BC_TSETV, ra, var->u.s.info, rc);
    }
    break;
    }
  default:
    lua_assert(0);  /* invalid var kind to store */
    return;
  }
  emitINS(fs, ins);
  freeexp(fs, e);
}

static void indexexp(FuncState *fs, ExpDesc *t, ExpDesc *e)
{
  /* already called: exp2val(fs, e) */
  t->k = VINDEXED;
  if (isnumK(e)) {
    lua_Number n = expnumV(e);
    int32_t k = lj_num2int(n);
    if (checku8(k) && n == cast_num(k)) {
      t->u.s.aux = BCMAX_C+1+(uint32_t)k;  /* 256..511: const byte key */
      return;
    }
  } else if (isstrK(e)) {
    BCReg idx = strK(fs, e);
    if (idx <= BCMAX_C) {
      t->u.s.aux = ~idx;  /* -256..-1: const string key */
      return;
    }
  }
  t->u.s.aux = exp2anyreg(fs, e);  /* 0..255: register */
}

static void methodexp(FuncState *fs, ExpDesc *e, ExpDesc *key)
{
  BCReg idx, func, tab = exp2anyreg(fs, e);
  freeexp(fs, e);
  func = fs->freereg;
  emitAD(fs, BC_MOV, func+1, tab);
  lua_assert(isstrK(key));
  idx = strK(fs, key);
  if (idx <= BCMAX_C) {
    reserveregs(fs, 2);
    emitABC(fs, BC_TGETS, func, tab, idx);
  } else {
    reserveregs(fs, 3);
    emitAD(fs, BC_KSTR, func+2, idx);
    emitABC(fs, BC_TGETV, func, tab, func+2);
    fs->freereg--;
  }
  e->u.s.info = func;
  e->k = VNONRELOC;
}

/* -- Code emitter: conditionals ------------------------------------------ */

static void invertjump(FuncState *fs, ExpDesc *e)
{
  BCIns *i = bcptr(fs, e) - 1;
  setbc_op(i, bc_op(*i)^1);
}

static BCPos jumponcond(FuncState *fs, ExpDesc *e, int cond)
{
  if (e->k == VRELOCABLE) {
    BCIns *i = bcptr(fs, e);
    if (bc_op(*i) == BC_NOT) {
      *i = BCINS_AD(cond ? BC_ISF : BC_IST, 0, bc_d(*i));
      return emit_jump(fs);
    }
    /* else go through */
  }
  if (e->k != VNONRELOC) {
    reserveregs(fs, 1);
    discharge2reg(fs, e, fs->freereg-1);
  }
  freeexp(fs, e);
  emitAD(fs, cond ? BC_ISTC : BC_ISFC, NO_REG, e->u.s.info);
  return emit_jump(fs);
}

static void goiftrue(FuncState *fs, ExpDesc *e)
{
  BCPos pc;  /* PC of last jump. */
  dischargevars(fs, e);
  switch (e->k) {
  case VKSTR: case VKNUM: case VKTRUE:
    pc = NO_JMP;  /* always true; do nothing */
    break;
  case VJMP:
    invertjump(fs, e);
    pc = e->u.s.info;
    break;
  case VKFALSE:
    if (!hasjumps(e)) {
      pc = emit_jump(fs);  /* always jump */
      break;
    }
    /* fallthrough */
  default:
    pc = jumponcond(fs, e, 0);
    break;
  }
  concatjumps(fs, &e->f, pc);  /* insert last jump in `f' list */
  patchtohere(fs, e->t);
  e->t = NO_JMP;
}

static void goiffalse(FuncState *fs, ExpDesc *e)
{
  BCPos pc;  /* PC of last jump. */
  dischargevars(fs, e);
  switch (e->k) {
  case VKNIL: case VKFALSE:
    pc = NO_JMP;  /* always false; do nothing */
    break;
  case VJMP:
    pc = e->u.s.info;
    break;
  case VKTRUE:
    if (!hasjumps(e)) {
      pc = emit_jump(fs);  /* always jump */
      break;
    }
    /* fallthrough */
  default:
    pc = jumponcond(fs, e, 1);
    break;
  }
  concatjumps(fs, &e->t, pc);  /* insert last jump in `t' list */
  patchtohere(fs, e->f);
  e->f = NO_JMP;
}

/* -- Code emitter: operators --------------------------------------------- */

static int foldarith(BinOpr opr, ExpDesc *e1, ExpDesc *e2)
{
  TValue o;
  if (!isnumKexp(e1) || !isnumKexp(e2)) return 0;
  setnumV(&o, lj_vm_foldarith(expnumV(e1), expnumV(e2), (int)opr-OPR_ADD));
  if (tvisnan(&o) || tvismzero(&o)) return 0;  /* Avoid NaN and -0 as consts. */
  setnumV(&e1->u.nval, numV(&o));
  return 1;
}

static void codearith(FuncState *fs, BinOpr opr, ExpDesc *e1, ExpDesc *e2)
{
  BCReg rb, rc, t;
  uint32_t op;
  if (foldarith(opr, e1, e2))
    return;
  if (opr == OPR_POW) {
    op = BC_POW;
    rc = exp2anyreg(fs, e2);
    rb = exp2anyreg(fs, e1);
  } else {
    op = opr-OPR_ADD+BC_ADDVV;
    /* must discharge 2nd operand first since VINDEXED might free regs */
    exp2val(fs, e2);
    if (isnumK(e2) && (rc = numK(fs, e2)) <= BCMAX_C)
      op -= BC_ADDVV-BC_ADDVN;
    else
      rc = exp2anyreg(fs, e2);
    /* emit_prebinop discharges 1st operand, but may need to use KNUM/KSHORT */
    lua_assert(isnumK(e1) || e1->k == VNONRELOC);
    exp2val(fs, e1);
    /* avoid two consts to satisfy bytecode constraints */
    if (isnumK(e1) && !isnumK(e2) && (t = numK(fs, e1)) <= BCMAX_B) {
      rb = rc; rc = t; op -= BC_ADDVV-BC_ADDNV;
    } else {
      rb = exp2anyreg(fs, e1);
    }
  }
  /* using freeexp might cause asserts if the order is wrong */
  if (e1->k == VNONRELOC && e1->u.s.info >= fs->nactvar) fs->freereg--;
  if (e2->k == VNONRELOC && e2->u.s.info >= fs->nactvar) fs->freereg--;
  e1->u.s.info = emitABC(fs, op, 0, rb, rc);
  e1->k = VRELOCABLE;
}

static void codecomp(FuncState *fs, BinOpr opr, ExpDesc *e1, ExpDesc *e2)
{
  ExpDesc *eret = e1;
  BCIns ins;
  exp2val(fs, e1);
  if (opr == OPR_EQ || opr == OPR_NE) {
    BCOp op = opr == OPR_EQ ? BC_ISEQV : BC_ISNEV;
    BCReg ra;
    if (isK(e1)) { e1 = e2; e2 = eret; }  /* need constant in 2nd arg */
    ra = exp2anyreg(fs, e1);  /* first arg must be in a reg */
    exp2val(fs, e2);
    switch (e2->k) {
    case VKNIL: case VKFALSE: case VKTRUE:
      ins = BCINS_AD(op+(BC_ISEQP-BC_ISEQV), ra, priK(e2));
      break;
    case VKSTR:
      ins = BCINS_AD(op+(BC_ISEQS-BC_ISEQV), ra, strK(fs, e2));
      break;
    case VKNUM:
      ins = BCINS_AD(op+(BC_ISEQN-BC_ISEQV), ra, numK(fs, e2));
      break;
    default:
      ins = BCINS_AD(op, ra, exp2anyreg(fs, e2));
      break;
    }
  } else {
    uint32_t op = opr-OPR_LT+BC_ISLT;
    BCReg ra;
    if ((op-BC_ISLT) & 1) {  /* GT -> LT, GE -> LE */
      e1 = e2; e2 = eret;  /* swap operands */
      op = ((op-BC_ISLT)^3)+BC_ISLT;
    }
    ra = exp2anyreg(fs, e1);
    ins = BCINS_AD(op, ra, exp2anyreg(fs, e2));
  }
  /* using freeexp might cause asserts if the order is wrong */
  if (e1->k == VNONRELOC && e1->u.s.info >= fs->nactvar) fs->freereg--;
  if (e2->k == VNONRELOC && e2->u.s.info >= fs->nactvar) fs->freereg--;
  emitINS(fs, ins);
  eret->u.s.info = emit_jump(fs);
  eret->k = VJMP;
}

static void emit_unop(FuncState *fs, UnOpr uop, ExpDesc *e)
{
  BCOp op = BC_LEN;
  switch (uop) {
  case OPR_MINUS:
    if (isnumKexp(e) && expnumV(e) != 0) {  /* Avoid const-folding to -0. */
      setnumV(&e->u.nval, -expnumV(e));
      return;
    }
    op = BC_UNM;
    /* fallthrough */
  case OPR_LEN:
    exp2anyreg(fs, e);
    break;
  case OPR_NOT:
    /* interchange true and false lists */
    { BCPos temp = e->f; e->f = e->t; e->t = temp; }
    removevalues(fs, e->f);
    removevalues(fs, e->t);
    dischargevars(fs, e);
    switch (e->k) {
    case VKNIL: case VKFALSE:
      e->k = VKTRUE;
      return;
    case VKSTR: case VKNUM: case VKTRUE:
      e->k = VKFALSE;
      return;
    case VJMP:
      invertjump(fs, e);
      return;
    case VRELOCABLE:
      reserveregs(fs, 1);
      setbc_a(bcptr(fs, e), fs->freereg-1);
      e->u.s.info = fs->freereg-1;
      e->k = VNONRELOC;
      break;
    case VNONRELOC:
      break;
    default: lua_assert(0); return;
    }
    op = BC_NOT;
    break;
  default: lua_assert(0); return;
  }
  freeexp(fs, e);
  e->u.s.info = emitAD(fs, op, 0, e->u.s.info);
  e->k = VRELOCABLE;
}

static void prepare_binop(FuncState *fs, BinOpr op, ExpDesc *e)
{
  switch (op) {
  case OPR_AND:
    goiftrue(fs, e);
    break;
  case OPR_OR:
    goiffalse(fs, e);
    break;
  case OPR_CONCAT:
    exp2nextreg(fs, e);  /* operand must be on the `stack' */
    break;
  case OPR_EQ: case OPR_NE:
    if (!isKexp(e)) exp2anyreg(fs, e);
    break;
  default:
    if (!isnumKexp(e)) exp2anyreg(fs, e);
    break;
  }
}

static void emit_binop(FuncState *fs, BinOpr op, ExpDesc *e1, ExpDesc *e2)
{
  switch (op) {
  case OPR_AND:
    lua_assert(e1->t == NO_JMP);  /* list must be closed */
    dischargevars(fs, e2);
    concatjumps(fs, &e2->f, e1->f);
    *e1 = *e2;
    break;
  case OPR_OR:
    lua_assert(e1->f == NO_JMP);  /* list must be closed */
    dischargevars(fs, e2);
    concatjumps(fs, &e2->t, e1->t);
    *e1 = *e2;
    break;
  case OPR_CONCAT:
    exp2val(fs, e2);
    if (e2->k == VRELOCABLE && bc_op(*bcptr(fs, e2)) == BC_CAT) {
      lua_assert(e1->u.s.info == bc_b(*bcptr(fs, e2))-1);
      freeexp(fs, e1);
      setbc_b(bcptr(fs, e2), e1->u.s.info);
      e1->u.s.info = e2->u.s.info;
    } else {
      exp2nextreg(fs, e2);
      freeexp(fs, e2);
      freeexp(fs, e1);
      e1->u.s.info = emitABC(fs, BC_CAT, 0, e1->u.s.info, e2->u.s.info);
    }
    e1->k = VRELOCABLE;
    break;
  case OPR_ADD: case OPR_SUB: case OPR_MUL:
  case OPR_DIV: case OPR_MOD: case OPR_POW:
    codearith(fs, op, e1, e2);
    break;
  case OPR_EQ: case OPR_NE:
  case OPR_LT: case OPR_LE: case OPR_GT: case OPR_GE:
    codecomp(fs, op, e1, e2);
    break;
  default: lua_assert(0); break;
  }
}

/* -- Lexer support ------------------------------------------------------- */

static int testnext(LexState *ls, LexToken tok)
{
  if (ls->token == tok) {
    lj_lex_next(ls);
    return 1;
  }
  return 0;
}

static void checknext(LexState *ls, LexToken tok)
{
  if (ls->token != tok)
    err_token(ls, tok);
  lj_lex_next(ls);
}

static void checkmatch(LexState *ls, LexToken what, LexToken who, BCLine line)
{
  if (!testnext(ls, what)) {
    if (line == ls->linenumber) {
      err_token(ls, what);
    } else {
      const char *swhat = lj_lex_token2str(ls, what);
      const char *swho = lj_lex_token2str(ls, who);
      lj_lex_error(ls, ls->token, LJ_ERR_XMATCH, swhat, swho, line);
    }
  }
}

static GCstr *str_checkname(LexState *ls)
{
  GCstr *s;
  if (ls->token != TK_name)
    err_token(ls, TK_name);
  s = strV(&ls->tokenval);
  lj_lex_next(ls);
  return s;
}

static void init_exp(ExpDesc *e, ExpKind k, uint32_t info)
{
  e->k = k;
  e->u.s.info = info;
  e->f = e->t = NO_JMP;
}

static void checkname(LexState *ls, ExpDesc *e)
{
  init_exp(e, VKSTR, 0);
  e->u.sval = str_checkname(ls);
}

/* -- Variable handling --------------------------------------------------- */

#define getlocvar(fs, i)	((fs)->pt->varinfo[(fs)->actvar[(i)]])

static BCReg registerlocalvar(LexState *ls, GCstr *name)
{
  FuncState *fs = ls->fs;
  GCproto *pt = fs->pt;
  if (LJ_UNLIKELY(fs->nlocvars >= pt->sizevarinfo)) {
    MSize oldsize = pt->sizevarinfo;
    checklimit(fs, fs->nlocvars, 32767, "local variables");
    lj_mem_growvec(fs->L, pt->varinfo, pt->sizevarinfo, 32767, VarInfo);
    while (oldsize < pt->sizevarinfo) pt->varinfo[oldsize++].name = NULL;
  }
  pt->varinfo[fs->nlocvars].name = name;
  lj_gc_objbarrier(ls->L, pt, name);
  return fs->nlocvars++;
}

static void new_localvar(LexState *ls, GCstr *name, BCReg n)
{
  FuncState *fs = ls->fs;
  checklimit(fs, fs->nactvar+n, LJ_MAX_LOCVAR, "local variables");
  fs->actvar[fs->nactvar+n] = cast(uint16_t, registerlocalvar(ls, name));
}

#define new_localvarliteral(ls,v,n) \
  new_localvar(ls, lj_parse_keepstr(ls, "" v, sizeof(v)-1), n)

static void adjustlocalvars(LexState *ls, BCReg nvars)
{
  FuncState *fs = ls->fs;
  fs->nactvar = cast_byte(fs->nactvar + nvars);
  for (; nvars; nvars--)
    getlocvar(fs, fs->nactvar - nvars).startpc = fs->pc;
}

static void removevars(LexState *ls, BCReg tolevel)
{
  FuncState *fs = ls->fs;
  while (fs->nactvar > tolevel)
    getlocvar(fs, --fs->nactvar).endpc = fs->pc;
}

static uint32_t indexupvalue(FuncState *fs, GCstr *name, ExpDesc *v)
{
  uint32_t i;
  GCproto *pt = fs->pt;
  for (i = 0; i < fs->nuv; i++) {
    if (fs->upvalues[i].k == v->k && fs->upvalues[i].info == v->u.s.info) {
      lua_assert(pt->uvname[i] == name);
      return i;
    }
  }
  /* Not found, create a new upvalue for this name. */
  if (LJ_UNLIKELY(fs->nuv >= pt->sizeuvname)) {
    MSize oldsize = pt->sizeuvname;
    checklimit(fs, fs->nuv, LJ_MAX_UPVAL, "upvalues");
    lj_mem_growvec(fs->L, pt->uvname, pt->sizeuvname, LJ_MAX_UPVAL, GCstr *);
    while (oldsize < pt->sizeuvname) pt->uvname[oldsize++] = NULL;
  }
  pt->uvname[fs->nuv] = name;
  lj_gc_objbarrier(fs->L, pt, name);
  lua_assert(v->k == VLOCAL || v->k == VUPVAL);
  fs->upvalues[fs->nuv].k = cast_byte(v->k);
  fs->upvalues[fs->nuv].info = cast_byte(v->u.s.info);
  return fs->nuv++;
}

static BCReg searchvar(FuncState *fs, GCstr *n)
{
  int i;
  for (i = fs->nactvar-1; i >= 0; i--) {
    if (n == getlocvar(fs, i).name)
      return (BCReg)i;
  }
  return (BCReg)-1;  /* Not found. */
}

static void markupval(FuncState *fs, BCReg level)
{
  FuncBlock *bl = fs->bl;
  while (bl && bl->nactvar > level) bl = bl->previous;
  if (bl) bl->upval = 1;
}

static int singlevaraux(FuncState *fs, GCstr *name, ExpDesc *e, int first)
{
  if (fs == NULL) {  /* no more levels? */
    init_exp(e, VGLOBAL, 0);  /* default is global variable */
    e->u.sval = name;
    return 1;
  } else {
    BCReg reg = searchvar(fs, name);  /* look up at current level */
    if ((int32_t)reg >= 0) {
      init_exp(e, VLOCAL, reg);
      if (!first)
	markupval(fs, reg);  /* local will be used as an upval */
      return 0;
    } else {  /* not found at current level; try upper one */
      if (singlevaraux(fs->prev, name, e, 0))  /* global? */
	return 1;
      e->u.s.info = indexupvalue(fs, name, e);  /* else was local or upvalue */
      e->k = VUPVAL;  /* upvalue in this level */
      return 0;
    }
  }
}

#define singlevar(ls, e) singlevaraux((ls)->fs, str_checkname(ls), (e), 1)

static void adjust_assign(LexState *ls, BCReg nvars, BCReg nexps, ExpDesc *e)
{
  FuncState *fs = ls->fs;
  int32_t extra = (int32_t)nvars - (int32_t)nexps;
  if (e->k == VCALL) {
    extra++;  /* includes call itself */
    if (extra < 0) extra = 0;
    setbc_b(bcptr(fs, e), extra+1);
    if (extra > 1) reserveregs(fs, (BCReg)extra-1);
  } else {
    if (e->k != VVOID) exp2nextreg(fs, e);  /* close last expression */
    if (extra > 0) {
      BCReg reg = fs->freereg;
      reserveregs(fs, (BCReg)extra);
      nilK(fs, reg, (BCReg)extra);
    }
  }
}

/* -- Function handling --------------------------------------------------- */

/* Forward declaration. */
static void chunk(LexState *ls);

static void open_func(LexState *ls, FuncState *fs)
{
  lua_State *L = ls->L;
  GCproto *pt = lj_func_newproto(L);
  fs->pt = pt;
  fs->prev = ls->fs;  /* linked list of funcstates */
  fs->ls = ls;
  fs->L = L;
  ls->fs = fs;
  fs->pc = 0;
  fs->lasttarget = 0;
  fs->jpc = NO_JMP;
  fs->freereg = 0;
  fs->nkgc = 0;
  fs->nkn = 0;
  fs->nlocvars = 0;
  fs->nactvar = 0;
  fs->nuv = 0;
  fs->bl = NULL;
  pt->chunkname = ls->chunkname;
  pt->framesize = 2;  /* registers 0/1 are always valid */
  fs->kt = lj_tab_new(L, 0, 0);
  /* anchor table of constants and prototype (to avoid being collected) */
  settabV(L, L->top, fs->kt);
  incr_top(L);
  setprotoV(L, L->top, pt);
  incr_top(L);
}

static void collectk(FuncState *fs, GCproto *pt)
{
  GCtab *kt;
  TValue *array;
  Node *node;
  BCReg nkgc;
  MSize i, hmask, sizek;
  GCRef *kstart;
  checklimitgt(fs, fs->nkn, BCMAX_D+1, "constants");
  checklimitgt(fs, fs->nkgc, BCMAX_D+1, "constants");
  nkgc = round_nkgc(fs->nkgc);
  sizek = (MSize)(nkgc*sizeof(MRef) + fs->nkn*sizeof(lua_Number));
  kstart = lj_mem_newt(fs->L, sizek, GCRef);
  if (nkgc) setgcrefnull(kstart[0]);  /* May be uninitialized otherwise. */
  pt->k.gc = kstart + nkgc;
  pt->sizekn = fs->nkn;
  pt->sizekgc = fs->nkgc;
  kt = fs->kt;
  array = tvref(kt->array);
  for (i = 0; i < kt->asize; i++)
    if (tvisnum(&array[i]))
      pt->k.n[array[i].u32.lo] = cast_num(i);
  node = noderef(kt->node);
  hmask = kt->hmask;
  for (i = 0; i <= hmask; i++) {
    Node *n = &node[i];
    if (tvisnum(&n->val)) {
      ptrdiff_t kidx = (ptrdiff_t)n->val.u32.lo;
      if (tvisnum(&n->key)) {
	pt->k.n[kidx] = numV(&n->key);
      } else {
	GCobj *o = gcV(&n->key);
	setgcref(pt->k.gc[~kidx], o);
	lj_gc_objbarrier(fs->L, pt, o);
      }
    }
  }
}

static void collectuv(FuncState *fs, GCproto *pt)
{
  uint32_t i;
  pt->uv = lj_mem_newvec(fs->L, fs->nuv, int16_t);
  pt->sizeuv = fs->nuv;
  for (i = 0; i < pt->sizeuv; i++) {
    uint32_t v = fs->upvalues[i].info;
    if (fs->upvalues[i].k == VUPVAL) v = ~v;
    pt->uv[i] = (int16_t)v;
  }
}

static void finalret(FuncState *fs, GCproto *pt)
{
  BCPos lastpc = fs->pc;
  if (lastpc > fs->lasttarget) {
    switch (bc_op(pt->bc[lastpc-1])) {
    case BC_CALLMT: case BC_CALLT:
    case BC_RETM: case BC_RET: case BC_RET0: case BC_RET1:
      goto suppress_return;  /* already got a return */
    default: break;
    }
  }
  if (fs->pt->flags & PROTO_HAS_FNEW)
    emitAJ(fs, BC_UCLO, 0, 0);
  emitAD(fs, BC_RET0, 0, 1);  /* final return */
suppress_return:
  /* may need to fixup returns encoded before first function was created */
  if (fs->pt->flags & PROTO_FIXUP_RETURN) {
    BCPos pc;
    for (pc = 0; pc < lastpc; pc++) {
      BCIns i = pt->bc[pc];
      BCPos offset;
      switch (bc_op(i)) {
      case BC_CALLMT: case BC_CALLT:
      case BC_RETM: case BC_RET: case BC_RET0: case BC_RET1:
	offset = emitINS(fs, i)-(pc+1)+BCBIAS_J;  /* copy return ins */
	if (offset > BCMAX_D)
	  err_syntax(fs->ls, LJ_ERR_XFIXUP);
	pt->bc[pc] = BCINS_AD(BC_UCLO, 0, offset);  /* replace w/ UCLO+branch */
	break;
      case BC_UCLO: return;  /* we're done */
      default: break;
      }
    }
  }
}

static void close_func(LexState *ls)
{
  lua_State *L = ls->L;
  FuncState *fs = ls->fs;
  GCproto *pt = fs->pt;
  removevars(ls, 0);
  finalret(fs, pt);
  lj_mem_reallocvec(L, pt->bc, pt->sizebc, fs->pc, BCIns);
  pt->sizebc = fs->pc;
  collectk(fs, pt);
  collectuv(fs, pt);
  lj_mem_reallocvec(L, pt->lineinfo, pt->sizelineinfo, fs->pc, BCLine);
  pt->sizelineinfo = fs->pc;
  lj_mem_reallocvec(L, pt->varinfo, pt->sizevarinfo, fs->nlocvars, VarInfo);
  pt->sizevarinfo = fs->nlocvars;
  lj_mem_reallocvec(L, pt->uvname, pt->sizeuvname, fs->nuv, GCstr *);
  pt->sizeuvname = fs->nuv;
  lua_assert(fs->bl == NULL);
  lj_vmevent_send(L, BC,
    setprotoV(L, L->top++, pt);
  );
  ls->fs = fs->prev;
  L->top -= 2;  /* Remove table and prototype from the stack. */
  lua_assert(ls->fs != NULL || ls->token == TK_eof);
  keep_token(ls);  /* Re-anchor last token. */
}

GCproto *lj_parse(LexState *ls)
{
  struct FuncState fs;
  ls->level = 0;
  open_func(ls, &fs);
  fs.pt->flags |= PROTO_IS_VARARG;  /* Main chunk is always a vararg func. */
  lj_lex_next(ls);  /* Read-ahead first token. */
  chunk(ls);
  if (ls->token != TK_eof)
    err_token(ls, TK_eof);
  fs.pt->lastlinedefined = ls->linenumber;
  close_func(ls);
  lua_assert(fs.prev == NULL);
  lua_assert(fs.pt->sizeuv == 0);
  lua_assert(ls->fs == NULL);
  return fs.pt;
}

/* -- Expressions --------------------------------------------------------- */

/* forward declaration */
static void expr(LexState *ls, ExpDesc *v);

static void field(LexState *ls, ExpDesc *v)
{
  /* field -> ['.' | ':'] NAME */
  FuncState *fs = ls->fs;
  ExpDesc key;
  exp2anyreg(fs, v);
  lj_lex_next(ls);  /* skip the dot or colon */
  checkname(ls, &key);
  indexexp(fs, v, &key);
}

static void yindex(LexState *ls, ExpDesc *v)
{
  /* index -> '[' expr ']' */
  lj_lex_next(ls);  /* skip the '[' */
  expr(ls, v);
  exp2val(ls->fs, v);
  checknext(ls, ']');
}

static void kexp2tv(TValue *v, ExpDesc *e)
{
  switch (e->k) {
  case VKNIL: case VKFALSE: case VKTRUE: v->it = ~(int32_t)e->k; break;
  case VKSTR:
    setgcref(v->gcr, obj2gco(e->u.sval)); v->it = LJ_TSTR; break;
  case VKNUM: setnumV(v, expnumV(e)); break;
  default: lua_assert(0); break;
  }
}

static void constructor(LexState *ls, ExpDesc *e)
{
  FuncState *fs = ls->fs;
  BCLine line = ls->linenumber;
  GCtab *t = NULL;
  int vcall = 0, needarr = 0;
  int32_t narr = 1;  /* first array index */
  uint32_t nhash = 0;  /* number of hash entries */
  BCReg freg = fs->freereg;
  BCPos pc = emitAD(fs, BC_TNEW, freg, 0);
  init_exp(e, VNONRELOC, freg);
  reserveregs(fs, 1);
  freg++;
  checknext(ls, '{');
  while (ls->token != '}') {
    ExpDesc key, val;
    vcall = 0;
    if (ls->token == '[') {
      yindex(ls, &key);  /* already calls exp2val */
      if (!isK(&key)) indexexp(fs, e, &key);
      if (isnumK(&key) && expnumV(&key) == 0) needarr = 1; else nhash++;
      checknext(ls, '=');
    } else if (ls->token == TK_name && lj_lex_lookahead(ls) == '=') {
      checkname(ls, &key);
      checknext(ls, '=');
      nhash++;
    } else {
      init_exp(&key, VKNUM, 0);
      setintV(&key.u.nval, narr);
      narr++;
      needarr = vcall = 1;
    }
    expr(ls, &val);
    if (isKexp(&val) && isK(&key) && key.k != VKNIL) {
      TValue k;
      if (!t) {  /* create template table on demand */
	BCReg kidx;
	t = lj_tab_new(fs->L, 0, 0);
	kidx = gcK(fs, obj2gco(t), LJ_TTAB);
	fs->pt->bc[pc] = BCINS_AD(BC_TDUP, freg-1, kidx);
      }
      vcall = 0;
      kexp2tv(&k, &key);
      kexp2tv(lj_tab_set(fs->L, t, &k), &val);
      if (val.k == VKSTR)
	lj_gc_objbarriert(fs->L, t, val.u.sval);
    } else {
      if (isK(&key)) indexexp(fs, e, &key);
      if (val.k != VCALL) vcall = 0;
      storevar(fs, e, &val);
    }
    fs->freereg = freg;
    if (!testnext(ls, ',') && !testnext(ls, ';')) break;
  }
  checkmatch(ls, '}', '{', line);
  if (vcall) {
    BCIns *i = &fs->pt->bc[fs->pc-1];
    ExpDesc en;
    lua_assert(bc_a(*i)==freg && bc_op(*i) == (narr>256?BC_TSETV:BC_TSETB));
    init_exp(&en, VKNUM, 0);
    setintV(&en.u.nval, narr-1);
    if (narr > 256) { fs->pc--; i--; }
    *i = BCINS_AD(BC_TSETM, freg, numK(fs, &en));
    setbc_b(i-1, 0);
  }
  if (pc == fs->pc-1) {  /* make expr relocable if possible */
    e->u.s.info = pc;
    fs->freereg--;
    e->k = VRELOCABLE;
  } else {
    e->k = VNONRELOC;  /* indexexp may have changed it */
  }
  if (!t) {  /* Construct TNEW RD: hhhhhaaaaaaaaaaa. */
    if (!needarr) narr = 0;
    else if (narr < 3) narr = 3;
    else if (narr > 0x7ff) narr = 0x7ff;
    setbc_d(&fs->pt->bc[pc], (uint32_t)narr | (hsize2hbits(nhash) << 11));
  }
}

static void parlist(LexState *ls)
{
  /* parlist -> [ param { `,' param } ] */
  FuncState *fs = ls->fs;
  GCproto *pt = fs->pt;
  BCReg nparams = 0;
  if (ls->token != ')') {  /* is `parlist' not empty? */
    do {
      switch (ls->token) {
      case TK_name:  /* param -> NAME */
	new_localvar(ls, str_checkname(ls), nparams++);
	break;
      case TK_dots:  /* param -> `...' */
	lj_lex_next(ls);
	pt->flags |= PROTO_IS_VARARG;
	break;
      default:
	err_syntax(ls, LJ_ERR_XPARAM);
	break;
      }
    } while (!(pt->flags & PROTO_IS_VARARG) && testnext(ls, ','));
  }
  adjustlocalvars(ls, nparams);
  pt->numparams = cast_byte(fs->nactvar);
  reserveregs(fs, fs->nactvar);  /* reserve register for parameters */
}

static void body(LexState *ls, ExpDesc *e, int needself, BCLine line)
{
  /* body ->  `(' parlist `)' chunk END */
  FuncState *fs, new_fs;
  BCReg kidx;
  open_func(ls, &new_fs);
  new_fs.pt->linedefined = line;
  checknext(ls, '(');
  if (needself) {
    new_localvarliteral(ls, "self", 0);
    adjustlocalvars(ls, 1);
  }
  parlist(ls);
  checknext(ls, ')');
  chunk(ls);
  new_fs.pt->lastlinedefined = ls->linenumber;
  checkmatch(ls, TK_end, TK_function, line);
  close_func(ls);
  fs = ls->fs;
  kidx = gcK(fs, obj2gco(new_fs.pt), LJ_TPROTO);
  init_exp(e, VRELOCABLE, emitAD(fs, BC_FNEW, 0, kidx));
  if (!(fs->pt->flags & PROTO_HAS_FNEW)) {
    if (fs->pt->flags & PROTO_HAS_RETURN)
      fs->pt->flags |= PROTO_FIXUP_RETURN;
    fs->pt->flags |= PROTO_HAS_FNEW;
  }
}

static BCReg explist1(LexState *ls, ExpDesc *v)
{
  /* explist1 -> expr { `,' expr } */
  BCReg n = 1;  /* at least one expression */
  expr(ls, v);
  while (testnext(ls, ',')) {
    exp2nextreg(ls->fs, v);
    expr(ls, v);
    n++;
  }
  return n;
}

static void funcargs(LexState *ls, ExpDesc *e)
{
  FuncState *fs = ls->fs;
  ExpDesc args;
  BCIns ins;
  BCReg base;
  BCLine line = ls->linenumber;
  switch (ls->token) {
    case '(': {  /* funcargs -> `(' [ explist1 ] `)' */
      if (line != ls->lastline)
	err_syntax(ls, LJ_ERR_XAMBIG);
      lj_lex_next(ls);
      if (ls->token == ')') {  /* arg list is empty? */
	args.k = VVOID;
      } else {
	explist1(ls, &args);
	if (args.k == VCALL)
	  setbc_b(bcptr(fs, &args), 0);
      }
      checkmatch(ls, ')', '(', line);
      break;
    }
    case '{': {  /* funcargs -> constructor */
      constructor(ls, &args);
      break;
    }
    case TK_string: {  /* funcargs -> STRING */
      init_exp(&args, VKSTR, 0);
      args.u.sval = strV(&ls->tokenval);
      lj_lex_next(ls);  /* must use `seminfo' before `next' */
      break;
    }
    default: {
      err_syntax(ls, LJ_ERR_XFUNARG);
      return;
    }
  }
  lua_assert(e->k == VNONRELOC);
  base = e->u.s.info;  /* base register for call */
  if (args.k == VCALL) {
    ins = BCINS_ABC(BC_CALLM, base, 2, args.u.s.aux - base - 1);
  } else {
    if (args.k != VVOID)
      exp2nextreg(fs, &args);  /* close last argument */
    ins = BCINS_ABC(BC_CALL, base, 2, fs->freereg - base);
  }
  init_exp(e, VCALL, emitINS(fs, ins));
  e->u.s.aux = base;
  fs->pt->lineinfo[fs->pc - 1] = line;
  fs->freereg = base+1;  /* call removes function and arguments and leaves
			    (unless changed) one result */
}

static void prefixexp(LexState *ls, ExpDesc *v)
{
  /* prefixexp -> NAME | '(' expr ')' */
  switch (ls->token) {
    case '(': {
      BCLine line = ls->linenumber;
      lj_lex_next(ls);
      expr(ls, v);
      checkmatch(ls, ')', '(', line);
      dischargevars(ls->fs, v);
      return;
    }
    case TK_name: {
      singlevar(ls, v);
      return;
    }
    default: {
      err_syntax(ls, LJ_ERR_XSYMBOL);
      return;
    }
  }
}

static void primaryexp(LexState *ls, ExpDesc *v)
{
  /* primaryexp ->
	prefixexp { `.' NAME | `[' exp `]' | `:' NAME funcargs | funcargs } */
  FuncState *fs = ls->fs;
  prefixexp(ls, v);
  for (;;) {
    switch (ls->token) {
      case '.':  /* field */
	field(ls, v);
	break;
      case '[': {  /* `[' exp1 `]' */
	ExpDesc key;
	exp2anyreg(fs, v);
	yindex(ls, &key);
	indexexp(fs, v, &key);
	break;
      }
      case ':': {  /* `:' NAME funcargs */
	ExpDesc key;
	lj_lex_next(ls);
	checkname(ls, &key);
	methodexp(fs, v, &key);
	funcargs(ls, v);
	break;
      }
      case '(': case TK_string: case '{':  /* funcargs */
	exp2nextreg(fs, v);
	funcargs(ls, v);
	break;
      default: return;
    }
  }
}

static void simpleexp(LexState *ls, ExpDesc *v)
{
  /* simpleexp -> NUMBER | STRING | NIL | true | false | ... |
		  constructor | FUNCTION body | primaryexp */
  switch (ls->token) {
  case TK_number:
    init_exp(v, VKNUM, 0);
    setnumV(&v->u.nval, numV(&ls->tokenval));
    break;
  case TK_string:
    init_exp(v, VKSTR, 0);
    v->u.sval = strV(&ls->tokenval);
    break;
  case TK_nil:
    init_exp(v, VKNIL, 0);
    break;
  case TK_true:
    init_exp(v, VKTRUE, 0);
    break;
  case TK_false:
    init_exp(v, VKFALSE, 0);
    break;
  case TK_dots: {  /* vararg */
    FuncState *fs = ls->fs;
    BCReg base;
    checkcond(ls, fs->pt->flags & PROTO_IS_VARARG, LJ_ERR_XDOTS);
    reserveregs(fs, 1);
    base = fs->freereg-1;
    init_exp(v, VCALL, emitABC(fs, BC_VARG, base, 2, 1));
    v->u.s.aux = base;
    break;
  }
  case '{':  /* constructor */
    constructor(ls, v);
    return;
  case TK_function:
    lj_lex_next(ls);
    body(ls, v, 0, ls->linenumber);
    return;
  default:
    primaryexp(ls, v);
    return;
  }
  lj_lex_next(ls);
}

static void enterlevel(LexState *ls)
{
  if (++ls->level >= LJ_MAX_XLEVEL)
    lj_lex_error(ls, 0, LJ_ERR_XLEVELS);
}

#define leavelevel(ls)	((ls)->level--)

static UnOpr getunopr(LexToken tok)
{
  switch (tok) {
  case TK_not: return OPR_NOT;
  case '-': return OPR_MINUS;
  case '#': return OPR_LEN;
  default: return OPR_NOUNOPR;
  }
}

static BinOpr getbinopr(LexToken tok)
{
  switch (tok) {
  case '+': return OPR_ADD;
  case '-': return OPR_SUB;
  case '*': return OPR_MUL;
  case '/': return OPR_DIV;
  case '%': return OPR_MOD;
  case '^': return OPR_POW;
  case TK_concat: return OPR_CONCAT;
  case TK_ne: return OPR_NE;
  case TK_eq: return OPR_EQ;
  case '<': return OPR_LT;
  case TK_le: return OPR_LE;
  case '>': return OPR_GT;
  case TK_ge: return OPR_GE;
  case TK_and: return OPR_AND;
  case TK_or: return OPR_OR;
  default: return OPR_NOBINOPR;
  }
}

static const struct {
  uint8_t left;  /* left priority for each binary operator */
  uint8_t right; /* right priority */
} priority[] = {  /* ORDER OPR */
  {6,6}, {6,6}, {7,7}, {7,7}, {7,7},	/* ADD SUB MUL DIV MOD */
  {10,9}, {5,4},			/* POW CONCAT (right associative) */
  {3,3}, {3,3},				/* EQ NE */
  {3,3}, {3,3}, {3,3}, {3,3},		/* LT GE GT LE */
  {2,2}, {1,1}				/* AND OR */
};

#define UNARY_PRIORITY	8  /* priority for unary operators */

/*
** subexpr -> (simpleexp | unop subexpr) { binop subexpr }
** where `binop' is any binary operator with a priority higher than `limit'
*/
static BinOpr subexpr(LexState *ls, ExpDesc *v, uint32_t limit)
{
  BinOpr op;
  UnOpr uop;
  enterlevel(ls);
  uop = getunopr(ls->token);
  if (uop != OPR_NOUNOPR) {
    lj_lex_next(ls);
    subexpr(ls, v, UNARY_PRIORITY);
    emit_unop(ls->fs, uop, v);
  } else {
    simpleexp(ls, v);
  }
  /* expand while operators have priorities higher than `limit' */
  op = getbinopr(ls->token);
  while (op != OPR_NOBINOPR && priority[op].left > limit) {
    ExpDesc v2;
    BinOpr nextop;
    lj_lex_next(ls);
    prepare_binop(ls->fs, op, v);
    /* read sub-expression with higher priority */
    nextop = subexpr(ls, &v2, priority[op].right);
    emit_binop(ls->fs, op, v, &v2);
    op = nextop;
  }
  leavelevel(ls);
  return op;  /* return first untreated operator */
}

static void expr(LexState *ls, ExpDesc *v)
{
  subexpr(ls, v, 0);
}

static BCPos condexpr(LexState *ls)
{
  /* cond -> exp */
  ExpDesc v;
  expr(ls, &v);  /* read condition */
  if (v.k == VKNIL) v.k = VKFALSE;  /* `falses' are all equal here */
  goiftrue(ls->fs, &v);
  return v.f;
}

/* -- Scope handling ------------------------------------------------------ */

static void enterblock(FuncState *fs, FuncBlock *bl, int isbreakable)
{
  bl->breaklist = NO_JMP;
  bl->isbreakable = (uint8_t)isbreakable;
  bl->nactvar = fs->nactvar;
  bl->upval = 0;
  bl->previous = fs->bl;
  fs->bl = bl;
  lua_assert(fs->freereg == fs->nactvar);
}

static void leaveblock(FuncState *fs)
{
  FuncBlock *bl = fs->bl;
  fs->bl = bl->previous;
  removevars(fs->ls, bl->nactvar);
  fs->freereg = fs->nactvar;  /* free registers */
  lua_assert(bl->nactvar == fs->nactvar);
  /* a block either controls scope or breaks (never both) */
  lua_assert(!bl->isbreakable || !bl->upval);
  if (bl->upval)
    emitAJ(fs, BC_UCLO, bl->nactvar, 0);
  else  /* avoid in upval case, it clears lasttarget and kills UCLO+JMP join */
    patchtohere(fs, bl->breaklist);
}

static void block(LexState *ls)
{
  /* block -> chunk */
  FuncState *fs = ls->fs;
  FuncBlock bl;
  enterblock(fs, &bl, 0);
  chunk(ls);
  lua_assert(bl.breaklist == NO_JMP);
  leaveblock(fs);
}

/* -- Statements ---------------------------------------------------------- */

/*
** structure to chain all variables in the left-hand side of an
** assignment
*/
struct LHS_assign {
  ExpDesc v;  /* variable (global, local, upvalue, or indexed) */
  struct LHS_assign *prev;
};

/*
** check whether, in an assignment to a local variable, the local variable
** is needed in a previous assignment (to a table). If so, save original
** local value in a safe place and use this safe copy in the previous
** assignment.
*/
static void check_conflict(LexState *ls, struct LHS_assign *lh,
			   const ExpDesc *v)
{
  FuncState *fs = ls->fs;
  BCReg reg = fs->freereg;  /* eventual position to save local variable */
  int conflict = 0;
  for (; lh; lh = lh->prev) {
    if (lh->v.k == VINDEXED) {
      if (lh->v.u.s.info == v->u.s.info) {  /* conflict? */
	conflict = 1;
	lh->v.u.s.info = reg;  /* previous assignment will use safe copy */
      }
      if (lh->v.u.s.aux == v->u.s.info) {  /* conflict? */
	conflict = 1;
	lh->v.u.s.aux = reg;  /* previous assignment will use safe copy */
      }
    }
  }
  if (conflict) {
    emitAD(fs, BC_MOV, reg, v->u.s.info);  /* make copy */
    reserveregs(fs, 1);
  }
}

static void assignment(LexState *ls, struct LHS_assign *lh, BCReg nvars)
{
  ExpDesc e;
  checkcond(ls, VLOCAL <= lh->v.k && lh->v.k <= VINDEXED, LJ_ERR_XSYNTAX);
  if (testnext(ls, ',')) {  /* assignment -> `,' primaryexp assignment */
    struct LHS_assign nv;
    nv.prev = lh;
    primaryexp(ls, &nv.v);
    if (nv.v.k == VLOCAL)
      check_conflict(ls, lh, &nv.v);
    checklimit(ls->fs, ls->level + nvars, LJ_MAX_XLEVEL, "variable names");
    assignment(ls, &nv, nvars+1);
  } else {  /* assignment -> `=' explist1 */
    BCReg nexps;
    checknext(ls, '=');
    nexps = explist1(ls, &e);
    if (nexps == nvars) {
      if (e.k == VCALL) {
	if (bc_op(*bcptr(ls->fs, &e)) == BC_VARG) {
	  ls->fs->freereg--;
	  e.k = VRELOCABLE;
	} else {
	  e.u.s.info = e.u.s.aux;
	  e.k = VNONRELOC;
	}
      }
      storevar(ls->fs, &lh->v, &e);
      return;
    }
    adjust_assign(ls, nvars, nexps, &e);
    if (nexps > nvars)
      ls->fs->freereg -= nexps - nvars;  /* remove extra values */
  }
  init_exp(&e, VNONRELOC, ls->fs->freereg-1);  /* default assignment */
  storevar(ls->fs, &lh->v, &e);
}

static void breakstat(LexState *ls)
{
  FuncState *fs = ls->fs;
  FuncBlock *bl = fs->bl;
  int upval = 0;
  while (bl && !bl->isbreakable) {
    upval |= bl->upval;
    bl = bl->previous;
  }
  if (!bl)
    err_syntax(ls, LJ_ERR_XBREAK);
  if (upval)
    emitAJ(fs, BC_UCLO, bl->nactvar, 0);
  concatjumps(fs, &bl->breaklist, emit_jump(fs));
}

static void whilestat(LexState *ls, BCLine line)
{
  /* whilestat -> WHILE cond DO block END */
  FuncState *fs = ls->fs;
  BCPos start, loop, condexit;
  FuncBlock bl;
  lj_lex_next(ls);  /* skip WHILE */
  start = fs->lasttarget = fs->pc;
  condexit = condexpr(ls);
  enterblock(fs, &bl, 1);
  checknext(ls, TK_do);
  loop = emitAD(fs, BC_LOOP, fs->nactvar, 0);
  block(ls);
  patchlist(fs, emit_jump(fs), start);
  checkmatch(ls, TK_end, TK_while, line);
  leaveblock(fs);
  patchtohere(fs, condexit);  /* false conditions finish the loop */
  fixjump(fs, loop, fs->pc);
}

static void repeatstat(LexState *ls, BCLine line)
{
  /* repeatstat -> REPEAT block UNTIL cond */
  FuncState *fs = ls->fs;
  BCPos loop = fs->lasttarget = fs->pc;
  BCPos condexit;
  FuncBlock bl1, bl2;
  enterblock(fs, &bl1, 1);  /* loop block */
  enterblock(fs, &bl2, 0);  /* scope block */
  lj_lex_next(ls);  /* skip REPEAT */
  emitAD(fs, BC_LOOP, fs->nactvar, 0);
  chunk(ls);
  checkmatch(ls, TK_until, TK_repeat, line);
  condexit = condexpr(ls);  /* read condition (inside scope block) */
  if (!bl2.upval) {  /* no upvalues? */
    leaveblock(fs);  /* finish scope */
  } else {  /* complete semantics when there are upvalues */
    breakstat(ls);  /* if condition then break */
    patchtohere(fs, condexit);  /* else... */
    leaveblock(fs);  /* finish scope... */
    condexit = emit_jump(fs);  /* and repeat */
  }
  patchlist(fs, condexit, loop);  /* close the loop */
  fixjump(fs, loop, fs->pc);
  leaveblock(fs);  /* finish loop */
}

static void exp1(LexState *ls)
{
  ExpDesc e;
  expr(ls, &e);
  exp2nextreg(ls->fs, &e);
}

static void forbody(LexState *ls, BCReg base, BCLine line, BCReg nvars,
		    int isnum)
{
  /* forbody -> DO block */
  FuncBlock bl;
  FuncState *fs = ls->fs;
  BCPos loop, loopend;
  adjustlocalvars(ls, 3);  /* control variables */
  checknext(ls, TK_do);
  loop = isnum ? emitAJ(fs, BC_FORI, base, NO_JMP) :
		 emitAJ(fs, BC_JMP, fs->freereg, NO_JMP);
  enterblock(fs, &bl, 0);  /* scope for declared variables */
  adjustlocalvars(ls, nvars);
  reserveregs(fs, nvars);
  block(ls);
  leaveblock(fs);  /* end of scope for declared variables */
  if (isnum) {
    loopend = emitAJ(fs, BC_FORL, base, NO_JMP);
    fixjump(fs, loop, fs->pc);
  } else {
    fixjump(fs, loop, fs->pc);
    emitABC(fs, BC_ITERC, base+3, nvars+1, 2+1);
    loopend = emitAJ(fs, BC_ITERL, base+3, NO_JMP);
    fs->pt->lineinfo[loopend-1] = line;
  }
  fs->pt->lineinfo[loopend] = line;  /* pretend last op starts the loop */
  fixjump(fs, loopend, loop+1);
}

static void fornum(LexState *ls, GCstr *varname, BCLine line)
{
  /* fornum -> NAME = exp1,exp1[,exp1] forbody */
  FuncState *fs = ls->fs;
  BCReg base = fs->freereg;
  new_localvarliteral(ls, "(for index)", FORL_IDX);
  new_localvarliteral(ls, "(for limit)", FORL_STOP);
  new_localvarliteral(ls, "(for step)", FORL_STEP);
  new_localvar(ls, varname, FORL_EXT);
  checknext(ls, '=');
  exp1(ls);  /* initial value */
  checknext(ls, ',');
  exp1(ls);  /* limit */
  if (testnext(ls, ',')) {
    exp1(ls);  /* optional step */
  } else {  /* default step = 1 */
    emitAD(fs, BC_KSHORT, fs->freereg, 1);
    reserveregs(fs, 1);
  }
  forbody(ls, base, line, 1, 1);
}

static void forlist(LexState *ls, GCstr *indexname)
{
  /* forlist -> NAME {,NAME} IN explist1 forbody */
  FuncState *fs = ls->fs;
  ExpDesc e;
  BCReg nvars = 0;
  BCLine line;
  BCReg base = fs->freereg;
  /* create control variables */
  new_localvarliteral(ls, "(for generator)", nvars++);
  new_localvarliteral(ls, "(for state)", nvars++);
  new_localvarliteral(ls, "(for control)", nvars++);
  /* create declared variables */
  new_localvar(ls, indexname, nvars++);
  while (testnext(ls, ','))
    new_localvar(ls, str_checkname(ls), nvars++);
  checknext(ls, TK_in);
  line = ls->linenumber;
  adjust_assign(ls, 3, explist1(ls, &e), &e);
  checkframe(fs, 3);  /* extra space to call generator */
  forbody(ls, base, line, nvars - 3, 0);
}

static void forstat(LexState *ls, BCLine line)
{
  /* forstat -> FOR (fornum | forlist) END */
  FuncState *fs = ls->fs;
  GCstr *varname;
  FuncBlock bl;
  enterblock(fs, &bl, 1);  /* scope for loop and control variables */
  lj_lex_next(ls);  /* skip `for' */
  varname = str_checkname(ls);  /* first variable name */
  switch (ls->token) {
    case '=': fornum(ls, varname, line); break;
    case ',': case TK_in: forlist(ls, varname); break;
    default: err_syntax(ls, LJ_ERR_XFOR);
  }
  checkmatch(ls, TK_end, TK_for, line);
  leaveblock(fs);  /* loop scope (`break' jumps to this point) */
}

static BCPos test_then_block(LexState *ls)
{
  /* test_then_block -> [IF | ELSEIF] cond THEN block */
  BCPos condexit;
  lj_lex_next(ls);  /* skip IF or ELSEIF */
  condexit = condexpr(ls);
  checknext(ls, TK_then);
  block(ls);  /* `then' part */
  return condexit;
}

static void ifstat(LexState *ls, BCLine line)
{
  /* ifstat -> IF cond THEN block {ELSEIF cond THEN block} [ELSE block] END */
  FuncState *fs = ls->fs;
  BCPos flist;
  BCPos escapelist = NO_JMP;
  flist = test_then_block(ls);  /* IF cond THEN block */
  while (ls->token == TK_elseif) {
    concatjumps(fs, &escapelist, emit_jump(fs));
    patchtohere(fs, flist);
    flist = test_then_block(ls);  /* ELSEIF cond THEN block */
  }
  if (ls->token == TK_else) {
    concatjumps(fs, &escapelist, emit_jump(fs));
    patchtohere(fs, flist);
    lj_lex_next(ls);  /* skip ELSE (after patch, for correct line info) */
    block(ls);  /* `else' part */
  } else {
    concatjumps(fs, &escapelist, flist);
  }
  patchtohere(fs, escapelist);
  checkmatch(ls, TK_end, TK_if, line);
}

static void localfunc(LexState *ls)
{
  ExpDesc v, b;
  FuncState *fs = ls->fs;
  new_localvar(ls, str_checkname(ls), 0);
  init_exp(&v, VLOCAL, fs->freereg);
  reserveregs(fs, 1);
  adjustlocalvars(ls, 1);
  body(ls, &b, 0, ls->linenumber);
  storevar(fs, &v, &b);
  /* debug information will only see the variable after this point! */
  getlocvar(fs, fs->nactvar - 1).startpc = fs->pc;
}

static void localstat(LexState *ls)
{
  /* stat -> LOCAL NAME {`,' NAME} [`=' explist1] */
  BCReg nvars = 0;
  BCReg nexps;
  ExpDesc e;
  do {
    new_localvar(ls, str_checkname(ls), nvars++);
  } while (testnext(ls, ','));
  if (testnext(ls, '=')) {
    nexps = explist1(ls, &e);
  } else {
    e.k = VVOID;
    nexps = 0;
  }
  adjust_assign(ls, nvars, nexps, &e);
  adjustlocalvars(ls, nvars);
}

static int func_name(LexState *ls, ExpDesc *v)
{
  /* func_name -> NAME {field} [`:' NAME] */
  int needself = 0;
  singlevar(ls, v);
  while (ls->token == '.')
    field(ls, v);
  if (ls->token == ':') {
    needself = 1;
    field(ls, v);
  }
  return needself;
}

static void funcstat(LexState *ls, BCLine line)
{
  /* funcstat -> FUNCTION func_name body */
  FuncState *fs;
  int needself;
  ExpDesc v, b;
  lj_lex_next(ls);  /* skip FUNCTION */
  needself = func_name(ls, &v);
  body(ls, &b, needself, line);
  fs = ls->fs;
  storevar(fs, &v, &b);
  fs->pt->lineinfo[fs->pc - 1] = line;
}

static void exprstat(LexState *ls)
{
  /* stat -> func | assignment */
  FuncState *fs = ls->fs;
  struct LHS_assign v;
  primaryexp(ls, &v.v);
  if (v.v.k == VCALL) {  /* stat -> func */
    setbc_b(bcptr(fs, &v.v), 1);  /* call statement uses no results */
  } else {  /* stat -> assignment */
    v.prev = NULL;
    assignment(ls, &v, 1);
  }
}

static int block_follow(LexToken token)
{
  switch (token) {
  case TK_else: case TK_elseif: case TK_end: case TK_until: case TK_eof:
    return 1;
  default:
    return 0;
  }
}

static void retstat(LexState *ls)
{
  /* stat -> RETURN explist */
  BCIns ins;
  FuncState *fs = ls->fs;
  lj_lex_next(ls);  /* skip RETURN */
  fs->pt->flags |= PROTO_HAS_RETURN;
  if (block_follow(ls->token) || ls->token == ';') {
    ins = BCINS_AD(BC_RET0, 0, 1);  /* return no values */
  } else {
    ExpDesc e;
    BCReg nret = explist1(ls, &e);  /* optional return values */
    if (nret == 1) {
      if (e.k == VCALL) {
	BCIns *i = bcptr(fs, &e);
	/* It doesn't pay off to add BC_VARGT just for 'return ...'. */
	if (bc_op(*i) == BC_VARG) goto notailcall;
	fs->pc--;
	ins = BCINS_AD(bc_op(*i)-BC_CALL+BC_CALLT, bc_a(*i), bc_c(*i));
      } else {
	ins = BCINS_AD(BC_RET1, exp2anyreg(fs, &e), 2);
      }
    } else {
      if (e.k == VCALL) {
      notailcall:
	setbc_b(bcptr(fs, &e), 0);
	ins = BCINS_AD(BC_RETM, fs->nactvar, e.u.s.aux - fs->nactvar);
      } else {
	exp2nextreg(fs, &e);  /* values must go to the `stack' */
	ins = BCINS_AD(BC_RET, fs->nactvar, nret+1);
      }
    }
  }
  if (fs->pt->flags & PROTO_HAS_FNEW)
    emitAJ(fs, BC_UCLO, 0, 0);
  emitINS(fs, ins);
}

static int statement(LexState *ls)
{
  BCLine line = ls->linenumber;  /* may be needed for error messages */
  switch (ls->token) {
  case TK_if:
    ifstat(ls, line);
    return 0;
  case TK_while:
    whilestat(ls, line);
    return 0;
  case TK_do:
    lj_lex_next(ls);  /* skip DO */
    block(ls);
    checkmatch(ls, TK_end, TK_do, line);
    return 0;
  case TK_for:
    forstat(ls, line);
    return 0;
  case TK_repeat:
    repeatstat(ls, line);
    return 0;
  case TK_function:
    funcstat(ls, line);
    return 0;
  case TK_local:
    lj_lex_next(ls);  /* skip LOCAL */
    if (testnext(ls, TK_function))  /* local function? */
      localfunc(ls);
    else
      localstat(ls);
    return 0;
  case TK_return:
    retstat(ls);
    return 1;  /* must be last statement */
  case TK_break:
    lj_lex_next(ls);  /* skip BREAK */
    breakstat(ls);
    return 1;  /* must be last statement */
  default:
    exprstat(ls);
    return 0;
  }
}

static void chunk(LexState *ls)
{
  /* chunk -> { stat [`;'] } */
  int islast = 0;
  enterlevel(ls);
  while (!islast && !block_follow(ls->token)) {
    islast = statement(ls);
    testnext(ls, ';');
    lua_assert(ls->fs->pt->framesize >= ls->fs->freereg &&
	       ls->fs->freereg >= ls->fs->nactvar);
    ls->fs->freereg = ls->fs->nactvar;  /* free registers */
  }
  leavelevel(ls);
}

