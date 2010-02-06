/*
** Lua parser (source code -> bytecode).
** Copyright (C) 2005-2010 Mike Pall. See Copyright Notice in luajit.h
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
  VKNUM,	/* nval = number value */
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
    struct {
      uint32_t info;	/* Primary info. */
      uint32_t aux;	/* Secondary info. */
    } s;
    TValue nval;	/* Number value. */
    GCstr *sval;	/* String value. */
  } u;
  ExpKind k;
  BCPos t;		/* True condition jump list. */
  BCPos f;		/* False condition jump list. */
} ExpDesc;

/* Macros for expressions. */
#define expr_hasnojump(e)	((e)->t != (e)->f)

#define expr_isk(e)		((e)->k <= VKLAST)
#define expr_isk_nojump(e)	(expr_isk(e) && !expr_hasnojump(e))
#define expr_isnumk(e)		((e)->k == VKNUM)
#define expr_isnumk_nojump(e)	(expr_isnumk(e) && !expr_hasnojump(e))
#define expr_isstrk(e)		((e)->k == VKSTR)

#define expr_numV(e)		check_exp(expr_isnumk((e)), numV(&(e)->u.nval))

/* Initialize expression. */
static LJ_AINLINE void expr_init(ExpDesc *e, ExpKind k, uint32_t info)
{
  e->k = k;
  e->u.s.info = info;
  e->f = e->t = NO_JMP;
}

/* Per-function linked list of scope blocks. */
typedef struct FuncScope {
  struct FuncScope *prev;	/* Link to outer scope. */
  BCPos breaklist;		/* Jump list for loop breaks. */
  uint8_t nactvar;		/* Number of active vars outside the scope. */
  uint8_t upval;		/* Some variable in the scope is an upvalue. */
  uint8_t isbreakable;		/* Scope is a loop and allows a break. */
} FuncScope;

/* Upvalue type and location. */
typedef struct UVLoc {
  uint8_t k;			/* Upvalue type (ExpKind). */
  uint8_t info;			/* Upvalue location (BCReg or uvidx). */
} UVLoc;

/* Per-function state. */
typedef struct FuncState {
  GCproto *pt;			/* Prototype object to be built. */
  GCtab *kt;			/* Hash table for constants. */
  LexState *ls;			/* Lexer state. */
  lua_State *L;			/* Lua state. */
  FuncScope *bl;		/* Current scope. */
  struct FuncState *prev;	/* Enclosing function. */
  BCPos pc;			/* Next bytecode position. */
  BCPos lasttarget;		/* Bytecode position of last jump target. */
  BCPos jpc;			/* Pending jump list to next bytecode. */
  BCReg freereg;		/* First free register. */
  BCReg nkn, nkgc;		/* Number of lua_Number/GCobj constants */
  BCLine linedefined;		/* First line of the function definition. */
  MSize nvarinfo;		/* Number of entries in varinfo. */
  uint8_t nactvar;		/* Number of active local variables. */
  uint8_t flags;		/* Prototype flags. */
  uint8_t framesize;		/* Fixed frame size. */
  uint8_t nuv;			/* Number of upvalues */
  uint16_t varmap[LJ_MAX_LOCVAR];  /* Map from register to varinfo index. */
  UVLoc uvloc[LJ_MAX_UPVAL];	/* Upvalue type and location. */
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
  if (fs->linedefined == 0)
    lj_lex_error(fs->ls, 0, LJ_ERR_XLIMM, limit, what);
  else
    lj_lex_error(fs->ls, 0, LJ_ERR_XLIMF, fs->linedefined, limit, what);
}

#define checklimit(fs, v, l, m)		if ((v) >= (l)) err_limit(fs, l, m)
#define checklimitgt(fs, v, l, m)	if ((v) > (l)) err_limit(fs, l, m)
#define checkcond(ls, c, em)		{ if (!(c)) err_syntax(ls, em); }

/* -- Management of constants --------------------------------------------- */

/* Return bytecode encoding for primitive constant. */
#define const_pri(e)		check_exp((e)->k <= VKTRUE, (e)->k)

/* Add a number constant. */
static BCReg const_num(FuncState *fs, ExpDesc *e)
{
  lua_State *L = fs->L;
  TValue *val;
  lua_assert(expr_isnumk(e));
  val = lj_tab_set(L, fs->kt, &e->u.nval);
  if (tvisnum(val))
    return val->u32.lo;
  val->u64 = fs->nkn;
  return fs->nkn++;
}

/* Add a GC object constant. */
static BCReg const_gc(FuncState *fs, GCobj *gc, int itype)
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

/* Add a string constant. */
static BCReg const_str(FuncState *fs, ExpDesc *e)
{
  lua_assert(expr_isstrk(e) || e->k == VGLOBAL);
  return const_gc(fs, obj2gco(e->u.sval), LJ_TSTR);
}

/* Anchor string constant to avoid GC. */
GCstr *lj_parse_keepstr(LexState *ls, const char *str, size_t len)
{
  lua_State *L = ls->L;
  GCstr *s = lj_str_new(L, str, len);
  TValue *tv = lj_tab_setstr(L, ls->fs->kt, s);
  if (tvisnil(tv)) setboolV(tv, 1);
  lj_gc_check(L);
  return s;
}

/* -- Jump list handling -------------------------------------------------- */

/* Get next element in jump list. */
static BCPos jmp_next(FuncState *fs, BCPos pc)
{
  ptrdiff_t delta = bc_j(proto_ins(fs->pt, pc));
  if ((BCPos)delta == NO_JMP)
    return NO_JMP;
  else
    return (BCPos)(((ptrdiff_t)pc+1)+delta);
}

/* Check if any of the instructions on the jump list produce no value. */
static int jmp_novalue(FuncState *fs, BCPos list)
{
  for (; list != NO_JMP; list = jmp_next(fs, list)) {
    BCOp op = bc_op(proto_ins(fs->pt, list >= 1 ? list-1 : list));
    if (!(op == BC_ISTC || op == BC_ISFC)) return 1;
  }
  return 0;
}

/* Patch register of test instructions. */
static int jmp_patchtestreg(FuncState *fs, BCPos pc, BCReg reg)
{
  BCIns *i = proto_insptr(fs->pt, pc >= 1 ? pc-1 : pc);
  BCOp op = bc_op(*i);
  if (!(op == BC_ISTC || op == BC_ISFC))
    return 0;  /* Cannot patch other instructions. */
  if (reg != NO_REG && reg != bc_d(*i)) {
    setbc_a(i, reg);
  } else {  /* Nothing to store or already in the right register. */
    setbc_op(i, op+(BC_IST-BC_ISTC));
    setbc_a(i, 0);
  }
  return 1;
}

/* Drop values for all instructions on jump list. */
static void jmp_dropval(FuncState *fs, BCPos list)
{
  for (; list != NO_JMP; list = jmp_next(fs, list))
    jmp_patchtestreg(fs, list, NO_REG);
}

/* Patch jump instruction to target. */
static void jmp_patchins(FuncState *fs, BCPos pc, BCPos dest)
{
  BCIns *jmp = proto_insptr(fs->pt, pc);
  BCPos offset = dest-(pc+1)+BCBIAS_J;
  lua_assert(dest != NO_JMP);
  if (offset > BCMAX_D)
    err_syntax(fs->ls, LJ_ERR_XJUMP);
  setbc_d(jmp, offset);
}

/* Append to jump list. */
static void jmp_append(FuncState *fs, BCPos *l1, BCPos l2)
{
  if (l2 == NO_JMP) {
    return;
  } else if (*l1 == NO_JMP) {
    *l1 = l2;
  } else {
    BCPos list = *l1;
    BCPos next;
    while ((next = jmp_next(fs, list)) != NO_JMP)  /* Find last element. */
      list = next;
    jmp_patchins(fs, list, l2);
  }
}

/* Patch jump list and preserve produced values. */
static void jmp_patchval(FuncState *fs, BCPos list, BCPos vtarget,
		       BCReg reg, BCPos dtarget)
{
  while (list != NO_JMP) {
    BCPos next = jmp_next(fs, list);
    if (jmp_patchtestreg(fs, list, reg))
      jmp_patchins(fs, list, vtarget);  /* Jump to target with value. */
    else
      jmp_patchins(fs, list, dtarget);  /* Jump to default target. */
    list = next;
  }
}

/* Jump to following instruction. Append to list of pending jumps. */
static void jmp_tohere(FuncState *fs, BCPos list)
{
  fs->lasttarget = fs->pc;
  jmp_append(fs, &fs->jpc, list);
}

/* Patch jump list to target. */
static void jmp_patch(FuncState *fs, BCPos list, BCPos target)
{
  if (target == fs->pc) {
    jmp_tohere(fs, list);
  } else {
    lua_assert(target < fs->pc);
    jmp_patchval(fs, list, target, NO_REG, target);
  }
}

/* -- Bytecode register allocator ----------------------------------------- */

/* Bump frame size. */
static void bcreg_bump(FuncState *fs, BCReg n)
{
  BCReg sz = fs->freereg + n;
  if (sz > fs->framesize) {
    if (sz >= LJ_MAX_SLOTS)
      err_syntax(fs->ls, LJ_ERR_XSLOTS);
    fs->framesize = cast_byte(sz);
  }
}

/* Reserve registers. */
static void bcreg_reserve(FuncState *fs, BCReg n)
{
  bcreg_bump(fs, n);
  fs->freereg += n;
}

/* Free register. */
static void bcreg_free(FuncState *fs, BCReg reg)
{
  if (reg >= fs->nactvar) {
    fs->freereg--;
    lua_assert(reg == fs->freereg);
  }
}

/* Free register for expression. */
static void expr_free(FuncState *fs, ExpDesc *e)
{
  if (e->k == VNONRELOC)
    bcreg_free(fs, e->u.s.info);
}

/* -- Bytecode emitter ---------------------------------------------------- */

/* Emit bytecode instruction. */
static BCPos bcemit_INS(FuncState *fs, BCIns i)
{
  GCproto *pt;
  BCIns *bc;
  BCLine *lineinfo;
  jmp_patchval(fs, fs->jpc, fs->pc, NO_REG, fs->pc);
  fs->jpc = NO_JMP;
  pt = fs->pt;
  bc = proto_bc(pt);
  lineinfo = proto_lineinfo(pt);
  if (LJ_UNLIKELY(fs->pc >= pt->sizebc)) {
    checklimit(fs, fs->pc, LJ_MAX_BCINS, "bytecode instructions");
    lj_mem_growvec(fs->L, bc, pt->sizebc, LJ_MAX_BCINS, BCIns);
    setmref(pt->bc, bc);
    lj_mem_growvec(fs->L, lineinfo, pt->sizelineinfo, LJ_MAX_BCINS, BCLine);
    setmref(pt->lineinfo, lineinfo);
  }
  bc[fs->pc] = i;
  lineinfo[fs->pc] = fs->ls->lastline;
  return fs->pc++;
}

#define bcemit_ABC(fs, o, a, b, c)	bcemit_INS(fs, BCINS_ABC(o, a, b, c))
#define bcemit_AD(fs, o, a, d)		bcemit_INS(fs, BCINS_AD(o, a, d))
#define bcemit_AJ(fs, o, a, j)		bcemit_INS(fs, BCINS_AJ(o, a, j))

#define bcptr(fs, e)			(proto_insptr((fs)->pt, (e)->u.s.info))

/* -- Bytecode emitter for expressions ------------------------------------ */

/* Discharge non-constant expression to any register. */
static void expr_discharge(FuncState *fs, ExpDesc *e)
{
  BCIns ins;
  if (e->k == VUPVAL) {
    ins = BCINS_AD(BC_UGET, 0, e->u.s.info);
  } else if (e->k == VGLOBAL) {
    ins = BCINS_AD(BC_GGET, 0, const_str(fs, e));
  } else if (e->k == VINDEXED) {
    BCReg rc = e->u.s.aux;
    if ((int32_t)rc < 0) {
      ins = BCINS_ABC(BC_TGETS, 0, e->u.s.info, ~rc);
    } else if (rc > BCMAX_C) {
      ins = BCINS_ABC(BC_TGETB, 0, e->u.s.info, rc-(BCMAX_C+1));
    } else {
      bcreg_free(fs, rc);
      ins = BCINS_ABC(BC_TGETV, 0, e->u.s.info, rc);
    }
    bcreg_free(fs, e->u.s.info);
  } else if (e->k == VCALL) {
    e->u.s.info = e->u.s.aux;
    e->k = VNONRELOC;
    return;
  } else if (e->k == VLOCAL) {
    e->k = VNONRELOC;
    return;
  } else {
    return;
  }
  e->u.s.info = bcemit_INS(fs, ins);
  e->k = VRELOCABLE;
}

/* Discharge an expression to a specific register. Ignore branches. */
static void expr_toreg_nobranch(FuncState *fs, ExpDesc *e, BCReg reg)
{
  BCIns ins;
  expr_discharge(fs, e);
  if (e->k <= VKTRUE) {
    ins = BCINS_AD(BC_KPRI, reg, const_pri(e));
  } else if (e->k == VKSTR) {
    ins = BCINS_AD(BC_KSTR, reg, const_str(fs, e));
  } else if (e->k == VKNUM) {
    lua_Number n = expr_numV(e);
    int32_t k = lj_num2int(n);
    if (checki16(k) && n == cast_num(k))
      ins = BCINS_AD(BC_KSHORT, reg, (BCReg)(uint16_t)k);
    else
      ins = BCINS_AD(BC_KNUM, reg, const_num(fs, e));
  } else if (e->k == VRELOCABLE) {
    setbc_a(bcptr(fs, e), reg);
    goto noins;
  } else if (e->k == VNONRELOC) {
    if (reg == e->u.s.info)
      goto noins;
    ins = BCINS_AD(BC_MOV, reg, e->u.s.info);
  } else {
    lua_assert(e->k == VVOID || e->k == VJMP);
    return;
  }
  bcemit_INS(fs, ins);
noins:
  e->u.s.info = reg;
  e->k = VNONRELOC;
}

/* Forward declaration. */
static BCPos bcemit_jmp(FuncState *fs);

/* Discharge an expression to a specific register. */
static void expr_toreg(FuncState *fs, ExpDesc *e, BCReg reg)
{
  expr_toreg_nobranch(fs, e, reg);
  if (e->k == VJMP)
    jmp_append(fs, &e->t, e->u.s.info);  /* Add it to the true jump list. */
  if (expr_hasnojump(e)) {  /* Discharge expression with branches. */
    BCPos jend, jfalse = NO_JMP, jtrue = NO_JMP;
    if (jmp_novalue(fs, e->t) || jmp_novalue(fs, e->f)) {
      BCPos jval = (e->k == VJMP) ? NO_JMP : bcemit_jmp(fs);
      jfalse = bcemit_AD(fs, BC_KPRI, reg, VKFALSE);
      bcemit_AJ(fs, BC_JMP, fs->freereg, 1);
      jtrue = bcemit_AD(fs, BC_KPRI, reg, VKTRUE);
      jmp_tohere(fs, jval);
    }
    jend = fs->pc;
    fs->lasttarget = jend;
    jmp_patchval(fs, e->f, jend, reg, jfalse);
    jmp_patchval(fs, e->t, jend, reg, jtrue);
  }
  e->f = e->t = NO_JMP;
  e->u.s.info = reg;
  e->k = VNONRELOC;
}

/* Discharge an expression to the next free register. */
static void expr_tonextreg(FuncState *fs, ExpDesc *e)
{
  expr_discharge(fs, e);
  expr_free(fs, e);
  bcreg_reserve(fs, 1);
  expr_toreg(fs, e, fs->freereg - 1);
}

/* Discharge an expression to any register. */
static BCReg expr_toanyreg(FuncState *fs, ExpDesc *e)
{
  expr_discharge(fs, e);
  if (e->k == VNONRELOC) {
    if (!expr_hasnojump(e)) return e->u.s.info;  /* Already in a register. */
    if (e->u.s.info >= fs->nactvar) {
      expr_toreg(fs, e, e->u.s.info);  /* Discharge to temp. register. */
      return e->u.s.info;
    }
  }
  expr_tonextreg(fs, e);  /* Discharge to next register. */
  return e->u.s.info;
}

/* Partially discharge expression to a value. */
static void expr_toval(FuncState *fs, ExpDesc *e)
{
  if (expr_hasnojump(e))
    expr_toanyreg(fs, e);
  else
    expr_discharge(fs, e);
}

/* Emit store for LHS expression. */
static void bcemit_store(FuncState *fs, ExpDesc *var, ExpDesc *e)
{
  BCIns ins;
  if (var->k == VLOCAL) {
    expr_free(fs, e);
    expr_toreg(fs, e, var->u.s.info);
    return;
  } else if (var->k == VUPVAL) {
    expr_toval(fs, e);
    if (e->k <= VKTRUE)
      ins = BCINS_AD(BC_USETP, var->u.s.info, const_pri(e));
    else if (e->k == VKSTR)
      ins = BCINS_AD(BC_USETS, var->u.s.info, const_str(fs, e));
    else if (e->k == VKNUM)
      ins = BCINS_AD(BC_USETN, var->u.s.info, const_num(fs, e));
    else
      ins = BCINS_AD(BC_USETV, var->u.s.info, expr_toanyreg(fs, e));
  } else if (var->k == VGLOBAL) {
    BCReg ra = expr_toanyreg(fs, e);
    ins = BCINS_AD(BC_GSET, ra, const_str(fs, var));
  } else {
    BCReg ra, rc;
    lua_assert(var->k == VINDEXED);
    ra = expr_toanyreg(fs, e);
    rc = var->u.s.aux;
    if ((int32_t)rc < 0) {
      ins = BCINS_ABC(BC_TSETS, ra, var->u.s.info, ~rc);
    } else if (rc > BCMAX_C) {
      ins = BCINS_ABC(BC_TSETB, ra, var->u.s.info, rc-(BCMAX_C+1));
    } else {
      /* Free late alloced key reg to avoid assert on free of value reg. */
      /* This can only happen when called from expr_table(). */
      lua_assert(e->k != VNONRELOC || ra < fs->nactvar ||
		 rc < ra || (bcreg_free(fs, rc),1));
      ins = BCINS_ABC(BC_TSETV, ra, var->u.s.info, rc);
    }
  }
  bcemit_INS(fs, ins);
  expr_free(fs, e);
}

/* Emit method lookup expression. */
static void bcemit_method(FuncState *fs, ExpDesc *e, ExpDesc *key)
{
  BCReg idx, func, obj = expr_toanyreg(fs, e);
  expr_free(fs, e);
  func = fs->freereg;
  bcemit_AD(fs, BC_MOV, func+1, obj);  /* Copy object to first argument. */
  lua_assert(expr_isstrk(key));
  idx = const_str(fs, key);
  if (idx <= BCMAX_C) {
    bcreg_reserve(fs, 2);
    bcemit_ABC(fs, BC_TGETS, func, obj, idx);
  } else {
    bcreg_reserve(fs, 3);
    bcemit_AD(fs, BC_KSTR, func+2, idx);
    bcemit_ABC(fs, BC_TGETV, func, obj, func+2);
    fs->freereg--;
  }
  e->u.s.info = func;
  e->k = VNONRELOC;
}

/* Emit bytecode to set a range of registers to nil. */
static void bcemit_nil(FuncState *fs, BCReg from, BCReg n)
{
  BCIns *pr;
  if (fs->pc > fs->lasttarget) {  /* No jumps to current position? */
    BCReg pfrom, pto;
    pr = proto_insptr(fs->pt, fs->pc-1);
    pfrom = bc_a(*pr);
    switch (bc_op(*pr)) {  /* Try to merge with the previous instruction. */
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
      fs->pc--;  /* Drop KPRI. */
      break;
    case BC_KNIL:
      pto = bc_d(*pr);
      if (pfrom <= from && from <= pto+1) {  /* Can we connect both ranges? */
	if (from+n-1 > pto)
	  setbc_d(pr, from+n-1);  /* Patch previous instruction range. */
	return;
      }
      break;
    default:
      break;
    }
  }
  /* Emit new instruction or replace old instruction. */
  bcemit_INS(fs, n == 1 ? BCINS_AD(BC_KPRI, from, VKNIL) :
			  BCINS_AD(BC_KNIL, from, from+n-1));
}

/* -- Bytecode emitter for branches --------------------------------------- */

/* Emit unconditional branch. */
static BCPos bcemit_jmp(FuncState *fs)
{
  BCPos jpc = fs->jpc;
  BCPos j = fs->pc - 1;
  fs->jpc = NO_JMP;
  if ((int32_t)j >= (int32_t)fs->lasttarget &&
      bc_op(proto_ins(fs->pt, j)) == BC_UCLO)
    setbc_j(proto_insptr(fs->pt, j), NO_JMP);
  else
    j = bcemit_AJ(fs, BC_JMP, fs->freereg, NO_JMP);
  jmp_append(fs, &j, jpc);
  return j;
}

/* Invert branch condition of bytecode instruction. */
static void invertcond(FuncState *fs, ExpDesc *e)
{
  BCIns *i = bcptr(fs, e) - 1;
  setbc_op(i, bc_op(*i)^1);
}

/* Emit conditional branch. */
static BCPos bcemit_branch(FuncState *fs, ExpDesc *e, int cond)
{
  BCPos pc;
  if (e->k == VRELOCABLE) {
    BCIns *i = bcptr(fs, e);
    if (bc_op(*i) == BC_NOT) {
      *i = BCINS_AD(cond ? BC_ISF : BC_IST, 0, bc_d(*i));
      return bcemit_jmp(fs);
    }
  }
  if (e->k != VNONRELOC) {
    bcreg_reserve(fs, 1);
    expr_toreg_nobranch(fs, e, fs->freereg-1);
  }
  bcemit_AD(fs, cond ? BC_ISTC : BC_ISFC, NO_REG, e->u.s.info);
  pc = bcemit_jmp(fs);
  expr_free(fs, e);
  return pc;
}

/* Emit branch on true condition. */
static void bcemit_branch_t(FuncState *fs, ExpDesc *e)
{
  BCPos pc;
  expr_discharge(fs, e);
  if (e->k == VKSTR || e->k == VKNUM || e->k == VKTRUE)
    pc = NO_JMP;  /* Never jump. */
  else if (e->k == VJMP)
    invertcond(fs, e), pc = e->u.s.info;
  else if (e->k == VKFALSE && !expr_hasnojump(e))
    pc = bcemit_jmp(fs);  /* Always jump. */
  else
    pc = bcemit_branch(fs, e, 0);
  jmp_append(fs, &e->f, pc);
  jmp_tohere(fs, e->t);
  e->t = NO_JMP;
}

/* Emit branch on false condition. */
static void bcemit_branch_f(FuncState *fs, ExpDesc *e)
{
  BCPos pc;
  expr_discharge(fs, e);
  if (e->k == VKNIL || e->k == VKFALSE)
    pc = NO_JMP;  /* Never jump. */
  else if (e->k == VJMP)
    pc = e->u.s.info;
  else if (e->k == VKTRUE && !expr_hasnojump(e))
    pc = bcemit_jmp(fs);  /* Always jump. */
  else
    pc = bcemit_branch(fs, e, 1);
  jmp_append(fs, &e->t, pc);
  jmp_tohere(fs, e->f);
  e->f = NO_JMP;
}

/* -- Bytecode emitter for operators -------------------------------------- */

/* Try constant-folding of arithmetic operators. */
static int foldarith(BinOpr opr, ExpDesc *e1, ExpDesc *e2)
{
  TValue o;
  if (!expr_isnumk_nojump(e1) || !expr_isnumk_nojump(e2)) return 0;
  setnumV(&o, lj_vm_foldarith(expr_numV(e1), expr_numV(e2), (int)opr-OPR_ADD));
  if (tvisnan(&o) || tvismzero(&o)) return 0;  /* Avoid NaN and -0 as consts. */
  setnumV(&e1->u.nval, numV(&o));
  return 1;
}

/* Emit arithmetic operator. */
static void bcemit_arith(FuncState *fs, BinOpr opr, ExpDesc *e1, ExpDesc *e2)
{
  BCReg rb, rc, t;
  uint32_t op;
  if (foldarith(opr, e1, e2))
    return;
  if (opr == OPR_POW) {
    op = BC_POW;
    rc = expr_toanyreg(fs, e2);
    rb = expr_toanyreg(fs, e1);
  } else {
    op = opr-OPR_ADD+BC_ADDVV;
    /* Must discharge 2nd operand first since VINDEXED might free regs. */
    expr_toval(fs, e2);
    if (expr_isnumk(e2) && (rc = const_num(fs, e2)) <= BCMAX_C)
      op -= BC_ADDVV-BC_ADDVN;
    else
      rc = expr_toanyreg(fs, e2);
    /* 1st operand discharged by bcemit_binop_left, but need KNUM/KSHORT. */
    lua_assert(expr_isnumk(e1) || e1->k == VNONRELOC);
    expr_toval(fs, e1);
    /* Avoid two consts to satisfy bytecode constraints. */
    if (expr_isnumk(e1) && !expr_isnumk(e2) &&
	(t = const_num(fs, e1)) <= BCMAX_B) {
      rb = rc; rc = t; op -= BC_ADDVV-BC_ADDNV;
    } else {
      rb = expr_toanyreg(fs, e1);
    }
  }
  /* Using expr_free might cause asserts if the order is wrong. */
  if (e1->k == VNONRELOC && e1->u.s.info >= fs->nactvar) fs->freereg--;
  if (e2->k == VNONRELOC && e2->u.s.info >= fs->nactvar) fs->freereg--;
  e1->u.s.info = bcemit_ABC(fs, op, 0, rb, rc);
  e1->k = VRELOCABLE;
}

/* Emit comparison operator. */
static void bcemit_comp(FuncState *fs, BinOpr opr, ExpDesc *e1, ExpDesc *e2)
{
  ExpDesc *eret = e1;
  BCIns ins;
  expr_toval(fs, e1);
  if (opr == OPR_EQ || opr == OPR_NE) {
    BCOp op = opr == OPR_EQ ? BC_ISEQV : BC_ISNEV;
    BCReg ra;
    if (expr_isk(e1)) { e1 = e2; e2 = eret; }  /* Need constant in 2nd arg. */
    ra = expr_toanyreg(fs, e1);  /* First arg must be in a reg. */
    expr_toval(fs, e2);
    switch (e2->k) {
    case VKNIL: case VKFALSE: case VKTRUE:
      ins = BCINS_AD(op+(BC_ISEQP-BC_ISEQV), ra, const_pri(e2));
      break;
    case VKSTR:
      ins = BCINS_AD(op+(BC_ISEQS-BC_ISEQV), ra, const_str(fs, e2));
      break;
    case VKNUM:
      ins = BCINS_AD(op+(BC_ISEQN-BC_ISEQV), ra, const_num(fs, e2));
      break;
    default:
      ins = BCINS_AD(op, ra, expr_toanyreg(fs, e2));
      break;
    }
  } else {
    uint32_t op = opr-OPR_LT+BC_ISLT;
    BCReg ra;
    if ((op-BC_ISLT) & 1) {  /* GT -> LT, GE -> LE */
      e1 = e2; e2 = eret;  /* Swap operands. */
      op = ((op-BC_ISLT)^3)+BC_ISLT;
    }
    ra = expr_toanyreg(fs, e1);
    ins = BCINS_AD(op, ra, expr_toanyreg(fs, e2));
  }
  /* Using expr_free might cause asserts if the order is wrong. */
  if (e1->k == VNONRELOC && e1->u.s.info >= fs->nactvar) fs->freereg--;
  if (e2->k == VNONRELOC && e2->u.s.info >= fs->nactvar) fs->freereg--;
  bcemit_INS(fs, ins);
  eret->u.s.info = bcemit_jmp(fs);
  eret->k = VJMP;
}

/* Fixup left side of binary operator. */
static void bcemit_binop_left(FuncState *fs, BinOpr op, ExpDesc *e)
{
  if (op == OPR_AND) {
    bcemit_branch_t(fs, e);
  } else if (op == OPR_OR) {
    bcemit_branch_f(fs, e);
  } else if (op == OPR_CONCAT) {
    expr_tonextreg(fs, e);
  } else if (op == OPR_EQ || op == OPR_NE) {
    if (!expr_isk_nojump(e)) expr_toanyreg(fs, e);
  } else {
    if (!expr_isnumk_nojump(e)) expr_toanyreg(fs, e);
  }
}

/* Emit binary operator. */
static void bcemit_binop(FuncState *fs, BinOpr op, ExpDesc *e1, ExpDesc *e2)
{
  if (op <= OPR_POW) {
    bcemit_arith(fs, op, e1, e2);
  } else if (op == OPR_AND) {
    lua_assert(e1->t == NO_JMP);  /* List must be closed. */
    expr_discharge(fs, e2);
    jmp_append(fs, &e2->f, e1->f);
    *e1 = *e2;
  } else if (op == OPR_OR) {
    lua_assert(e1->f == NO_JMP);  /* List must be closed. */
    expr_discharge(fs, e2);
    jmp_append(fs, &e2->t, e1->t);
    *e1 = *e2;
  } else if (op == OPR_CONCAT) {
    expr_toval(fs, e2);
    if (e2->k == VRELOCABLE && bc_op(*bcptr(fs, e2)) == BC_CAT) {
      lua_assert(e1->u.s.info == bc_b(*bcptr(fs, e2))-1);
      expr_free(fs, e1);
      setbc_b(bcptr(fs, e2), e1->u.s.info);
      e1->u.s.info = e2->u.s.info;
    } else {
      expr_tonextreg(fs, e2);
      expr_free(fs, e2);
      expr_free(fs, e1);
      e1->u.s.info = bcemit_ABC(fs, BC_CAT, 0, e1->u.s.info, e2->u.s.info);
    }
    e1->k = VRELOCABLE;
  } else {
    lua_assert(op == OPR_NE || op == OPR_EQ ||
	       op == OPR_LT || op == OPR_GE || op == OPR_LE || op == OPR_GT);
    bcemit_comp(fs, op, e1, e2);
  }
}

/* Emit unary operator. */
static void bcemit_unop(FuncState *fs, BCOp op, ExpDesc *e)
{
  if (op == BC_NOT) {
    /* Swap true and false lists. */
    { BCPos temp = e->f; e->f = e->t; e->t = temp; }
    jmp_dropval(fs, e->f);
    jmp_dropval(fs, e->t);
    expr_discharge(fs, e);
    if (e->k == VKNIL || e->k == VKFALSE) {
      e->k = VKTRUE;
      return;
    } else if (expr_isk(e)) {
      e->k = VKFALSE;
      return;
    } else if (e->k == VJMP) { 
      invertcond(fs, e);
      return;
    } else if (e->k == VRELOCABLE) {
      bcreg_reserve(fs, 1);
      setbc_a(bcptr(fs, e), fs->freereg-1);
      e->u.s.info = fs->freereg-1;
      e->k = VNONRELOC;
    } else {
      lua_assert(e->k == VNONRELOC);
    }
  } else {
    lua_assert(op == BC_UNM || op == BC_LEN);
    /* Constant-fold negations. But avoid folding to -0. */
    if (op == BC_UNM && expr_isnumk_nojump(e) && expr_numV(e) != 0) {
      setnumV(&e->u.nval, -expr_numV(e));
      return;
    }
    expr_toanyreg(fs, e);
  }
  expr_free(fs, e);
  e->u.s.info = bcemit_AD(fs, op, 0, e->u.s.info);
  e->k = VRELOCABLE;
}

/* -- Lexer support ------------------------------------------------------- */

/* Check and consume optional token. */
static int lex_opt(LexState *ls, LexToken tok)
{
  if (ls->token == tok) {
    lj_lex_next(ls);
    return 1;
  }
  return 0;
}

/* Check and consume token. */
static void lex_check(LexState *ls, LexToken tok)
{
  if (ls->token != tok)
    err_token(ls, tok);
  lj_lex_next(ls);
}

/* Check for matching token. */
static void lex_match(LexState *ls, LexToken what, LexToken who, BCLine line)
{
  if (!lex_opt(ls, what)) {
    if (line == ls->linenumber) {
      err_token(ls, what);
    } else {
      const char *swhat = lj_lex_token2str(ls, what);
      const char *swho = lj_lex_token2str(ls, who);
      lj_lex_error(ls, ls->token, LJ_ERR_XMATCH, swhat, swho, line);
    }
  }
}

/* Check for string token. */
static GCstr *lex_str(LexState *ls)
{
  GCstr *s;
  if (ls->token != TK_name)
    err_token(ls, TK_name);
  s = strV(&ls->tokenval);
  lj_lex_next(ls);
  return s;
}

/* -- Variable handling --------------------------------------------------- */

#define var_get(fs, i)	(proto_varinfo((fs)->pt)[(fs)->varmap[(i)]])

/* Define a new local variable. */
static void var_new(LexState *ls, BCReg n, GCstr *name)
{
  FuncState *fs = ls->fs;
  GCproto *pt = fs->pt;
  VarInfo *varinfo = proto_varinfo(pt);
  checklimit(fs, fs->nactvar+n, LJ_MAX_LOCVAR, "local variables");
  if (LJ_UNLIKELY(fs->nvarinfo >= pt->sizevarinfo)) {
    MSize oldsize = pt->sizevarinfo;
    checklimit(fs, fs->nvarinfo, 32767, "local variables");
    lj_mem_growvec(fs->L, varinfo, pt->sizevarinfo, 32767, VarInfo);
    setmref(pt->varinfo, varinfo);
    while (oldsize < pt->sizevarinfo) setgcrefnull(varinfo[oldsize++].name);
  }
  setgcref(varinfo[fs->nvarinfo].name, obj2gco(name));
  lj_gc_objbarrier(ls->L, pt, name);
  fs->varmap[fs->nactvar+n] = (uint16_t)(fs->nvarinfo++);
}

#define var_new_lit(ls, v, n) \
  var_new(ls, (n), lj_parse_keepstr(ls, "" v, sizeof(v)-1))

/* Add local variables. */
static void var_add(LexState *ls, BCReg nvars)
{
  FuncState *fs = ls->fs;
  fs->nactvar = cast_byte(fs->nactvar + nvars);
  for (; nvars; nvars--)
    var_get(fs, fs->nactvar - nvars).startpc = fs->pc;
}

/* Remove local variables. */
static void var_remove(LexState *ls, BCReg tolevel)
{
  FuncState *fs = ls->fs;
  while (fs->nactvar > tolevel)
    var_get(fs, --fs->nactvar).endpc = fs->pc;
}

/* Lookup local variable name. */
static BCReg var_lookup_local(FuncState *fs, GCstr *n)
{
  int i;
  for (i = fs->nactvar-1; i >= 0; i--) {
    if (n == gco2str(gcref(var_get(fs, i).name)))
      return (BCReg)i;
  }
  return (BCReg)-1;  /* Not found. */
}

/* Lookup upvalue name. */
static uint32_t var_lookup_uv(FuncState *fs, GCstr *name, ExpDesc *v)
{
  uint32_t i;
  GCproto *pt = fs->pt;
  GCRef *uvname;
  for (i = 0; i < fs->nuv; i++) {
    if (fs->uvloc[i].info == v->u.s.info && fs->uvloc[i].k == v->k) {
      lua_assert(gco2str(proto_uvname(pt, i)) == name);
      return i;
    }
  }
  /* Not found, create a new upvalue for this name. */
  uvname = mref(pt->uvname, GCRef);
  if (LJ_UNLIKELY(fs->nuv >= pt->sizeuvname)) {
    MSize oldsize = pt->sizeuvname;
    checklimit(fs, fs->nuv, LJ_MAX_UPVAL, "upvalues");
    lj_mem_growvec(fs->L, uvname, pt->sizeuvname, LJ_MAX_UPVAL, GCRef);
    setmref(pt->uvname, uvname);
    while (oldsize < pt->sizeuvname) setgcrefnull(uvname[oldsize++]);
  }
  setgcref(uvname[fs->nuv], obj2gco(name));
  lj_gc_objbarrier(fs->L, pt, name);
  lua_assert(v->k == VLOCAL || v->k == VUPVAL);
  fs->uvloc[fs->nuv].k = cast_byte(v->k);
  fs->uvloc[fs->nuv].info = cast_byte(v->u.s.info);
  return fs->nuv++;
}

/* Forward declaration. */
static void scope_uvmark(FuncState *fs, BCReg level);

/* Recursively lookup variables in enclosing functions. */
static int var_lookup_(FuncState *fs, GCstr *name, ExpDesc *e, int first)
{
  if (fs) {
    BCReg reg = var_lookup_local(fs, name);
    if ((int32_t)reg >= 0) {  /* Local in this function? */
      expr_init(e, VLOCAL, reg);
      if (!first)
	scope_uvmark(fs, reg);  /* Scope now has an upvalue. */
      return 1;
    } else if (var_lookup_(fs->prev, name, e, 0)) {  /* In outer function? */
      e->u.s.info = var_lookup_uv(fs, name, e);  /* Make it an upvalue here. */
      e->k = VUPVAL;
      return 1;
    }
  } else {  /* Not found in any function, must be a global. */
    expr_init(e, VGLOBAL, 0);
    e->u.sval = name;
  }
  return 0;  /* Global. */
}

/* Lookup variable name. */
#define var_lookup(ls, e) \
  var_lookup_((ls)->fs, lex_str(ls), (e), 1)

/* -- Function state management ------------------------------------------- */

/* Fixup constants for prototype. */
static void fs_fixup_k(FuncState *fs, GCproto *pt)
{
  GCtab *kt;
  TValue *array;
  Node *node;
  BCReg nkgc;
  MSize i, hmask, sizek;
  GCRef *kptr;
  checklimitgt(fs, fs->nkn, BCMAX_D+1, "constants");
  checklimitgt(fs, fs->nkgc, BCMAX_D+1, "constants");
  nkgc = round_nkgc(fs->nkgc);
  sizek = (MSize)(nkgc*sizeof(GCRef) + fs->nkn*sizeof(lua_Number));
  kptr = lj_mem_newt(fs->L, sizek, GCRef);
  if (nkgc) setgcrefnull(kptr[0]);  /* May be uninitialized otherwise. */
  kptr += nkgc;
  setmref(pt->k, kptr);
  pt->sizekn = fs->nkn;
  pt->sizekgc = fs->nkgc;
  kt = fs->kt;
  array = tvref(kt->array);
  for (i = 0; i < kt->asize; i++)
    if (tvisnum(&array[i]))
      ((lua_Number *)kptr)[array[i].u32.lo] = cast_num(i);
  node = noderef(kt->node);
  hmask = kt->hmask;
  for (i = 0; i <= hmask; i++) {
    Node *n = &node[i];
    if (tvisnum(&n->val)) {
      ptrdiff_t kidx = (ptrdiff_t)n->val.u32.lo;
      if (tvisnum(&n->key)) {
	((lua_Number *)kptr)[kidx] = numV(&n->key);
      } else {
	GCobj *o = gcV(&n->key);
	setgcref(kptr[~kidx], o);
	lj_gc_objbarrier(fs->L, pt, o);
      }
    }
  }
}

/* Fixup upvalues for prototype. */
static void fs_fixup_uv(FuncState *fs, GCproto *pt)
{
  uint32_t i;
  uint16_t *uv = lj_mem_newvec(fs->L, fs->nuv, uint16_t);
  setmref(pt->uv, uv);
  pt->sizeuv = fs->nuv;
  for (i = 0; i < pt->sizeuv; i++) {
    uint32_t v = fs->uvloc[i].info;
    if (fs->uvloc[i].k == VLOCAL) v |= 0x8000;
    uv[i] = (uint16_t)v;
  }
}

/* Check if bytecode op returns. */
static int bcopisret(BCOp op)
{
  switch (op) {
  case BC_CALLMT: case BC_CALLT:
  case BC_RETM: case BC_RET: case BC_RET0: case BC_RET1:
    return 1;
  default:
    return 0;
  }
}

/* Fixup return instruction for prototype. */
static void fs_fixup_ret(FuncState *fs, GCproto *pt)
{
  BCPos lastpc = fs->pc;
  if (lastpc <= fs->lasttarget || !bcopisret(bc_op(proto_ins(pt, lastpc-1)))) {
    if (fs->flags & PROTO_HAS_FNEW)
      bcemit_AJ(fs, BC_UCLO, 0, 0);
    bcemit_AD(fs, BC_RET0, 0, 1);  /* Need final return. */
  }
  /* May need to fixup returns encoded before first function was created. */
  if (fs->flags & PROTO_FIXUP_RETURN) {
    BCPos pc;
    for (pc = 0; pc < lastpc; pc++) {
      BCIns i = proto_ins(pt, pc);
      BCPos offset;
      switch (bc_op(i)) {
      case BC_CALLMT: case BC_CALLT:
      case BC_RETM: case BC_RET: case BC_RET0: case BC_RET1:
	offset = bcemit_INS(fs, i)-(pc+1)+BCBIAS_J;  /* Copy return ins. */
	if (offset > BCMAX_D)
	  err_syntax(fs->ls, LJ_ERR_XFIXUP);
	/* Replace with UCLO plus branch. */
	*proto_insptr(pt, pc) = BCINS_AD(BC_UCLO, 0, offset);
	break;
      case BC_UCLO:
	return;  /* We're done. */
      default:
	break;
      }
    }
  }
}

/* Finish a FuncState and return the new prototype. */
static GCproto *fs_finish(LexState *ls, BCLine line)
{
  lua_State *L = ls->L;
  FuncState *fs = ls->fs;
  GCproto *pt = fs->pt;
  BCIns *bc;
  GCRef *uvname;
  BCLine *lineinfo;
  VarInfo *varinfo;

  /* Apply final fixups. */
  var_remove(ls, 0);
  fs_fixup_ret(fs, pt);

  /* Reallocate arrays. */
  bc = proto_bc(pt);
  lj_mem_reallocvec(L, bc, pt->sizebc, fs->pc, BCIns);
  setmref(pt->bc, bc);
  pt->sizebc = fs->pc;
  fs_fixup_k(fs, pt);
  fs_fixup_uv(fs, pt);
  lineinfo = proto_lineinfo(pt);
  lj_mem_reallocvec(L, lineinfo, pt->sizelineinfo, fs->pc, BCLine);
  setmref(pt->lineinfo, lineinfo);
  pt->sizelineinfo = fs->pc;
  varinfo = proto_varinfo(pt);
  lj_mem_reallocvec(L, varinfo, pt->sizevarinfo, fs->nvarinfo, VarInfo);
  setmref(pt->varinfo, varinfo);
  pt->sizevarinfo = fs->nvarinfo;
  uvname = mref(pt->uvname, GCRef);
  lj_mem_reallocvec(L, uvname, pt->sizeuvname, fs->nuv, GCRef);
  setmref(pt->uvname, uvname);
  pt->sizeuvname = fs->nuv;
  lua_assert(fs->bl == NULL);

  /* Initialize prototype fields. */
  pt->flags = fs->flags;
  pt->framesize = fs->framesize;
  pt->linedefined = fs->linedefined;
  pt->lastlinedefined = line;

  lj_vmevent_send(L, BC,
    setprotoV(L, L->top++, pt);
  );

  /* Remove FuncState from list. Pop const table and prototype. */
  ls->fs = fs->prev;
  L->top -= 2;
  lua_assert(ls->fs != NULL || ls->token == TK_eof);
  /* Re-anchor last string token to avoid GC. */
  if (ls->token == TK_name || ls->token == TK_string) {
    TValue *tv = lj_tab_setstr(ls->L, ls->fs->kt, strV(&ls->tokenval));
    if (tvisnil(tv)) setboolV(tv, 1);
  }
  return pt;
}

/* Initialize a new FuncState. */
static void fs_init(LexState *ls, FuncState *fs, BCLine line)
{
  lua_State *L = ls->L;
  GCproto *pt = lj_func_newproto(L);
  fs->pt = pt;
  fs->prev = ls->fs; ls->fs = fs;  /* Append to list. */
  fs->ls = ls;
  fs->L = L;
  fs->pc = 0;
  fs->lasttarget = 0;
  fs->jpc = NO_JMP;
  fs->freereg = 0;
  fs->nkgc = 0;
  fs->nkn = 0;
  fs->nvarinfo = 0;
  fs->nactvar = 0;
  fs->nuv = 0;
  fs->bl = NULL;
  setgcref(pt->chunkname, obj2gco(ls->chunkname));
  fs->flags = 0;
  fs->framesize = 2;  /* Minimum frame size. */
  fs->linedefined = line;
  fs->kt = lj_tab_new(L, 0, 0);
  /* Anchor table of constants and prototype (to avoid being collected). */
  settabV(L, L->top, fs->kt);
  incr_top(L);
  setprotoV(L, L->top, pt);
  incr_top(L);
}

/* -- Expressions --------------------------------------------------------- */

/* Forward declaration. */
static void expr(LexState *ls, ExpDesc *v);

/* Return string expression. */
static void expr_str(LexState *ls, ExpDesc *e)
{
  expr_init(e, VKSTR, 0);
  e->u.sval = lex_str(ls);
}

/* Return index expression. */
static void expr_index(FuncState *fs, ExpDesc *t, ExpDesc *e)
{
  /* Already called: expr_toval(fs, e). */
  t->k = VINDEXED;
  if (expr_isnumk(e)) {
    lua_Number n = expr_numV(e);
    int32_t k = lj_num2int(n);
    if (checku8(k) && n == cast_num(k)) {
      t->u.s.aux = BCMAX_C+1+(uint32_t)k;  /* 256..511: const byte key */
      return;
    }
  } else if (expr_isstrk(e)) {
    BCReg idx = const_str(fs, e);
    if (idx <= BCMAX_C) {
      t->u.s.aux = ~idx;  /* -256..-1: const string key */
      return;
    }
  }
  t->u.s.aux = expr_toanyreg(fs, e);  /* 0..255: register */
}

/* Parse index expression with named field. */
static void expr_field(LexState *ls, ExpDesc *v)
{
  FuncState *fs = ls->fs;
  ExpDesc key;
  expr_toanyreg(fs, v);
  lj_lex_next(ls);  /* Skip dot or colon. */
  expr_str(ls, &key);
  expr_index(fs, v, &key);
}

/* Parse index expression with brackets. */
static void expr_bracket(LexState *ls, ExpDesc *v)
{
  lj_lex_next(ls);  /* Skip '['. */
  expr(ls, v);
  expr_toval(ls->fs, v);
  lex_check(ls, ']');
}

/* Get value of constant expression. */
static void expr_kvalue(TValue *v, ExpDesc *e)
{
  if (e->k <= VKTRUE) {
    v->it = ~(int32_t)e->k;
  } else if (e->k == VKSTR) {
    setgcref(v->gcr, obj2gco(e->u.sval));
    v->it = LJ_TSTR;
  } else {
    lua_assert(e->k == VKNUM);
    setnumV(v, expr_numV(e));
  }
}

/* Parse table constructor expression. */
static void expr_table(LexState *ls, ExpDesc *e)
{
  FuncState *fs = ls->fs;
  BCLine line = ls->linenumber;
  GCtab *t = NULL;
  int vcall = 0, needarr = 0;
  int32_t narr = 1;  /* First array index. */
  uint32_t nhash = 0;  /* Number of hash entries. */
  BCReg freg = fs->freereg;
  BCPos pc = bcemit_AD(fs, BC_TNEW, freg, 0);
  expr_init(e, VNONRELOC, freg);
  bcreg_reserve(fs, 1);
  freg++;
  lex_check(ls, '{');
  while (ls->token != '}') {
    ExpDesc key, val;
    vcall = 0;
    if (ls->token == '[') {
      expr_bracket(ls, &key);  /* Already calls expr_toval. */
      if (!expr_isk(&key)) expr_index(fs, e, &key);
      if (expr_isnumk(&key) && expr_numV(&key) == 0) needarr = 1; else nhash++;
      lex_check(ls, '=');
    } else if (ls->token == TK_name && lj_lex_lookahead(ls) == '=') {
      expr_str(ls, &key);
      lex_check(ls, '=');
      nhash++;
    } else {
      expr_init(&key, VKNUM, 0);
      setintV(&key.u.nval, narr);
      narr++;
      needarr = vcall = 1;
    }
    expr(ls, &val);
    if (expr_isk_nojump(&val) && expr_isk(&key) && key.k != VKNIL) {
      TValue k;
      if (!t) {  /* Create template table on demand. */
	BCReg kidx;
	t = lj_tab_new(fs->L, 0, 0);
	kidx = const_gc(fs, obj2gco(t), LJ_TTAB);
	*proto_insptr(fs->pt, pc) = BCINS_AD(BC_TDUP, freg-1, kidx);
      }
      vcall = 0;
      expr_kvalue(&k, &key);
      expr_kvalue(lj_tab_set(fs->L, t, &k), &val);
      if (val.k == VKSTR)
	lj_gc_objbarriert(fs->L, t, val.u.sval);
    } else {
      if (expr_isk(&key)) expr_index(fs, e, &key);
      if (val.k != VCALL) vcall = 0;
      bcemit_store(fs, e, &val);
    }
    fs->freereg = freg;
    if (!lex_opt(ls, ',') && !lex_opt(ls, ';')) break;
  }
  lex_match(ls, '}', '{', line);
  if (vcall) {
    BCIns *i = proto_insptr(fs->pt, fs->pc-1);
    ExpDesc en;
    lua_assert(bc_a(*i)==freg && bc_op(*i) == (narr>256?BC_TSETV:BC_TSETB));
    expr_init(&en, VKNUM, 0);
    setintV(&en.u.nval, narr-1);
    if (narr > 256) { fs->pc--; i--; }
    *i = BCINS_AD(BC_TSETM, freg, const_num(fs, &en));
    setbc_b(i-1, 0);
  }
  if (pc == fs->pc-1) {  /* Make expr relocable if possible. */
    e->u.s.info = pc;
    fs->freereg--;
    e->k = VRELOCABLE;
  } else {
    e->k = VNONRELOC;  /* May have been changed by expr_index. */
  }
  if (!t) {  /* Construct TNEW RD: hhhhhaaaaaaaaaaa. */
    if (!needarr) narr = 0;
    else if (narr < 3) narr = 3;
    else if (narr > 0x7ff) narr = 0x7ff;
    setbc_d(proto_insptr(fs->pt, pc), (uint32_t)narr|(hsize2hbits(nhash)<<11));
  }
}

/* Parse function parameters. */
static void parse_params(LexState *ls, int needself)
{
  FuncState *fs = ls->fs;
  GCproto *pt = fs->pt;
  BCReg nparams = 0;
  lex_check(ls, '(');
  if (needself) {
    var_new_lit(ls, "self", 0);
    var_add(ls, 1);
  }
  if (ls->token != ')') {
    do {
      if (ls->token == TK_name) {
	var_new(ls, nparams++, lex_str(ls));
      } else if (ls->token == TK_dots) {
	lj_lex_next(ls);
	fs->flags |= PROTO_IS_VARARG;
	break;
      } else {
	err_syntax(ls, LJ_ERR_XPARAM);
      }
    } while (lex_opt(ls, ','));
  }
  var_add(ls, nparams);
  pt->numparams = cast_byte(fs->nactvar);
  bcreg_reserve(fs, fs->nactvar);
  lex_check(ls, ')');
}

/* Forward declaration. */
static void parse_chunk(LexState *ls);

/* Parse body of a function. */
static void parse_body(LexState *ls, ExpDesc *e, int needself, BCLine line)
{
  FuncState *fs, cfs;
  BCReg kidx;
  BCLine lastline;
  GCproto *pt;
  fs_init(ls, &cfs, line);
  parse_params(ls, needself);
  parse_chunk(ls);
  lastline = ls->linenumber;
  lex_match(ls, TK_end, TK_function, line);
  pt = fs_finish(ls, lastline);
  /* Store new prototype in the constant array of the parent. */
  fs = ls->fs;
  kidx = const_gc(fs, obj2gco(pt), LJ_TPROTO);
  expr_init(e, VRELOCABLE, bcemit_AD(fs, BC_FNEW, 0, kidx));
  if (!(fs->flags & PROTO_HAS_FNEW)) {
    if (fs->flags & PROTO_HAS_RETURN)
      fs->flags |= PROTO_FIXUP_RETURN;
    fs->flags |= PROTO_HAS_FNEW;
  }
}

/* Parse expression list. Last expression is left open. */
static BCReg expr_list(LexState *ls, ExpDesc *v)
{
  BCReg n = 1;
  expr(ls, v);
  while (lex_opt(ls, ',')) {
    expr_tonextreg(ls->fs, v);
    expr(ls, v);
    n++;
  }
  return n;
}

/* Parse function argument list. */
static void parse_args(LexState *ls, ExpDesc *e)
{
  FuncState *fs = ls->fs;
  ExpDesc args;
  BCIns ins;
  BCReg base;
  BCLine line = ls->linenumber;
  if (ls->token == '(') {
    if (line != ls->lastline)
      err_syntax(ls, LJ_ERR_XAMBIG);
    lj_lex_next(ls);
    if (ls->token == ')') {  /* f(). */
      args.k = VVOID;
    } else {
      expr_list(ls, &args);
      if (args.k == VCALL)  /* f(a, b, g()) or f(a, b, ...). */
	setbc_b(bcptr(fs, &args), 0);  /* Pass on multiple results. */
    }
    lex_match(ls, ')', '(', line);
  } else if (ls->token == '{') {
    expr_table(ls, &args);
  } else if (ls->token == TK_string) {
    expr_init(&args, VKSTR, 0);
    args.u.sval = strV(&ls->tokenval);
    lj_lex_next(ls);
  } else {
    err_syntax(ls, LJ_ERR_XFUNARG);
    return;  /* Silence compiler. */
  }
  lua_assert(e->k == VNONRELOC);
  base = e->u.s.info;  /* Base register for call. */
  if (args.k == VCALL) {
    ins = BCINS_ABC(BC_CALLM, base, 2, args.u.s.aux - base - 1);
  } else {
    if (args.k != VVOID)
      expr_tonextreg(fs, &args);
    ins = BCINS_ABC(BC_CALL, base, 2, fs->freereg - base);
  }
  expr_init(e, VCALL, bcemit_INS(fs, ins));
  e->u.s.aux = base;
  proto_lineinfo(fs->pt)[fs->pc - 1] = line;
  fs->freereg = base+1;  /* Leave one result by default. */
}

/* Parse primary expression. */
static void expr_primary(LexState *ls, ExpDesc *v)
{
  FuncState *fs = ls->fs;
  /* Parse prefix expression. */
  if (ls->token == '(') {
    BCLine line = ls->linenumber;
    lj_lex_next(ls);
    expr(ls, v);
    lex_match(ls, ')', '(', line);
    expr_discharge(ls->fs, v);
  } else if (ls->token == TK_name) {
    var_lookup(ls, v);
  } else {
    err_syntax(ls, LJ_ERR_XSYMBOL);
  }
  for (;;) {  /* Parse multiple expression suffixes. */
    if (ls->token == '.') {
      expr_field(ls, v);
    } else if (ls->token == '[') {
      ExpDesc key;
      expr_toanyreg(fs, v);
      expr_bracket(ls, &key);
      expr_index(fs, v, &key);
    } else if (ls->token == ':') {
      ExpDesc key;
      lj_lex_next(ls);
      expr_str(ls, &key);
      bcemit_method(fs, v, &key);
      parse_args(ls, v);
    } else if (ls->token == '(' || ls->token == TK_string || ls->token == '{') {
      expr_tonextreg(fs, v);
      parse_args(ls, v);
    } else {
      break;
    }
  }
}

/* Parse simple expression. */
static void expr_simple(LexState *ls, ExpDesc *v)
{
  switch (ls->token) {
  case TK_number:
    expr_init(v, VKNUM, 0);
    setnumV(&v->u.nval, numV(&ls->tokenval));
    break;
  case TK_string:
    expr_init(v, VKSTR, 0);
    v->u.sval = strV(&ls->tokenval);
    break;
  case TK_nil:
    expr_init(v, VKNIL, 0);
    break;
  case TK_true:
    expr_init(v, VKTRUE, 0);
    break;
  case TK_false:
    expr_init(v, VKFALSE, 0);
    break;
  case TK_dots: {  /* Vararg. */
    FuncState *fs = ls->fs;
    BCReg base;
    checkcond(ls, fs->flags & PROTO_IS_VARARG, LJ_ERR_XDOTS);
    bcreg_reserve(fs, 1);
    base = fs->freereg-1;
    expr_init(v, VCALL, bcemit_ABC(fs, BC_VARG, base, 2, 1));
    v->u.s.aux = base;
    break;
  }
  case '{':  /* Table constructor. */
    expr_table(ls, v);
    return;
  case TK_function:
    lj_lex_next(ls);
    parse_body(ls, v, 0, ls->linenumber);
    return;
  default:
    expr_primary(ls, v);
    return;
  }
  lj_lex_next(ls);
}

/* Manage syntactic levels to avoid blowing up the stack. */
static void synlevel_begin(LexState *ls)
{
  if (++ls->level >= LJ_MAX_XLEVEL)
    lj_lex_error(ls, 0, LJ_ERR_XLEVELS);
}

#define synlevel_end(ls)	((ls)->level--)

/* Convert token to binary operator. */
static BinOpr token2binop(LexToken tok)
{
  switch (tok) {
  case '+':	return OPR_ADD;
  case '-':	return OPR_SUB;
  case '*':	return OPR_MUL;
  case '/':	return OPR_DIV;
  case '%':	return OPR_MOD;
  case '^':	return OPR_POW;
  case TK_concat: return OPR_CONCAT;
  case TK_ne:	return OPR_NE;
  case TK_eq:	return OPR_EQ;
  case '<':	return OPR_LT;
  case TK_le:	return OPR_LE;
  case '>':	return OPR_GT;
  case TK_ge:	return OPR_GE;
  case TK_and:	return OPR_AND;
  case TK_or:	return OPR_OR;
  default:	return OPR_NOBINOPR;
  }
}

/* Priorities for each binary operator. ORDER OPR. */
static const struct {
  uint8_t left;		/* Left priority. */
  uint8_t right;	/* Right priority. */
} priority[] = {
  {6,6}, {6,6}, {7,7}, {7,7}, {7,7},	/* ADD SUB MUL DIV MOD */
  {10,9}, {5,4},			/* POW CONCAT (right associative) */
  {3,3}, {3,3},				/* EQ NE */
  {3,3}, {3,3}, {3,3}, {3,3},		/* LT GE GT LE */
  {2,2}, {1,1}				/* AND OR */
};

#define UNARY_PRIORITY		8  /* Priority for unary operators. */

/* Forward declaration. */
static BinOpr expr_binop(LexState *ls, ExpDesc *v, uint32_t limit);

/* Parse unary expression. */
static void expr_unop(LexState *ls, ExpDesc *v)
{
  BCOp op;
  if (ls->token == TK_not) {
    op = BC_NOT;
  } else if (ls->token == '-') {
    op = BC_UNM;
  } else if (ls->token == '#') {
    op = BC_LEN;
  } else {
    expr_simple(ls, v);
    return;
  }
  lj_lex_next(ls);
  expr_binop(ls, v, UNARY_PRIORITY);
  bcemit_unop(ls->fs, op, v);
}

/* Parse binary expressions with priority higher than the limit. */
static BinOpr expr_binop(LexState *ls, ExpDesc *v, uint32_t limit)
{
  BinOpr op;
  synlevel_begin(ls);
  expr_unop(ls, v);
  op = token2binop(ls->token);
  while (op != OPR_NOBINOPR && priority[op].left > limit) {
    ExpDesc v2;
    BinOpr nextop;
    lj_lex_next(ls);
    bcemit_binop_left(ls->fs, op, v);
    /* Parse binary expression with higher priority. */
    nextop = expr_binop(ls, &v2, priority[op].right);
    bcemit_binop(ls->fs, op, v, &v2);
    op = nextop;
  }
  synlevel_end(ls);
  return op;  /* Return unconsumed binary operator (if any). */
}

/* Parse expression. */
static void expr(LexState *ls, ExpDesc *v)
{
  expr_binop(ls, v, 0);  /* Priority 0: parse whole expression. */
}

/* Assign expression to the next register. */
static void expr_next(LexState *ls)
{
  ExpDesc e;
  expr(ls, &e);
  expr_tonextreg(ls->fs, &e);
}

/* Parse conditional expression. */
static BCPos expr_cond(LexState *ls)
{
  ExpDesc v;
  expr(ls, &v);
  if (v.k == VKNIL) v.k = VKFALSE;
  bcemit_branch_t(ls->fs, &v);
  return v.f;
}

/* -- Scope handling ------------------------------------------------------ */

/* Begin a scope. */
static void scope_begin(FuncState *fs, FuncScope *bl, int isbreakable)
{
  bl->breaklist = NO_JMP;
  bl->isbreakable = (uint8_t)isbreakable;
  bl->nactvar = fs->nactvar;
  bl->upval = 0;
  bl->prev = fs->bl;
  fs->bl = bl;
  lua_assert(fs->freereg == fs->nactvar);
}

/* End a scope. */
static void scope_end(FuncState *fs)
{
  FuncScope *bl = fs->bl;
  fs->bl = bl->prev;
  var_remove(fs->ls, bl->nactvar);
  fs->freereg = fs->nactvar;
  lua_assert(bl->nactvar == fs->nactvar);
  /* A scope is either breakable or has upvalues. */
  lua_assert(!bl->isbreakable || !bl->upval);
  if (bl->upval)
    bcemit_AJ(fs, BC_UCLO, bl->nactvar, 0);
  else  /* Avoid in upval case, it clears lasttarget and kills UCLO+JMP join. */
    jmp_tohere(fs, bl->breaklist);
}

/* Mark scope as having an upvalue. */
static void scope_uvmark(FuncState *fs, BCReg level)
{
  FuncScope *bl;
  for (bl = fs->bl; bl && bl->nactvar > level; bl = bl->prev)
    ;
  if (bl)
    bl->upval = 1;
}

/* Parse 'break' statement. */
static void parse_break(LexState *ls)
{
  FuncState *fs = ls->fs;
  FuncScope *bl;
  int upval = 0;
  for (bl = fs->bl; bl && !bl->isbreakable; bl = bl->prev)
    upval |= bl->upval;  /* Collect upvalues in intervening scopes. */
  if (!bl)  /* Error if no breakable scope found. */
    err_syntax(ls, LJ_ERR_XBREAK);
  if (upval)
    bcemit_AJ(fs, BC_UCLO, bl->nactvar, 0);  /* Close upvalues. */
  jmp_append(fs, &bl->breaklist, bcemit_jmp(fs));
}

/* Check for end of block. */
static int endofblock(LexToken token)
{
  switch (token) {
  case TK_else: case TK_elseif: case TK_end: case TK_until: case TK_eof:
    return 1;
  default:
    return 0;
  }
}

/* Parse 'return' statement. */
static void parse_return(LexState *ls)
{
  BCIns ins;
  FuncState *fs = ls->fs;
  lj_lex_next(ls);  /* Skip 'return'. */
  fs->flags |= PROTO_HAS_RETURN;
  if (endofblock(ls->token) || ls->token == ';') {  /* Bare return. */
    ins = BCINS_AD(BC_RET0, 0, 1);
  } else {  /* Return with one or more values. */
    ExpDesc e;  /* Receives the _last_ expression in the list. */
    BCReg nret = expr_list(ls, &e);
    if (nret == 1) {  /* Return one result. */
      if (e.k == VCALL) {  /* Check for tail call. */
	BCIns *i = bcptr(fs, &e);
	/* It doesn't pay off to add BC_VARGT just for 'return ...'. */
	if (bc_op(*i) == BC_VARG) goto notailcall;
	fs->pc--;
	ins = BCINS_AD(bc_op(*i)-BC_CALL+BC_CALLT, bc_a(*i), bc_c(*i));
      } else {  /* Can return the result from any register. */
	ins = BCINS_AD(BC_RET1, expr_toanyreg(fs, &e), 2);
      }
    } else {
      if (e.k == VCALL) {  /* Append all results from a call. */
      notailcall:
	setbc_b(bcptr(fs, &e), 0);
	ins = BCINS_AD(BC_RETM, fs->nactvar, e.u.s.aux - fs->nactvar);
      } else {
	expr_tonextreg(fs, &e);  /* Force contiguous registers. */
	ins = BCINS_AD(BC_RET, fs->nactvar, nret+1);
      }
    }
  }
  if (fs->flags & PROTO_HAS_FNEW)
    bcemit_AJ(fs, BC_UCLO, 0, 0);  /* May need to close upvalues first. */
  bcemit_INS(fs, ins);
}

/* Parse a block. */
static void parse_block(LexState *ls)
{
  FuncState *fs = ls->fs;
  FuncScope bl;
  scope_begin(fs, &bl, 0);
  parse_chunk(ls);
  lua_assert(bl.breaklist == NO_JMP);
  scope_end(fs);
}

/* -- Assignments --------------------------------------------------------- */

/* List of LHS variables. */
typedef struct LHSVarList {
  ExpDesc v;			/* LHS variable. */
  struct LHSVarList *prev;	/* Link to previous LHS variable. */
} LHSVarList;

/* Eliminate write-after-read hazards for local variable assignment. */
static void assign_hazard(LexState *ls, LHSVarList *lh, const ExpDesc *v)
{
  FuncState *fs = ls->fs;
  BCReg reg = v->u.s.info;  /* Check against this variable. */
  BCReg tmp = fs->freereg;  /* Rename to this temp. register (if needed). */
  int hazard = 0;
  for (; lh; lh = lh->prev) {
    if (lh->v.k == VINDEXED) {
      if (lh->v.u.s.info == reg) {  /* t[i], t = 1, 2 */
	hazard = 1;
	lh->v.u.s.info = tmp;
      }
      if (lh->v.u.s.aux == reg) {  /* t[i], i = 1, 2 */
	hazard = 1;
	lh->v.u.s.aux = tmp;
      }
    }
  }
  if (hazard) {
    bcemit_AD(fs, BC_MOV, tmp, reg);  /* Rename conflicting variable. */
    bcreg_reserve(fs, 1);
  }
}

/* Adjust LHS/RHS of an assignment. */
static void assign_adjust(LexState *ls, BCReg nvars, BCReg nexps, ExpDesc *e)
{
  FuncState *fs = ls->fs;
  int32_t extra = (int32_t)nvars - (int32_t)nexps;
  if (e->k == VCALL) {
    extra++;  /* Compensate for the VCALL itself. */
    if (extra < 0) extra = 0;
    setbc_b(bcptr(fs, e), extra+1);  /* Fixup call results. */
    if (extra > 1) bcreg_reserve(fs, (BCReg)extra-1);
  } else {
    if (e->k != VVOID)
      expr_tonextreg(fs, e);  /* Close last expression. */
    if (extra > 0) {  /* Leftover LHS are set to nil. */
      BCReg reg = fs->freereg;
      bcreg_reserve(fs, (BCReg)extra);
      bcemit_nil(fs, reg, (BCReg)extra);
    }
  }
}

/* Recursively parse assignment statement. */
static void parse_assignment(LexState *ls, LHSVarList *lh, BCReg nvars)
{
  ExpDesc e;
  checkcond(ls, VLOCAL <= lh->v.k && lh->v.k <= VINDEXED, LJ_ERR_XSYNTAX);
  if (lex_opt(ls, ',')) {  /* Collect LHS list and recurse upwards. */
    LHSVarList vl;
    vl.prev = lh;
    expr_primary(ls, &vl.v);
    if (vl.v.k == VLOCAL)
      assign_hazard(ls, lh, &vl.v);
    checklimit(ls->fs, ls->level + nvars, LJ_MAX_XLEVEL, "variable names");
    parse_assignment(ls, &vl, nvars+1);
  } else {  /* Parse RHS. */
    BCReg nexps;
    lex_check(ls, '=');
    nexps = expr_list(ls, &e);
    if (nexps == nvars) {
      if (e.k == VCALL) {
	if (bc_op(*bcptr(ls->fs, &e)) == BC_VARG) {  /* Vararg assignment. */
	  ls->fs->freereg--;
	  e.k = VRELOCABLE;
	} else {  /* Multiple call results. */
	  e.u.s.info = e.u.s.aux;  /* Base of call is not relocatable. */
	  e.k = VNONRELOC;
	}
      }
      bcemit_store(ls->fs, &lh->v, &e);
      return;
    }
    assign_adjust(ls, nvars, nexps, &e);
    if (nexps > nvars)
      ls->fs->freereg -= nexps - nvars;  /* Drop leftover regs. */
  }
  /* Assign RHS to LHS and recurse downwards. */
  expr_init(&e, VNONRELOC, ls->fs->freereg-1);
  bcemit_store(ls->fs, &lh->v, &e);
}

/* Parse call statement or assignment. */
static void parse_call_assign(LexState *ls)
{
  FuncState *fs = ls->fs;
  LHSVarList vl;
  expr_primary(ls, &vl.v);
  if (vl.v.k == VCALL) {  /* Function call statement. */
    setbc_b(bcptr(fs, &vl.v), 1);  /* No results. */
  } else {  /* Start of an assignment. */
    vl.prev = NULL;
    parse_assignment(ls, &vl, 1);
  }
}

/* Parse 'local' statement. */
static void parse_local(LexState *ls)
{
  if (lex_opt(ls, TK_function)) {  /* Local function declaration. */
    ExpDesc v, b;
    FuncState *fs = ls->fs;
    var_new(ls, 0, lex_str(ls));
    expr_init(&v, VLOCAL, fs->freereg);
    bcreg_reserve(fs, 1);
    var_add(ls, 1);
    parse_body(ls, &b, 0, ls->linenumber);
    bcemit_store(fs, &v, &b);
    /* The upvalue is in scope, but the local is only valid after the store. */
    var_get(fs, fs->nactvar - 1).startpc = fs->pc;
  } else {  /* Local variable declaration. */
    ExpDesc e;
    BCReg nexps, nvars = 0;
    do {  /* Collect LHS. */
      var_new(ls, nvars++, lex_str(ls));
    } while (lex_opt(ls, ','));
    if (lex_opt(ls, '=')) {  /* Optional RHS. */
      nexps = expr_list(ls, &e);
    } else {  /* Or implicitly set to nil. */
      e.k = VVOID;
      nexps = 0;
    }
    assign_adjust(ls, nvars, nexps, &e);
    var_add(ls, nvars);
  }
}

/* Parse 'function' statement. */
static void parse_func(LexState *ls, BCLine line)
{
  FuncState *fs;
  ExpDesc v, b;
  int needself = 0;
  lj_lex_next(ls);  /* Skip 'function'. */
  /* Parse function name. */
  var_lookup(ls, &v);
  while (ls->token == '.')  /* Multiple dot-separated fields. */
    expr_field(ls, &v);
  if (ls->token == ':') {  /* Optional colon to signify method call. */
    needself = 1;
    expr_field(ls, &v);
  }
  parse_body(ls, &b, needself, line);
  fs = ls->fs;
  bcemit_store(fs, &v, &b);
  proto_lineinfo(fs->pt)[fs->pc - 1] = line;  /* Set line for the store. */
}

/* -- Loop and conditional statements ------------------------------------- */

/* Parse 'while' statement. */
static void parse_while(LexState *ls, BCLine line)
{
  FuncState *fs = ls->fs;
  BCPos start, loop, condexit;
  FuncScope bl;
  lj_lex_next(ls);  /* Skip 'while'. */
  start = fs->lasttarget = fs->pc;
  condexit = expr_cond(ls);
  scope_begin(fs, &bl, 1);
  lex_check(ls, TK_do);
  loop = bcemit_AD(fs, BC_LOOP, fs->nactvar, 0);
  parse_block(ls);
  jmp_patch(fs, bcemit_jmp(fs), start);
  lex_match(ls, TK_end, TK_while, line);
  scope_end(fs);
  jmp_tohere(fs, condexit);
  jmp_patchins(fs, loop, fs->pc);
}

/* Parse 'repeat' statement. */
static void parse_repeat(LexState *ls, BCLine line)
{
  FuncState *fs = ls->fs;
  BCPos loop = fs->lasttarget = fs->pc;
  BCPos condexit;
  FuncScope bl1, bl2;
  scope_begin(fs, &bl1, 1);  /* Breakable loop scope. */
  scope_begin(fs, &bl2, 0);  /* Inner scope. */
  lj_lex_next(ls);  /* Skip 'repeat'. */
  bcemit_AD(fs, BC_LOOP, fs->nactvar, 0);
  parse_chunk(ls);
  lex_match(ls, TK_until, TK_repeat, line);
  condexit = expr_cond(ls);  /* Parse condition (still inside inner scope). */
  if (!bl2.upval) {  /* No upvalues? Just end inner scope. */
    scope_end(fs);
  } else {  /* Otherwise generate: cond: UCLO+JMP out, !cond: UCLO+JMP loop. */
    parse_break(ls);  /* Break from loop and close upvalues. */
    jmp_tohere(fs, condexit);
    scope_end(fs);  /* End inner scope and close upvalues. */
    condexit = bcemit_jmp(fs);
  }
  jmp_patch(fs, condexit, loop);  /* Jump backwards if !cond. */
  jmp_patchins(fs, loop, fs->pc);
  scope_end(fs);  /* End loop scope. */
}

/* Parse body of a 'for' statement. */
static void parse_for_body(LexState *ls, BCReg base, BCLine line,
			   BCReg nvars, int isnum)
{
  FuncScope bl;
  FuncState *fs = ls->fs;
  BCPos loop, loopend;
  var_add(ls, 3);  /* Hidden control variables. */
  lex_check(ls, TK_do);
  loop = isnum ? bcemit_AJ(fs, BC_FORI, base, NO_JMP) :
		 bcemit_AJ(fs, BC_JMP, fs->freereg, NO_JMP);
  scope_begin(fs, &bl, 0);  /* Scope for visible variables. */
  var_add(ls, nvars);
  bcreg_reserve(fs, nvars);
  parse_block(ls);
  scope_end(fs);
  /* Perform loop inversion. Loop control instructions are at the end. */
  if (isnum) {
    loopend = bcemit_AJ(fs, BC_FORL, base, NO_JMP);
    jmp_patchins(fs, loop, fs->pc);
  } else {
    jmp_patchins(fs, loop, fs->pc);
    bcemit_ABC(fs, BC_ITERC, base+3, nvars+1, 2+1);
    loopend = bcemit_AJ(fs, BC_ITERL, base+3, NO_JMP);
    proto_lineinfo(fs->pt)[loopend-1] = line;
  }
  proto_lineinfo(fs->pt)[loopend] = line;  /* Fix line for control ins. */
  jmp_patchins(fs, loopend, loop+1);
}

/* Parse numeric 'for'. */
static void parse_for_num(LexState *ls, GCstr *varname, BCLine line)
{
  FuncState *fs = ls->fs;
  BCReg base = fs->freereg;
  /* Hidden control variables. */
  var_new_lit(ls, "(for index)", FORL_IDX);
  var_new_lit(ls, "(for limit)", FORL_STOP);
  var_new_lit(ls, "(for step)", FORL_STEP);
  /* Visible copy of index variable. */
  var_new(ls, FORL_EXT, varname);
  lex_check(ls, '=');
  expr_next(ls);
  lex_check(ls, ',');
  expr_next(ls);
  if (lex_opt(ls, ',')) {
    expr_next(ls);
  } else {
    bcemit_AD(fs, BC_KSHORT, fs->freereg, 1);  /* Default step is 1. */
    bcreg_reserve(fs, 1);
  }
  parse_for_body(ls, base, line, 1, 1);
}

/* Parse 'for' iterator. */
static void parse_for_iter(LexState *ls, GCstr *indexname)
{
  FuncState *fs = ls->fs;
  ExpDesc e;
  BCReg nvars = 0;
  BCLine line;
  BCReg base = fs->freereg;
  /* Hidden control variables. */
  var_new_lit(ls, "(for generator)", nvars++);
  var_new_lit(ls, "(for state)", nvars++);
  var_new_lit(ls, "(for control)", nvars++);
  /* Visible variables returned from iterator. */
  var_new(ls, nvars++, indexname);
  while (lex_opt(ls, ','))
    var_new(ls, nvars++, lex_str(ls));
  lex_check(ls, TK_in);
  line = ls->linenumber;
  assign_adjust(ls, 3, expr_list(ls, &e), &e);
  bcreg_bump(fs, 3);  /* The iterator needs another 3 slots (func + 2 args). */
  parse_for_body(ls, base, line, nvars - 3, 0);
}

/* Parse 'for' statement. */
static void parse_for(LexState *ls, BCLine line)
{
  FuncState *fs = ls->fs;
  GCstr *varname;
  FuncScope bl;
  scope_begin(fs, &bl, 1);  /* Breakable loop scope. */
  lj_lex_next(ls);  /* Skip 'for'. */
  varname = lex_str(ls);  /* Get first variable name. */
  if (ls->token == '=')
    parse_for_num(ls, varname, line);
  else if (ls->token == ',' || ls->token == TK_in)
    parse_for_iter(ls, varname);
  else
    err_syntax(ls, LJ_ERR_XFOR);
  lex_match(ls, TK_end, TK_for, line);
  scope_end(fs);  /* Resolve break list. */
}

/* Parse condition and 'then' block. */
static BCPos parse_then(LexState *ls)
{
  BCPos condexit;
  lj_lex_next(ls);  /* Skip 'if' or 'elseif'. */
  condexit = expr_cond(ls);
  lex_check(ls, TK_then);
  parse_block(ls);
  return condexit;
}

/* Parse 'if' statement. */
static void parse_if(LexState *ls, BCLine line)
{
  FuncState *fs = ls->fs;
  BCPos flist;
  BCPos escapelist = NO_JMP;
  flist = parse_then(ls);
  while (ls->token == TK_elseif) {  /* Parse multiple 'elseif' blocks. */
    jmp_append(fs, &escapelist, bcemit_jmp(fs));
    jmp_tohere(fs, flist);
    flist = parse_then(ls);
  }
  if (ls->token == TK_else) {  /* Parse optional 'else' block. */
    jmp_append(fs, &escapelist, bcemit_jmp(fs));
    jmp_tohere(fs, flist);
    lj_lex_next(ls);  /* Skip 'else'. */
    parse_block(ls);
  } else {
    jmp_append(fs, &escapelist, flist);
  }
  jmp_tohere(fs, escapelist);
  lex_match(ls, TK_end, TK_if, line);
}

/* -- Parse statements ---------------------------------------------------- */

/* Parse a statement. Returns 1 if it must be the last one in a chunk. */
static int parse_stmt(LexState *ls)
{
  BCLine line = ls->linenumber;
  switch (ls->token) {
  case TK_if:
    parse_if(ls, line);
    break;
  case TK_while:
    parse_while(ls, line);
    break;
  case TK_do:
    lj_lex_next(ls);
    parse_block(ls);
    lex_match(ls, TK_end, TK_do, line);
    break;
  case TK_for:
    parse_for(ls, line);
    break;
  case TK_repeat:
    parse_repeat(ls, line);
    break;
  case TK_function:
    parse_func(ls, line);
    break;
  case TK_local:
    lj_lex_next(ls);
    parse_local(ls);
    break;
  case TK_return:
    parse_return(ls);
    return 1;  /* Must be last. */
  case TK_break:
    lj_lex_next(ls);
    parse_break(ls);
    return 1;  /* Must be last. */
  default:
    parse_call_assign(ls);
    break;
  }
  return 0;
}

/* A chunk is a list of statements optionally separated by semicolons. */
static void parse_chunk(LexState *ls)
{
  int islast = 0;
  synlevel_begin(ls);
  while (!islast && !endofblock(ls->token)) {
    islast = parse_stmt(ls);
    lex_opt(ls, ';');
    lua_assert(ls->fs->framesize >= ls->fs->freereg &&
	       ls->fs->freereg >= ls->fs->nactvar);
    ls->fs->freereg = ls->fs->nactvar;  /* Free registers after each stmt. */
  }
  synlevel_end(ls);
}

/* Entry point of bytecode parser. */
GCproto *lj_parse(LexState *ls)
{
  struct FuncState fs;
  GCproto *pt;
  ls->level = 0;
  fs_init(ls, &fs, 0);
  fs.flags |= PROTO_IS_VARARG;  /* Main chunk is always a vararg func. */
  lj_lex_next(ls);  /* Read-ahead first token. */
  parse_chunk(ls);
  if (ls->token != TK_eof)
    err_token(ls, TK_eof);
  pt = fs_finish(ls, ls->linenumber);
  lua_assert(fs.prev == NULL);
  lua_assert(ls->fs == NULL);
  lua_assert(pt->sizeuv == 0);
  return pt;
}

