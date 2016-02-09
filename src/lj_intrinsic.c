/*
** FFI Intrinsic system.
*/

#define LUA_CORE
#include "lj_arch.h"
#include "lj_tab.h"
#include "lj_err.h"
#include "lj_intrinsic.h"

#if LJ_HASINTRINSICS

#include "lj_lib.h"
#include "lj_err.h"
#include "lj_str.h"
#include "lj_char.h"
#include "lj_cdata.h"
#include "lj_cconv.h"
#include "lj_jit.h"
#include "lj_trace.h"
#include "lj_dispatch.h"
#include "lj_target.h"

typedef enum RegFlags {
  REGFLAG_64BIT = REGKIND_GPR64 << 6, /* 64 bit override */
  REGFLAG_BLACKLIST = 1 << 17,
}RegFlags;

typedef struct RegEntry {
  const char* name;
  unsigned int slot; /* Slot and Info */
}RegEntry;

#define RIDENUM(name)	RID_##name,

#define MKREG(name) {#name, RID_##name},
#define MKREGGPR(reg, name) {#name, RID_##reg},
#define MKREG_GPR64(reg, name) {#name, REGFLAG_64BIT|RID_##reg},

#if LJ_64
#define GPRDEF2(_) \
  _(EAX, eax) _(ECX, ecx) _(EDX, edx) _(EBX, ebx) _(ESP|REGFLAG_BLACKLIST, esp)  \
  _(EBP, ebp) _(ESI, esi) _(EDI, edi) _(R8D, r8d) _(R9D, r9d) _(R10D, r10d)  \
  _(R11D, r11d) _(R12D, r12d) _(R13D, r13d) _(R14D, r14d) _(R15D, r15d)

#define GPRDEF_R64(_) \
  _(EAX, rax) _(ECX, rcx) _(EDX, rdx) _(EBX, rbx) _(ESP|REGFLAG_BLACKLIST, rsp) _(EBP, rbp) _(ESI, rsi) _(EDI, rdi)
#else
#define GPRDEF2(_) \
  _(EAX, eax) _(ECX, ecx) _(EDX, edx) _(EBX, ebx) _(ESP|REGFLAG_BLACKLIST, esp) _(EBP, ebp) _(ESI, esi) _(EDI, edi) 
#endif

RegEntry reglut[] = {
  GPRDEF2(MKREGGPR)
#if LJ_64
  GPRDEF_R64(MKREG_GPR64)
#endif
};

static CTypeID register_intrinsic(lua_State *L, CIntrinsic* src, CType *func)
{
  CTState *cts = ctype_cts(L);
  CType *ct;
  CTypeID id;
  CIntrinsic *intrins;
  lua_assert(ctype_isintrinsic(func->info));

  if (cts->intr.top+1 >= LJ_INTRINS_MAXID) lj_err_msg(cts->L, LJ_ERR_TABOV);

  if ((cts->intr.top+1) > cts->intr.sizetab) {
    lj_mem_growvec(cts->L, cts->intr.tab, cts->intr.sizetab, LJ_INTRINS_MAXID, CIntrinsic);
  }

  id = ctype_typeid(cts, func);
  ct = func;

  /* Upper bits of size are used for modified link */
  ct->size = (ct->size & 0xffff0000) | cts->intr.top;
  intrins = &cts->intr.tab[cts->intr.top++];
  memcpy(intrins, src, sizeof(CIntrinsic));
  intrins->id = id;

  return id;
}

static void lj_intrinsic_new(lua_State *L, CTypeID id, void* wrapmc)
{
  CTState *cts = ctype_cts(L);
  GCcdata *cd;
  lua_assert(ctype_isintrinsic(ctype_get(cts, id)->info));
  cd = lj_cdata_new(cts, id, CTSIZE_PTR);
  *(void **)cdataptr(cd) = wrapmc;
  setcdataV(L, L->top++, cd);
}

static int parse_fprreg(const char *name, uint32_t len)
{
  uint32_t rid = 0, kind = REGKIND_FPR64;
  uint32_t pos = 3;

  if (len < 3 || name[0] != 'x' || 
      name[1] != 'm' || name[2] != 'm')
    return -1;

  if (lj_char_isdigit((uint8_t)name[3])) {
    rid = name[3] - '0';
    pos = 4;

    if (LJ_64 && lj_char_isdigit((uint8_t)name[4])) {
      rid = rid*10;
      rid += name[4] - '0';
      pos++;
    }

    if (rid >= RID_NUM_FPR) {
      return -1;
    }
    rid += RID_MIN_FPR;
  } else {
    return -1;
  }

  if (pos < len) {
    if (name[pos] == 'f') {
      kind = REGKIND_FPR32;
      pos++;
    } else if (name[pos] == 'v') {
      kind = REGKIND_V128;
      pos++;
    } else {
      kind = REGKIND_FPR64;
    }
  }

  if (pos < len) {
    return -1;
  }

  return reg_make(rid, kind);
}

int lj_intrinsic_getreg(CTState *cts, GCstr *name) {

  if (strdata(name)[0] == 'x') {
    return parse_fprreg(strdata(name), name->len);
  } else {
    cTValue *reginfotv = lj_tab_getstr(cts->miscmap, name);

    if (reginfotv && !tvisnil(reginfotv)) {
      return (uint32_t)(uintptr_t)lightudV(reginfotv);
    }
  }

  return -1;
}

static CType *setarg_casttype(CTState *cts, CType *ctarg, CType *ct) {
  CTypeID id;

  if (ctype_isvector(ct->info)) {
    CTypeID argid = ctype_typeid(cts, ctarg);
    id = lj_ctype_intern(cts, CTINFO(CT_PTR, CTALIGN_PTR|ctype_typeid(cts, ct)),
                         CTSIZE_PTR);
    ctarg = ctype_get(cts, argid);
  } else {
    id = ctype_typeid(cts, ct);
  }

  ctarg->size |= id << 16;
  return ctarg;
}

enum IntrinsRegSet {
  REGSET_IN,
  REGSET_OUT,
  REGSET_MOD,
};

/* Walks through either a Lua table(array) of register names or ctype linked list 
** of typed parameters  who's name will be the register for that specific parameter.
** The register names are converted into a register id\kind which are packed 
** together into a uint8_t that is saved into one of the register lists of the 
** CIntrinsic passed in.
*/
static RegSet process_reglist(lua_State *L, CIntrinsic *intrins, int regsetid, 
                              CTypeID liststart)
{
  CTState *cts = ctype_cts(L);
  uint32_t i, count = 0;
  RegSet rset = 0;
  const char *listname;
  uint8_t *regout = NULL;
  CTypeID sib = liststart;

  if (regsetid == REGSET_IN) {
    listname = "in";
    regout = intrins->in;
  } else if(regsetid == REGSET_OUT) {
    listname = "out";
    regout = intrins->out;
  } else {
    listname = "mod";
  }

  for (i = 1; sib; i++) {
    CType *ctarg = ctype_get(cts, sib);
    GCstr *str = strref(ctarg->name);
    Reg r = 0;
    int32_t reg = -1;
    sib = ctarg->sib;

    if (i > LJ_INTRINS_MAXREG && regsetid != REGSET_MOD) {
      lj_err_callerv(L, LJ_ERR_FFI_REGOV, listname, LJ_INTRINS_MAXREG);
    }

    reg = lj_intrinsic_getreg(cts, str);

    if (reg < 0) {
      /* Unrecognized register name */
      lj_err_callerv(L, LJ_ERR_FFI_BADREG, "invalid name", strdata(str), listname);
    }

    /* Pack the register info into the ctype argument */
    ctarg->size = reg & 0xff;
    setarg_casttype(cts, ctarg, ctype_rawchild(cts, ctarg));

    r = reg_rid(reg);

    /* Check for duplicate registers in the list */
    if (rset_test(rset, r)) {
      lj_err_callerv(L, LJ_ERR_FFI_BADREG, "duplicate", strdata(str), listname);
    }
    rset_set(rset, r);

    if (regsetid == REGSET_OUT && reg_isgpr(reg)) {
      CType *ct = ctype_rawchild(cts, ctarg);

      if (ctype_ispointer(ct->info) && !LJ_64) {
        reg = reg_make(r, REGKIND_GPR32CD);
      } else if (ctype_isnum(ct->info) && (ct->info & CTF_UNSIGNED) && ct->size == 4) {
        reg = reg_make(r, REGKIND_GPR32CD);
      }
    }

    intrins->flags |= reg & 0xff00;

    if (reg & REGFLAG_BLACKLIST) {
      lj_err_callerv(L, LJ_ERR_FFI_BADREG, "blacklisted", strdata(str), listname);
    }

    if (regout) {
      regout[count++] = (uint8_t)reg;
    }
  }

  if (regsetid == REGSET_IN) {
    intrins->insz = (uint8_t)count;
  } else if (regsetid == REGSET_OUT) {
    intrins->outsz = (uint8_t)count;
  }

  return rset;
}

static void setopcode(lua_State *L, CIntrinsic *intrins, uint32_t opcode)
{
  int len;

  if (opcode == 0) {
    lj_err_callermsg(L, "bad opcode literal");
  }

#if LJ_TARGET_X86ORX64
  if (opcode <= 0xff) {
    len = 1;
  } else if (opcode <= 0xffff) {
    len = 2;
  } else if (opcode <= 0xffffff) {
    len = 3;
  } else {
    len = 4;
  }

  opcode = lj_bswap(opcode);
  if (len < 4) {
    opcode |= (uint8_t)(int8_t)-(len+1);
  } else {
    lj_err_callermsg(L, "bad opcode literal");
  }
#endif 

  intrins->opcode = opcode;
}

static int parse_opstr(lua_State *L, GCstr *opstr, CIntrinsic *intrins, int* buildflags)
{
  const char *op = strdata(opstr);
  uint32_t opcode = 0;
  uint32_t i;

  /* Parse the opcode number if this is not a template */
  if (op[0] != '?') {
    for (i = 0; i < opstr->len && lj_char_isxdigit((uint8_t)op[i]); i++) {
    }

    if (i == 0 || i > 8) {
      /* invalid or no hex number */
      lj_err_callerv(L, LJ_ERR_FFI_BADOPSTR, op, "invalid opcode number");
    }

    /* Scan hex digits. */
    for (; i; i--, op++) {
      uint32_t d = *op; if (d > '9') d += 9;
      opcode = (opcode << 4) + (d & 15);
    }

  } else {
    *buildflags |= INTRINSFLAG_TEMPLATE;
    op++;
  }
  
  return opcode;
}

extern int lj_asm_intrins(lua_State *L, IntrinWrapState *state);

static IntrinsicWrapper lj_intrinsic_buildwrap(lua_State *L, CIntrinsic *intrins,
                                               void* target, MSize targetsz, RegSet mod)
{
  IntrinWrapState state = { 0 };
  state.intrins = intrins;
  state.target = target;
  state.targetsz = targetsz;
  state.mod = mod;
  state.wrapper = 0;

  int err = lj_asm_intrins(L, &state);

  if (err != 0) {
    const char* reason = "unknown error";

    if (err == -(LJ_TRERR_BADRA+2)) {
      reason = "too many live registers";
    } else if (err == -(LJ_TRERR_MCODEOV+2)) {
      reason = "code too large for mcode area";
    } else if(err == -1 && tvisstr(L->top-1)) {
      reason = strVdata(L->top-1);
    }

    lj_err_callerv(L, LJ_ERR_FFI_INTRWRAP, reason);
  }

  return (IntrinsicWrapper)state.wrapper;
}

CTypeID lj_intrinsic_template(lua_State *L, int narg)
{
  CTState *cts = ctype_cts(L);
  CType *ct;
  CTypeID id;
  CIntrinsic* intrins;
  GCstr *name = lj_lib_checkstr(L, narg);

  id = lj_ctype_getname(cts, &ct, name, 1u << CT_FUNC);

  if (!id) {
    lj_err_argv(L, narg, LJ_ERR_FFI_NODECL, name);
  } else if (!ctype_isintrinsic(ct->info)) {
    lj_err_arg(L, narg, LJ_ERR_FFI_INVTYPE);
  }

  intrins = lj_intrinsic_get(cts, ct->size);

  /* Can't be a template if it an opcode */
  if ((intrins->opcode && intrins->outsz <= 4) || intrins->wrapped)
    lj_err_arg(L, narg, LJ_ERR_FFI_INVTYPE);

  return id;
}

int lj_intrinsic_create(lua_State *L)
{
  CTState *cts = ctype_cts(L);  
  CTypeID id = lj_intrinsic_template(L, 1);
  void *intrinsmc;
  MSize asmsz;
  CIntrinsic* intrins = lj_intrinsic_get(cts, ctype_get(cts, id)->size);
  
  lj_cconv_ct_tv(cts, ctype_get(cts, CTID_P_CVOID), (uint8_t *)&intrinsmc,
                 L->base+1, CCF_ARG(2));

  asmsz = lj_lib_checkint(L, 3);
  if (asmsz <= 0 || asmsz > 0xffff ||
      asmsz > (MSize)(L2J(L)->param[JIT_P_sizemcode] << 10)) {
    lj_err_callermsg(L, "bad code size");
  }

  intrinsmc = lj_intrinsic_buildwrap(L, intrins, intrinsmc, asmsz, 
                                     intrin_getmodrset(cts, intrins));
  lj_intrinsic_new(L, id, intrinsmc);
  return 1;
}

GCcdata *lj_intrinsic_createffi(CTState *cts, CType *func)
{
  GCcdata *cd;
  CIntrinsic *intrins = lj_intrinsic_get(cts, func->size);
  CTypeID id = ctype_typeid(cts, func);
  RegSet mod = intrin_getmodrset(cts, intrins);
  uint32_t op = intrins->opcode;
  void* mcode = ((char*)&op) + (4-intrin_oplen(intrins));
  
  if (intrins->opcode == 0) {
    lj_err_callermsg(cts->L, "expected non template intrinsic");
  }

  intrins->wrapped = lj_intrinsic_buildwrap(cts->L, intrins, mcode,
                                            intrin_oplen(intrins), mod);

  cd = lj_cdata_new(cts, id, CTSIZE_PTR);
  *(void **)cdataptr(cd) = intrins->wrapped;
  return cd;
}

int lj_intrinsic_fromcdef(lua_State *L, CTypeID fid, GCstr *opstr, uint32_t imm)
{
  CTState *cts = ctype_cts(L);
  CType *func = ctype_get(cts, fid);
  CTypeID sib = func->sib, retid = ctype_cid(func->info);
  uint32_t opcode;
  int buildflags = 0;
  CIntrinsic _intrins;
  CIntrinsic* intrins = &_intrins;
  memset(intrins, 0, sizeof(CIntrinsic));
  
  opcode = parse_opstr(L, opstr, intrins, &buildflags);

  if (!opcode && !(buildflags & INTRINSFLAG_TEMPLATE)) {
    return 0;
  }

  if (sib) {
    process_reglist(L, intrins, REGSET_IN, sib);
  }
  

  if (retid != CTID_VOID) {
    CType *ct = ctype_get(cts, retid);

    /* Check if the intrinsic had __reglist declared on it */
    if (ctype_isfield(ct->info)) {
      process_reglist(L, intrins, REGSET_OUT, retid);
      sib = retid;
    }
  } else {
    sib = retid;
  }

  /* If were a template theres no opcode to set */
  if (opcode) {
    setopcode(L, intrins, opcode);
  } 
  register_intrinsic(L, intrins, ctype_get(cts, fid));

  lua_assert(sib > 0 && sib < cts->top);
  return sib;
}

/* Pre-create cdata for any output values that need boxing the wrapper will directly
 * save the values into the cdata 
 */
static void *setup_results(lua_State *L, CIntrinsic *intrins, CTypeID id) 
{
  MSize i;
  CTState *cts = ctype_cts(L);
  CTypeID sib = 0;
  void *outcontext = L->top;

  if (id == CTID_VOID)
    return NULL;

  sib = id;
  for (i = 0; i < intrins->outsz; i++) {
    CType *ret = ctype_get(cts, sib);
    CTypeID retid = ctype_cid(ret->info);
    CType *ct = ctype_raw(cts, retid);
    CTypeID rawid = ctype_typeid(cts, ct);
    lua_assert(ctype_isfield(ret->info) && ctype_cid(ret->info));
    sib = ret->sib;

    /* Don't box what can be represented with a lua_number */
    if (rawid == CTID_INT32 || rawid == CTID_FLOAT || rawid == CTID_DOUBLE)
      ct = NULL;

    if (ct) {
      GCcdata *cd;
      if (!(ct->info & CTF_VLA) && ctype_align(ct->info) <= CT_MEMALIGN)
        cd = lj_cdata_new(cts, retid, ct->size);
      else
        cd = lj_cdata_newv(L, retid, ct->size, ctype_align(ct->info));

      setcdataV(L, L->top++, cd);
    } else {
      L->top++;
    }
  }

  return outcontext;
}

int lj_intrinsic_call(CTState *cts, CType *ct)
{
  lua_State *L = cts->L;
  CIntrinsic *intrins = lj_intrinsic_get(cts, ct->size);
  CTypeID fid, funcid = ctype_typeid(cts, ct);
  TValue *o;
  MSize ngpr = 0, nfpr = 0, narg;
  void* outcontent = L->top;
  uint32_t reg = 0;
  RegContext context;
  memset(&context, 0, sizeof(RegContext));

  /* Skip initial attributes. */
  fid = ct->sib;
  while (fid) {
    CType *ctf = ctype_get(cts, fid);
    if (!ctype_isattrib(ctf->info)) break;
    fid = ctf->sib;
  }

  narg = (MSize)((L->top-L->base)-1);

  /* Check for wrong number of arguments passed in. */
  if (narg < intrins->insz || narg > intrins->insz) {
    lj_err_caller(L, LJ_ERR_FFI_NUMARG);  
  }
    
  /* Walk through all passed arguments. */
  for (o = L->base+1, narg = 0; narg < intrins->insz; o++, narg++) {
    CType *ctf = ctype_get(cts, fid);
    CType *d = ctype_get(cts, ctf->size >> 16); /* Use saved raw type we want to cast to */
    void *dp;
    fid = ctf->sib;
    lua_assert(ctype_isfield(ctf->info));

    reg = ctf->size & 0xff;

    /* nil only makes sense for gpr based ptr arguments */
    if (tvisnil(o) && (!reg_isgpr(reg) || !ctype_isptr(d->info))) {
      lj_err_arg(L, narg+1, LJ_ERR_NOVAL);
    }

    if (reg_isgpr(reg)) {
      lua_assert((ctype_isnum(d->info) && d->size <= 8) || ctype_isptr(d->info));
      dp = &context.gpr[ngpr++];
    } else {
      lua_assert(reg_isvec(reg) || (ctype_isnum(d->info) && (d->info & CTF_FP)));
      dp = &context.fpr[nfpr++];
    }

    lj_cconv_ct_tv(cts, d, (uint8_t *)dp, o, CCF_ARG(narg+1));
  }

  /* Pass in the return type chain so the results are typed */
  outcontent = setup_results(L, intrins, ctype_cid(ctype_get(cts, funcid)->info));

  /* Execute the intrinsic through the wrapper created on first lookup */
  return (*(IntrinsicWrapper*)cdataptr(cdataV(L->base)))(&context, outcontent);
}

void lj_intrinsic_init(lua_State *L)
{
  uint32_t i, count = (uint32_t)(sizeof(reglut)/sizeof(RegEntry));
  GCtab *t = ctype_cts(L)->miscmap;

  /* Build register name lookup table */
  for (i = 0; i < count; i++) {
    TValue *slot = lj_tab_setstr(L, t, lj_str_newz(L, reglut[i].name));
    setlightudV(slot, (void*)(uintptr_t)reglut[i].slot);
  }
}

#else

int lj_intrinsic_call(CTState *cts, CType *ct)
{
  UNUSED(cts); UNUSED(ct);
  return 0;
}

void lj_intrinsic_init(lua_State *L)
{
}
#endif




