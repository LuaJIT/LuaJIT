/*
** FFI Intrinsic system.
*/

#ifndef _LJ_INTRINSIC_H
#define _LJ_INTRINSIC_H

#include "lj_arch.h"
#include "lj_obj.h"
#include "lj_clib.h"
#include "lj_ctype.h"

#if !defined(LJ_INTRINS_MAXREG) || LJ_INTRINS_MAXREG < 8
#define LJ_INTRINS_MAXREG 8
#endif

/* The max number of dynamic registers in each reglist(in/out)*/
#define LJ_INTRINS_MAXDYNREG 2

typedef struct LJ_ALIGN(16) RegContext {
  intptr_t gpr[LJ_INTRINS_MAXREG];
  double fpr[LJ_INTRINS_MAXREG];
} RegContext;

typedef enum REGMODE {
  DYNREG_FIXED = 0,
  /* one input register and optionally one output */
  DYNREG_ONE,
  /* 1(R) register in, 1 out(M) which can be a memory address to store the value */
  DYNREG_ONESTORE,
  /* 2 in 0 out first must always be treated as indirect */
  DYNREG_TWOSTORE,
  /* one input(M) register and the second is part of part of the opcode */
  DYNREG_OPEXT,
  /* Two input register and one output same register that's same RID the second input */ 
  DYNREG_INOUT,
  /* Two input registers with M dynamic output register */
  DYNREG_TWOIN,

  DYNREG_SWAPREGS = DYNREG_INOUT,
} REGMODE;

typedef enum INTRINSFLAGS {
  INTRINSFLAG_REGMODEMASK = 7,

  INTRINSFLAG_MEMORYSIDE   = 0x08, /* has memory side effects so needs an IR memory barrier */

  /* Intrinsic should be emitted as a naked function that is called */
  INTRINSFLAG_CALLED = 0x20,
  /* MODRM should always be set as indirect mode */
  INTRINSFLAG_INDIRECT = 0x40,
  /* Don't fuse load into op */
  INTRINSFLAG_NOFUSE   = 0x80,
  /* Force REX.w 64 bit size override bit to be set for x64 */
  INTRINSFLAG_REXW     = 0x100,
  /* Append a user supplied prefixed before the opcode and its REX byte */
  INTRINSFLAG_PREFIX   = 0x200,
  /* Opcode has an immediate byte that needs to be set at construction time */
  INTRINSFLAG_IMMB     = 0x400,
  /* Opcode is larger than the emit system normally handles x86/x64(4 bytes) */
  INTRINSFLAG_LARGEOP  = 0x800,
 
  /* Opcode uses ymm registers */
  INTRINSFLAG_VEX256   = 0x4000,
  /* Input parameters names explicitly declare input registers */
  INTRINSFLAG_EXPLICTREGS = 0x10000,
  /* Intrinsic is a template with no machine code set until instantiate at runtime with
  ** user supplied code.
  */
  INTRINSFLAG_TEMPLATE    = 0x40000,

  INTRINSFLAG_CALLEDIND = INTRINSFLAG_CALLED | INTRINSFLAG_INDIRECT
} INTRINSFLAGS;

typedef struct AsmHeader {
  union{
    uintptr_t target;
    struct {
      uint16_t asmsz;
      uint16_t asmofs;
    };
  };
  uint32_t totalzs;
} AsmHeader;

#define intrin_regmode(intrins) ((intrins)->flags & INTRINSFLAG_REGMODEMASK)
#define intrin_setregmode(intrins, mode) \
  (intrins)->flags = ((intrins)->flags & ~INTRINSFLAG_REGMODEMASK)|(mode)

#define intrin_getopextb(intrins) ((intrins)->out[3])
#define intrin_setopextb(intrins, opext) \
  lua_assert((intrins)->outsz < 4); \
  ((intrins)->out[3] = (opext))
#define intrin_oplen(intrins) (((intrins)->flags & INTRINSFLAG_LARGEOP) ? 4 : (-(int8_t)(intrins)->opcode)-1)

/* odd numbered have an dynamic output */
#define intrin_dynrout(intrins) (intrin_regmode(intrins) && reg_isdyn(intrins->out[0]))
/* Get the optional RegSet of registers modified by the intrinsic */
#define intrin_getmodrset(cts, intrins) \
  ((ctype_get(cts, (intrins)->id)->size >> 16) ? \
    ctype_get(cts, ctype_get(cts, (intrins)->id)->size >> 16)->size : 0)

#define RKDEF_FPR(_) \
  _(FPR64, IRT_NUM,   CTID_DOUBLE) \
  _(FPR32, IRT_FLOAT, CTID_FLOAT) \
  _(V128,  0,         0) \
  _(V256,  0,         0) \
  _(FPR6,  0,         0) \
  _(FPR7,  0,         0) \

#define RKDEF_GPR(_) \
  _(GPRI32,  IRT_INT, CTID_INT32) \
  _(GPR32CD, IRT_U32, CTID_UINT32) \
  _(GPR64,   IRT_U64, CTID_UINT64) \
  _(GPR3,    0,       0) \
  _(GPR4,    0,       0) \
  _(GPR5,    0,       0) \
  _(GPR6,    0,       0) \
  _(GPR7,    0,       0) \
                
#define MKREGKIND(name, irt, ct) REGKIND_##name,

typedef enum REGKINDGPR {
  RKDEF_GPR(MKREGKIND)
} REGKINDGPR;

typedef enum REGKINDFPR {
  RKDEF_FPR(MKREGKIND)
  REGKIND_VEC_START = REGKIND_V128,
} REGKINDFPR;

uint8_t regkind_it[16];
CTypeID1 regkind_ct[16];

#define reg_rid(r) ((r)&63)
#define reg_kind(r) (((r) >> 6) & 3)
#define reg_make(r, kind) ((r) | (kind << 6))
#define reg_setrid(reg, rid) (((reg)&0xc0) | reg_rid(rid))
#define reg_isgpr(reg) (reg_rid(reg) < RID_MAX_GPR)
#define reg_isfp(reg) (reg_rid(reg) >= RID_MIN_FPR)
#define reg_isvec(reg) (reg_rid(reg) >= RID_MIN_FPR && reg_kind(reg) >= REGKIND_VEC_START)
#define reg_isdyn(reg) (reg_rid(reg) == RID_DYN_GPR || reg_rid(reg) == RID_DYN_FPR)

#define reg_irt(reg) (reg_isgpr(reg) ? rk_irtgpr(reg_kind(reg)) : rk_irtfpr(reg_kind(reg)))
#define rk_irtgpr(kind) ((IRType)regkind_it[(kind)])
#define rk_irtfpr(kind) ((IRType)regkind_it[(kind)+8])
#define rk_irt(rid, kind) ((rid) < RID_MAX_GPR ? rk_irtgpr(kind) : rk_irtfpr(kind))
#define rk_isvec(kind) ((kind) >= REGKIND_VEC_START)

#define rk_ctypegpr(kind) (regkind_ct[(kind)])
#define rk_ctypefpr(kind) (regkind_ct[(kind)+8])
#define rk_ctype(rid, kind) ((rid) < RID_MAX_GPR ? rk_ctypegpr(kind) : rk_ctypefpr(kind))

LJ_FUNC void lj_intrinsic_init(lua_State *L);
LJ_FUNC int lj_intrinsic_create(lua_State *L);
LJ_FUNC GCcdata *lj_intrinsic_createffi(CTState *cts, CType *func);
LJ_FUNC int lj_intrinsic_fromcdef(lua_State *L, CTypeID fid, GCstr *opcode, uint32_t imm);
LJ_FUNC int lj_intrinsic_call(CTState *cts, CType *ct);
int lj_intrinsic_getreg(CTState *cts, GCstr *name);

#define LJ_INTRINS_MAXID 0x1fff

static LJ_AINLINE CIntrinsic *lj_intrinsic_get(CTState *cts, CTSize id)
{
  lua_assert((id & LJ_INTRINS_MAXID) < cts->intr.top);
  return cts->intr.tab + (id & LJ_INTRINS_MAXID);
}
#endif

