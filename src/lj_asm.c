/*
** IR assembler (SSA IR -> machine code).
** Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
*/

#define lj_asm_c
#define LUA_CORE

#include "lj_obj.h"

#if LJ_HASJIT

#include "lj_gc.h"
#include "lj_str.h"
#include "lj_tab.h"
#include "lj_frame.h"
#if LJ_HASFFI
#include "lj_ctype.h"
#endif
#include "lj_ir.h"
#include "lj_jit.h"
#include "lj_iropt.h"
#include "lj_mcode.h"
#include "lj_iropt.h"
#include "lj_trace.h"
#include "lj_snap.h"
#include "lj_asm.h"
#include "lj_dispatch.h"
#include "lj_vm.h"
#include "lj_target.h"

/* -- Assembler state and common macros ----------------------------------- */

/* Assembler state. */
typedef struct ASMState {
  RegCost cost[RID_MAX];  /* Reference and blended allocation cost for regs. */

  MCode *mcp;		/* Current MCode pointer (grows down). */
  MCode *mclim;		/* Lower limit for MCode memory + red zone. */

  IRIns *ir;		/* Copy of pointer to IR instructions/constants. */
  jit_State *J;		/* JIT compiler state. */

  x86ModRM mrm;		/* Fused x86 address operand. */

  RegSet freeset;	/* Set of free registers. */
  RegSet modset;	/* Set of registers modified inside the loop. */
  RegSet weakset;	/* Set of weakly referenced registers. */
  RegSet phiset;	/* Set of PHI registers. */

  uint32_t flags;	/* Copy of JIT compiler flags. */
  int loopinv;		/* Loop branch inversion (0:no, 1:yes, 2:yes+CC_P). */

  int32_t evenspill;	/* Next even spill slot. */
  int32_t oddspill;	/* Next odd spill slot (or 0). */

  IRRef curins;		/* Reference of current instruction. */
  IRRef stopins;	/* Stop assembly before hitting this instruction. */
  IRRef orignins;	/* Original T->nins. */

  IRRef snapref;	/* Current snapshot is active after this reference. */
  IRRef snaprename;	/* Rename highwater mark for snapshot check. */
  SnapNo snapno;	/* Current snapshot number. */
  SnapNo loopsnapno;	/* Loop snapshot number. */

  IRRef fuseref;	/* Fusion limit (loopref, 0 or FUSE_DISABLED). */
  IRRef sectref;	/* Section base reference (loopref or 0). */
  IRRef loopref;	/* Reference of LOOP instruction (or 0). */

  BCReg topslot;	/* Number of slots for stack check (unless 0). */
  MSize gcsteps;	/* Accumulated number of GC steps (per section). */

  GCtrace *T;		/* Trace to assemble. */
  GCtrace *parent;	/* Parent trace (or NULL). */

  MCode *mcbot;		/* Bottom of reserved MCode. */
  MCode *mctop;		/* Top of generated MCode. */
  MCode *mcloop;	/* Pointer to loop MCode (or NULL). */
  MCode *invmcp;	/* Points to invertible loop branch (or NULL). */
  MCode *testmcp;	/* Pending opportunity to remove test r,r. */
  MCode *realign;	/* Realign loop if not NULL. */

  IRRef1 phireg[RID_MAX];  /* PHI register references. */
  uint16_t parentmap[LJ_MAX_JSLOTS];  /* Parent slot to RegSP map. */
} ASMState;

#define IR(ref)			(&as->ir[(ref)])

#define ASMREF_TMP1		REF_TRUE	/* Temp. register. */
#define ASMREF_TMP2		REF_FALSE	/* Temp. register. */
#define ASMREF_L		REF_NIL		/* Stores register for L. */

/* Check for variant to invariant references. */
#define iscrossref(as, ref)	((ref) < as->sectref)

/* Inhibit memory op fusion from variant to invariant references. */
#define FUSE_DISABLED		(~(IRRef)0)
#define mayfuse(as, ref)	((ref) > as->fuseref)
#define neverfuse(as)		(as->fuseref == FUSE_DISABLED)
#define canfuse(as, ir)		(!neverfuse(as) && !irt_isphi((ir)->t))
#define opisfusableload(o) \
  ((o) == IR_ALOAD || (o) == IR_HLOAD || (o) == IR_ULOAD || \
   (o) == IR_FLOAD || (o) == IR_XLOAD || (o) == IR_SLOAD || (o) == IR_VLOAD)

/* Instruction selection for XMM moves. */
#define XMM_MOVRR(as)	((as->flags & JIT_F_SPLIT_XMM) ? XO_MOVSD : XO_MOVAPS)
#define XMM_MOVRM(as)	((as->flags & JIT_F_SPLIT_XMM) ? XO_MOVLPD : XO_MOVSD)

/* Sparse limit checks using a red zone before the actual limit. */
#define MCLIM_REDZONE	64
#define checkmclim(as) \
  if (LJ_UNLIKELY(as->mcp < as->mclim)) asm_mclimit(as)

static LJ_NORET LJ_NOINLINE void asm_mclimit(ASMState *as)
{
  lj_mcode_limiterr(as->J, (size_t)(as->mctop - as->mcp + 4*MCLIM_REDZONE));
}

/* -- Emit x86 instructions ----------------------------------------------- */

#define MODRM(mode, r1, r2)	((MCode)((mode)+(((r1)&7)<<3)+((r2)&7)))

#if LJ_64
#define REXRB(p, rr, rb) \
    { MCode rex = 0x40 + (((rr)>>1)&4) + (((rb)>>3)&1); \
      if (rex != 0x40) *--(p) = rex; }
#define FORCE_REX		0x200
#define REX_64			(FORCE_REX|0x080000)
#else
#define REXRB(p, rr, rb)	((void)0)
#define FORCE_REX		0
#define REX_64			0
#endif

#define emit_i8(as, i)		(*--as->mcp = (MCode)(i))
#define emit_i32(as, i)		(*(int32_t *)(as->mcp-4) = (i), as->mcp -= 4)
#define emit_u32(as, u)		(*(uint32_t *)(as->mcp-4) = (u), as->mcp -= 4)

#define emit_x87op(as, xo) \
  (*(uint16_t *)(as->mcp-2) = (uint16_t)(xo), as->mcp -= 2)

/* op */
static LJ_AINLINE MCode *emit_op(x86Op xo, Reg rr, Reg rb, Reg rx,
				 MCode *p, int delta)
{
  int n = (int8_t)xo;
#if defined(__GNUC__)
  if (__builtin_constant_p(xo) && n == -2)
    p[delta-2] = (MCode)(xo >> 24);
  else if (__builtin_constant_p(xo) && n == -3)
    *(uint16_t *)(p+delta-3) = (uint16_t)(xo >> 16);
  else
#endif
    *(uint32_t *)(p+delta-5) = (uint32_t)xo;
  p += n + delta;
#if LJ_64
  {
    uint32_t rex = 0x40 + ((rr>>1)&(4+(FORCE_REX>>1)))+((rx>>2)&2)+((rb>>3)&1);
    if (rex != 0x40) {
      rex |= (rr >> 16);
      if (n == -4) { *p = (MCode)rex; rex = (MCode)(xo >> 8); }
      else if ((xo & 0xffffff) == 0x6600fd) { *p = (MCode)rex; rex = 0x66; }
      *--p = (MCode)rex;
    }
  }
#else
  UNUSED(rr); UNUSED(rb); UNUSED(rx);
#endif
  return p;
}

/* op + modrm */
#define emit_opm(xo, mode, rr, rb, p, delta) \
  (p[(delta)-1] = MODRM((mode), (rr), (rb)), \
   emit_op((xo), (rr), (rb), 0, (p), (delta)))

/* op + modrm + sib */
#define emit_opmx(xo, mode, scale, rr, rb, rx, p) \
  (p[-1] = MODRM((scale), (rx), (rb)), \
   p[-2] = MODRM((mode), (rr), RID_ESP), \
   emit_op((xo), (rr), (rb), (rx), (p), -1))

/* op r1, r2 */
static void emit_rr(ASMState *as, x86Op xo, Reg r1, Reg r2)
{
  MCode *p = as->mcp;
  as->mcp = emit_opm(xo, XM_REG, r1, r2, p, 0);
}

#if LJ_64 && defined(LUA_USE_ASSERT)
/* [addr] is sign-extended in x64 and must be in lower 2G (not 4G). */
static int32_t ptr2addr(const void *p)
{
  lua_assert((uintptr_t)p < (uintptr_t)0x80000000);
  return i32ptr(p);
}
#else
#define ptr2addr(p)	(i32ptr((p)))
#endif

/* op r, [addr] */
static void emit_rma(ASMState *as, x86Op xo, Reg rr, const void *addr)
{
  MCode *p = as->mcp;
  *(int32_t *)(p-4) = ptr2addr(addr);
#if LJ_64
  p[-5] = MODRM(XM_SCALE1, RID_ESP, RID_EBP);
  as->mcp = emit_opm(xo, XM_OFS0, rr, RID_ESP, p, -5);
#else
  as->mcp = emit_opm(xo, XM_OFS0, rr, RID_EBP, p, -4);
#endif
}

/* op r, [base+ofs] */
static void emit_rmro(ASMState *as, x86Op xo, Reg rr, Reg rb, int32_t ofs)
{
  MCode *p = as->mcp;
  x86Mode mode;
  if (ra_hasreg(rb)) {
    if (ofs == 0 && (rb&7) != RID_EBP) {
      mode = XM_OFS0;
    } else if (checki8(ofs)) {
      *--p = (MCode)ofs;
      mode = XM_OFS8;
    } else {
      p -= 4;
      *(int32_t *)p = ofs;
      mode = XM_OFS32;
    }
    if ((rb&7) == RID_ESP)
      *--p = MODRM(XM_SCALE1, RID_ESP, RID_ESP);
  } else {
    *(int32_t *)(p-4) = ofs;
#if LJ_64
    p[-5] = MODRM(XM_SCALE1, RID_ESP, RID_EBP);
    p -= 5;
    rb = RID_ESP;
#else
    p -= 4;
    rb = RID_EBP;
#endif
    mode = XM_OFS0;
  }
  as->mcp = emit_opm(xo, mode, rr, rb, p, 0);
}

/* op r, [base+idx*scale+ofs] */
static void emit_rmrxo(ASMState *as, x86Op xo, Reg rr, Reg rb, Reg rx,
		       x86Mode scale, int32_t ofs)
{
  MCode *p = as->mcp;
  x86Mode mode;
  if (ofs == 0 && (rb&7) != RID_EBP) {
    mode = XM_OFS0;
  } else if (checki8(ofs)) {
    mode = XM_OFS8;
    *--p = (MCode)ofs;
  } else {
    mode = XM_OFS32;
    p -= 4;
    *(int32_t *)p = ofs;
  }
  as->mcp = emit_opmx(xo, mode, scale, rr, rb, rx, p);
}

/* op r, i */
static void emit_gri(ASMState *as, x86Group xg, Reg rb, int32_t i)
{
  MCode *p = as->mcp;
  x86Op xo;
  if (checki8(i)) {
    *--p = (MCode)i;
    xo = XG_TOXOi8(xg);
  } else {
    p -= 4;
    *(int32_t *)p = i;
    xo = XG_TOXOi(xg);
  }
  as->mcp = emit_opm(xo, XM_REG, (Reg)(xg & 7) | (rb & REX_64), rb, p, 0);
}

/* op [base+ofs], i */
static void emit_gmroi(ASMState *as, x86Group xg, Reg rb, int32_t ofs,
		       int32_t i)
{
  x86Op xo;
  if (checki8(i)) {
    emit_i8(as, i);
    xo = XG_TOXOi8(xg);
  } else {
    emit_i32(as, i);
    xo = XG_TOXOi(xg);
  }
  emit_rmro(as, xo, (Reg)(xg & 7), rb, ofs);
}

#define emit_shifti(as, xg, r, i) \
  (emit_i8(as, (i)), emit_rr(as, XO_SHIFTi, (Reg)(xg), (r)))

/* op r, rm/mrm */
static void emit_mrm(ASMState *as, x86Op xo, Reg rr, Reg rb)
{
  MCode *p = as->mcp;
  x86Mode mode = XM_REG;
  if (rb == RID_MRM) {
    rb = as->mrm.base;
    if (rb == RID_NONE) {
      rb = RID_EBP;
      mode = XM_OFS0;
      p -= 4;
      *(int32_t *)p = as->mrm.ofs;
      if (as->mrm.idx != RID_NONE)
	goto mrmidx;
#if LJ_64
      *--p = MODRM(XM_SCALE1, RID_ESP, RID_EBP);
      rb = RID_ESP;
#endif
    } else {
      if (as->mrm.ofs == 0 && (rb&7) != RID_EBP) {
	mode = XM_OFS0;
      } else if (checki8(as->mrm.ofs)) {
	*--p = (MCode)as->mrm.ofs;
	mode = XM_OFS8;
      } else {
	p -= 4;
	*(int32_t *)p = as->mrm.ofs;
	mode = XM_OFS32;
      }
      if (as->mrm.idx != RID_NONE) {
      mrmidx:
	as->mcp = emit_opmx(xo, mode, as->mrm.scale, rr, rb, as->mrm.idx, p);
	return;
      }
      if ((rb&7) == RID_ESP)
	*--p = MODRM(XM_SCALE1, RID_ESP, RID_ESP);
    }
  }
  as->mcp = emit_opm(xo, mode, rr, rb, p, 0);
}

static void emit_addptr(ASMState *as, Reg r, int32_t ofs)
{
  if (ofs) {
    if ((as->flags & JIT_F_LEA_AGU))
      emit_rmro(as, XO_LEA, r, r, ofs);
    else
      emit_gri(as, XG_ARITHi(XOg_ADD), r, ofs);
  }
}

/* op rm/mrm, i */
static void emit_gmrmi(ASMState *as, x86Group xg, Reg rb, int32_t i)
{
  x86Op xo;
  if (checki8(i)) {
    emit_i8(as, i);
    xo = XG_TOXOi8(xg);
  } else {
    emit_i32(as, i);
    xo = XG_TOXOi(xg);
  }
  emit_mrm(as, xo, (Reg)(xg & 7) | (rb & REX_64), (rb & ~REX_64));
}

/* -- Emit moves ---------------------------------------------------------- */

/* mov [base+ofs], i */
static void emit_movmroi(ASMState *as, Reg base, int32_t ofs, int32_t i)
{
  emit_i32(as, i);
  emit_rmro(as, XO_MOVmi, 0, base, ofs);
}

/* mov [base+ofs], r */
#define emit_movtomro(as, r, base, ofs) \
  emit_rmro(as, XO_MOVto, (r), (base), (ofs))

/* Get/set global_State fields. */
#define emit_opgl(as, xo, r, field) \
  emit_rma(as, (xo), (r), (void *)&J2G(as->J)->field)
#define emit_getgl(as, r, field)	emit_opgl(as, XO_MOV, (r), field)
#define emit_setgl(as, r, field)	emit_opgl(as, XO_MOVto, (r), field)
#define emit_setgli(as, field, i) \
  (emit_i32(as, i), emit_opgl(as, XO_MOVmi, 0, field))

/* mov r, i / xor r, r */
static void emit_loadi(ASMState *as, Reg r, int32_t i)
{
  /* XOR r,r is shorter, but modifies the flags. This is bad for HIOP. */
  if (i == 0 && !(LJ_32 && (IR(as->curins)->o == IR_HIOP ||
			    (as->curins+1 < as->T->nins &&
			     IR(as->curins+1)->o == IR_HIOP)))) {
    emit_rr(as, XO_ARITH(XOg_XOR), r, r);
  } else {
    MCode *p = as->mcp;
    *(int32_t *)(p-4) = i;
    p[-5] = (MCode)(XI_MOVri+(r&7));
    p -= 5;
    REXRB(p, 0, r);
    as->mcp = p;
  }
}

/* mov r, addr */
#define emit_loada(as, r, addr) \
  emit_loadi(as, (r), ptr2addr((addr)))

#if LJ_64
/* mov r, imm64 or shorter 32 bit extended load. */
static void emit_loadu64(ASMState *as, Reg r, uint64_t u64)
{
  if (checku32(u64)) {  /* 32 bit load clears upper 32 bits. */
    emit_loadi(as, r, (int32_t)u64);
  } else if (checki32((int64_t)u64)) {  /* Sign-extended 32 bit load. */
    MCode *p = as->mcp;
    *(int32_t *)(p-4) = (int32_t)u64;
    as->mcp = emit_opm(XO_MOVmi, XM_REG, REX_64, r, p, -4);
  } else {  /* Full-size 64 bit load. */
    MCode *p = as->mcp;
    *(uint64_t *)(p-8) = u64;
    p[-9] = (MCode)(XI_MOVri+(r&7));
    p[-10] = 0x48 + ((r>>3)&1);
    p -= 10;
    as->mcp = p;
  }
}
#endif

/* movsd r, [&tv->n] / xorps r, r */
static void emit_loadn(ASMState *as, Reg r, cTValue *tv)
{
  if (tvispzero(tv))  /* Use xor only for +0. */
    emit_rr(as, XO_XORPS, r, r);
  else
    emit_rma(as, XMM_MOVRM(as), r, &tv->n);
}

/* -- Emit branches ------------------------------------------------------- */

/* Label for short jumps. */
typedef MCode *MCLabel;

#if LJ_32 && LJ_HASFFI
/* jmp short target */
static void emit_sjmp(ASMState *as, MCLabel target)
{
  MCode *p = as->mcp;
  ptrdiff_t delta = target - p;
  lua_assert(delta == (int8_t)delta);
  p[-1] = (MCode)(int8_t)delta;
  p[-2] = XI_JMPs;
  as->mcp = p - 2;
}
#endif

/* jcc short target */
static void emit_sjcc(ASMState *as, int cc, MCLabel target)
{
  MCode *p = as->mcp;
  ptrdiff_t delta = target - p;
  lua_assert(delta == (int8_t)delta);
  p[-1] = (MCode)(int8_t)delta;
  p[-2] = (MCode)(XI_JCCs+(cc&15));
  as->mcp = p - 2;
}

/* jcc short (pending target) */
static MCLabel emit_sjcc_label(ASMState *as, int cc)
{
  MCode *p = as->mcp;
  p[-1] = 0;
  p[-2] = (MCode)(XI_JCCs+(cc&15));
  as->mcp = p - 2;
  return p;
}

/* Fixup jcc short target. */
static void emit_sfixup(ASMState *as, MCLabel source)
{
  source[-1] = (MCode)(as->mcp-source);
}

/* Return label pointing to current PC. */
#define emit_label(as)		((as)->mcp)

/* Compute relative 32 bit offset for jump and call instructions. */
static LJ_AINLINE int32_t jmprel(MCode *p, MCode *target)
{
  ptrdiff_t delta = target - p;
  lua_assert(delta == (int32_t)delta);
  return (int32_t)delta;
}

/* jcc target */
static void emit_jcc(ASMState *as, int cc, MCode *target)
{
  MCode *p = as->mcp;
  *(int32_t *)(p-4) = jmprel(p, target);
  p[-5] = (MCode)(XI_JCCn+(cc&15));
  p[-6] = 0x0f;
  as->mcp = p - 6;
}

/* call target */
static void emit_call_(ASMState *as, MCode *target)
{
  MCode *p = as->mcp;
#if LJ_64
  if (target-p != (int32_t)(target-p)) {
    /* Assumes RID_RET is never an argument to calls and always clobbered. */
    emit_rr(as, XO_GROUP5, XOg_CALL, RID_RET);
    emit_loadu64(as, RID_RET, (uint64_t)target);
    return;
  }
#endif
  *(int32_t *)(p-4) = jmprel(p, target);
  p[-5] = XI_CALL;
  as->mcp = p - 5;
}

#define emit_call(as, f)	emit_call_(as, (MCode *)(void *)(f))

/* -- Register allocator debugging ---------------------------------------- */

/* #define LUAJIT_DEBUG_RA */

#ifdef LUAJIT_DEBUG_RA

#include <stdio.h>
#include <stdarg.h>

#define RIDNAME(name)	#name,
static const char *const ra_regname[] = {
  GPRDEF(RIDNAME)
  FPRDEF(RIDNAME)
  "mrm",
  NULL
};
#undef RIDNAME

static char ra_dbg_buf[65536];
static char *ra_dbg_p;
static char *ra_dbg_merge;
static MCode *ra_dbg_mcp;

static void ra_dstart(void)
{
  ra_dbg_p = ra_dbg_buf;
  ra_dbg_merge = NULL;
  ra_dbg_mcp = NULL;
}

static void ra_dflush(void)
{
  fwrite(ra_dbg_buf, 1, (size_t)(ra_dbg_p-ra_dbg_buf), stdout);
  ra_dstart();
}

static void ra_dprintf(ASMState *as, const char *fmt, ...)
{
  char *p;
  va_list argp;
  va_start(argp, fmt);
  p = ra_dbg_mcp == as->mcp ? ra_dbg_merge : ra_dbg_p;
  ra_dbg_mcp = NULL;
  p += sprintf(p, "%08x  \e[36m%04d ", (uintptr_t)as->mcp, as->curins-REF_BIAS);
  for (;;) {
    const char *e = strchr(fmt, '$');
    if (e == NULL) break;
    memcpy(p, fmt, (size_t)(e-fmt));
    p += e-fmt;
    if (e[1] == 'r') {
      Reg r = va_arg(argp, Reg) & RID_MASK;
      if (r <= RID_MAX) {
	const char *q;
	for (q = ra_regname[r]; *q; q++)
	  *p++ = *q >= 'A' && *q <= 'Z' ? *q + 0x20 : *q;
      } else {
	*p++ = '?';
	lua_assert(0);
      }
    } else if (e[1] == 'f' || e[1] == 'i') {
      IRRef ref;
      if (e[1] == 'f')
	ref = va_arg(argp, IRRef);
      else
	ref = va_arg(argp, IRIns *) - as->ir;
      if (ref >= REF_BIAS)
	p += sprintf(p, "%04d", ref - REF_BIAS);
      else
	p += sprintf(p, "K%03d", REF_BIAS - ref);
    } else if (e[1] == 's') {
      uint32_t slot = va_arg(argp, uint32_t);
      p += sprintf(p, "[esp+0x%x]", sps_scale(slot));
    } else {
      lua_assert(0);
    }
    fmt = e+2;
  }
  va_end(argp);
  while (*fmt)
    *p++ = *fmt++;
  *p++ = '\e'; *p++ = '['; *p++ = 'm'; *p++ = '\n';
  if (p > ra_dbg_buf+sizeof(ra_dbg_buf)-256) {
    fwrite(ra_dbg_buf, 1, (size_t)(p-ra_dbg_buf), stdout);
    p = ra_dbg_buf;
  }
  ra_dbg_p = p;
}

#define RA_DBG_START()	ra_dstart()
#define RA_DBG_FLUSH()	ra_dflush()
#define RA_DBG_REF() \
  do { char *_p = ra_dbg_p; ra_dprintf(as, ""); \
       ra_dbg_merge = _p; ra_dbg_mcp = as->mcp; } while (0)
#define RA_DBGX(x)	ra_dprintf x

#else
#define RA_DBG_START()	((void)0)
#define RA_DBG_FLUSH()	((void)0)
#define RA_DBG_REF()	((void)0)
#define RA_DBGX(x)	((void)0)
#endif

/* -- Register allocator -------------------------------------------------- */

#define ra_free(as, r)		rset_set(as->freeset, (r))
#define ra_modified(as, r)	rset_set(as->modset, (r))
#define ra_weak(as, r)		rset_set(as->weakset, (r))
#define ra_noweak(as, r)	rset_clear(as->weakset, (r))

#define ra_used(ir)		(ra_hasreg((ir)->r) || ra_hasspill((ir)->s))

/* Setup register allocator. */
static void ra_setup(ASMState *as)
{
  /* Initially all regs (except the stack pointer) are free for use. */
  as->freeset = RSET_ALL;
  as->modset = RSET_EMPTY;
  as->weakset = RSET_EMPTY;
  as->phiset = RSET_EMPTY;
  memset(as->phireg, 0, sizeof(as->phireg));
  memset(as->cost, 0, sizeof(as->cost));
  as->cost[RID_ESP] = REGCOST(~0u, 0u);
}

/* Rematerialize constants. */
static Reg ra_rematk(ASMState *as, IRIns *ir)
{
  Reg r = ir->r;
  lua_assert(ra_hasreg(r) && !ra_hasspill(ir->s));
  ra_free(as, r);
  ra_modified(as, r);
  ir->r = RID_INIT;  /* Do not keep any hint. */
  RA_DBGX((as, "remat     $i $r", ir, r));
  if (ir->o == IR_KNUM) {
    emit_loadn(as, r, ir_knum(ir));
  } else if (ir->o == IR_BASE) {
    ra_sethint(ir->r, RID_BASE);  /* Restore BASE register hint. */
    emit_getgl(as, r, jit_base);
  } else if (ir->o == IR_KPRI) {  /* REF_NIL stores ASMREF_L register. */
    lua_assert(irt_isnil(ir->t));
    emit_getgl(as, r, jit_L);
#if LJ_64
  } else if (ir->o == IR_KINT64) {
    emit_loadu64(as, r, ir_kint64(ir)->u64);
#endif
  } else {
    lua_assert(ir->o == IR_KINT || ir->o == IR_KGC ||
	       ir->o == IR_KPTR || ir->o == IR_KKPTR || ir->o == IR_KNULL);
    emit_loadi(as, r, ir->i);
  }
  return r;
}

/* Force a spill. Allocate a new spill slot if needed. */
static int32_t ra_spill(ASMState *as, IRIns *ir)
{
  int32_t slot = ir->s;
  if (!ra_hasspill(slot)) {
    if (irt_is64(ir->t)) {
      slot = as->evenspill;
      as->evenspill += 2;
    } else if (as->oddspill) {
      slot = as->oddspill;
      as->oddspill = 0;
    } else {
      slot = as->evenspill;
      as->oddspill = slot+1;
      as->evenspill += 2;
    }
    if (as->evenspill > 256)
      lj_trace_err(as->J, LJ_TRERR_SPILLOV);
    ir->s = (uint8_t)slot;
  }
  return sps_scale(slot);
}

/* Release the temporarily allocated register in ASMREF_TMP1/ASMREF_TMP2. */
static Reg ra_releasetmp(ASMState *as, IRRef ref)
{
  IRIns *ir = IR(ref);
  Reg r = ir->r;
  lua_assert(ra_hasreg(r) && !ra_hasspill(ir->s));
  ra_free(as, r);
  ra_modified(as, r);
  ir->r = RID_INIT;
  return r;
}

/* Use 64 bit operations to handle 64 bit IR types. */
#if LJ_64
#define REX_64IR(ir, r)		((r) + (irt_is64((ir)->t) ? REX_64 : 0))
#else
#define REX_64IR(ir, r)		(r)
#endif

/* Generic move between two regs. */
static void ra_movrr(ASMState *as, IRIns *ir, Reg r1, Reg r2)
{
  UNUSED(ir);
  if (r1 < RID_MAX_GPR)
    emit_rr(as, XO_MOV, REX_64IR(ir, r1), r2);
  else
    emit_rr(as, XMM_MOVRR(as), r1, r2);
}

/* Restore a register (marked as free). Rematerialize or force a spill. */
static Reg ra_restore(ASMState *as, IRRef ref)
{
  IRIns *ir = IR(ref);
  if (irref_isk(ref) || ref == REF_BASE) {
    return ra_rematk(as, ir);
  } else {
    int32_t ofs = ra_spill(as, ir);  /* Force a spill slot. */
    Reg r = ir->r;
    lua_assert(ra_hasreg(r));
    ra_sethint(ir->r, r);  /* Keep hint. */
    ra_free(as, r);
    if (!rset_test(as->weakset, r)) {  /* Only restore non-weak references. */
      ra_modified(as, r);
      RA_DBGX((as, "restore   $i $r", ir, r));
      if (r < RID_MAX_GPR)
	emit_rmro(as, XO_MOV, REX_64IR(ir, r), RID_ESP, ofs);
      else
	emit_rmro(as, irt_isnum(ir->t) ? XMM_MOVRM(as) : XO_MOVSS,
		  r, RID_ESP, ofs);
    }
    return r;
  }
}

/* Save a register to a spill slot. */
static void ra_save(ASMState *as, IRIns *ir, Reg r)
{
  RA_DBGX((as, "save      $i $r", ir, r));
  if (r < RID_MAX_GPR)
    emit_rmro(as, XO_MOVto, REX_64IR(ir, r), RID_ESP, sps_scale(ir->s));
  else
    emit_rmro(as, irt_isnum(ir->t) ? XO_MOVSDto : XO_MOVSSto,
	      r, RID_ESP, sps_scale(ir->s));
}

#define MINCOST(r) \
  if (LJ_LIKELY(allow&RID2RSET(r)) && as->cost[r] < cost) \
    cost = as->cost[r]

/* Evict the register with the lowest cost, forcing a restore. */
static Reg ra_evict(ASMState *as, RegSet allow)
{
  IRRef ref;
  RegCost cost = ~(RegCost)0;
  lua_assert(allow != RSET_EMPTY);
  if (allow < RID2RSET(RID_MAX_GPR)) {
    MINCOST(RID_EAX);MINCOST(RID_ECX);MINCOST(RID_EDX);MINCOST(RID_EBX);
    MINCOST(RID_EBP);MINCOST(RID_ESI);MINCOST(RID_EDI);
#if LJ_64
    MINCOST(RID_R8D);MINCOST(RID_R9D);MINCOST(RID_R10D);MINCOST(RID_R11D);
    MINCOST(RID_R12D);MINCOST(RID_R13D);MINCOST(RID_R14D);MINCOST(RID_R15D);
#endif
  } else {
    MINCOST(RID_XMM0);MINCOST(RID_XMM1);MINCOST(RID_XMM2);MINCOST(RID_XMM3);
    MINCOST(RID_XMM4);MINCOST(RID_XMM5);MINCOST(RID_XMM6);MINCOST(RID_XMM7);
#if LJ_64
    MINCOST(RID_XMM8);MINCOST(RID_XMM9);MINCOST(RID_XMM10);MINCOST(RID_XMM11);
    MINCOST(RID_XMM12);MINCOST(RID_XMM13);MINCOST(RID_XMM14);MINCOST(RID_XMM15);
#endif
  }
  ref = regcost_ref(cost);
  lua_assert(ref >= as->T->nk && ref < as->T->nins);
  /* Preferably pick any weak ref instead of a non-weak, non-const ref. */
  if (!irref_isk(ref) && (as->weakset & allow)) {
    IRIns *ir = IR(ref);
    if (!rset_test(as->weakset, ir->r))
      ref = regcost_ref(as->cost[rset_pickbot((as->weakset & allow))]);
  }
  return ra_restore(as, ref);
}

/* Pick any register (marked as free). Evict on-demand. */
static Reg ra_pick(ASMState *as, RegSet allow)
{
  RegSet pick = as->freeset & allow;
  if (!pick)
    return ra_evict(as, allow);
  else
    return rset_picktop(pick);
}

/* Get a scratch register (marked as free). */
static Reg ra_scratch(ASMState *as, RegSet allow)
{
  Reg r = ra_pick(as, allow);
  ra_modified(as, r);
  RA_DBGX((as, "scratch        $r", r));
  return r;
}

/* Evict all registers from a set (if not free). */
static void ra_evictset(ASMState *as, RegSet drop)
{
  as->modset |= drop;
  drop &= ~as->freeset;
  while (drop) {
    Reg r = rset_picktop(drop);
    ra_restore(as, regcost_ref(as->cost[r]));
    rset_clear(drop, r);
    checkmclim(as);
  }
}

/* Evict (rematerialize) all registers allocated to constants. */
static void ra_evictk(ASMState *as)
{
  RegSet work = ~as->freeset & RSET_ALL;
  while (work) {
    Reg r = rset_pickbot(work);
    IRRef ref = regcost_ref(as->cost[r]);
    if (irref_isk(ref)) {
      ra_rematk(as, IR(ref));
      checkmclim(as);
    }
    rset_clear(work, r);
  }
}

/* Allocate a register for ref from the allowed set of registers.
** Note: this function assumes the ref does NOT have a register yet!
** Picks an optimal register, sets the cost and marks the register as non-free.
*/
static Reg ra_allocref(ASMState *as, IRRef ref, RegSet allow)
{
  IRIns *ir = IR(ref);
  RegSet pick = as->freeset & allow;
  Reg r;
  lua_assert(ra_noreg(ir->r));
  if (pick) {
    /* First check register hint from propagation or PHI. */
    if (ra_hashint(ir->r)) {
      r = ra_gethint(ir->r);
      if (rset_test(pick, r))  /* Use hint register if possible. */
	goto found;
      /* Rematerialization is cheaper than missing a hint. */
      if (rset_test(allow, r) && irref_isk(regcost_ref(as->cost[r]))) {
	ra_rematk(as, IR(regcost_ref(as->cost[r])));
	goto found;
      }
      RA_DBGX((as, "hintmiss  $f $r", ref, r));
    }
    /* Invariants should preferably get unmodified registers. */
    if (ref < as->loopref && !irt_isphi(ir->t)) {
      if ((pick & ~as->modset))
	pick &= ~as->modset;
      r = rset_pickbot(pick);  /* Reduce conflicts with inverse allocation. */
    } else {
#if LJ_64
      /* We've got plenty of regs, so get callee-save regs if possible. */
      if ((pick & ~RSET_SCRATCH))
	pick &= ~RSET_SCRATCH;
#endif
      r = rset_picktop(pick);
    }
  } else {
    r = ra_evict(as, allow);
  }
found:
  RA_DBGX((as, "alloc     $f $r", ref, r));
  ir->r = (uint8_t)r;
  rset_clear(as->freeset, r);
  ra_noweak(as, r);
  as->cost[r] = REGCOST_REF_T(ref, irt_t(ir->t));
  return r;
}

/* Allocate a register on-demand. */
static Reg ra_alloc1(ASMState *as, IRRef ref, RegSet allow)
{
  Reg r = IR(ref)->r;
  /* Note: allow is ignored if the register is already allocated. */
  if (ra_noreg(r)) r = ra_allocref(as, ref, allow);
  ra_noweak(as, r);
  return r;
}

/* Rename register allocation and emit move. */
static void ra_rename(ASMState *as, Reg down, Reg up)
{
  IRRef ren, ref = regcost_ref(as->cost[up] = as->cost[down]);
  IRIns *ir = IR(ref);
  ir->r = (uint8_t)up;
  as->cost[down] = 0;
  lua_assert((down < RID_MAX_GPR) == (up < RID_MAX_GPR));
  lua_assert(!rset_test(as->freeset, down) && rset_test(as->freeset, up));
  ra_free(as, down);  /* 'down' is free ... */
  ra_modified(as, down);
  rset_clear(as->freeset, up);  /* ... and 'up' is now allocated. */
  ra_noweak(as, up);
  RA_DBGX((as, "rename    $f $r $r", regcost_ref(as->cost[up]), down, up));
  ra_movrr(as, ir, down, up);  /* Backwards codegen needs inverse move. */
  if (!ra_hasspill(IR(ref)->s)) {  /* Add the rename to the IR. */
    lj_ir_set(as->J, IRT(IR_RENAME, IRT_NIL), ref, as->snapno);
    ren = tref_ref(lj_ir_emit(as->J));
    as->ir = as->T->ir;  /* The IR may have been reallocated. */
    IR(ren)->r = (uint8_t)down;
    IR(ren)->s = SPS_NONE;
  }
}

/* Pick a destination register (marked as free).
** Caveat: allow is ignored if there's already a destination register.
** Use ra_destreg() to get a specific register.
*/
static Reg ra_dest(ASMState *as, IRIns *ir, RegSet allow)
{
  Reg dest = ir->r;
  if (ra_hasreg(dest)) {
    ra_free(as, dest);
    ra_modified(as, dest);
  } else {
    dest = ra_scratch(as, allow);
  }
  if (LJ_UNLIKELY(ra_hasspill(ir->s))) ra_save(as, ir, dest);
  return dest;
}

/* Force a specific destination register (marked as free). */
static void ra_destreg(ASMState *as, IRIns *ir, Reg r)
{
  Reg dest = ra_dest(as, ir, RID2RSET(r));
  if (dest != r) {
    ra_scratch(as, RID2RSET(r));
    ra_movrr(as, ir, dest, r);
  }
}

/* Propagate dest register to left reference. Emit moves as needed.
** This is a required fixup step for all 2-operand machine instructions.
*/
static void ra_left(ASMState *as, Reg dest, IRRef lref)
{
  IRIns *ir = IR(lref);
  Reg left = ir->r;
  if (ra_noreg(left)) {
    if (irref_isk(lref)) {
      if (ir->o == IR_KNUM) {
	cTValue *tv = ir_knum(ir);
	/* FP remat needs a load except for +0. Still better than eviction. */
	if (tvispzero(tv) || !(as->freeset & RSET_FPR)) {
	  emit_loadn(as, dest, tv);
	  return;
	}
#if LJ_64
      } else if (ir->o == IR_KINT64) {
	emit_loadu64(as, dest, ir_kint64(ir)->u64);
	return;
#endif
      } else {
	lua_assert(ir->o == IR_KINT || ir->o == IR_KGC ||
		   ir->o == IR_KPTR || ir->o == IR_KKPTR || ir->o == IR_KNULL);
	emit_loadi(as, dest, ir->i);
	return;
      }
    }
    if (!ra_hashint(left) && !iscrossref(as, lref))
      ra_sethint(ir->r, dest);  /* Propagate register hint. */
    left = ra_allocref(as, lref, dest < RID_MAX_GPR ? RSET_GPR : RSET_FPR);
  }
  ra_noweak(as, left);
  /* Move needed for true 3-operand instruction: y=a+b ==> y=a; y+=b. */
  if (dest != left) {
    /* Use register renaming if dest is the PHI reg. */
    if (irt_isphi(ir->t) && as->phireg[dest] == lref) {
      ra_modified(as, left);
      ra_rename(as, left, dest);
    } else {
      ra_movrr(as, ir, dest, left);
    }
  }
}

/* -- Exit stubs ---------------------------------------------------------- */

/* Generate an exit stub group at the bottom of the reserved MCode memory. */
static MCode *asm_exitstub_gen(ASMState *as, ExitNo group)
{
  ExitNo i, groupofs = (group*EXITSTUBS_PER_GROUP) & 0xff;
  MCode *mxp = as->mcbot;
  MCode *mxpstart = mxp;
  if (mxp + (2+2)*EXITSTUBS_PER_GROUP+8+5 >= as->mctop)
    asm_mclimit(as);
  /* Push low byte of exitno for each exit stub. */
  *mxp++ = XI_PUSHi8; *mxp++ = (MCode)groupofs;
  for (i = 1; i < EXITSTUBS_PER_GROUP; i++) {
    *mxp++ = XI_JMPs; *mxp++ = (MCode)((2+2)*(EXITSTUBS_PER_GROUP - i) - 2);
    *mxp++ = XI_PUSHi8; *mxp++ = (MCode)(groupofs + i);
  }
  /* Push the high byte of the exitno for each exit stub group. */
  *mxp++ = XI_PUSHi8; *mxp++ = (MCode)((group*EXITSTUBS_PER_GROUP)>>8);
  /* Store DISPATCH at original stack slot 0. Account for the two push ops. */
  *mxp++ = XI_MOVmi;
  *mxp++ = MODRM(XM_OFS8, 0, RID_ESP);
  *mxp++ = MODRM(XM_SCALE1, RID_ESP, RID_ESP);
  *mxp++ = 2*sizeof(void *);
  *(int32_t *)mxp = ptr2addr(J2GG(as->J)->dispatch); mxp += 4;
  /* Jump to exit handler which fills in the ExitState. */
  *mxp++ = XI_JMP; mxp += 4;
  *((int32_t *)(mxp-4)) = jmprel(mxp, (MCode *)(void *)lj_vm_exit_handler);
  /* Commit the code for this group (even if assembly fails later on). */
  lj_mcode_commitbot(as->J, mxp);
  as->mcbot = mxp;
  as->mclim = as->mcbot + MCLIM_REDZONE;
  return mxpstart;
}

/* Setup all needed exit stubs. */
static void asm_exitstub_setup(ASMState *as, ExitNo nexits)
{
  ExitNo i;
  if (nexits >= EXITSTUBS_PER_GROUP*LJ_MAX_EXITSTUBGR)
    lj_trace_err(as->J, LJ_TRERR_SNAPOV);
  for (i = 0; i < (nexits+EXITSTUBS_PER_GROUP-1)/EXITSTUBS_PER_GROUP; i++)
    if (as->J->exitstubgroup[i] == NULL)
      as->J->exitstubgroup[i] = asm_exitstub_gen(as, i);
}

/* -- Snapshot and guard handling ----------------------------------------- */

/* Can we rematerialize a KNUM instead of forcing a spill? */
static int asm_snap_canremat(ASMState *as)
{
  Reg r;
  for (r = RID_MIN_FPR; r < RID_MAX_FPR; r++)
    if (irref_isk(regcost_ref(as->cost[r])))
      return 1;
  return 0;
}

/* Allocate registers or spill slots for refs escaping to a snapshot. */
static void asm_snap_alloc(ASMState *as)
{
  SnapShot *snap = &as->T->snap[as->snapno];
  SnapEntry *map = &as->T->snapmap[snap->mapofs];
  MSize n, nent = snap->nent;
  for (n = 0; n < nent; n++) {
    IRRef ref = snap_ref(map[n]);
    if (!irref_isk(ref)) {
      IRIns *ir = IR(ref);
      if (!ra_used(ir)) {
	RegSet allow = irt_isnum(ir->t) ? RSET_FPR : RSET_GPR;
	/* Get a weak register if we have a free one or can rematerialize. */
	if ((as->freeset & allow) ||
	    (allow == RSET_FPR && asm_snap_canremat(as))) {
	  Reg r = ra_allocref(as, ref, allow);  /* Allocate a register. */
	  if (!irt_isphi(ir->t))
	    ra_weak(as, r);  /* But mark it as weakly referenced. */
	  checkmclim(as);
	  RA_DBGX((as, "snapreg   $f $r", ref, ir->r));
	} else {
	  ra_spill(as, ir);  /* Otherwise force a spill slot. */
	  RA_DBGX((as, "snapspill $f $s", ref, ir->s));
	}
      }
    }
  }
}

/* All guards for a snapshot use the same exitno. This is currently the
** same as the snapshot number. Since the exact origin of the exit cannot
** be determined, all guards for the same snapshot must exit with the same
** RegSP mapping.
** A renamed ref which has been used in a prior guard for the same snapshot
** would cause an inconsistency. The easy way out is to force a spill slot.
*/
static int asm_snap_checkrename(ASMState *as, IRRef ren)
{
  SnapShot *snap = &as->T->snap[as->snapno];
  SnapEntry *map = &as->T->snapmap[snap->mapofs];
  MSize n, nent = snap->nent;
  for (n = 0; n < nent; n++) {
    IRRef ref = snap_ref(map[n]);
    if (ref == ren) {
      IRIns *ir = IR(ref);
      ra_spill(as, ir);  /* Register renamed, so force a spill slot. */
      RA_DBGX((as, "snaprensp $f $s", ref, ir->s));
      return 1;  /* Found. */
    }
  }
  return 0;  /* Not found. */
}

/* Prepare snapshot for next guard instruction. */
static void asm_snap_prep(ASMState *as)
{
  if (as->curins < as->snapref) {
    do {
      lua_assert(as->snapno != 0);
      as->snapno--;
      as->snapref = as->T->snap[as->snapno].ref;
    } while (as->curins < as->snapref);
    asm_snap_alloc(as);
    as->snaprename = as->T->nins;
  } else {
    /* Process any renames above the highwater mark. */
    for (; as->snaprename < as->T->nins; as->snaprename++) {
      IRIns *ir = IR(as->snaprename);
      if (asm_snap_checkrename(as, ir->op1))
	ir->op2 = REF_BIAS-1;  /* Kill rename. */
    }
  }
}

/* Emit conditional branch to exit for guard.
** It's important to emit this *after* all registers have been allocated,
** because rematerializations may invalidate the flags.
*/
static void asm_guardcc(ASMState *as, int cc)
{
  MCode *target = exitstub_addr(as->J, as->snapno);
  MCode *p = as->mcp;
  if (LJ_UNLIKELY(p == as->invmcp)) {
    as->loopinv = 1;
    *(int32_t *)(p+1) = jmprel(p+5, target);
    target = p;
    cc ^= 1;
    if (as->realign) {
      emit_sjcc(as, cc, target);
      return;
    }
  }
  emit_jcc(as, cc, target);
}

/* -- Memory operand fusion ----------------------------------------------- */

/* Arch-specific field offsets. */
static const uint8_t field_ofs[IRFL__MAX+1] = {
#define FLOFS(name, ofs)	(uint8_t)(ofs),
IRFLDEF(FLOFS)
#undef FLOFS
  0
};

/* Limit linear search to this distance. Avoids O(n^2) behavior. */
#define CONFLICT_SEARCH_LIM	31

/* Check if a reference is a signed 32 bit constant. */
static int asm_isk32(ASMState *as, IRRef ref, int32_t *k)
{
  if (irref_isk(ref)) {
    IRIns *ir = IR(ref);
    if (ir->o != IR_KINT64) {
      *k = ir->i;
      return 1;
    } else if (checki32((int64_t)ir_kint64(ir)->u64)) {
      *k = (int32_t)ir_kint64(ir)->u64;
      return 1;
    }
  }
  return 0;
}

/* Check if there's no conflicting instruction between curins and ref.
** Also avoid fusing loads if there are multiple references.
*/
static int noconflict(ASMState *as, IRRef ref, IROp conflict, int noload)
{
  IRIns *ir = as->ir;
  IRRef i = as->curins;
  if (i > ref + CONFLICT_SEARCH_LIM)
    return 0;  /* Give up, ref is too far away. */
  while (--i > ref) {
    if (ir[i].o == conflict)
      return 0;  /* Conflict found. */
    else if (!noload && (ir[i].op1 == ref || ir[i].op2 == ref))
      return 0;
  }
  return 1;  /* Ok, no conflict. */
}

/* Fuse array base into memory operand. */
static IRRef asm_fuseabase(ASMState *as, IRRef ref)
{
  IRIns *irb = IR(ref);
  as->mrm.ofs = 0;
  if (irb->o == IR_FLOAD) {
    IRIns *ira = IR(irb->op1);
    lua_assert(irb->op2 == IRFL_TAB_ARRAY);
    /* We can avoid the FLOAD of t->array for colocated arrays. */
    if (ira->o == IR_TNEW && ira->op1 <= LJ_MAX_COLOSIZE &&
	noconflict(as, irb->op1, IR_NEWREF, 1)) {
      as->mrm.ofs = (int32_t)sizeof(GCtab);  /* Ofs to colocated array. */
      return irb->op1;  /* Table obj. */
    }
  } else if (irb->o == IR_ADD && irref_isk(irb->op2)) {
    /* Fuse base offset (vararg load). */
    as->mrm.ofs = IR(irb->op2)->i;
    return irb->op1;
  }
  return ref;  /* Otherwise use the given array base. */
}

/* Fuse array reference into memory operand. */
static void asm_fusearef(ASMState *as, IRIns *ir, RegSet allow)
{
  IRIns *irx;
  lua_assert(ir->o == IR_AREF);
  as->mrm.base = (uint8_t)ra_alloc1(as, asm_fuseabase(as, ir->op1), allow);
  irx = IR(ir->op2);
  if (irref_isk(ir->op2)) {
    as->mrm.ofs += 8*irx->i;
    as->mrm.idx = RID_NONE;
  } else {
    rset_clear(allow, as->mrm.base);
    as->mrm.scale = XM_SCALE8;
    /* Fuse a constant ADD (e.g. t[i+1]) into the offset.
    ** Doesn't help much without ABCelim, but reduces register pressure.
    */
    if (!LJ_64 &&  /* Has bad effects with negative index on x64. */
	mayfuse(as, ir->op2) && ra_noreg(irx->r) &&
	irx->o == IR_ADD && irref_isk(irx->op2)) {
      as->mrm.ofs += 8*IR(irx->op2)->i;
      as->mrm.idx = (uint8_t)ra_alloc1(as, irx->op1, allow);
    } else {
      as->mrm.idx = (uint8_t)ra_alloc1(as, ir->op2, allow);
    }
  }
}

/* Fuse array/hash/upvalue reference into memory operand.
** Caveat: this may allocate GPRs for the base/idx registers. Be sure to
** pass the final allow mask, excluding any GPRs used for other inputs.
** In particular: 2-operand GPR instructions need to call ra_dest() first!
*/
static void asm_fuseahuref(ASMState *as, IRRef ref, RegSet allow)
{
  IRIns *ir = IR(ref);
  if (ra_noreg(ir->r)) {
    switch ((IROp)ir->o) {
    case IR_AREF:
      if (mayfuse(as, ref)) {
	asm_fusearef(as, ir, allow);
	return;
      }
      break;
    case IR_HREFK:
      if (mayfuse(as, ref)) {
	as->mrm.base = (uint8_t)ra_alloc1(as, ir->op1, allow);
	as->mrm.ofs = (int32_t)(IR(ir->op2)->op2 * sizeof(Node));
	as->mrm.idx = RID_NONE;
	return;
      }
      break;
    case IR_UREFC:
      if (irref_isk(ir->op1)) {
	GCfunc *fn = ir_kfunc(IR(ir->op1));
	GCupval *uv = &gcref(fn->l.uvptr[(ir->op2 >> 8)])->uv;
	as->mrm.ofs = ptr2addr(&uv->tv);
	as->mrm.base = as->mrm.idx = RID_NONE;
	return;
      }
      break;
    default:
      lua_assert(ir->o == IR_HREF || ir->o == IR_NEWREF || ir->o == IR_UREFO);
      break;
    }
  }
  as->mrm.base = (uint8_t)ra_alloc1(as, ref, allow);
  as->mrm.ofs = 0;
  as->mrm.idx = RID_NONE;
}

/* Fuse FLOAD/FREF reference into memory operand. */
static void asm_fusefref(ASMState *as, IRIns *ir, RegSet allow)
{
  lua_assert(ir->o == IR_FLOAD || ir->o == IR_FREF);
  as->mrm.ofs = field_ofs[ir->op2];
  as->mrm.idx = RID_NONE;
  if (irref_isk(ir->op1)) {
    as->mrm.ofs += IR(ir->op1)->i;
    as->mrm.base = RID_NONE;
  } else {
    as->mrm.base = (uint8_t)ra_alloc1(as, ir->op1, allow);
  }
}

/* Fuse string reference into memory operand. */
static void asm_fusestrref(ASMState *as, IRIns *ir, RegSet allow)
{
  IRIns *irr;
  lua_assert(ir->o == IR_STRREF);
  as->mrm.base = as->mrm.idx = RID_NONE;
  as->mrm.scale = XM_SCALE1;
  as->mrm.ofs = sizeof(GCstr);
  if (irref_isk(ir->op1)) {
    as->mrm.ofs += IR(ir->op1)->i;
  } else {
    Reg r = ra_alloc1(as, ir->op1, allow);
    rset_clear(allow, r);
    as->mrm.base = (uint8_t)r;
  }
  irr = IR(ir->op2);
  if (irref_isk(ir->op2)) {
    as->mrm.ofs += irr->i;
  } else {
    Reg r;
    /* Fuse a constant add into the offset, e.g. string.sub(s, i+10). */
    if (!LJ_64 &&  /* Has bad effects with negative index on x64. */
	mayfuse(as, ir->op2) && irr->o == IR_ADD && irref_isk(irr->op2)) {
      as->mrm.ofs += IR(irr->op2)->i;
      r = ra_alloc1(as, irr->op1, allow);
    } else {
      r = ra_alloc1(as, ir->op2, allow);
    }
    if (as->mrm.base == RID_NONE)
      as->mrm.base = (uint8_t)r;
    else
      as->mrm.idx = (uint8_t)r;
  }
}

static void asm_fusexref(ASMState *as, IRRef ref, RegSet allow)
{
  IRIns *ir = IR(ref);
  as->mrm.idx = RID_NONE;
  if (ir->o == IR_KPTR || ir->o == IR_KKPTR) {
    as->mrm.ofs = ir->i;
    as->mrm.base = RID_NONE;
  } else if (ir->o == IR_STRREF) {
    asm_fusestrref(as, ir, allow);
  } else {
    as->mrm.ofs = 0;
    if (canfuse(as, ir) && ir->o == IR_ADD && ra_noreg(ir->r)) {
      /* Gather (base+idx*sz)+ofs as emitted by cdata ptr/array indexing. */
      IRIns *irx;
      IRRef idx;
      Reg r;
      if (asm_isk32(as, ir->op2, &as->mrm.ofs)) {  /* Recognize x+ofs. */
	ref = ir->op1;
	ir = IR(ref);
	if (!(ir->o == IR_ADD && canfuse(as, ir) && ra_noreg(ir->r)))
	  goto noadd;
      }
      as->mrm.scale = XM_SCALE1;
      idx = ir->op1;
      ref = ir->op2;
      irx = IR(idx);
      if (!(irx->o == IR_BSHL || irx->o == IR_ADD)) {  /* Try other operand. */
	idx = ir->op2;
	ref = ir->op1;
	irx = IR(idx);
      }
      if (canfuse(as, irx) && ra_noreg(irx->r)) {
	if (irx->o == IR_BSHL && irref_isk(irx->op2) && IR(irx->op2)->i <= 3) {
	  /* Recognize idx<<b with b = 0-3, corresponding to sz = (1),2,4,8. */
	  idx = irx->op1;
	  as->mrm.scale = (uint8_t)(IR(irx->op2)->i << 6);
	} else if (irx->o == IR_ADD && irx->op1 == irx->op2) {
	  /* FOLD does idx*2 ==> idx<<1 ==> idx+idx. */
	  idx = irx->op1;
	  as->mrm.scale = XM_SCALE2;
	}
      }
      r = ra_alloc1(as, idx, allow);
      rset_clear(allow, r);
      as->mrm.idx = (uint8_t)r;
    }
  noadd:
    as->mrm.base = (uint8_t)ra_alloc1(as, ref, allow);
  }
}

/* Fuse load into memory operand. */
static Reg asm_fuseload(ASMState *as, IRRef ref, RegSet allow)
{
  IRIns *ir = IR(ref);
  if (ra_hasreg(ir->r)) {
    if (allow != RSET_EMPTY) {  /* Fast path. */
      ra_noweak(as, ir->r);
      return ir->r;
    }
  fusespill:
    /* Force a spill if only memory operands are allowed (asm_x87load). */
    as->mrm.base = RID_ESP;
    as->mrm.ofs = ra_spill(as, ir);
    as->mrm.idx = RID_NONE;
    return RID_MRM;
  }
  if (ir->o == IR_KNUM) {
    RegSet avail = as->freeset & ~as->modset & RSET_FPR;
    lua_assert(allow != RSET_EMPTY);
    if (!(avail & (avail-1))) {  /* Fuse if less than two regs available. */
      as->mrm.ofs = ptr2addr(ir_knum(ir));
      as->mrm.base = as->mrm.idx = RID_NONE;
      return RID_MRM;
    }
  } else if (mayfuse(as, ref)) {
    RegSet xallow = (allow & RSET_GPR) ? allow : RSET_GPR;
    if (ir->o == IR_SLOAD) {
      if (!(ir->op2 & (IRSLOAD_PARENT|IRSLOAD_CONVERT)) &&
	  noconflict(as, ref, IR_RETF, 0)) {
	as->mrm.base = (uint8_t)ra_alloc1(as, REF_BASE, xallow);
	as->mrm.ofs = 8*((int32_t)ir->op1-1) + ((ir->op2&IRSLOAD_FRAME)?4:0);
	as->mrm.idx = RID_NONE;
	return RID_MRM;
      }
    } else if (ir->o == IR_FLOAD) {
      /* Generic fusion is only ok for 32 bit operand (but see asm_comp). */
      if ((irt_isint(ir->t) || irt_isaddr(ir->t)) &&
	  noconflict(as, ref, IR_FSTORE, 0)) {
	asm_fusefref(as, ir, xallow);
	return RID_MRM;
      }
    } else if (ir->o == IR_ALOAD || ir->o == IR_HLOAD || ir->o == IR_ULOAD) {
      if (noconflict(as, ref, ir->o + IRDELTA_L2S, 0)) {
	asm_fuseahuref(as, ir->op1, xallow);
	return RID_MRM;
      }
    } else if (ir->o == IR_XLOAD) {
      /* Generic fusion is not ok for 8/16 bit operands (but see asm_comp).
      ** Fusing unaligned memory operands is ok on x86 (except for SIMD types).
      */
      if ((!irt_typerange(ir->t, IRT_I8, IRT_U16)) &&
	  noconflict(as, ref, IR_XSTORE, 0)) {
	asm_fusexref(as, ir->op1, xallow);
	return RID_MRM;
      }
    } else if (ir->o == IR_VLOAD) {
      asm_fuseahuref(as, ir->op1, xallow);
      return RID_MRM;
    }
  }
  if (!(as->freeset & allow) &&
      (allow == RSET_EMPTY || ra_hasspill(ir->s) || iscrossref(as, ref)))
    goto fusespill;
  return ra_allocref(as, ref, allow);
}

/* -- Calls --------------------------------------------------------------- */

/* Generate a call to a C function. */
static void asm_gencall(ASMState *as, const CCallInfo *ci, IRRef *args)
{
  uint32_t n, nargs = CCI_NARGS(ci);
  int32_t ofs = STACKARG_OFS;
  uint32_t gprs = REGARG_GPRS;
#if LJ_64
  Reg fpr = REGARG_FIRSTFPR;
#endif
  lua_assert(!(nargs > 2 && (ci->flags&CCI_FASTCALL)));  /* Avoid stack adj. */
  if ((void *)ci->func)
    emit_call(as, ci->func);
  for (n = 0; n < nargs; n++) {  /* Setup args. */
    IRRef ref = args[n];
    IRIns *ir = IR(ref);
    Reg r;
#if LJ_64 && LJ_ABI_WIN
    /* Windows/x64 argument registers are strictly positional. */
    r = irt_isfp(ir->t) ? (fpr <= REGARG_LASTFPR ? fpr : 0) : (gprs & 31);
    fpr++; gprs >>= 5;
#elif LJ_64
    /* POSIX/x64 argument registers are used in order of appearance. */
    if (irt_isfp(ir->t)) {
      r = fpr <= REGARG_LASTFPR ? fpr : 0; fpr++;
    } else {
      r = gprs & 31; gprs >>= 5;
    }
#else
    if (irt_isfp(ir->t) || !(ci->flags & CCI_FASTCALL)) {
      r = 0;
    } else {
      r = gprs & 31; gprs >>= 5;
    }
#endif
    if (r) {  /* Argument is in a register. */
      if (r < RID_MAX_GPR && ref < ASMREF_TMP1) {
#if LJ_64
	if (ir->o == IR_KINT64)
	  emit_loadu64(as, r, ir_kint64(ir)->u64);
	else
#endif
	  emit_loadi(as, r, ir->i);
      } else {
	lua_assert(rset_test(as->freeset, r));  /* Must have been evicted. */
	if (ra_hasreg(ir->r)) {
	  ra_noweak(as, ir->r);
	  ra_movrr(as, ir, r, ir->r);
	} else {
	  ra_allocref(as, ref, RID2RSET(r));
	}
      }
    } else if (irt_isfp(ir->t)) {  /* FP argument is on stack. */
      lua_assert(!(irt_isfloat(ir->t) && irref_isk(ref)));  /* No float k. */
      if (LJ_32 && (ofs & 4) && irref_isk(ref)) {
	/* Split stores for unaligned FP consts. */
	emit_movmroi(as, RID_ESP, ofs, (int32_t)ir_knum(ir)->u32.lo);
	emit_movmroi(as, RID_ESP, ofs+4, (int32_t)ir_knum(ir)->u32.hi);
      } else {
	r = ra_alloc1(as, ref, RSET_FPR);
	emit_rmro(as, irt_isnum(ir->t) ? XO_MOVSDto : XO_MOVSSto,
		  r, RID_ESP, ofs);
      }
      ofs += (LJ_32 && irt_isfloat(ir->t)) ? 4 : 8;
    } else {  /* Non-FP argument is on stack. */
      if (LJ_32 && ref < ASMREF_TMP1) {
	emit_movmroi(as, RID_ESP, ofs, ir->i);
      } else {
	r = ra_alloc1(as, ref, RSET_GPR);
	emit_movtomro(as, REX_64IR(ir, r), RID_ESP, ofs);
      }
      ofs += sizeof(intptr_t);
    }
  }
}

/* Setup result reg/sp for call. Evict scratch regs. */
static void asm_setupresult(ASMState *as, IRIns *ir, const CCallInfo *ci)
{
  RegSet drop = RSET_SCRATCH;
  if ((ci->flags & CCI_NOFPRCLOBBER))
    drop &= ~RSET_FPR;
  if (ra_hasreg(ir->r))
    rset_clear(drop, ir->r);  /* Dest reg handled below. */
  ra_evictset(as, drop);  /* Evictions must be performed first. */
  if (ra_used(ir)) {
    if (irt_isfp(ir->t)) {
      int32_t ofs = sps_scale(ir->s);  /* Use spill slot or temp slots. */
#if LJ_64
      if ((ci->flags & CCI_CASTU64)) {
	Reg dest = ir->r;
	if (ra_hasreg(dest)) {
	  ra_free(as, dest);
	  ra_modified(as, dest);
	  emit_rr(as, XO_MOVD, dest|REX_64, RID_RET);  /* Really MOVQ. */
	} else {
	  emit_movtomro(as, RID_RET|REX_64, RID_ESP, ofs);
	}
      } else {
	ra_destreg(as, ir, RID_FPRET);
      }
#else
      /* Number result is in x87 st0 for x86 calling convention. */
      Reg dest = ir->r;
      if (ra_hasreg(dest)) {
	ra_free(as, dest);
	ra_modified(as, dest);
	emit_rmro(as, irt_isnum(ir->t) ? XMM_MOVRM(as) : XO_MOVSS,
		  dest, RID_ESP, ofs);
      }
      if ((ci->flags & CCI_CASTU64)) {
	emit_movtomro(as, RID_RET, RID_ESP, ofs);
	emit_movtomro(as, RID_RETHI, RID_ESP, ofs+4);
      } else {
	emit_rmro(as, irt_isnum(ir->t) ? XO_FSTPq : XO_FSTPd,
		  irt_isnum(ir->t) ? XOg_FSTPq : XOg_FSTPd, RID_ESP, ofs);
      }
#endif
    } else {
      lua_assert(!irt_ispri(ir->t));
      ra_destreg(as, ir, RID_RET);
    }
  }
}

/* Collect arguments from CALL* and CARG instructions. */
static void asm_collectargs(ASMState *as, IRIns *ir,
			    const CCallInfo *ci, IRRef *args)
{
  uint32_t n = CCI_NARGS(ci);
  lua_assert(n <= CCI_NARGS_MAX);
  if ((ci->flags & CCI_L)) { *args++ = ASMREF_L; n--; }
  while (n-- > 1) {
    ir = IR(ir->op1);
    lua_assert(ir->o == IR_CARG);
    args[n] = ir->op2;
  }
  args[0] = ir->op1;
  lua_assert(IR(ir->op1)->o != IR_CARG);
}

static void asm_call(ASMState *as, IRIns *ir)
{
  IRRef args[CCI_NARGS_MAX];
  const CCallInfo *ci = &lj_ir_callinfo[ir->op2];
  asm_collectargs(as, ir, ci, args);
  asm_setupresult(as, ir, ci);
  asm_gencall(as, ci, args);
}

/* Reconstruct CCallInfo flags for CALLX*. */
static uint32_t asm_callx_flags(ASMState *as, IRIns *ir)
{
  uint32_t nargs = 0;
  if (ir->op1 != REF_NIL) {  /* Count number of arguments first. */
    IRIns *ira = IR(ir->op1);
    nargs++;
    while (ira->o == IR_CARG) { nargs++; ira = IR(ira->op1); }
  }
  /* NYI: fastcall etc. */
  return (nargs | (ir->t.irt << CCI_OTSHIFT));
}

static void asm_callx(ASMState *as, IRIns *ir)
{
  IRRef args[CCI_NARGS_MAX];
  CCallInfo ci;
  IRIns *irf;
  ci.flags = asm_callx_flags(as, ir);
  asm_collectargs(as, ir, &ci, args);
  asm_setupresult(as, ir, &ci);
  irf = IR(ir->op2);
  if (LJ_32 && irref_isk(ir->op2)) {  /* Call to constant address on x86. */
    ci.func = (ASMFunction)(void *)(uintptr_t)(uint32_t)irf->i;
  } else {
    /* Prefer a non-argument register or RID_RET for indirect calls. */
    RegSet allow = (RSET_GPR & ~RSET_SCRATCH)|RID2RSET(RID_RET);
    Reg r = ra_alloc1(as, ir->op2, allow);
    emit_rr(as, XO_GROUP5, XOg_CALL, r);
    ci.func = (ASMFunction)(void *)0;
  }
  asm_gencall(as, &ci, args);
}

/* -- Returns ------------------------------------------------------------- */

/* Return to lower frame. Guard that it goes to the right spot. */
static void asm_retf(ASMState *as, IRIns *ir)
{
  Reg base = ra_alloc1(as, REF_BASE, RSET_GPR);
  void *pc = ir_kptr(IR(ir->op2));
  int32_t delta = 1+bc_a(*((const BCIns *)pc - 1));
  as->topslot -= (BCReg)delta;
  if ((int32_t)as->topslot < 0) as->topslot = 0;
  emit_setgl(as, base, jit_base);
  emit_addptr(as, base, -8*delta);
  asm_guardcc(as, CC_NE);
  emit_gmroi(as, XG_ARITHi(XOg_CMP), base, -4, ptr2addr(pc));
}

/* -- Type conversions ---------------------------------------------------- */

static void asm_tointg(ASMState *as, IRIns *ir, Reg left)
{
  Reg tmp = ra_scratch(as, rset_exclude(RSET_FPR, left));
  Reg dest = ra_dest(as, ir, RSET_GPR);
  asm_guardcc(as, CC_P);
  asm_guardcc(as, CC_NE);
  emit_rr(as, XO_UCOMISD, left, tmp);
  emit_rr(as, XO_CVTSI2SD, tmp, dest);
  if (!(as->flags & JIT_F_SPLIT_XMM))
    emit_rr(as, XO_XORPS, tmp, tmp);  /* Avoid partial register stall. */
  emit_rr(as, XO_CVTTSD2SI, dest, left);
  /* Can't fuse since left is needed twice. */
}

static void asm_tobit(ASMState *as, IRIns *ir)
{
  Reg dest = ra_dest(as, ir, RSET_GPR);
  Reg tmp = ra_noreg(IR(ir->op1)->r) ?
	      ra_alloc1(as, ir->op1, RSET_FPR) :
	      ra_scratch(as, RSET_FPR);
  Reg right = asm_fuseload(as, ir->op2, rset_exclude(RSET_FPR, tmp));
  emit_rr(as, XO_MOVDto, tmp, dest);
  emit_mrm(as, XO_ADDSD, tmp, right);
  ra_left(as, tmp, ir->op1);
}

static void asm_conv(ASMState *as, IRIns *ir)
{
  IRType st = (IRType)(ir->op2 & IRCONV_SRCMASK);
  int st64 = (st == IRT_I64 || st == IRT_U64 || (LJ_64 && st == IRT_P64));
  int stfp = (st == IRT_NUM || st == IRT_FLOAT);
  IRRef lref = ir->op1;
  lua_assert(irt_type(ir->t) != st);
  lua_assert(!(LJ_32 && (irt_isint64(ir->t) || st64)));  /* Handled by SPLIT. */
  if (irt_isfp(ir->t)) {
    Reg dest = ra_dest(as, ir, RSET_FPR);
    if (stfp) {  /* FP to FP conversion. */
      Reg left = asm_fuseload(as, lref, RSET_FPR);
      emit_mrm(as, st == IRT_NUM ? XO_CVTSD2SS : XO_CVTSS2SD, dest, left);
      if (left == dest) return;  /* Avoid the XO_XORPS. */
    } else if (LJ_32 && st == IRT_U32) {  /* U32 to FP conversion on x86. */
      /* number = (2^52+2^51 .. u32) - (2^52+2^51) */
      cTValue *k = lj_ir_k64_find(as->J, U64x(43380000,00000000));
      Reg bias = ra_scratch(as, rset_exclude(RSET_FPR, dest));
      if (irt_isfloat(ir->t))
	emit_rr(as, XO_CVTSD2SS, dest, dest);
      emit_rr(as, XO_SUBSD, dest, bias);  /* Subtract 2^52+2^51 bias. */
      emit_rr(as, XO_XORPS, dest, bias);  /* Merge bias and integer. */
      emit_loadn(as, bias, k);
      emit_mrm(as, XO_MOVD, dest, asm_fuseload(as, lref, RSET_GPR));
      return;
    } else {  /* Integer to FP conversion. */
      Reg left = (LJ_64 && (st == IRT_U32 || st == IRT_U64)) ?
		 ra_alloc1(as, lref, RSET_GPR) :
		 asm_fuseload(as, lref, RSET_GPR);
      if (LJ_64 && st == IRT_U64) {
	MCLabel l_end = emit_label(as);
	const void *k = lj_ir_k64_find(as->J, U64x(43f00000,00000000));
	emit_rma(as, XO_ADDSD, dest, k);  /* Add 2^64 to compensate. */
	emit_sjcc(as, CC_NS, l_end);
	emit_rr(as, XO_TEST, left|REX_64, left);  /* Check if u64 >= 2^63. */
      }
      emit_mrm(as, irt_isnum(ir->t) ? XO_CVTSI2SD : XO_CVTSI2SS,
	       dest|((LJ_64 && (st64 || st == IRT_U32)) ? REX_64 : 0), left);
    }
    if (!(as->flags & JIT_F_SPLIT_XMM))
      emit_rr(as, XO_XORPS, dest, dest);  /* Avoid partial register stall. */
  } else if (stfp) {  /* FP to integer conversion. */
    if (irt_isguard(ir->t)) {
      /* Checked conversions are only supported from number to int. */
      lua_assert(irt_isint(ir->t) && st == IRT_NUM);
      asm_tointg(as, ir, ra_alloc1(as, lref, RSET_FPR));
    } else {
      Reg dest = ra_dest(as, ir, RSET_GPR);
      x86Op op = st == IRT_NUM ?
		 ((ir->op2 & IRCONV_TRUNC) ? XO_CVTTSD2SI : XO_CVTSD2SI) :
		 ((ir->op2 & IRCONV_TRUNC) ? XO_CVTTSS2SI : XO_CVTSS2SI);
      if (LJ_32 && irt_isu32(ir->t)) {  /* FP to U32 conversion on x86. */
	/* u32 = (int32_t)(number - 2^31) + 2^31 */
	Reg tmp = ra_noreg(IR(lref)->r) ? ra_alloc1(as, lref, RSET_FPR) :
					  ra_scratch(as, RSET_FPR);
	emit_gri(as, XG_ARITHi(XOg_ADD), dest, (int32_t)0x80000000);
	emit_rr(as, op, dest, tmp);
	if (st == IRT_NUM)
	  emit_rma(as, XO_ADDSD, tmp,
		   lj_ir_k64_find(as->J, U64x(c1e00000,00000000)));
	else
	  emit_rma(as, XO_ADDSS, tmp,
		   lj_ir_k64_find(as->J, U64x(00000000,cf000000)));
	ra_left(as, tmp, lref);
      } else if (LJ_64 && irt_isu64(ir->t)) {
	/* For inputs in [2^63,2^64-1] add -2^64 and convert again. */
	Reg tmp = ra_noreg(IR(lref)->r) ? ra_alloc1(as, lref, RSET_FPR) :
					  ra_scratch(as, RSET_FPR);
	MCLabel l_end = emit_label(as);
	emit_rr(as, op, dest|REX_64, tmp);
	if (st == IRT_NUM)
	  emit_rma(as, XO_ADDSD, tmp,
		   lj_ir_k64_find(as->J, U64x(c3f00000,00000000)));
	else
	  emit_rma(as, XO_ADDSS, tmp,
		   lj_ir_k64_find(as->J, U64x(00000000,df800000)));
	emit_sjcc(as, CC_NS, l_end);
	emit_rr(as, XO_TEST, dest|REX_64, dest);  /* Check if dest < 2^63. */
	emit_rr(as, op, dest|REX_64, tmp);
	ra_left(as, tmp, lref);
      } else {
	Reg left = asm_fuseload(as, lref, RSET_FPR);
	if (LJ_64 && irt_isu32(ir->t))
	  emit_rr(as, XO_MOV, dest, dest);  /* Zero hiword. */
	emit_mrm(as, op,
		 dest|((LJ_64 &&
			(irt_is64(ir->t) || irt_isu32(ir->t))) ? REX_64 : 0),
		 left);
      }
    }
  } else if (st >= IRT_I8 && st <= IRT_U16) {  /* Extend to 32 bit integer. */
    Reg left, dest = ra_dest(as, ir, RSET_GPR);
    RegSet allow = RSET_GPR;
    x86Op op;
    lua_assert(irt_isint(ir->t) || irt_isu32(ir->t));
    if (st == IRT_I8) {
      op = XO_MOVSXb; allow = RSET_GPR8; dest |= FORCE_REX;
    } else if (st == IRT_U8) {
      op = XO_MOVZXb; allow = RSET_GPR8; dest |= FORCE_REX;
    } else if (st == IRT_I16) {
      op = XO_MOVSXw;
    } else {
      op = XO_MOVZXw;
    }
    left = asm_fuseload(as, lref, allow);
    /* Add extra MOV if source is already in wrong register. */
    if (!LJ_64 && left != RID_MRM && !rset_test(allow, left)) {
      Reg tmp = ra_scratch(as, allow);
      emit_rr(as, op, dest, tmp);
      emit_rr(as, XO_MOV, tmp, left);
    } else {
      emit_mrm(as, op, dest, left);
    }
  } else {  /* 32/64 bit integer conversions. */
    if (LJ_32) {  /* Only need to handle 32/32 bit no-op (cast) on x86. */
      Reg dest = ra_dest(as, ir, RSET_GPR);
      ra_left(as, dest, lref);  /* Do nothing, but may need to move regs. */
    } else if (irt_is64(ir->t)) {
      Reg dest = ra_dest(as, ir, RSET_GPR);
      if (st64 || !(ir->op2 & IRCONV_SEXT)) {
	/* 64/64 bit no-op (cast) or 32 to 64 bit zero extension. */
	ra_left(as, dest, lref);  /* Do nothing, but may need to move regs. */
      } else {  /* 32 to 64 bit sign extension. */
	Reg left = asm_fuseload(as, lref, RSET_GPR);
	emit_mrm(as, XO_MOVSXd, dest|REX_64, left);
      }
    } else {
      Reg dest = ra_dest(as, ir, RSET_GPR);
      if (st64) {
	Reg left = asm_fuseload(as, lref, RSET_GPR);
	/* This is either a 32 bit reg/reg mov which zeroes the hiword
	** or a load of the loword from a 64 bit address.
	*/
	emit_mrm(as, XO_MOV, dest, left);
      } else {  /* 32/32 bit no-op (cast). */
	ra_left(as, dest, lref);  /* Do nothing, but may need to move regs. */
      }
    }
  }
}

#if LJ_32 && LJ_HASFFI
/* No SSE conversions to/from 64 bit on x86, so resort to ugly x87 code. */

/* 64 bit integer to FP conversion in 32 bit mode. */
static void asm_conv_fp_int64(ASMState *as, IRIns *ir)
{
  Reg hi = ra_alloc1(as, ir->op1, RSET_GPR);
  Reg lo = ra_alloc1(as, (ir-1)->op1, rset_exclude(RSET_GPR, hi));
  int32_t ofs = sps_scale(ir->s);  /* Use spill slot or temp slots. */
  Reg dest = ir->r;
  if (ra_hasreg(dest)) {
    ra_free(as, dest);
    ra_modified(as, dest);
    emit_rmro(as, irt_isnum(ir->t) ? XMM_MOVRM(as) : XO_MOVSS,
	      dest, RID_ESP, ofs);
  }
  emit_rmro(as, irt_isnum(ir->t) ? XO_FSTPq : XO_FSTPd,
	    irt_isnum(ir->t) ? XOg_FSTPq : XOg_FSTPd, RID_ESP, ofs);
  if (((ir-1)->op2 & IRCONV_SRCMASK) == IRT_U64) {
    /* For inputs in [2^63,2^64-1] add 2^64 to compensate. */
    MCLabel l_end = emit_label(as);
    emit_rma(as, XO_FADDq, XOg_FADDq,
	     lj_ir_k64_find(as->J, U64x(43f00000,00000000)));
    emit_sjcc(as, CC_NS, l_end);
    emit_rr(as, XO_TEST, hi, hi);  /* Check if u64 >= 2^63. */
  } else {
    lua_assert(((ir-1)->op2 & IRCONV_SRCMASK) == IRT_I64);
  }
  emit_rmro(as, XO_FILDq, XOg_FILDq, RID_ESP, 0);
  /* NYI: Avoid narrow-to-wide store-to-load forwarding stall. */
  emit_rmro(as, XO_MOVto, hi, RID_ESP, 4);
  emit_rmro(as, XO_MOVto, lo, RID_ESP, 0);
}

/* FP to 64 bit integer conversion in 32 bit mode. */
static void asm_conv_int64_fp(ASMState *as, IRIns *ir)
{
  IRType st = (IRType)((ir-1)->op2 & IRCONV_SRCMASK);
  IRType dt = (((ir-1)->op2 & IRCONV_DSTMASK) >> IRCONV_DSH);
  Reg lo, hi;
  lua_assert(st == IRT_NUM || st == IRT_FLOAT);
  lua_assert(dt == IRT_I64 || dt == IRT_U64);
  lua_assert(((ir-1)->op2 & IRCONV_TRUNC));
  hi = ra_dest(as, ir, RSET_GPR);
  lo = ra_dest(as, ir-1, rset_exclude(RSET_GPR, hi));
  if (ra_used(ir-1)) emit_rmro(as, XO_MOV, lo, RID_ESP, 0);
  /* NYI: Avoid wide-to-narrow store-to-load forwarding stall. */
  if (!(as->flags & JIT_F_SSE3)) {  /* Set FPU rounding mode to default. */
    emit_rmro(as, XO_FLDCW, XOg_FLDCW, RID_ESP, 4);
    emit_rmro(as, XO_MOVto, lo, RID_ESP, 4);
    emit_gri(as, XG_ARITHi(XOg_AND), lo, 0xf3ff);
  }
  if (dt == IRT_U64) {
    /* For inputs in [2^63,2^64-1] add -2^64 and convert again. */
    MCLabel l_pop, l_end = emit_label(as);
    emit_x87op(as, XI_FPOP);
    l_pop = emit_label(as);
    emit_sjmp(as, l_end);
    emit_rmro(as, XO_MOV, hi, RID_ESP, 4);
    if ((as->flags & JIT_F_SSE3))
      emit_rmro(as, XO_FISTTPq, XOg_FISTTPq, RID_ESP, 0);
    else
      emit_rmro(as, XO_FISTPq, XOg_FISTPq, RID_ESP, 0);
    emit_rma(as, XO_FADDq, XOg_FADDq,
	     lj_ir_k64_find(as->J, U64x(c3f00000,00000000)));
    emit_sjcc(as, CC_NS, l_pop);
    emit_rr(as, XO_TEST, hi, hi);  /* Check if out-of-range (2^63). */
  }
  emit_rmro(as, XO_MOV, hi, RID_ESP, 4);
  if ((as->flags & JIT_F_SSE3)) {  /* Truncation is easy with SSE3. */
    emit_rmro(as, XO_FISTTPq, XOg_FISTTPq, RID_ESP, 0);
  } else {  /* Otherwise set FPU rounding mode to truncate before the store. */
    emit_rmro(as, XO_FISTPq, XOg_FISTPq, RID_ESP, 0);
    emit_rmro(as, XO_FLDCW, XOg_FLDCW, RID_ESP, 0);
    emit_rmro(as, XO_MOVtow, lo, RID_ESP, 0);
    emit_rmro(as, XO_ARITHw(XOg_OR), lo, RID_ESP, 0);
    emit_loadi(as, lo, 0xc00);
    emit_rmro(as, XO_FNSTCW, XOg_FNSTCW, RID_ESP, 0);
  }
  if (dt == IRT_U64)
    emit_x87op(as, XI_FDUP);
  emit_mrm(as, st == IRT_NUM ? XO_FLDq : XO_FLDd,
	   st == IRT_NUM ? XOg_FLDq: XOg_FLDd,
	   asm_fuseload(as, ir->op1, RSET_EMPTY));
}
#endif

static void asm_strto(ASMState *as, IRIns *ir)
{
  /* Force a spill slot for the destination register (if any). */
  const CCallInfo *ci = &lj_ir_callinfo[IRCALL_lj_str_tonum];
  IRRef args[2];
  RegSet drop = RSET_SCRATCH;
  if ((drop & RSET_FPR) != RSET_FPR && ra_hasreg(ir->r))
    rset_set(drop, ir->r);  /* WIN64 doesn't spill all FPRs. */
  ra_evictset(as, drop);
  asm_guardcc(as, CC_E);
  emit_rr(as, XO_TEST, RID_RET, RID_RET);  /* Test return status. */
  args[0] = ir->op1;      /* GCstr *str */
  args[1] = ASMREF_TMP1;  /* TValue *n  */
  asm_gencall(as, ci, args);
  /* Store the result to the spill slot or temp slots. */
  emit_rmro(as, XO_LEA, ra_releasetmp(as, ASMREF_TMP1)|REX_64,
	    RID_ESP, sps_scale(ir->s));
}

static void asm_tostr(ASMState *as, IRIns *ir)
{
  IRIns *irl = IR(ir->op1);
  IRRef args[2];
  args[0] = ASMREF_L;
  as->gcsteps++;
  if (irt_isnum(irl->t)) {
    const CCallInfo *ci = &lj_ir_callinfo[IRCALL_lj_str_fromnum];
    args[1] = ASMREF_TMP1;  /* const lua_Number * */
    asm_setupresult(as, ir, ci);  /* GCstr * */
    asm_gencall(as, ci, args);
    emit_rmro(as, XO_LEA, ra_releasetmp(as, ASMREF_TMP1)|REX_64,
	      RID_ESP, ra_spill(as, irl));
  } else {
    const CCallInfo *ci = &lj_ir_callinfo[IRCALL_lj_str_fromint];
    args[1] = ir->op1;  /* int32_t k */
    asm_setupresult(as, ir, ci);  /* GCstr * */
    asm_gencall(as, ci, args);
  }
}

/* -- Memory references --------------------------------------------------- */

static void asm_aref(ASMState *as, IRIns *ir)
{
  Reg dest = ra_dest(as, ir, RSET_GPR);
  asm_fusearef(as, ir, RSET_GPR);
  if (!(as->mrm.idx == RID_NONE && as->mrm.ofs == 0))
    emit_mrm(as, XO_LEA, dest, RID_MRM);
  else if (as->mrm.base != dest)
    emit_rr(as, XO_MOV, dest, as->mrm.base);
}

/* Must match with hash*() in lj_tab.c. */
static uint32_t ir_khash(IRIns *ir)
{
  uint32_t lo, hi;
  if (irt_isstr(ir->t)) {
    return ir_kstr(ir)->hash;
  } else if (irt_isnum(ir->t)) {
    lo = ir_knum(ir)->u32.lo;
    hi = ir_knum(ir)->u32.hi << 1;
  } else if (irt_ispri(ir->t)) {
    lua_assert(!irt_isnil(ir->t));
    return irt_type(ir->t)-IRT_FALSE;
  } else {
    lua_assert(irt_isgcv(ir->t));
    lo = u32ptr(ir_kgc(ir));
    hi = lo + HASH_BIAS;
  }
  return hashrot(lo, hi);
}

/* Merge NE(HREF, niltv) check. */
static MCode *merge_href_niltv(ASMState *as, IRIns *ir)
{
  /* Assumes nothing else generates NE of HREF. */
  if ((ir[1].o == IR_NE || ir[1].o == IR_EQ) && ir[1].op1 == as->curins &&
      ra_hasreg(ir->r)) {
    MCode *p = as->mcp;
    p += (LJ_64 && *p != XI_ARITHi) ? 7+6 : 6+6;
    /* Ensure no loop branch inversion happened. */
    if (p[-6] == 0x0f && p[-5] == XI_JCCn+(CC_NE^(ir[1].o & 1))) {
      as->mcp = p;  /* Kill cmp reg, imm32 + jz exit. */
      return p + *(int32_t *)(p-4);  /* Return exit address. */
    }
  }
  return NULL;
}

/* Inlined hash lookup. Specialized for key type and for const keys.
** The equivalent C code is:
**   Node *n = hashkey(t, key);
**   do {
**     if (lj_obj_equal(&n->key, key)) return &n->val;
**   } while ((n = nextnode(n)));
**   return niltv(L);
*/
static void asm_href(ASMState *as, IRIns *ir)
{
  MCode *nilexit = merge_href_niltv(as, ir);  /* Do this before any restores. */
  RegSet allow = RSET_GPR;
  Reg dest = ra_dest(as, ir, allow);
  Reg tab = ra_alloc1(as, ir->op1, rset_clear(allow, dest));
  Reg key = RID_NONE, tmp = RID_NONE;
  IRIns *irkey = IR(ir->op2);
  int isk = irref_isk(ir->op2);
  IRType1 kt = irkey->t;
  uint32_t khash;
  MCLabel l_end, l_loop, l_next;

  if (!isk) {
    rset_clear(allow, tab);
    key = ra_alloc1(as, ir->op2, irt_isnum(kt) ? RSET_FPR : allow);
    if (!irt_isstr(kt))
      tmp = ra_scratch(as, rset_exclude(allow, key));
  }

  /* Key not found in chain: jump to exit (if merged with NE) or load niltv. */
  l_end = emit_label(as);
  if (nilexit && ir[1].o == IR_NE) {
    emit_jcc(as, CC_E, nilexit);  /* XI_JMP is not found by lj_asm_patchexit. */
    nilexit = NULL;
  } else {
    emit_loada(as, dest, niltvg(J2G(as->J)));
  }

  /* Follow hash chain until the end. */
  l_loop = emit_sjcc_label(as, CC_NZ);
  emit_rr(as, XO_TEST, dest, dest);
  emit_rmro(as, XO_MOV, dest, dest, offsetof(Node, next));
  l_next = emit_label(as);

  /* Type and value comparison. */
  if (nilexit)
    emit_jcc(as, CC_E, nilexit);
  else
    emit_sjcc(as, CC_E, l_end);
  if (irt_isnum(kt)) {
    if (isk) {
      /* Assumes -0.0 is already canonicalized to +0.0. */
      emit_gmroi(as, XG_ARITHi(XOg_CMP), dest, offsetof(Node, key.u32.lo),
		 (int32_t)ir_knum(irkey)->u32.lo);
      emit_sjcc(as, CC_NE, l_next);
      emit_gmroi(as, XG_ARITHi(XOg_CMP), dest, offsetof(Node, key.u32.hi),
		 (int32_t)ir_knum(irkey)->u32.hi);
    } else {
      emit_sjcc(as, CC_P, l_next);
      emit_rmro(as, XO_UCOMISD, key, dest, offsetof(Node, key.n));
      emit_sjcc(as, CC_AE, l_next);
      /* The type check avoids NaN penalties and complaints from Valgrind. */
#if LJ_64
      emit_u32(as, LJ_TISNUM);
      emit_rmro(as, XO_ARITHi, XOg_CMP, dest, offsetof(Node, key.it));
#else
      emit_i8(as, LJ_TISNUM);
      emit_rmro(as, XO_ARITHi8, XOg_CMP, dest, offsetof(Node, key.it));
#endif
    }
#if LJ_64
  } else if (irt_islightud(kt)) {
    emit_rmro(as, XO_CMP, key|REX_64, dest, offsetof(Node, key.u64));
#endif
  } else {
    if (!irt_ispri(kt)) {
      lua_assert(irt_isaddr(kt));
      if (isk)
	emit_gmroi(as, XG_ARITHi(XOg_CMP), dest, offsetof(Node, key.gcr),
		   ptr2addr(ir_kgc(irkey)));
      else
	emit_rmro(as, XO_CMP, key, dest, offsetof(Node, key.gcr));
      emit_sjcc(as, CC_NE, l_next);
    }
    lua_assert(!irt_isnil(kt));
    emit_i8(as, irt_toitype(kt));
    emit_rmro(as, XO_ARITHi8, XOg_CMP, dest, offsetof(Node, key.it));
  }
  emit_sfixup(as, l_loop);
  checkmclim(as);

  /* Load main position relative to tab->node into dest. */
  khash = isk ? ir_khash(irkey) : 1;
  if (khash == 0) {
    emit_rmro(as, XO_MOV, dest, tab, offsetof(GCtab, node));
  } else {
    emit_rmro(as, XO_ARITH(XOg_ADD), dest, tab, offsetof(GCtab, node));
    if ((as->flags & JIT_F_PREFER_IMUL)) {
      emit_i8(as, sizeof(Node));
      emit_rr(as, XO_IMULi8, dest, dest);
    } else {
      emit_shifti(as, XOg_SHL, dest, 3);
      emit_rmrxo(as, XO_LEA, dest, dest, dest, XM_SCALE2, 0);
    }
    if (isk) {
      emit_gri(as, XG_ARITHi(XOg_AND), dest, (int32_t)khash);
      emit_rmro(as, XO_MOV, dest, tab, offsetof(GCtab, hmask));
    } else if (irt_isstr(kt)) {
      emit_rmro(as, XO_ARITH(XOg_AND), dest, key, offsetof(GCstr, hash));
      emit_rmro(as, XO_MOV, dest, tab, offsetof(GCtab, hmask));
    } else {  /* Must match with hashrot() in lj_tab.c. */
      emit_rmro(as, XO_ARITH(XOg_AND), dest, tab, offsetof(GCtab, hmask));
      emit_rr(as, XO_ARITH(XOg_SUB), dest, tmp);
      emit_shifti(as, XOg_ROL, tmp, HASH_ROT3);
      emit_rr(as, XO_ARITH(XOg_XOR), dest, tmp);
      emit_shifti(as, XOg_ROL, dest, HASH_ROT2);
      emit_rr(as, XO_ARITH(XOg_SUB), tmp, dest);
      emit_shifti(as, XOg_ROL, dest, HASH_ROT1);
      emit_rr(as, XO_ARITH(XOg_XOR), tmp, dest);
      if (irt_isnum(kt)) {
	emit_rr(as, XO_ARITH(XOg_ADD), dest, dest);
#if LJ_64
	emit_shifti(as, XOg_SHR|REX_64, dest, 32);
	emit_rr(as, XO_MOV, tmp, dest);
	emit_rr(as, XO_MOVDto, key|REX_64, dest);
#else
	emit_rmro(as, XO_MOV, dest, RID_ESP, ra_spill(as, irkey)+4);
	emit_rr(as, XO_MOVDto, key, tmp);
#endif
      } else {
	emit_rr(as, XO_MOV, tmp, key);
	emit_rmro(as, XO_LEA, dest, key, HASH_BIAS);
      }
    }
  }
}

static void asm_hrefk(ASMState *as, IRIns *ir)
{
  IRIns *kslot = IR(ir->op2);
  IRIns *irkey = IR(kslot->op1);
  int32_t ofs = (int32_t)(kslot->op2 * sizeof(Node));
  Reg dest = ra_used(ir) ? ra_dest(as, ir, RSET_GPR) : RID_NONE;
  Reg node = ra_alloc1(as, ir->op1, RSET_GPR);
#if !LJ_64
  MCLabel l_exit;
#endif
  lua_assert(ofs % sizeof(Node) == 0);
  if (ra_hasreg(dest)) {
    if (ofs != 0) {
      if (dest == node && !(as->flags & JIT_F_LEA_AGU))
	emit_gri(as, XG_ARITHi(XOg_ADD), dest, ofs);
      else
	emit_rmro(as, XO_LEA, dest, node, ofs);
    } else if (dest != node) {
      emit_rr(as, XO_MOV, dest, node);
    }
  }
  asm_guardcc(as, CC_NE);
#if LJ_64
  if (!irt_ispri(irkey->t)) {
    Reg key = ra_scratch(as, rset_exclude(RSET_GPR, node));
    emit_rmro(as, XO_CMP, key|REX_64, node,
	       ofs + (int32_t)offsetof(Node, key.u64));
    lua_assert(irt_isnum(irkey->t) || irt_isgcv(irkey->t));
    /* Assumes -0.0 is already canonicalized to +0.0. */
    emit_loadu64(as, key, irt_isnum(irkey->t) ? ir_knum(irkey)->u64 :
			  ((uint64_t)irt_toitype(irkey->t) << 32) |
			  (uint64_t)(uint32_t)ptr2addr(ir_kgc(irkey)));
  } else {
    lua_assert(!irt_isnil(irkey->t));
    emit_i8(as, irt_toitype(irkey->t));
    emit_rmro(as, XO_ARITHi8, XOg_CMP, node,
	      ofs + (int32_t)offsetof(Node, key.it));
  }
#else
  l_exit = emit_label(as);
  if (irt_isnum(irkey->t)) {
    /* Assumes -0.0 is already canonicalized to +0.0. */
    emit_gmroi(as, XG_ARITHi(XOg_CMP), node,
	       ofs + (int32_t)offsetof(Node, key.u32.lo),
	       (int32_t)ir_knum(irkey)->u32.lo);
    emit_sjcc(as, CC_NE, l_exit);
    emit_gmroi(as, XG_ARITHi(XOg_CMP), node,
	       ofs + (int32_t)offsetof(Node, key.u32.hi),
	       (int32_t)ir_knum(irkey)->u32.hi);
  } else {
    if (!irt_ispri(irkey->t)) {
      lua_assert(irt_isgcv(irkey->t));
      emit_gmroi(as, XG_ARITHi(XOg_CMP), node,
		 ofs + (int32_t)offsetof(Node, key.gcr),
		 ptr2addr(ir_kgc(irkey)));
      emit_sjcc(as, CC_NE, l_exit);
    }
    lua_assert(!irt_isnil(irkey->t));
    emit_i8(as, irt_toitype(irkey->t));
    emit_rmro(as, XO_ARITHi8, XOg_CMP, node,
	      ofs + (int32_t)offsetof(Node, key.it));
  }
#endif
}

static void asm_newref(ASMState *as, IRIns *ir)
{
  const CCallInfo *ci = &lj_ir_callinfo[IRCALL_lj_tab_newkey];
  IRRef args[3];
  IRIns *irkey;
  Reg tmp;
  args[0] = ASMREF_L;     /* lua_State *L */
  args[1] = ir->op1;      /* GCtab *t     */
  args[2] = ASMREF_TMP1;  /* cTValue *key */
  asm_setupresult(as, ir, ci);  /* TValue * */
  asm_gencall(as, ci, args);
  tmp = ra_releasetmp(as, ASMREF_TMP1);
  irkey = IR(ir->op2);
  if (irt_isnum(irkey->t)) {
    /* For numbers use the constant itself or a spill slot as a TValue. */
    if (irref_isk(ir->op2))
      emit_loada(as, tmp, ir_knum(irkey));
    else
      emit_rmro(as, XO_LEA, tmp|REX_64, RID_ESP, ra_spill(as, irkey));
  } else {
    /* Otherwise use g->tmptv to hold the TValue. */
    if (!irref_isk(ir->op2)) {
      Reg src = ra_alloc1(as, ir->op2, rset_exclude(RSET_GPR, tmp));
      emit_movtomro(as, REX_64IR(irkey, src), tmp, 0);
    } else if (!irt_ispri(irkey->t)) {
      emit_movmroi(as, tmp, 0, irkey->i);
    }
    if (!(LJ_64 && irt_islightud(irkey->t)))
      emit_movmroi(as, tmp, 4, irt_toitype(irkey->t));
    emit_loada(as, tmp, &J2G(as->J)->tmptv);
  }
}

static void asm_uref(ASMState *as, IRIns *ir)
{
  /* NYI: Check that UREFO is still open and not aliasing a slot. */
  Reg dest = ra_dest(as, ir, RSET_GPR);
  if (irref_isk(ir->op1)) {
    GCfunc *fn = ir_kfunc(IR(ir->op1));
    MRef *v = &gcref(fn->l.uvptr[(ir->op2 >> 8)])->uv.v;
    emit_rma(as, XO_MOV, dest, v);
  } else {
    Reg uv = ra_scratch(as, RSET_GPR);
    Reg func = ra_alloc1(as, ir->op1, RSET_GPR);
    if (ir->o == IR_UREFC) {
      emit_rmro(as, XO_LEA, dest, uv, offsetof(GCupval, tv));
      asm_guardcc(as, CC_NE);
      emit_i8(as, 1);
      emit_rmro(as, XO_ARITHib, XOg_CMP, uv, offsetof(GCupval, closed));
    } else {
      emit_rmro(as, XO_MOV, dest, uv, offsetof(GCupval, v));
    }
    emit_rmro(as, XO_MOV, uv, func,
	      (int32_t)offsetof(GCfuncL, uvptr) + 4*(int32_t)(ir->op2 >> 8));
  }
}

static void asm_fref(ASMState *as, IRIns *ir)
{
  Reg dest = ra_dest(as, ir, RSET_GPR);
  asm_fusefref(as, ir, RSET_GPR);
  emit_mrm(as, XO_LEA, dest, RID_MRM);
}

static void asm_strref(ASMState *as, IRIns *ir)
{
  Reg dest = ra_dest(as, ir, RSET_GPR);
  asm_fusestrref(as, ir, RSET_GPR);
  if (as->mrm.base == RID_NONE)
    emit_loadi(as, dest, as->mrm.ofs);
  else if (as->mrm.base == dest && as->mrm.idx == RID_NONE)
    emit_gri(as, XG_ARITHi(XOg_ADD), dest, as->mrm.ofs);
  else
    emit_mrm(as, XO_LEA, dest, RID_MRM);
}

/* -- Loads and stores ---------------------------------------------------- */

static void asm_fxload(ASMState *as, IRIns *ir)
{
  Reg dest = ra_dest(as, ir, irt_isnum(ir->t) ? RSET_FPR : RSET_GPR);
  x86Op xo;
  if (ir->o == IR_FLOAD)
    asm_fusefref(as, ir, RSET_GPR);
  else
    asm_fusexref(as, ir->op1, RSET_GPR);
    /* ir->op2 is ignored -- unaligned loads are ok on x86. */
  switch (irt_type(ir->t)) {
  case IRT_I8: xo = XO_MOVSXb; break;
  case IRT_U8: xo = XO_MOVZXb; break;
  case IRT_I16: xo = XO_MOVSXw; break;
  case IRT_U16: xo = XO_MOVZXw; break;
  case IRT_NUM: xo = XMM_MOVRM(as); break;
  case IRT_FLOAT: xo = XO_MOVSS; break;
  default:
    if (LJ_64 && irt_is64(ir->t))
      dest |= REX_64;
    else
      lua_assert(irt_isint(ir->t) || irt_isu32(ir->t) || irt_isaddr(ir->t));
    xo = XO_MOV;
    break;
  }
  emit_mrm(as, xo, dest, RID_MRM);
}

static void asm_fxstore(ASMState *as, IRIns *ir)
{
  RegSet allow = RSET_GPR;
  Reg src = RID_NONE, osrc = RID_NONE;
  int32_t k = 0;
  /* The IRT_I16/IRT_U16 stores should never be simplified for constant
  ** values since mov word [mem], imm16 has a length-changing prefix.
  */
  if (irt_isi16(ir->t) || irt_isu16(ir->t) || irt_isfp(ir->t) ||
      !asm_isk32(as, ir->op2, &k)) {
    RegSet allow8 = irt_isfp(ir->t) ? RSET_FPR :
		    (irt_isi8(ir->t) || irt_isu8(ir->t)) ? RSET_GPR8 : RSET_GPR;
    src = osrc = ra_alloc1(as, ir->op2, allow8);
    if (!LJ_64 && !rset_test(allow8, src)) {  /* Already in wrong register. */
      rset_clear(allow, osrc);
      src = ra_scratch(as, allow8);
    }
    rset_clear(allow, src);
  }
  if (ir->o == IR_FSTORE)
    asm_fusefref(as, IR(ir->op1), allow);
  else
    asm_fusexref(as, ir->op1, allow);
    /* ir->op2 is ignored -- unaligned stores are ok on x86. */
  if (ra_hasreg(src)) {
    x86Op xo;
    switch (irt_type(ir->t)) {
    case IRT_I8: case IRT_U8: xo = XO_MOVtob; src |= FORCE_REX; break;
    case IRT_I16: case IRT_U16: xo = XO_MOVtow; break;
    case IRT_NUM: xo = XO_MOVSDto; break;
    case IRT_FLOAT: xo = XO_MOVSSto; break;
#if LJ_64
    case IRT_LIGHTUD: lua_assert(0);  /* NYI: mask 64 bit lightuserdata. */
#endif
    default:
      if (LJ_64 && irt_is64(ir->t))
	src |= REX_64;
      else
	lua_assert(irt_isint(ir->t) || irt_isu32(ir->t) || irt_isaddr(ir->t));
      xo = XO_MOVto;
      break;
    }
    emit_mrm(as, xo, src, RID_MRM);
    if (!LJ_64 && src != osrc) {
      ra_noweak(as, osrc);
      emit_rr(as, XO_MOV, src, osrc);
    }
  } else {
    if (irt_isi8(ir->t) || irt_isu8(ir->t)) {
      emit_i8(as, k);
      emit_mrm(as, XO_MOVmib, 0, RID_MRM);
    } else {
      lua_assert(irt_is64(ir->t) || irt_isint(ir->t) || irt_isu32(ir->t) ||
		 irt_isaddr(ir->t));
      emit_i32(as, k);
      emit_mrm(as, XO_MOVmi, REX_64IR(ir, 0), RID_MRM);
    }
  }
}

#if LJ_64
static Reg asm_load_lightud64(ASMState *as, IRIns *ir, int typecheck)
{
  if (ra_used(ir) || typecheck) {
    Reg dest = ra_dest(as, ir, RSET_GPR);
    if (typecheck) {
      Reg tmp = ra_scratch(as, rset_exclude(RSET_GPR, dest));
      asm_guardcc(as, CC_NE);
      emit_i8(as, -2);
      emit_rr(as, XO_ARITHi8, XOg_CMP, tmp);
      emit_shifti(as, XOg_SAR|REX_64, tmp, 47);
      emit_rr(as, XO_MOV, tmp|REX_64, dest);
    }
    return dest;
  } else {
    return RID_NONE;
  }
}
#endif

static void asm_ahuvload(ASMState *as, IRIns *ir)
{
  lua_assert(irt_isnum(ir->t) || irt_ispri(ir->t) || irt_isaddr(ir->t) ||
	     (LJ_DUALNUM && irt_isint(ir->t)));
#if LJ_64
  if (irt_islightud(ir->t)) {
    Reg dest = asm_load_lightud64(as, ir, 1);
    if (ra_hasreg(dest)) {
      asm_fuseahuref(as, ir->op1, RSET_GPR);
      emit_mrm(as, XO_MOV, dest|REX_64, RID_MRM);
    }
    return;
  } else
#endif
  if (ra_used(ir)) {
    RegSet allow = irt_isnum(ir->t) ? RSET_FPR : RSET_GPR;
    Reg dest = ra_dest(as, ir, allow);
    asm_fuseahuref(as, ir->op1, RSET_GPR);
    emit_mrm(as, dest < RID_MAX_GPR ? XO_MOV : XMM_MOVRM(as), dest, RID_MRM);
  } else {
    asm_fuseahuref(as, ir->op1, RSET_GPR);
  }
  /* Always do the type check, even if the load result is unused. */
  as->mrm.ofs += 4;
  asm_guardcc(as, irt_isnum(ir->t) ? CC_AE : CC_NE);
  if (LJ_64 && irt_type(ir->t) >= IRT_NUM) {
    lua_assert(irt_isinteger(ir->t) || irt_isnum(ir->t));
    emit_u32(as, LJ_TISNUM);
    emit_mrm(as, XO_ARITHi, XOg_CMP, RID_MRM);
  } else {
    emit_i8(as, irt_toitype(ir->t));
    emit_mrm(as, XO_ARITHi8, XOg_CMP, RID_MRM);
  }
}

static void asm_ahustore(ASMState *as, IRIns *ir)
{
  if (irt_isnum(ir->t)) {
    Reg src = ra_alloc1(as, ir->op2, RSET_FPR);
    asm_fuseahuref(as, ir->op1, RSET_GPR);
    emit_mrm(as, XO_MOVSDto, src, RID_MRM);
#if LJ_64
  } else if (irt_islightud(ir->t)) {
    Reg src = ra_alloc1(as, ir->op2, RSET_GPR);
    asm_fuseahuref(as, ir->op1, rset_exclude(RSET_GPR, src));
    emit_mrm(as, XO_MOVto, src|REX_64, RID_MRM);
#endif
  } else {
    IRIns *irr = IR(ir->op2);
    RegSet allow = RSET_GPR;
    Reg src = RID_NONE;
    if (!irref_isk(ir->op2)) {
      src = ra_alloc1(as, ir->op2, allow);
      rset_clear(allow, src);
    }
    asm_fuseahuref(as, ir->op1, allow);
    if (ra_hasreg(src)) {
      emit_mrm(as, XO_MOVto, src, RID_MRM);
    } else if (!irt_ispri(irr->t)) {
      lua_assert(irt_isaddr(ir->t) || (LJ_DUALNUM && irt_isinteger(ir->t)));
      emit_i32(as, irr->i);
      emit_mrm(as, XO_MOVmi, 0, RID_MRM);
    }
    as->mrm.ofs += 4;
    emit_i32(as, (int32_t)irt_toitype(ir->t));
    emit_mrm(as, XO_MOVmi, 0, RID_MRM);
  }
}

static void asm_sload(ASMState *as, IRIns *ir)
{
  int32_t ofs = 8*((int32_t)ir->op1-1) + ((ir->op2 & IRSLOAD_FRAME) ? 4 : 0);
  IRType1 t = ir->t;
  Reg base;
  lua_assert(!(ir->op2 & IRSLOAD_PARENT));  /* Handled by asm_head_side(). */
  lua_assert(irt_isguard(t) || !(ir->op2 & IRSLOAD_TYPECHECK));
  lua_assert(LJ_DUALNUM ||
	     !irt_isint(t) || (ir->op2 & (IRSLOAD_CONVERT|IRSLOAD_FRAME)));
  if ((ir->op2 & IRSLOAD_CONVERT) && irt_isguard(t) && irt_isint(t)) {
    Reg left = ra_scratch(as, RSET_FPR);
    asm_tointg(as, ir, left);  /* Frees dest reg. Do this before base alloc. */
    base = ra_alloc1(as, REF_BASE, RSET_GPR);
    emit_rmro(as, XMM_MOVRM(as), left, base, ofs);
    t.irt = IRT_NUM;  /* Continue with a regular number type check. */
#if LJ_64
  } else if (irt_islightud(t)) {
    Reg dest = asm_load_lightud64(as, ir, (ir->op2 & IRSLOAD_TYPECHECK));
    if (ra_hasreg(dest)) {
      base = ra_alloc1(as, REF_BASE, RSET_GPR);
      emit_rmro(as, XO_MOV, dest|REX_64, base, ofs);
    }
    return;
#endif
  } else if (ra_used(ir)) {
    RegSet allow = irt_isnum(t) ? RSET_FPR : RSET_GPR;
    Reg dest = ra_dest(as, ir, allow);
    base = ra_alloc1(as, REF_BASE, RSET_GPR);
    lua_assert(irt_isnum(t) || irt_isint(t) || irt_isaddr(t));
    if ((ir->op2 & IRSLOAD_CONVERT)) {
      t.irt = irt_isint(t) ? IRT_NUM : IRT_INT;  /* Check for original type. */
      emit_rmro(as, irt_isint(t) ? XO_CVTSI2SD : XO_CVTSD2SI, dest, base, ofs);
    } else if (irt_isnum(t)) {
      emit_rmro(as, XMM_MOVRM(as), dest, base, ofs);
    } else {
      emit_rmro(as, XO_MOV, dest, base, ofs);
    }
  } else {
    if (!(ir->op2 & IRSLOAD_TYPECHECK))
      return;  /* No type check: avoid base alloc. */
    base = ra_alloc1(as, REF_BASE, RSET_GPR);
  }
  if ((ir->op2 & IRSLOAD_TYPECHECK)) {
    /* Need type check, even if the load result is unused. */
    asm_guardcc(as, irt_isnum(t) ? CC_AE : CC_NE);
    if (LJ_64 && irt_type(t) >= IRT_NUM) {
      lua_assert(irt_isinteger(t) || irt_isnum(t));
      emit_u32(as, LJ_TISNUM);
      emit_rmro(as, XO_ARITHi, XOg_CMP, base, ofs+4);
    } else {
      emit_i8(as, irt_toitype(t));
      emit_rmro(as, XO_ARITHi8, XOg_CMP, base, ofs+4);
    }
  }
}

/* -- Allocations --------------------------------------------------------- */

static void asm_snew(ASMState *as, IRIns *ir)
{
  const CCallInfo *ci = &lj_ir_callinfo[IRCALL_lj_str_new];
  IRRef args[3];
  args[0] = ASMREF_L;  /* lua_State *L    */
  args[1] = ir->op1;   /* const char *str */
  args[2] = ir->op2;   /* size_t len      */
  as->gcsteps++;
  asm_setupresult(as, ir, ci);  /* GCstr * */
  asm_gencall(as, ci, args);
}

static void asm_tnew(ASMState *as, IRIns *ir)
{
  const CCallInfo *ci = &lj_ir_callinfo[IRCALL_lj_tab_new1];
  IRRef args[2];
  args[0] = ASMREF_L;     /* lua_State *L    */
  args[1] = ASMREF_TMP1;  /* uint32_t ahsize */
  as->gcsteps++;
  asm_setupresult(as, ir, ci);  /* GCtab * */
  asm_gencall(as, ci, args);
  emit_loadi(as, ra_releasetmp(as, ASMREF_TMP1), ir->op1 | (ir->op2 << 24));
}

static void asm_tdup(ASMState *as, IRIns *ir)
{
  const CCallInfo *ci = &lj_ir_callinfo[IRCALL_lj_tab_dup];
  IRRef args[2];
  args[0] = ASMREF_L;  /* lua_State *L    */
  args[1] = ir->op1;   /* const GCtab *kt */
  as->gcsteps++;
  asm_setupresult(as, ir, ci);  /* GCtab * */
  asm_gencall(as, ci, args);
}

#if LJ_HASFFI
static void asm_cnew(ASMState *as, IRIns *ir)
{
  CTState *cts = ctype_ctsG(J2G(as->J));
  CTypeID typeid = (CTypeID)IR(ir->op1)->i;
  CTSize sz = (ir->o == IR_CNEWI || ir->op2 == REF_NIL) ?
	      lj_ctype_size(cts, typeid) : (CTSize)IR(ir->op2)->i;
  const CCallInfo *ci = &lj_ir_callinfo[IRCALL_lj_mem_newgco];
  IRRef args[2];
  lua_assert(sz != CTSIZE_INVALID);

  args[0] = ASMREF_L;     /* lua_State *L */
  args[1] = ASMREF_TMP1;  /* MSize size   */
  as->gcsteps++;
  asm_setupresult(as, ir, ci);  /* GCcdata * */

  /* Initialize immutable cdata object. */
  if (ir->o == IR_CNEWI) {
    RegSet allow = (RSET_GPR & ~RSET_SCRATCH);
#if LJ_64
    Reg r64 = sz == 8 ? REX_64 : 0;
    if (irref_isk(ir->op2)) {
      IRIns *irk = IR(ir->op2);
      uint64_t k = irk->o == IR_KINT64 ? ir_k64(irk)->u64 :
					 (uint64_t)(uint32_t)irk->i;
      if (sz == 4 || checki32((int64_t)k)) {
	emit_i32(as, (int32_t)k);
	emit_rmro(as, XO_MOVmi, r64, RID_RET, sizeof(GCcdata));
      } else {
	emit_movtomro(as, RID_ECX + r64, RID_RET, sizeof(GCcdata));
	emit_loadu64(as, RID_ECX, k);
      }
    } else {
      Reg r = ra_alloc1(as, ir->op2, allow);
      emit_movtomro(as, r + r64, RID_RET, sizeof(GCcdata));
    }
#else
    int32_t ofs = sizeof(GCcdata);
    if (LJ_HASFFI && sz == 8) {
      ofs += 4; ir++;
      lua_assert(ir->o == IR_HIOP);
    }
    do {
      if (irref_isk(ir->op2)) {
	emit_movmroi(as, RID_RET, ofs, IR(ir->op2)->i);
      } else {
	Reg r = ra_alloc1(as, ir->op2, allow);
	emit_movtomro(as, r, RID_RET, ofs);
	rset_clear(allow, r);
      }
      if (!LJ_HASFFI || ofs == sizeof(GCcdata)) break;
      ofs -= 4; ir--;
    } while (1);
#endif
    lua_assert(sz == 4 || (sz == 8 && (LJ_64 || LJ_HASFFI)));
  }

  /* Combine initialization of marked, gct and typeid. */
  emit_movtomro(as, RID_ECX, RID_RET, offsetof(GCcdata, marked));
  emit_gri(as, XG_ARITHi(XOg_OR), RID_ECX,
	   (int32_t)((~LJ_TCDATA<<8)+(typeid<<16)));
  emit_gri(as, XG_ARITHi(XOg_AND), RID_ECX, LJ_GC_WHITES);
  emit_opgl(as, XO_MOVZXb, RID_ECX, gc.currentwhite);

  asm_gencall(as, ci, args);
  emit_loadi(as, ra_releasetmp(as, ASMREF_TMP1), (int32_t)(sz+sizeof(GCcdata)));
}
#else
#define asm_cnew(as, ir)	((void)0)
#endif

/* -- Write barriers ------------------------------------------------------ */

static void asm_tbar(ASMState *as, IRIns *ir)
{
  Reg tab = ra_alloc1(as, ir->op1, RSET_GPR);
  Reg tmp = ra_scratch(as, rset_exclude(RSET_GPR, tab));
  MCLabel l_end = emit_label(as);
  emit_movtomro(as, tmp, tab, offsetof(GCtab, gclist));
  emit_setgl(as, tab, gc.grayagain);
  emit_getgl(as, tmp, gc.grayagain);
  emit_i8(as, ~LJ_GC_BLACK);
  emit_rmro(as, XO_ARITHib, XOg_AND, tab, offsetof(GCtab, marked));
  emit_sjcc(as, CC_Z, l_end);
  emit_i8(as, LJ_GC_BLACK);
  emit_rmro(as, XO_GROUP3b, XOg_TEST, tab, offsetof(GCtab, marked));
}

static void asm_obar(ASMState *as, IRIns *ir)
{
  const CCallInfo *ci = &lj_ir_callinfo[IRCALL_lj_gc_barrieruv];
  IRRef args[2];
  MCLabel l_end;
  Reg obj;
  /* No need for other object barriers (yet). */
  lua_assert(IR(ir->op1)->o == IR_UREFC);
  ra_evictset(as, RSET_SCRATCH);
  l_end = emit_label(as);
  args[0] = ASMREF_TMP1;  /* global_State *g */
  args[1] = ir->op1;      /* TValue *tv      */
  asm_gencall(as, ci, args);
  emit_loada(as, ra_releasetmp(as, ASMREF_TMP1), J2G(as->J));
  obj = IR(ir->op1)->r;
  emit_sjcc(as, CC_Z, l_end);
  emit_i8(as, LJ_GC_WHITES);
  if (irref_isk(ir->op2)) {
    GCobj *vp = ir_kgc(IR(ir->op2));
    emit_rma(as, XO_GROUP3b, XOg_TEST, &vp->gch.marked);
  } else {
    Reg val = ra_alloc1(as, ir->op2, rset_exclude(RSET_SCRATCH&RSET_GPR, obj));
    emit_rmro(as, XO_GROUP3b, XOg_TEST, val, (int32_t)offsetof(GChead, marked));
  }
  emit_sjcc(as, CC_Z, l_end);
  emit_i8(as, LJ_GC_BLACK);
  emit_rmro(as, XO_GROUP3b, XOg_TEST, obj,
	    (int32_t)offsetof(GCupval, marked)-(int32_t)offsetof(GCupval, tv));
}

/* -- FP/int arithmetic and logic operations ------------------------------ */

/* Load reference onto x87 stack. Force a spill to memory if needed. */
static void asm_x87load(ASMState *as, IRRef ref)
{
  IRIns *ir = IR(ref);
  if (ir->o == IR_KNUM) {
    cTValue *tv = ir_knum(ir);
    if (tvispzero(tv))  /* Use fldz only for +0. */
      emit_x87op(as, XI_FLDZ);
    else if (tvispone(tv))
      emit_x87op(as, XI_FLD1);
    else
      emit_rma(as, XO_FLDq, XOg_FLDq, tv);
  } else if (ir->o == IR_CONV && ir->op2 == IRCONV_NUM_INT && !ra_used(ir) &&
	     !irref_isk(ir->op1) && mayfuse(as, ir->op1)) {
    IRIns *iri = IR(ir->op1);
    emit_rmro(as, XO_FILDd, XOg_FILDd, RID_ESP, ra_spill(as, iri));
  } else {
    emit_mrm(as, XO_FLDq, XOg_FLDq, asm_fuseload(as, ref, RSET_EMPTY));
  }
}

/* Try to rejoin pow from EXP2, MUL and LOG2 (if still unsplit). */
static int fpmjoin_pow(ASMState *as, IRIns *ir)
{
  IRIns *irp = IR(ir->op1);
  if (irp == ir-1 && irp->o == IR_MUL && !ra_used(irp)) {
    IRIns *irpp = IR(irp->op1);
    if (irpp == ir-2 && irpp->o == IR_FPMATH &&
	irpp->op2 == IRFPM_LOG2 && !ra_used(irpp)) {
      /* The modified regs must match with the *.dasc implementation. */
      RegSet drop = RSET_RANGE(RID_XMM0, RID_XMM2+1)|RID2RSET(RID_EAX);
      IRIns *irx;
      if (ra_hasreg(ir->r))
	rset_clear(drop, ir->r);  /* Dest reg handled below. */
      ra_evictset(as, drop);
      ra_destreg(as, ir, RID_XMM0);
      emit_call(as, lj_vm_pow_sse);
      irx = IR(irpp->op1);
      if (ra_noreg(irx->r) && ra_gethint(irx->r) == RID_XMM1)
	irx->r = RID_INIT;  /* Avoid allocating xmm1 for x. */
      ra_left(as, RID_XMM0, irpp->op1);
      ra_left(as, RID_XMM1, irp->op2);
      return 1;
    }
  }
  return 0;
}

static void asm_fpmath(ASMState *as, IRIns *ir)
{
  IRFPMathOp fpm = ir->o == IR_FPMATH ? (IRFPMathOp)ir->op2 : IRFPM_OTHER;
  if (fpm == IRFPM_SQRT) {
    Reg dest = ra_dest(as, ir, RSET_FPR);
    Reg left = asm_fuseload(as, ir->op1, RSET_FPR);
    emit_mrm(as, XO_SQRTSD, dest, left);
  } else if (fpm <= IRFPM_TRUNC) {
    if (as->flags & JIT_F_SSE4_1) {  /* SSE4.1 has a rounding instruction. */
      Reg dest = ra_dest(as, ir, RSET_FPR);
      Reg left = asm_fuseload(as, ir->op1, RSET_FPR);
      /* ROUNDSD has a 4-byte opcode which doesn't fit in x86Op.
      ** Let's pretend it's a 3-byte opcode, and compensate afterwards.
      ** This is atrocious, but the alternatives are much worse.
      */
      /* Round down/up/trunc == 1001/1010/1011. */
      emit_i8(as, 0x09 + fpm);
      emit_mrm(as, XO_ROUNDSD, dest, left);
      if (LJ_64 && as->mcp[1] != (MCode)(XO_ROUNDSD >> 16)) {
	as->mcp[0] = as->mcp[1]; as->mcp[1] = 0x0f;  /* Swap 0F and REX. */
      }
      *--as->mcp = 0x66;  /* 1st byte of ROUNDSD opcode. */
    } else {  /* Call helper functions for SSE2 variant. */
      /* The modified regs must match with the *.dasc implementation. */
      RegSet drop = RSET_RANGE(RID_XMM0, RID_XMM3+1)|RID2RSET(RID_EAX);
      if (ra_hasreg(ir->r))
	rset_clear(drop, ir->r);  /* Dest reg handled below. */
      ra_evictset(as, drop);
      ra_destreg(as, ir, RID_XMM0);
      emit_call(as, fpm == IRFPM_FLOOR ? lj_vm_floor_sse :
		    fpm == IRFPM_CEIL ? lj_vm_ceil_sse : lj_vm_trunc_sse);
      ra_left(as, RID_XMM0, ir->op1);
    }
  } else if (fpm == IRFPM_EXP2 && fpmjoin_pow(as, ir)) {
    /* Rejoined to pow(). */
  } else {  /* Handle x87 ops. */
    int32_t ofs = sps_scale(ir->s);  /* Use spill slot or temp slots. */
    Reg dest = ir->r;
    if (ra_hasreg(dest)) {
      ra_free(as, dest);
      ra_modified(as, dest);
      emit_rmro(as, XMM_MOVRM(as), dest, RID_ESP, ofs);
    }
    emit_rmro(as, XO_FSTPq, XOg_FSTPq, RID_ESP, ofs);
    switch (fpm) {  /* st0 = lj_vm_*(st0) */
    case IRFPM_EXP: emit_call(as, lj_vm_exp); break;
    case IRFPM_EXP2: emit_call(as, lj_vm_exp2); break;
    case IRFPM_SIN: emit_x87op(as, XI_FSIN); break;
    case IRFPM_COS: emit_x87op(as, XI_FCOS); break;
    case IRFPM_TAN: emit_x87op(as, XI_FPOP); emit_x87op(as, XI_FPTAN); break;
    case IRFPM_LOG: case IRFPM_LOG2: case IRFPM_LOG10:
      /* Note: the use of fyl2xp1 would be pointless here. When computing
      ** log(1.0+eps) the precision is already lost after 1.0 is added.
      ** Subtracting 1.0 won't recover it. OTOH math.log1p would make sense.
      */
      emit_x87op(as, XI_FYL2X); break;
    case IRFPM_OTHER:
      switch (ir->o) {
      case IR_ATAN2:
	emit_x87op(as, XI_FPATAN); asm_x87load(as, ir->op2); break;
      case IR_LDEXP:
	emit_x87op(as, XI_FPOP1); emit_x87op(as, XI_FSCALE); break;
      default: lua_assert(0); break;
      }
      break;
    default: lua_assert(0); break;
    }
    asm_x87load(as, ir->op1);
    switch (fpm) {
    case IRFPM_LOG: emit_x87op(as, XI_FLDLN2); break;
    case IRFPM_LOG2: emit_x87op(as, XI_FLD1); break;
    case IRFPM_LOG10: emit_x87op(as, XI_FLDLG2); break;
    case IRFPM_OTHER:
      if (ir->o == IR_LDEXP) asm_x87load(as, ir->op2);
      break;
    default: break;
    }
  }
}

static void asm_fppowi(ASMState *as, IRIns *ir)
{
  /* The modified regs must match with the *.dasc implementation. */
  RegSet drop = RSET_RANGE(RID_XMM0, RID_XMM1+1)|RID2RSET(RID_EAX);
  if (ra_hasreg(ir->r))
    rset_clear(drop, ir->r);  /* Dest reg handled below. */
  ra_evictset(as, drop);
  ra_destreg(as, ir, RID_XMM0);
  emit_call(as, lj_vm_powi_sse);
  ra_left(as, RID_XMM0, ir->op1);
  ra_left(as, RID_EAX, ir->op2);
}

#if LJ_64 && LJ_HASFFI
static void asm_arith64(ASMState *as, IRIns *ir, IRCallID id)
{
  const CCallInfo *ci = &lj_ir_callinfo[id];
  IRRef args[2];
  args[0] = ir->op1;
  args[1] = ir->op2;
  asm_setupresult(as, ir, ci);
  asm_gencall(as, ci, args);
}
#endif

/* Find out whether swapping operands might be beneficial. */
static int swapops(ASMState *as, IRIns *ir)
{
  IRIns *irl = IR(ir->op1);
  IRIns *irr = IR(ir->op2);
  lua_assert(ra_noreg(irr->r));
  if (!irm_iscomm(lj_ir_mode[ir->o]))
    return 0;  /* Can't swap non-commutative operations. */
  if (irref_isk(ir->op2))
    return 0;  /* Don't swap constants to the left. */
  if (ra_hasreg(irl->r))
    return 1;  /* Swap if left already has a register. */
  if (ra_samehint(ir->r, irr->r))
    return 1;  /* Swap if dest and right have matching hints. */
  if (as->curins > as->loopref) {  /* In variant part? */
    if (ir->op2 < as->loopref && !irt_isphi(irr->t))
      return 0;  /* Keep invariants on the right. */
    if (ir->op1 < as->loopref && !irt_isphi(irl->t))
      return 1;  /* Swap invariants to the right. */
  }
  if (opisfusableload(irl->o))
    return 1;  /* Swap fusable loads to the right. */
  return 0;  /* Otherwise don't swap. */
}

static void asm_fparith(ASMState *as, IRIns *ir, x86Op xo)
{
  IRRef lref = ir->op1;
  IRRef rref = ir->op2;
  RegSet allow = RSET_FPR;
  Reg dest;
  Reg right = IR(rref)->r;
  if (ra_hasreg(right)) {
    rset_clear(allow, right);
    ra_noweak(as, right);
  }
  dest = ra_dest(as, ir, allow);
  if (lref == rref) {
    right = dest;
  } else if (ra_noreg(right)) {
    if (swapops(as, ir)) {
      IRRef tmp = lref; lref = rref; rref = tmp;
    }
    right = asm_fuseload(as, rref, rset_clear(allow, dest));
  }
  emit_mrm(as, xo, dest, right);
  ra_left(as, dest, lref);
}

static void asm_intarith(ASMState *as, IRIns *ir, x86Arith xa)
{
  IRRef lref = ir->op1;
  IRRef rref = ir->op2;
  RegSet allow = RSET_GPR;
  Reg dest, right;
  int32_t k = 0;
  if (as->testmcp == as->mcp) {  /* Drop test r,r instruction. */
    as->testmcp = NULL;
    as->mcp += (LJ_64 && *as->mcp != XI_TEST) ? 3 : 2;
  }
  right = IR(rref)->r;
  if (ra_hasreg(right)) {
    rset_clear(allow, right);
    ra_noweak(as, right);
  }
  dest = ra_dest(as, ir, allow);
  if (lref == rref) {
    right = dest;
  } else if (ra_noreg(right) && !asm_isk32(as, rref, &k)) {
    if (swapops(as, ir)) {
      IRRef tmp = lref; lref = rref; rref = tmp;
    }
    right = asm_fuseload(as, rref, rset_clear(allow, dest));
  }
  if (irt_isguard(ir->t))  /* For IR_ADDOV etc. */
    asm_guardcc(as, CC_O);
  if (xa != XOg_X_IMUL) {
    if (ra_hasreg(right))
      emit_mrm(as, XO_ARITH(xa), REX_64IR(ir, dest), right);
    else
      emit_gri(as, XG_ARITHi(xa), REX_64IR(ir, dest), k);
  } else if (ra_hasreg(right)) {  /* IMUL r, mrm. */
    emit_mrm(as, XO_IMUL, REX_64IR(ir, dest), right);
  } else {  /* IMUL r, r, k. */
    /* NYI: use lea/shl/add/sub (FOLD only does 2^k) depending on CPU. */
    Reg left = asm_fuseload(as, lref, RSET_GPR);
    x86Op xo;
    if (checki8(k)) { emit_i8(as, k); xo = XO_IMULi8;
    } else { emit_i32(as, k); xo = XO_IMULi; }
    emit_mrm(as, xo, REX_64IR(ir, dest), left);
    return;
  }
  ra_left(as, dest, lref);
}

/* LEA is really a 4-operand ADD with an independent destination register,
** up to two source registers and an immediate. One register can be scaled
** by 1, 2, 4 or 8. This can be used to avoid moves or to fuse several
** instructions.
**
** Currently only a few common cases are supported:
** - 3-operand ADD:    y = a+b; y = a+k   with a and b already allocated
** - Left ADD fusion:  y = (a+b)+k; y = (a+k)+b
** - Right ADD fusion: y = a+(b+k)
** The ommited variants have already been reduced by FOLD.
**
** There are more fusion opportunities, like gathering shifts or joining
** common references. But these are probably not worth the trouble, since
** array indexing is not decomposed and already makes use of all fields
** of the ModRM operand.
*/
static int asm_lea(ASMState *as, IRIns *ir)
{
  IRIns *irl = IR(ir->op1);
  IRIns *irr = IR(ir->op2);
  RegSet allow = RSET_GPR;
  Reg dest;
  as->mrm.base = as->mrm.idx = RID_NONE;
  as->mrm.scale = XM_SCALE1;
  as->mrm.ofs = 0;
  if (ra_hasreg(irl->r)) {
    rset_clear(allow, irl->r);
    ra_noweak(as, irl->r);
    as->mrm.base = irl->r;
    if (irref_isk(ir->op2) || ra_hasreg(irr->r)) {
      /* The PHI renaming logic does a better job in some cases. */
      if (ra_hasreg(ir->r) &&
	  ((irt_isphi(irl->t) && as->phireg[ir->r] == ir->op1) ||
	   (irt_isphi(irr->t) && as->phireg[ir->r] == ir->op2)))
	return 0;
      if (irref_isk(ir->op2)) {
	as->mrm.ofs = irr->i;
      } else {
	rset_clear(allow, irr->r);
	ra_noweak(as, irr->r);
	as->mrm.idx = irr->r;
      }
    } else if (irr->o == IR_ADD && mayfuse(as, ir->op2) &&
	       irref_isk(irr->op2)) {
      Reg idx = ra_alloc1(as, irr->op1, allow);
      rset_clear(allow, idx);
      as->mrm.idx = (uint8_t)idx;
      as->mrm.ofs = IR(irr->op2)->i;
    } else {
      return 0;
    }
  } else if (ir->op1 != ir->op2 && irl->o == IR_ADD && mayfuse(as, ir->op1) &&
	     (irref_isk(ir->op2) || irref_isk(irl->op2))) {
    Reg idx, base = ra_alloc1(as, irl->op1, allow);
    rset_clear(allow, base);
    as->mrm.base = (uint8_t)base;
    if (irref_isk(ir->op2)) {
      as->mrm.ofs = irr->i;
      idx = ra_alloc1(as, irl->op2, allow);
    } else {
      as->mrm.ofs = IR(irl->op2)->i;
      idx = ra_alloc1(as, ir->op2, allow);
    }
    rset_clear(allow, idx);
    as->mrm.idx = (uint8_t)idx;
  } else {
    return 0;
  }
  dest = ra_dest(as, ir, allow);
  emit_mrm(as, XO_LEA, dest, RID_MRM);
  return 1;  /* Success. */
}

static void asm_add(ASMState *as, IRIns *ir)
{
  if (irt_isnum(ir->t))
    asm_fparith(as, ir, XO_ADDSD);
  else if ((as->flags & JIT_F_LEA_AGU) || as->testmcp == as->mcp ||
	   irt_is64(ir->t) || !asm_lea(as, ir))
    asm_intarith(as, ir, XOg_ADD);
}

static void asm_neg_not(ASMState *as, IRIns *ir, x86Group3 xg)
{
  Reg dest = ra_dest(as, ir, RSET_GPR);
  emit_rr(as, XO_GROUP3, REX_64IR(ir, xg), dest);
  ra_left(as, dest, ir->op1);
}

static void asm_min_max(ASMState *as, IRIns *ir, int cc)
{
  Reg right, dest = ra_dest(as, ir, RSET_GPR);
  IRRef lref = ir->op1, rref = ir->op2;
  if (irref_isk(rref)) { lref = rref; rref = ir->op1; }
  right = ra_alloc1(as, rref, rset_exclude(RSET_GPR, dest));
  emit_rr(as, XO_CMOV + (cc<<24), REX_64IR(ir, dest), right);
  emit_rr(as, XO_CMP, REX_64IR(ir, dest), right);
  ra_left(as, dest, lref);
}

static void asm_bitswap(ASMState *as, IRIns *ir)
{
  Reg dest = ra_dest(as, ir, RSET_GPR);
  as->mcp = emit_op(XO_BSWAP + ((dest&7) << 24),
		    REX_64IR(ir, dest), 0, 0, as->mcp, 1);
  ra_left(as, dest, ir->op1);
}

static void asm_bitshift(ASMState *as, IRIns *ir, x86Shift xs)
{
  IRRef rref = ir->op2;
  IRIns *irr = IR(rref);
  Reg dest;
  if (irref_isk(rref)) {  /* Constant shifts. */
    int shift;
    dest = ra_dest(as, ir, RSET_GPR);
    shift = irr->i & (irt_is64(ir->t) ? 63 : 31);
    switch (shift) {
    case 0: break;
    case 1: emit_rr(as, XO_SHIFT1, REX_64IR(ir, xs), dest); break;
    default: emit_shifti(as, REX_64IR(ir, xs), dest, shift); break;
    }
  } else {  /* Variable shifts implicitly use register cl (i.e. ecx). */
    RegSet allow = rset_exclude(RSET_GPR, RID_ECX);
    Reg right = irr->r;
    if (ra_noreg(right)) {
      right = ra_allocref(as, rref, RID2RSET(RID_ECX));
    } else if (right != RID_ECX) {
      rset_clear(allow, right);
      ra_scratch(as, RID2RSET(RID_ECX));
    }
    dest = ra_dest(as, ir, allow);
    emit_rr(as, XO_SHIFTcl, REX_64IR(ir, xs), dest);
    if (right != RID_ECX) {
      ra_noweak(as, right);
      emit_rr(as, XO_MOV, RID_ECX, right);
    }
  }
  ra_left(as, dest, ir->op1);
  /*
  ** Note: avoid using the flags resulting from a shift or rotate!
  ** All of them cause a partial flag stall, except for r,1 shifts
  ** (but not rotates). And a shift count of 0 leaves the flags unmodified.
  */
}

/* -- Comparisons --------------------------------------------------------- */

/* Virtual flags for unordered FP comparisons. */
#define VCC_U	0x1000		/* Unordered. */
#define VCC_P	0x2000		/* Needs extra CC_P branch. */
#define VCC_S	0x4000		/* Swap avoids CC_P branch. */
#define VCC_PS	(VCC_P|VCC_S)

/* Map of comparisons to flags. ORDER IR. */
#define COMPFLAGS(ci, cin, cu, cf)	((ci)+((cu)<<4)+((cin)<<8)+(cf))
static const uint16_t asm_compmap[IR_ABC+1] = {
  /*                 signed non-eq unsigned flags */
  /* LT  */ COMPFLAGS(CC_GE, CC_G,  CC_AE, VCC_PS),
  /* GE  */ COMPFLAGS(CC_L,  CC_L,  CC_B,  0),
  /* LE  */ COMPFLAGS(CC_G,  CC_G,  CC_A,  VCC_PS),
  /* GT  */ COMPFLAGS(CC_LE, CC_L,  CC_BE, 0),
  /* ULT */ COMPFLAGS(CC_AE, CC_A,  CC_AE, VCC_U),
  /* UGE */ COMPFLAGS(CC_B,  CC_B,  CC_B,  VCC_U|VCC_PS),
  /* ULE */ COMPFLAGS(CC_A,  CC_A,  CC_A,  VCC_U),
  /* UGT */ COMPFLAGS(CC_BE, CC_B,  CC_BE, VCC_U|VCC_PS),
  /* EQ  */ COMPFLAGS(CC_NE, CC_NE, CC_NE, VCC_P),
  /* NE  */ COMPFLAGS(CC_E,  CC_E,  CC_E,  VCC_U|VCC_P),
  /* ABC */ COMPFLAGS(CC_BE, CC_B,  CC_BE, VCC_U|VCC_PS)  /* Same as UGT. */
};

/* FP and integer comparisons. */
static void asm_comp(ASMState *as, IRIns *ir, uint32_t cc)
{
  if (irt_isnum(ir->t)) {
    IRRef lref = ir->op1;
    IRRef rref = ir->op2;
    Reg left, right;
    MCLabel l_around;
    /*
    ** An extra CC_P branch is required to preserve ordered/unordered
    ** semantics for FP comparisons. This can be avoided by swapping
    ** the operands and inverting the condition (except for EQ and UNE).
    ** So always try to swap if possible.
    **
    ** Another option would be to swap operands to achieve better memory
    ** operand fusion. But it's unlikely that this outweighs the cost
    ** of the extra branches.
    */
    if (cc & VCC_S) {  /* Swap? */
      IRRef tmp = lref; lref = rref; rref = tmp;
      cc ^= (VCC_PS|(5<<4));  /* A <-> B, AE <-> BE, PS <-> none */
    }
    left = ra_alloc1(as, lref, RSET_FPR);
    right = asm_fuseload(as, rref, rset_exclude(RSET_FPR, left));
    l_around = emit_label(as);
    asm_guardcc(as, cc >> 4);
    if (cc & VCC_P) {  /* Extra CC_P branch required? */
      if (!(cc & VCC_U)) {
	asm_guardcc(as, CC_P);  /* Branch to exit for ordered comparisons. */
      } else if (l_around != as->invmcp) {
	emit_sjcc(as, CC_P, l_around);  /* Branch around for unordered. */
      } else {
	/* Patched to mcloop by asm_loop_fixup. */
	as->loopinv = 2;
	if (as->realign)
	  emit_sjcc(as, CC_P, as->mcp);
	else
	  emit_jcc(as, CC_P, as->mcp);
      }
    }
    emit_mrm(as, XO_UCOMISD, left, right);
  } else {
    IRRef lref = ir->op1, rref = ir->op2;
    IROp leftop = (IROp)(IR(lref)->o);
    Reg r64 = REX_64IR(ir, 0);
    int32_t imm = 0;
    lua_assert(irt_is64(ir->t) || irt_isint(ir->t) || irt_isaddr(ir->t));
    /* Swap constants (only for ABC) and fusable loads to the right. */
    if (irref_isk(lref) || (!irref_isk(rref) && opisfusableload(leftop))) {
      if ((cc & 0xc) == 0xc) cc ^= 3;  /* L <-> G, LE <-> GE */
      else if ((cc & 0xa) == 0x2) cc ^= 5;  /* A <-> B, AE <-> BE */
      lref = ir->op2; rref = ir->op1;
    }
    if (asm_isk32(as, rref, &imm)) {
      IRIns *irl = IR(lref);
      /* Check wether we can use test ins. Not for unsigned, since CF=0. */
      int usetest = (imm == 0 && (cc & 0xa) != 0x2);
      if (usetest && irl->o == IR_BAND && irl+1 == ir && !ra_used(irl)) {
	/* Combine comp(BAND(ref, r/imm), 0) into test mrm, r/imm. */
	Reg right, left = RID_NONE;
	RegSet allow = RSET_GPR;
	if (!asm_isk32(as, irl->op2, &imm)) {
	  left = ra_alloc1(as, irl->op2, allow);
	  rset_clear(allow, left);
	} else {  /* Try to Fuse IRT_I8/IRT_U8 loads, too. See below. */
	  IRIns *irll = IR(irl->op1);
	  if (opisfusableload((IROp)irll->o) &&
	      (irt_isi8(irll->t) || irt_isu8(irll->t))) {
	    IRType1 origt = irll->t;  /* Temporarily flip types. */
	    irll->t.irt = (irll->t.irt & ~IRT_TYPE) | IRT_INT;
	    as->curins--;  /* Skip to BAND to avoid failing in noconflict(). */
	    right = asm_fuseload(as, irl->op1, RSET_GPR);
	    as->curins++;
	    irll->t = origt;
	    if (right != RID_MRM) goto test_nofuse;
	    /* Fusion succeeded, emit test byte mrm, imm8. */
	    asm_guardcc(as, cc);
	    emit_i8(as, (imm & 0xff));
	    emit_mrm(as, XO_GROUP3b, XOg_TEST, RID_MRM);
	    return;
	  }
	}
	as->curins--;  /* Skip to BAND to avoid failing in noconflict(). */
	right = asm_fuseload(as, irl->op1, allow);
	as->curins++;  /* Undo the above. */
      test_nofuse:
	asm_guardcc(as, cc);
	if (ra_noreg(left)) {
	  emit_i32(as, imm);
	  emit_mrm(as, XO_GROUP3, r64 + XOg_TEST, right);
	} else {
	  emit_mrm(as, XO_TEST, r64 + left, right);
	}
      } else {
	Reg left;
	if (opisfusableload((IROp)irl->o) &&
	    ((irt_isu8(irl->t) && checku8(imm)) ||
	     ((irt_isi8(irl->t) || irt_isi16(irl->t)) && checki8(imm)) ||
	     (irt_isu16(irl->t) && checku16(imm) && checki8((int16_t)imm)))) {
	  /* Only the IRT_INT case is fused by asm_fuseload.
	  ** The IRT_I8/IRT_U8 loads and some IRT_I16/IRT_U16 loads
	  ** are handled here.
	  ** Note that cmp word [mem], imm16 should not be generated,
	  ** since it has a length-changing prefix. Compares of a word
	  ** against a sign-extended imm8 are ok, however.
	  */
	  IRType1 origt = irl->t;  /* Temporarily flip types. */
	  irl->t.irt = (irl->t.irt & ~IRT_TYPE) | IRT_INT;
	  left = asm_fuseload(as, lref, RSET_GPR);
	  irl->t = origt;
	  if (left == RID_MRM) {  /* Fusion succeeded? */
	    asm_guardcc(as, cc);
	    emit_i8(as, imm);
	    emit_mrm(as, (irt_isi8(origt) || irt_isu8(origt)) ?
			 XO_ARITHib : XO_ARITHiw8, r64 + XOg_CMP, RID_MRM);
	    return;
	  }  /* Otherwise handle register case as usual. */
	} else {
	  left = asm_fuseload(as, lref, RSET_GPR);
	}
	asm_guardcc(as, cc);
	if (usetest && left != RID_MRM) {
	  /* Use test r,r instead of cmp r,0. */
	  emit_rr(as, XO_TEST, r64 + left, left);
	  if (irl+1 == ir)  /* Referencing previous ins? */
	    as->testmcp = as->mcp;  /* Set flag to drop test r,r if possible. */
	} else {
	  emit_gmrmi(as, XG_ARITHi(XOg_CMP), r64 + left, imm);
	}
      }
    } else {
      Reg left = ra_alloc1(as, lref, RSET_GPR);
      Reg right = asm_fuseload(as, rref, rset_exclude(RSET_GPR, left));
      asm_guardcc(as, cc);
      emit_mrm(as, XO_CMP, r64 + left, right);
    }
  }
}

#if LJ_32 && LJ_HASFFI
/* 64 bit integer comparisons in 32 bit mode. */
static void asm_comp_int64(ASMState *as, IRIns *ir)
{
  uint32_t cc = asm_compmap[(ir-1)->o];
  RegSet allow = RSET_GPR;
  Reg lefthi = RID_NONE, leftlo = RID_NONE;
  Reg righthi = RID_NONE, rightlo = RID_NONE;
  MCLabel l_around;
  x86ModRM mrm;

  as->curins--;  /* Skip loword ins. Avoids failing in noconflict(), too. */

  /* Allocate/fuse hiword operands. */
  if (irref_isk(ir->op2)) {
    lefthi = asm_fuseload(as, ir->op1, allow);
  } else {
    lefthi = ra_alloc1(as, ir->op1, allow);
    righthi = asm_fuseload(as, ir->op2, allow);
    if (righthi == RID_MRM) {
      if (as->mrm.base != RID_NONE) rset_clear(allow, as->mrm.base);
      if (as->mrm.idx != RID_NONE) rset_clear(allow, as->mrm.idx);
    } else {
      rset_clear(allow, righthi);
    }
  }
  mrm = as->mrm;  /* Save state for hiword instruction. */

  /* Allocate/fuse loword operands. */
  if (irref_isk((ir-1)->op2)) {
    leftlo = asm_fuseload(as, (ir-1)->op1, allow);
  } else {
    leftlo = ra_alloc1(as, (ir-1)->op1, allow);
    rightlo = asm_fuseload(as, (ir-1)->op2, allow);
    if (rightlo == RID_MRM) {
      if (as->mrm.base != RID_NONE) rset_clear(allow, as->mrm.base);
      if (as->mrm.idx != RID_NONE) rset_clear(allow, as->mrm.idx);
    } else {
      rset_clear(allow, rightlo);
    }
  }

  /* All register allocations must be performed _before_ this point. */
  l_around = emit_label(as);
  as->invmcp = as->testmcp = NULL;  /* Cannot use these optimizations. */

  /* Loword comparison and branch. */
  asm_guardcc(as, cc >> 4);  /* Always use unsigned compare for loword. */
  if (ra_noreg(rightlo)) {
    int32_t imm = IR((ir-1)->op2)->i;
    if (imm == 0 && ((cc >> 4) & 0xa) != 0x2 && leftlo != RID_MRM)
      emit_rr(as, XO_TEST, leftlo, leftlo);
    else
      emit_gmrmi(as, XG_ARITHi(XOg_CMP), leftlo, imm);
  } else {
    emit_mrm(as, XO_CMP, leftlo, rightlo);
  }

  /* Hiword comparison and branches. */
  if ((cc & 15) != CC_NE)
    emit_sjcc(as, CC_NE, l_around);  /* Hiword unequal: skip loword compare. */
  if ((cc & 15) != CC_E)
    asm_guardcc(as, cc >> 8);  /* Hiword compare without equality check. */
  as->mrm = mrm;  /* Restore state. */
  if (ra_noreg(righthi)) {
    int32_t imm = IR(ir->op2)->i;
    if (imm == 0 && (cc & 0xa) != 0x2 && lefthi != RID_MRM)
      emit_rr(as, XO_TEST, lefthi, lefthi);
    else
      emit_gmrmi(as, XG_ARITHi(XOg_CMP), lefthi, imm);
  } else {
    emit_mrm(as, XO_CMP, lefthi, righthi);
  }
}
#endif

/* -- Support for 64 bit ops in 32 bit mode ------------------------------- */

/* Hiword op of a split 64 bit op. Previous op must be the loword op. */
static void asm_hiop(ASMState *as, IRIns *ir)
{
#if LJ_32 && LJ_HASFFI
  /* HIOP is marked as a store because it needs its own DCE logic. */
  int uselo = ra_used(ir-1), usehi = ra_used(ir);  /* Loword/hiword used? */
  if (LJ_UNLIKELY(!(as->flags & JIT_F_OPT_DCE))) uselo = usehi = 1;
  if ((ir-1)->o == IR_CONV) {  /* Conversions to/from 64 bit. */
    if (usehi || uselo) {
      if (irt_isfp(ir->t))
	asm_conv_fp_int64(as, ir);
      else
	asm_conv_int64_fp(as, ir);
    }
    as->curins--;  /* Always skip the CONV. */
    return;
  } else if ((ir-1)->o <= IR_NE) {  /* 64 bit integer comparisons. ORDER IR. */
    asm_comp_int64(as, ir);
    return;
  }
  if (!usehi) return;  /* Skip unused hiword op for all remaining ops. */
  switch ((ir-1)->o) {
  case IR_ADD:
    asm_intarith(as, ir, uselo ? XOg_ADC : XOg_ADD);
    break;
  case IR_SUB:
    asm_intarith(as, ir, uselo ? XOg_SBB : XOg_SUB);
    break;
  case IR_NEG: {
    Reg dest = ra_dest(as, ir, RSET_GPR);
    emit_rr(as, XO_GROUP3, XOg_NEG, dest);
    if (uselo) {
      emit_i8(as, 0);
      emit_rr(as, XO_ARITHi8, XOg_ADC, dest);
    }
    ra_left(as, dest, ir->op1);
    break;
    }
  case IR_CALLN:
    ra_destreg(as, ir, RID_RETHI);
    if (!uselo)
      ra_allocref(as, ir->op1, RID2RSET(RID_RET));  /* Mark call as used. */
    break;
  case IR_CNEWI:
    /* Nothing to do here. Handled by CNEWI itself. */
    break;
  default: lua_assert(0); break;
  }
#else
  UNUSED(as); UNUSED(ir); lua_assert(0);  /* Unused on x64 or without FFI. */
#endif
}

/* -- Stack handling ------------------------------------------------------ */

/* Get extent of the stack for a snapshot. */
static BCReg asm_stack_extent(ASMState *as, SnapShot *snap, BCReg *ptopslot)
{
  SnapEntry *map = &as->T->snapmap[snap->mapofs];
  MSize n, nent = snap->nent;
  BCReg baseslot = 0, topslot = 0;
  /* Must check all frames to find topslot (outer can be larger than inner). */
  for (n = 0; n < nent; n++) {
    SnapEntry sn = map[n];
    if ((sn & SNAP_FRAME)) {
      IRIns *ir = IR(snap_ref(sn));
      GCfunc *fn = ir_kfunc(ir);
      if (isluafunc(fn)) {
	BCReg s = snap_slot(sn);
	BCReg fs = s + funcproto(fn)->framesize;
	if (fs > topslot) topslot = fs;
	baseslot = s;
      }
    }
  }
  *ptopslot = topslot;
  return baseslot;
}

/* Check Lua stack size for overflow. Use exit handler as fallback. */
static void asm_stack_check(ASMState *as, BCReg topslot,
			    Reg pbase, RegSet allow, ExitNo exitno)
{
  /* Try to get an unused temp. register, otherwise spill/restore eax. */
  Reg r = allow ? rset_pickbot(allow) : RID_EAX;
  emit_jcc(as, CC_B, exitstub_addr(as->J, exitno));
  if (allow == RSET_EMPTY)  /* Restore temp. register. */
    emit_rmro(as, XO_MOV, r|REX_64, RID_ESP, 0);
  else
    ra_modified(as, r);
  emit_gri(as, XG_ARITHi(XOg_CMP), r, (int32_t)(8*topslot));
  if (ra_hasreg(pbase) && pbase != r)
    emit_rr(as, XO_ARITH(XOg_SUB), r, pbase);
  else
    emit_rmro(as, XO_ARITH(XOg_SUB), r, RID_NONE,
	      ptr2addr(&J2G(as->J)->jit_base));
  emit_rmro(as, XO_MOV, r, r, offsetof(lua_State, maxstack));
  emit_getgl(as, r, jit_L);
  if (allow == RSET_EMPTY)  /* Spill temp. register. */
    emit_rmro(as, XO_MOVto, r|REX_64, RID_ESP, 0);
}

/* Restore Lua stack from on-trace state. */
static void asm_stack_restore(ASMState *as, SnapShot *snap)
{
  SnapEntry *map = &as->T->snapmap[snap->mapofs];
  MSize n, nent = snap->nent;
  SnapEntry *flinks = map + nent + snap->depth;
  /* Store the value of all modified slots to the Lua stack. */
  for (n = 0; n < nent; n++) {
    SnapEntry sn = map[n];
    BCReg s = snap_slot(sn);
    int32_t ofs = 8*((int32_t)s-1);
    IRRef ref = snap_ref(sn);
    IRIns *ir = IR(ref);
    if ((sn & SNAP_NORESTORE))
      continue;
    if (irt_isnum(ir->t)) {
      Reg src = ra_alloc1(as, ref, RSET_FPR);
      emit_rmro(as, XO_MOVSDto, src, RID_BASE, ofs);
    } else {
      lua_assert(irt_ispri(ir->t) || irt_isaddr(ir->t) ||
		 (LJ_DUALNUM && irt_isinteger(ir->t)));
      if (!irref_isk(ref)) {
	Reg src = ra_alloc1(as, ref, rset_exclude(RSET_GPR, RID_BASE));
	emit_movtomro(as, REX_64IR(ir, src), RID_BASE, ofs);
      } else if (!irt_ispri(ir->t)) {
	emit_movmroi(as, RID_BASE, ofs, ir->i);
      }
      if ((sn & (SNAP_CONT|SNAP_FRAME))) {
	if (s != 0)  /* Do not overwrite link to previous frame. */
	  emit_movmroi(as, RID_BASE, ofs+4, (int32_t)(*flinks--));
      } else {
	if (!(LJ_64 && irt_islightud(ir->t)))
	  emit_movmroi(as, RID_BASE, ofs+4, irt_toitype(ir->t));
      }
    }
    checkmclim(as);
  }
  lua_assert(map + nent == flinks);
}

/* -- GC handling --------------------------------------------------------- */

/* Check GC threshold and do one or more GC steps. */
static void asm_gc_check(ASMState *as)
{
  const CCallInfo *ci = &lj_ir_callinfo[IRCALL_lj_gc_step_jit];
  IRRef args[2];
  MCLabel l_end;
  Reg tmp;
  ra_evictset(as, RSET_SCRATCH);
  l_end = emit_label(as);
  /* Exit trace if in GCSatomic or GCSfinalize. Avoids syncing GC objects. */
  asm_guardcc(as, CC_NE);  /* Assumes asm_snap_prep() already done. */
  emit_rr(as, XO_TEST, RID_RET, RID_RET);
  args[0] = ASMREF_TMP1;  /* global_State *g */
  args[1] = ASMREF_TMP2;  /* MSize steps     */
  asm_gencall(as, ci, args);
  tmp = ra_releasetmp(as, ASMREF_TMP1);
  emit_loada(as, tmp, J2G(as->J));
  emit_loadi(as, ra_releasetmp(as, ASMREF_TMP2), (int32_t)as->gcsteps);
  /* Jump around GC step if GC total < GC threshold. */
  emit_sjcc(as, CC_B, l_end);
  emit_opgl(as, XO_ARITH(XOg_CMP), tmp, gc.threshold);
  emit_getgl(as, tmp, gc.total);
  as->gcsteps = 0;
  checkmclim(as);
}

/* -- PHI and loop handling ----------------------------------------------- */

/* Break a PHI cycle by renaming to a free register (evict if needed). */
static void asm_phi_break(ASMState *as, RegSet blocked, RegSet blockedby,
			  RegSet allow)
{
  RegSet candidates = blocked & allow;
  if (candidates) {  /* If this register file has candidates. */
    /* Note: the set for ra_pick cannot be empty, since each register file
    ** has some registers never allocated to PHIs.
    */
    Reg down, up = ra_pick(as, ~blocked & allow);  /* Get a free register. */
    if (candidates & ~blockedby)  /* Optimize shifts, else it's a cycle. */
      candidates = candidates & ~blockedby;
    down = rset_picktop(candidates);  /* Pick candidate PHI register. */
    ra_rename(as, down, up);  /* And rename it to the free register. */
  }
}

/* PHI register shuffling.
**
** The allocator tries hard to preserve PHI register assignments across
** the loop body. Most of the time this loop does nothing, since there
** are no register mismatches.
**
** If a register mismatch is detected and ...
** - the register is currently free: rename it.
** - the register is blocked by an invariant: restore/remat and rename it.
** - Otherwise the register is used by another PHI, so mark it as blocked.
**
** The renames are order-sensitive, so just retry the loop if a register
** is marked as blocked, but has been freed in the meantime. A cycle is
** detected if all of the blocked registers are allocated. To break the
** cycle rename one of them to a free register and retry.
**
** Note that PHI spill slots are kept in sync and don't need to be shuffled.
*/
static void asm_phi_shuffle(ASMState *as)
{
  RegSet work;

  /* Find and resolve PHI register mismatches. */
  for (;;) {
    RegSet blocked = RSET_EMPTY;
    RegSet blockedby = RSET_EMPTY;
    RegSet phiset = as->phiset;
    while (phiset) {  /* Check all left PHI operand registers. */
      Reg r = rset_picktop(phiset);
      IRIns *irl = IR(as->phireg[r]);
      Reg left = irl->r;
      if (r != left) {  /* Mismatch? */
	if (!rset_test(as->freeset, r)) {  /* PHI register blocked? */
	  IRRef ref = regcost_ref(as->cost[r]);
	  if (irt_ismarked(IR(ref)->t)) {  /* Blocked by other PHI (w/reg)? */
	    rset_set(blocked, r);
	    if (ra_hasreg(left))
	      rset_set(blockedby, left);
	    left = RID_NONE;
	  } else {  /* Otherwise grab register from invariant. */
	    ra_restore(as, ref);
	    checkmclim(as);
	  }
	}
	if (ra_hasreg(left)) {
	  ra_rename(as, left, r);
	  checkmclim(as);
	}
      }
      rset_clear(phiset, r);
    }
    if (!blocked) break;  /* Finished. */
    if (!(as->freeset & blocked)) {  /* Break cycles if none are free. */
      asm_phi_break(as, blocked, blockedby, RSET_GPR);
      asm_phi_break(as, blocked, blockedby, RSET_FPR);
      checkmclim(as);
    }  /* Else retry some more renames. */
  }

  /* Restore/remat invariants whose registers are modified inside the loop. */
  work = as->modset & ~(as->freeset | as->phiset);
  while (work) {
    Reg r = rset_picktop(work);
    ra_restore(as, regcost_ref(as->cost[r]));
    rset_clear(work, r);
    checkmclim(as);
  }

  /* Allocate and save all unsaved PHI regs and clear marks. */
  work = as->phiset;
  while (work) {
    Reg r = rset_picktop(work);
    IRRef lref = as->phireg[r];
    IRIns *ir = IR(lref);
    if (ra_hasspill(ir->s)) {  /* Left PHI gained a spill slot? */
      irt_clearmark(ir->t);  /* Handled here, so clear marker now. */
      ra_alloc1(as, lref, RID2RSET(r));
      ra_save(as, ir, r);  /* Save to spill slot inside the loop. */
      checkmclim(as);
    }
    rset_clear(work, r);
  }
}

/* Emit renames for left PHIs which are only spilled outside the loop. */
static void asm_phi_fixup(ASMState *as)
{
  RegSet work = as->phiset;
  while (work) {
    Reg r = rset_picktop(work);
    IRRef lref = as->phireg[r];
    IRIns *ir = IR(lref);
    /* Left PHI gained a spill slot before the loop? */
    if (irt_ismarked(ir->t) && ra_hasspill(ir->s)) {
      IRRef ren;
      lj_ir_set(as->J, IRT(IR_RENAME, IRT_NIL), lref, as->loopsnapno);
      ren = tref_ref(lj_ir_emit(as->J));
      as->ir = as->T->ir;  /* The IR may have been reallocated. */
      IR(ren)->r = (uint8_t)r;
      IR(ren)->s = SPS_NONE;
    }
    irt_clearmark(ir->t);  /* Always clear marker. */
    rset_clear(work, r);
  }
}

/* Setup right PHI reference. */
static void asm_phi(ASMState *as, IRIns *ir)
{
  RegSet allow = (irt_isfp(ir->t) ? RSET_FPR : RSET_GPR) & ~as->phiset;
  RegSet afree = (as->freeset & allow);
  IRIns *irl = IR(ir->op1);
  IRIns *irr = IR(ir->op2);
  /* Spill slot shuffling is not implemented yet (but rarely needed). */
  if (ra_hasspill(irl->s) || ra_hasspill(irr->s))
    lj_trace_err(as->J, LJ_TRERR_NYIPHI);
  /* Leave at least one register free for non-PHIs (and PHI cycle breaking). */
  if ((afree & (afree-1))) {  /* Two or more free registers? */
    Reg r;
    if (ra_noreg(irr->r)) {  /* Get a register for the right PHI. */
      r = ra_allocref(as, ir->op2, allow);
    } else {  /* Duplicate right PHI, need a copy (rare). */
      r = ra_scratch(as, allow);
      ra_movrr(as, irr, r, irr->r);
    }
    ir->r = (uint8_t)r;
    rset_set(as->phiset, r);
    as->phireg[r] = (IRRef1)ir->op1;
    irt_setmark(irl->t);  /* Marks left PHIs _with_ register. */
    if (ra_noreg(irl->r))
      ra_sethint(irl->r, r); /* Set register hint for left PHI. */
  } else {  /* Otherwise allocate a spill slot. */
    /* This is overly restrictive, but it triggers only on synthetic code. */
    if (ra_hasreg(irl->r) || ra_hasreg(irr->r))
      lj_trace_err(as->J, LJ_TRERR_NYIPHI);
    ra_spill(as, ir);
    irl->s = irr->s = ir->s;  /* Sync left/right PHI spill slots. */
  }
}

/* Fixup the loop branch. */
static void asm_loop_fixup(ASMState *as)
{
  MCode *p = as->mctop;
  MCode *target = as->mcp;
  if (as->realign) {  /* Realigned loops use short jumps. */
    as->realign = NULL;  /* Stop another retry. */
    lua_assert(((intptr_t)target & 15) == 0);
    if (as->loopinv) {  /* Inverted loop branch? */
      p -= 5;
      p[0] = XI_JMP;
      lua_assert(target - p >= -128);
      p[-1] = (MCode)(target - p);  /* Patch sjcc. */
      if (as->loopinv == 2)
	p[-3] = (MCode)(target - p + 2);  /* Patch opt. short jp. */
    } else {
      lua_assert(target - p >= -128);
      p[-1] = (MCode)(int8_t)(target - p);  /* Patch short jmp. */
      p[-2] = XI_JMPs;
    }
  } else {
    MCode *newloop;
    p[-5] = XI_JMP;
    if (as->loopinv) {  /* Inverted loop branch? */
      /* asm_guardcc already inverted the jcc and patched the jmp. */
      p -= 5;
      newloop = target+4;
      *(int32_t *)(p-4) = (int32_t)(target - p);  /* Patch jcc. */
      if (as->loopinv == 2) {
	*(int32_t *)(p-10) = (int32_t)(target - p + 6);  /* Patch opt. jp. */
	newloop = target+8;
      }
    } else {  /* Otherwise just patch jmp. */
      *(int32_t *)(p-4) = (int32_t)(target - p);
      newloop = target+3;
    }
    /* Realign small loops and shorten the loop branch. */
    if (newloop >= p - 128) {
      as->realign = newloop;  /* Force a retry and remember alignment. */
      as->curins = as->stopins;  /* Abort asm_trace now. */
      as->T->nins = as->orignins;  /* Remove any added renames. */
    }
  }
}

/* Middle part of a loop. */
static void asm_loop(ASMState *as)
{
  /* LOOP is a guard, so the snapno is up to date. */
  as->loopsnapno = as->snapno;
  if (as->gcsteps)
    asm_gc_check(as);
  /* LOOP marks the transition from the variant to the invariant part. */
  as->testmcp = as->invmcp = NULL;
  as->sectref = 0;
  if (!neverfuse(as)) as->fuseref = 0;
  asm_phi_shuffle(as);
  asm_loop_fixup(as);
  as->mcloop = as->mcp;
  RA_DBGX((as, "===== LOOP ====="));
  if (!as->realign) RA_DBG_FLUSH();
}

/* -- Head of trace ------------------------------------------------------- */

/* Calculate stack adjustment. */
static int32_t asm_stack_adjust(ASMState *as)
{
  if (as->evenspill <= SPS_FIXED)
    return 0;
  return sps_scale((as->evenspill - SPS_FIXED + 3) & ~3);
}

/* Coalesce BASE register for a root trace. */
static void asm_head_root_base(ASMState *as)
{
  IRIns *ir = IR(REF_BASE);
  Reg r = ir->r;
  if (ra_hasreg(r)) {
    ra_free(as, r);
    if (rset_test(as->modset, r))
      ir->r = RID_INIT;  /* No inheritance for modified BASE register. */
    if (r != RID_BASE)
      emit_rr(as, XO_MOV, r, RID_BASE);
  }
}

/* Head of a root trace. */
static void asm_head_root(ASMState *as)
{
  int32_t spadj;
  asm_head_root_base(as);
  emit_setgli(as, vmstate, (int32_t)as->T->traceno);
  spadj = asm_stack_adjust(as);
  as->T->spadjust = (uint16_t)spadj;
  emit_addptr(as, RID_ESP|REX_64, -spadj);
  /* Root traces assume a checked stack for the starting proto. */
  as->T->topslot = gcref(as->T->startpt)->pt.framesize;
}

/* Coalesce or reload BASE register for a side trace. */
static RegSet asm_head_side_base(ASMState *as, Reg pbase, RegSet allow)
{
  IRIns *ir = IR(REF_BASE);
  Reg r = ir->r;
  if (ra_hasreg(r)) {
    ra_free(as, r);
    if (rset_test(as->modset, r))
      ir->r = RID_INIT;  /* No inheritance for modified BASE register. */
    if (pbase == r) {
      rset_clear(allow, r);  /* Mark same BASE register as coalesced. */
    } else if (ra_hasreg(pbase) && rset_test(as->freeset, pbase)) {
      rset_clear(allow, pbase);
      emit_rr(as, XO_MOV, r, pbase);  /* Move from coalesced parent register. */
    } else {
      emit_getgl(as, r, jit_base);  /* Otherwise reload BASE. */
    }
  }
  return allow;
}

/* Head of a side trace.
**
** The current simplistic algorithm requires that all slots inherited
** from the parent are live in a register between pass 2 and pass 3. This
** avoids the complexity of stack slot shuffling. But of course this may
** overflow the register set in some cases and cause the dreaded error:
** "NYI: register coalescing too complex". A refined algorithm is needed.
*/
static void asm_head_side(ASMState *as)
{
  IRRef1 sloadins[RID_MAX];
  RegSet allow = RSET_ALL;  /* Inverse of all coalesced registers. */
  RegSet live = RSET_EMPTY;  /* Live parent registers. */
  Reg pbase = as->parent->ir[REF_BASE].r;  /* Parent base register (if any). */
  int32_t spadj, spdelta;
  int pass2 = 0;
  int pass3 = 0;
  IRRef i;

  allow = asm_head_side_base(as, pbase, allow);

  /* Scan all parent SLOADs and collect register dependencies. */
  for (i = as->stopins; i > REF_BASE; i--) {
    IRIns *ir = IR(i);
    RegSP rs;
    lua_assert(ir->o == IR_SLOAD && (ir->op2 & IRSLOAD_PARENT));
    rs = as->parentmap[ir->op1];
    if (ra_hasreg(ir->r)) {
      rset_clear(allow, ir->r);
      if (ra_hasspill(ir->s))
	ra_save(as, ir, ir->r);
    } else if (ra_hasspill(ir->s)) {
      irt_setmark(ir->t);
      pass2 = 1;
    }
    if (ir->r == rs) {  /* Coalesce matching registers right now. */
      ra_free(as, ir->r);
    } else if (ra_hasspill(regsp_spill(rs))) {
      if (ra_hasreg(ir->r))
	pass3 = 1;
    } else if (ra_used(ir)) {
      sloadins[rs] = (IRRef1)i;
      rset_set(live, rs);  /* Block live parent register. */
    }
  }

  /* Calculate stack frame adjustment. */
  spadj = asm_stack_adjust(as);
  spdelta = spadj - (int32_t)as->parent->spadjust;
  if (spdelta < 0) {  /* Don't shrink the stack frame. */
    spadj = (int32_t)as->parent->spadjust;
    spdelta = 0;
  }
  as->T->spadjust = (uint16_t)spadj;

  /* Reload spilled target registers. */
  if (pass2) {
    for (i = as->stopins; i > REF_BASE; i--) {
      IRIns *ir = IR(i);
      if (irt_ismarked(ir->t)) {
	RegSet mask;
	Reg r;
	RegSP rs;
	irt_clearmark(ir->t);
	rs = as->parentmap[ir->op1];
	if (!ra_hasspill(regsp_spill(rs)))
	  ra_sethint(ir->r, rs);  /* Hint may be gone, set it again. */
	else if (sps_scale(regsp_spill(rs))+spdelta == sps_scale(ir->s))
	  continue;  /* Same spill slot, do nothing. */
	mask = (irt_isnum(ir->t) ? RSET_FPR : RSET_GPR) & allow;
	if (mask == RSET_EMPTY)
	  lj_trace_err(as->J, LJ_TRERR_NYICOAL);
	r = ra_allocref(as, i, mask);
	ra_save(as, ir, r);
	rset_clear(allow, r);
	if (r == rs) {  /* Coalesce matching registers right now. */
	  ra_free(as, r);
	  rset_clear(live, r);
	} else if (ra_hasspill(regsp_spill(rs))) {
	  pass3 = 1;
	}
	checkmclim(as);
      }
    }
  }

  /* Store trace number and adjust stack frame relative to the parent. */
  emit_setgli(as, vmstate, (int32_t)as->T->traceno);
  emit_addptr(as, RID_ESP|REX_64, -spdelta);

  /* Restore target registers from parent spill slots. */
  if (pass3) {
    RegSet work = ~as->freeset & RSET_ALL;
    while (work) {
      Reg r = rset_pickbot(work);
      IRIns *ir = IR(regcost_ref(as->cost[r]));
      RegSP rs = as->parentmap[ir->op1];
      rset_clear(work, r);
      if (ra_hasspill(regsp_spill(rs))) {
	int32_t ofs = sps_scale(regsp_spill(rs));
	ra_free(as, r);
	if (r < RID_MAX_GPR)
	  emit_rmro(as, XO_MOV, REX_64IR(ir, r), RID_ESP, ofs);
	else
	  emit_rmro(as, XMM_MOVRM(as), r, RID_ESP, ofs);
	checkmclim(as);
      }
    }
  }

  /* Shuffle registers to match up target regs with parent regs. */
  for (;;) {
    RegSet work;

    /* Repeatedly coalesce free live registers by moving to their target. */
    while ((work = as->freeset & live) != RSET_EMPTY) {
      Reg rp = rset_pickbot(work);
      IRIns *ir = IR(sloadins[rp]);
      rset_clear(live, rp);
      rset_clear(allow, rp);
      ra_free(as, ir->r);
      ra_movrr(as, ir, ir->r, rp);
      checkmclim(as);
    }

    /* We're done if no live registers remain. */
    if (live == RSET_EMPTY)
      break;

    /* Break cycles by renaming one target to a temp. register. */
    if (live & RSET_GPR) {
      RegSet tmpset = as->freeset & ~live & allow & RSET_GPR;
      if (tmpset == RSET_EMPTY)
	lj_trace_err(as->J, LJ_TRERR_NYICOAL);
      ra_rename(as, rset_pickbot(live & RSET_GPR), rset_pickbot(tmpset));
    }
    if (live & RSET_FPR) {
      RegSet tmpset = as->freeset & ~live & allow & RSET_FPR;
      if (tmpset == RSET_EMPTY)
	lj_trace_err(as->J, LJ_TRERR_NYICOAL);
      ra_rename(as, rset_pickbot(live & RSET_FPR), rset_pickbot(tmpset));
    }
    checkmclim(as);
    /* Continue with coalescing to fix up the broken cycle(s). */
  }

  /* Inherit top stack slot already checked by parent trace. */
  as->T->topslot = as->parent->topslot;
  if (as->topslot > as->T->topslot) {  /* Need to check for higher slot? */
    as->T->topslot = (uint8_t)as->topslot;  /* Remember for child traces. */
    /* Reuse the parent exit in the context of the parent trace. */
    asm_stack_check(as, as->topslot, pbase, allow & RSET_GPR, as->J->exitno);
  }
}

/* -- Tail of trace ------------------------------------------------------- */

/* Link to another trace. */
static void asm_tail_link(ASMState *as)
{
  SnapNo snapno = as->T->nsnap-1;  /* Last snapshot. */
  SnapShot *snap = &as->T->snap[snapno];
  BCReg baseslot = asm_stack_extent(as, snap, &as->topslot);

  checkmclim(as);
  ra_allocref(as, REF_BASE, RID2RSET(RID_BASE));

  if (as->T->link == TRACE_INTERP) {
    /* Setup fixed registers for exit to interpreter. */
    const BCIns *pc = snap_pc(as->T->snapmap[snap->mapofs + snap->nent]);
    int32_t mres;
    if (bc_op(*pc) == BC_JLOOP) {  /* NYI: find a better way to do this. */
      BCIns *retpc = &traceref(as->J, bc_d(*pc))->startins;
      if (bc_isret(bc_op(*retpc)))
	pc = retpc;
    }
    emit_loada(as, RID_DISPATCH, J2GG(as->J)->dispatch);
    emit_loada(as, RID_PC, pc);
    mres = (int32_t)(snap->nslots - baseslot);
    switch (bc_op(*pc)) {
    case BC_CALLM: case BC_CALLMT:
      mres -= (int32_t)(1 + bc_a(*pc) + bc_c(*pc)); break;
    case BC_RETM: mres -= (int32_t)(bc_a(*pc) + bc_d(*pc)); break;
    case BC_TSETM: mres -= (int32_t)bc_a(*pc); break;
    default: if (bc_op(*pc) < BC_FUNCF) mres = 0; break;
    }
    emit_loadi(as, RID_RET, mres);  /* Return MULTRES or 0. */
  } else if (baseslot) {
    /* Save modified BASE for linking to trace with higher start frame. */
    emit_setgl(as, RID_BASE, jit_base);
  }
  emit_addptr(as, RID_BASE, 8*(int32_t)baseslot);

  /* Sync the interpreter state with the on-trace state. */
  asm_stack_restore(as, snap);

  /* Root traces that grow the stack need to check the stack at the end. */
  if (!as->parent && as->topslot)
    asm_stack_check(as, as->topslot, RID_BASE, as->freeset & RSET_GPR, snapno);
}

/* Fixup the tail code. */
static void asm_tail_fixup(ASMState *as, TraceNo lnk)
{
  /* Note: don't use as->mcp swap + emit_*: emit_op overwrites more bytes. */
  MCode *p = as->mctop;
  MCode *target, *q;
  int32_t spadj = as->T->spadjust;
  if (spadj == 0) {
    p -= ((as->flags & JIT_F_LEA_AGU) ? 7 : 6) + (LJ_64 ? 1 : 0);
  } else {
    MCode *p1;
    /* Patch stack adjustment. */
    if (checki8(spadj)) {
      p -= 3;
      p1 = p-6;
      *p1 = (MCode)spadj;
    } else {
      p1 = p-9;
      *(int32_t *)p1 = spadj;
    }
    if ((as->flags & JIT_F_LEA_AGU)) {
#if LJ_64
      p1[-4] = 0x48;
#endif
      p1[-3] = (MCode)XI_LEA;
      p1[-2] = MODRM(checki8(spadj) ? XM_OFS8 : XM_OFS32, RID_ESP, RID_ESP);
      p1[-1] = MODRM(XM_SCALE1, RID_ESP, RID_ESP);
    } else {
#if LJ_64
      p1[-3] = 0x48;
#endif
      p1[-2] = (MCode)(checki8(spadj) ? XI_ARITHi8 : XI_ARITHi);
      p1[-1] = MODRM(XM_REG, XOg_ADD, RID_ESP);
    }
  }
  /* Patch exit branch. */
  target = lnk == TRACE_INTERP ? (MCode *)lj_vm_exit_interp :
				 traceref(as->J, lnk)->mcode;
  *(int32_t *)(p-4) = jmprel(p, target);
  p[-5] = XI_JMP;
  /* Drop unused mcode tail. Fill with NOPs to make the prefetcher happy. */
  for (q = as->mctop-1; q >= p; q--)
    *q = XI_NOP;
  as->mctop = p;
}

/* -- Instruction dispatch ------------------------------------------------ */

/* Assemble a single instruction. */
static void asm_ir(ASMState *as, IRIns *ir)
{
  switch ((IROp)ir->o) {
  /* Miscellaneous ops. */
  case IR_LOOP: asm_loop(as); break;
  case IR_NOP: case IR_XBAR: lua_assert(!ra_used(ir)); break;
  case IR_USE:
    ra_alloc1(as, ir->op1, irt_isfp(ir->t) ? RSET_FPR : RSET_GPR); break;
  case IR_PHI: asm_phi(as, ir); break;
  case IR_HIOP: asm_hiop(as, ir); break;

  /* Guarded assertions. */
  case IR_LT: case IR_GE: case IR_LE: case IR_GT:
  case IR_ULT: case IR_UGE: case IR_ULE: case IR_UGT:
  case IR_EQ: case IR_NE: case IR_ABC:
    asm_comp(as, ir, asm_compmap[ir->o]);
    break;

  case IR_RETF: asm_retf(as, ir); break;

  /* Bit ops. */
  case IR_BNOT: asm_neg_not(as, ir, XOg_NOT); break;
  case IR_BSWAP: asm_bitswap(as, ir); break;

  case IR_BAND: asm_intarith(as, ir, XOg_AND); break;
  case IR_BOR:  asm_intarith(as, ir, XOg_OR); break;
  case IR_BXOR: asm_intarith(as, ir, XOg_XOR); break;

  case IR_BSHL: asm_bitshift(as, ir, XOg_SHL); break;
  case IR_BSHR: asm_bitshift(as, ir, XOg_SHR); break;
  case IR_BSAR: asm_bitshift(as, ir, XOg_SAR); break;
  case IR_BROL: asm_bitshift(as, ir, XOg_ROL); break;
  case IR_BROR: asm_bitshift(as, ir, XOg_ROR); break;

  /* Arithmetic ops. */
  case IR_ADD: asm_add(as, ir); break;
  case IR_SUB:
    if (irt_isnum(ir->t))
      asm_fparith(as, ir, XO_SUBSD);
    else  /* Note: no need for LEA trick here. i-k is encoded as i+(-k). */
      asm_intarith(as, ir, XOg_SUB);
    break;
  case IR_MUL:
    if (irt_isnum(ir->t))
      asm_fparith(as, ir, XO_MULSD);
    else
      asm_intarith(as, ir, XOg_X_IMUL);
    break;
  case IR_DIV:
#if LJ_64 && LJ_HASFFI
    if (!irt_isnum(ir->t))
      asm_arith64(as, ir, irt_isi64(ir->t) ? IRCALL_lj_carith_divi64 :
					     IRCALL_lj_carith_divu64);
    else
#endif
      asm_fparith(as, ir, XO_DIVSD);
    break;
  case IR_MOD:
#if LJ_64 && LJ_HASFFI
    asm_arith64(as, ir, irt_isi64(ir->t) ? IRCALL_lj_carith_modi64 :
					   IRCALL_lj_carith_modu64);
#else
    lua_assert(0);
#endif
    break;

  case IR_NEG:
    if (irt_isnum(ir->t))
      asm_fparith(as, ir, XO_XORPS);
    else
      asm_neg_not(as, ir, XOg_NEG);
    break;
  case IR_ABS: asm_fparith(as, ir, XO_ANDPS); break;

  case IR_MIN:
    if (irt_isnum(ir->t))
      asm_fparith(as, ir, XO_MINSD);
    else
      asm_min_max(as, ir, CC_G);
    break;
  case IR_MAX:
    if (irt_isnum(ir->t))
      asm_fparith(as, ir, XO_MAXSD);
    else
      asm_min_max(as, ir, CC_L);
    break;

  case IR_FPMATH: case IR_ATAN2: case IR_LDEXP:
    asm_fpmath(as, ir);
    break;
  case IR_POW:
#if LJ_64 && LJ_HASFFI
    if (!irt_isnum(ir->t))
      asm_arith64(as, ir, irt_isi64(ir->t) ? IRCALL_lj_carith_powi64 :
					     IRCALL_lj_carith_powu64);
    else
#endif
      asm_fppowi(as, ir);
    break;

  /* Overflow-checking arithmetic ops. Note: don't use LEA here! */
  case IR_ADDOV: asm_intarith(as, ir, XOg_ADD); break;
  case IR_SUBOV: asm_intarith(as, ir, XOg_SUB); break;
  case IR_MULOV: asm_intarith(as, ir, XOg_X_IMUL); break;

  /* Memory references. */
  case IR_AREF: asm_aref(as, ir); break;
  case IR_HREF: asm_href(as, ir); break;
  case IR_HREFK: asm_hrefk(as, ir); break;
  case IR_NEWREF: asm_newref(as, ir); break;
  case IR_UREFO: case IR_UREFC: asm_uref(as, ir); break;
  case IR_FREF: asm_fref(as, ir); break;
  case IR_STRREF: asm_strref(as, ir); break;

  /* Loads and stores. */
  case IR_ALOAD: case IR_HLOAD: case IR_ULOAD: case IR_VLOAD:
    asm_ahuvload(as, ir);
    break;
  case IR_FLOAD: case IR_XLOAD: asm_fxload(as, ir); break;
  case IR_SLOAD: asm_sload(as, ir); break;

  case IR_ASTORE: case IR_HSTORE: case IR_USTORE: asm_ahustore(as, ir); break;
  case IR_FSTORE: case IR_XSTORE: asm_fxstore(as, ir); break;

  /* Allocations. */
  case IR_SNEW: case IR_XSNEW: asm_snew(as, ir); break;
  case IR_TNEW: asm_tnew(as, ir); break;
  case IR_TDUP: asm_tdup(as, ir); break;
  case IR_CNEW: case IR_CNEWI: asm_cnew(as, ir); break;

  /* Write barriers. */
  case IR_TBAR: asm_tbar(as, ir); break;
  case IR_OBAR: asm_obar(as, ir); break;

  /* Type conversions. */
  case IR_TOBIT: asm_tobit(as, ir); break;
  case IR_CONV: asm_conv(as, ir); break;
  case IR_TOSTR: asm_tostr(as, ir); break;
  case IR_STRTO: asm_strto(as, ir); break;

  /* Calls. */
  case IR_CALLN: case IR_CALLL: case IR_CALLS: asm_call(as, ir); break;
  case IR_CALLXS: asm_callx(as, ir); break;
  case IR_CARG: break;

  default:
    setintV(&as->J->errinfo, ir->o);
    lj_trace_err_info(as->J, LJ_TRERR_NYIIR);
    break;
  }
}

/* Assemble a trace in linear backwards order. */
static void asm_trace(ASMState *as)
{
  for (as->curins--; as->curins > as->stopins; as->curins--) {
    IRIns *ir = IR(as->curins);
    lua_assert(!(LJ_32 && irt_isint64(ir->t)));  /* Handled by SPLIT. */
    if (!ra_used(ir) && !ir_sideeff(ir) && (as->flags & JIT_F_OPT_DCE))
      continue;  /* Dead-code elimination can be soooo easy. */
    if (irt_isguard(ir->t))
      asm_snap_prep(as);
    RA_DBG_REF();
    checkmclim(as);
    asm_ir(as, ir);
  }
}

/* -- Trace setup --------------------------------------------------------- */

/* Ensure there are enough stack slots for call arguments. */
static Reg asm_setup_call_slots(ASMState *as, IRIns *ir, const CCallInfo *ci)
{
  IRRef args[CCI_NARGS_MAX];
  uint32_t nargs = (int)CCI_NARGS(ci);
  int nslots = 0;
  asm_collectargs(as, ir, ci, args);
#if LJ_64
  if (LJ_ABI_WIN) {
    nslots = (int)(nargs*2);  /* Only matters for more than four args. */
  } else {
    uint32_t i;
    int ngpr = 6, nfpr = 8;
    for (i = 0; i < nargs; i++)
      if (irt_isfp(IR(args[i])->t)) {
	if (nfpr > 0) nfpr--; else nslots += 2;
      } else {
	if (ngpr > 0) ngpr--; else nslots += 2;
      }
  }
  if (nslots > as->evenspill)  /* Leave room for args in stack slots. */
    as->evenspill = nslots;
  return irt_isfp(ir->t) ? REGSP_HINT(RID_FPRET) : REGSP_HINT(RID_RET);
#else
  if ((ci->flags & CCI_FASTCALL)) {
    lua_assert(nargs <= 2);
  } else {
    uint32_t i;
    for (i = 0; i < nargs; i++)
      nslots += irt_isnum(IR(args[i])->t) ? 2 : 1;
    if (nslots > as->evenspill)  /* Leave room for args. */
      as->evenspill = nslots;
  }
  return irt_isfp(ir->t) ? REGSP_INIT : REGSP_HINT(RID_RET);
#endif
}

/* Clear reg/sp for all instructions and add register hints. */
static void asm_setup_regsp(ASMState *as, GCtrace *T)
{
  IRRef i, nins;
  int inloop;

  ra_setup(as);

  /* Clear reg/sp for constants. */
  for (i = T->nk; i < REF_BIAS; i++)
    IR(i)->prev = REGSP_INIT;

  /* REF_BASE is used for implicit references to the BASE register. */
  IR(REF_BASE)->prev = REGSP_HINT(RID_BASE);

  nins = T->nins;
  if (IR(nins-1)->o == IR_RENAME) {
    do { nins--; } while (IR(nins-1)->o == IR_RENAME);
    T->nins = nins;  /* Remove any renames left over from ASM restart. */
  }
  as->snaprename = nins;
  as->snapref = nins;
  as->snapno = T->nsnap;

  as->stopins = REF_BASE;
  as->orignins = nins;
  as->curins = nins;

  inloop = 0;
  as->evenspill = SPS_FIRST;
  for (i = REF_FIRST; i < nins; i++) {
    IRIns *ir = IR(i);
    switch (ir->o) {
    case IR_LOOP:
      inloop = 1;
      break;
    /* Set hints for slot loads from a parent trace. */
    case IR_SLOAD:
      if ((ir->op2 & IRSLOAD_PARENT)) {
	RegSP rs = as->parentmap[ir->op1];
	lua_assert(regsp_used(rs));
	as->stopins = i;
	if (!ra_hasspill(regsp_spill(rs)) && ra_hasreg(regsp_reg(rs))) {
	  ir->prev = (uint16_t)REGSP_HINT(regsp_reg(rs));
	  continue;
	}
      }
      break;
    case IR_CALLXS: {
      CCallInfo ci;
      ci.flags = asm_callx_flags(as, ir);
      ir->prev = asm_setup_call_slots(as, ir, &ci);
      if (inloop)
	as->modset |= RSET_SCRATCH;
      continue;
      }
    case IR_CALLN: case IR_CALLL: case IR_CALLS: {
      const CCallInfo *ci = &lj_ir_callinfo[ir->op2];
      ir->prev = asm_setup_call_slots(as, ir, ci);
      if (inloop)
	as->modset |= (ci->flags & CCI_NOFPRCLOBBER) ?
		      (RSET_SCRATCH & ~RSET_FPR) : RSET_SCRATCH;
      continue;
      }
#if LJ_32 && LJ_HASFFI
    case IR_HIOP:
      if ((ir-1)->o == IR_CALLN) {
	ir->prev = REGSP_HINT(RID_RETHI);
	continue;
      }
      break;
#endif
    /* C calls evict all scratch regs and return results in RID_RET. */
    case IR_SNEW: case IR_XSNEW: case IR_NEWREF:
#if !LJ_64
      if (as->evenspill < 3)  /* lj_str_new and lj_tab_newkey need 3 args. */
	as->evenspill = 3;
#endif
    case IR_TNEW: case IR_TDUP: case IR_CNEW: case IR_CNEWI: case IR_TOSTR:
      ir->prev = REGSP_HINT(RID_RET);
      if (inloop)
	as->modset = RSET_SCRATCH;
      continue;
    case IR_STRTO: case IR_OBAR:
      if (inloop)
	as->modset = RSET_SCRATCH;
      break;
    case IR_POW:
      if (irt_isnum(ir->t)) {
	ir->prev = REGSP_HINT(RID_XMM0);
	if (inloop)
	  as->modset |= RSET_RANGE(RID_XMM0, RID_XMM1+1)|RID2RSET(RID_EAX);
	continue;
      }
      /* fallthrough */
    case IR_DIV: case IR_MOD:
#if LJ_64 && LJ_HASFFI
      if (!irt_isnum(ir->t)) {
	ir->prev = REGSP_HINT(RID_RET);
	if (inloop)
	  as->modset |= (RSET_SCRATCH & RSET_GPR);
	continue;
      }
#endif
      break;
    case IR_FPMATH:
      if (ir->op2 == IRFPM_EXP2) {  /* May be joined to lj_vm_pow_sse. */
	ir->prev = REGSP_HINT(RID_XMM0);
#if !LJ_64
	if (as->evenspill < 4)  /* Leave room for 16 byte scratch area. */
	  as->evenspill = 4;
#endif
	if (inloop)
	  as->modset |= RSET_RANGE(RID_XMM0, RID_XMM2+1)|RID2RSET(RID_EAX);
	continue;
      } else if (ir->op2 <= IRFPM_TRUNC && !(as->flags & JIT_F_SSE4_1)) {
	ir->prev = REGSP_HINT(RID_XMM0);
	if (inloop)
	  as->modset |= RSET_RANGE(RID_XMM0, RID_XMM3+1)|RID2RSET(RID_EAX);
	continue;
      }
      break;
    /* Non-constant shift counts need to be in RID_ECX. */
    case IR_BSHL: case IR_BSHR: case IR_BSAR: case IR_BROL: case IR_BROR:
      if (!irref_isk(ir->op2) && !ra_hashint(IR(ir->op2)->r)) {
	IR(ir->op2)->r = REGSP_HINT(RID_ECX);
	if (inloop)
	  rset_set(as->modset, RID_ECX);
      }
      break;
    /* Do not propagate hints across type conversions. */
    case IR_CONV: case IR_TOBIT:
      break;
    default:
      /* Propagate hints across likely 'op reg, imm' or 'op reg'. */
      if (irref_isk(ir->op2) && !irref_isk(ir->op1)) {
	ir->prev = IR(ir->op1)->prev;
	continue;
      }
      break;
    }
    ir->prev = REGSP_INIT;
  }
  if ((as->evenspill & 1))
    as->oddspill = as->evenspill++;
  else
    as->oddspill = 0;
}

/* -- Assembler core ------------------------------------------------------ */

/* Define this if you want to run LuaJIT with Valgrind. */
#ifdef LUAJIT_USE_VALGRIND
#include <valgrind/valgrind.h>
#define VG_INVALIDATE(p, sz)	VALGRIND_DISCARD_TRANSLATIONS(p, sz)
#else
#define VG_INVALIDATE(p, sz)	((void)0)
#endif

/* Assemble a trace. */
void lj_asm_trace(jit_State *J, GCtrace *T)
{
  ASMState as_;
  ASMState *as = &as_;

  /* Setup initial state. Copy some fields to reduce indirections. */
  as->J = J;
  as->T = T;
  as->ir = T->ir;
  as->flags = J->flags;
  as->loopref = J->loopref;
  as->realign = NULL;
  as->loopinv = 0;
  if (J->parent) {
    as->parent = traceref(J, J->parent);
    lj_snap_regspmap(as->parentmap, as->parent, J->exitno);
  } else {
    as->parent = NULL;
  }
  as->mctop = lj_mcode_reserve(J, &as->mcbot);  /* Reserve MCode memory. */
  as->mcp = as->mctop;
  as->mclim = as->mcbot + MCLIM_REDZONE;
  asm_exitstub_setup(as, T->nsnap);

  do {
    as->mcp = as->mctop;
    as->curins = T->nins;
    RA_DBG_START();
    RA_DBGX((as, "===== STOP ====="));
    /* Realign and leave room for backwards loop branch or exit branch. */
    if (as->realign) {
      int i = ((int)(intptr_t)as->realign) & 15;
      MCode *p = as->mctop;
      /* Fill unused mcode tail with NOPs to make the prefetcher happy. */
      while (i-- > 0)
	*--p = XI_NOP;
      as->mctop = p;
      as->mcp = p - (as->loopinv ? 5 : 2);  /* Space for short/near jmp. */
    } else {
      as->mcp = as->mctop - 5;  /* Space for exit branch (near jmp). */
    }
    as->invmcp = as->mcp;
    as->mcloop = NULL;
    as->testmcp = NULL;
    as->topslot = 0;
    as->gcsteps = 0;
    as->sectref = as->loopref;
    as->fuseref = (as->flags & JIT_F_OPT_FUSE) ? as->loopref : FUSE_DISABLED;

    /* Setup register allocation. */
    asm_setup_regsp(as, T);

    if (!as->loopref) {
      /* Leave room for ESP adjustment: add esp, imm or lea esp, [esp+imm] */
      as->mcp -= ((as->flags & JIT_F_LEA_AGU) ? 7 : 6)  + (LJ_64 ? 1 : 0);
      as->invmcp = NULL;
      asm_tail_link(as);
    }
    asm_trace(as);
  } while (as->realign);  /* Retry in case the MCode needs to be realigned. */

  RA_DBG_REF();
  checkmclim(as);
  if (as->gcsteps) {
    as->curins = as->T->snap[0].ref;
    asm_snap_prep(as);  /* The GC check is a guard. */
    asm_gc_check(as);
  }
  ra_evictk(as);
  if (as->parent)
    asm_head_side(as);
  else
    asm_head_root(as);
  asm_phi_fixup(as);

  RA_DBGX((as, "===== START ===="));
  RA_DBG_FLUSH();
  if (as->freeset != RSET_ALL)
    lj_trace_err(as->J, LJ_TRERR_BADRA);  /* Ouch! Should never happen. */

  /* Set trace entry point before fixing up tail to allow link to self. */
  T->mcode = as->mcp;
  T->mcloop = as->mcloop ? (MSize)(as->mcloop - as->mcp) : 0;
  if (!as->loopref)
    asm_tail_fixup(as, T->link);  /* Note: this may change as->mctop! */
  T->szmcode = (MSize)(as->mctop - as->mcp);
  VG_INVALIDATE(T->mcode, T->szmcode);
}

/* Patch exit jumps of existing machine code to a new target. */
void lj_asm_patchexit(jit_State *J, GCtrace *T, ExitNo exitno, MCode *target)
{
  MCode *p = T->mcode;
  MCode *mcarea = lj_mcode_patch(J, p, 0);
  MSize len = T->szmcode;
  MCode *px = exitstub_addr(J, exitno) - 6;
  MCode *pe = p+len-6;
  uint32_t stateaddr = u32ptr(&J2G(J)->vmstate);
  if (len > 5 && p[len-5] == XI_JMP && p+len-6 + *(int32_t *)(p+len-4) == px)
    *(int32_t *)(p+len-4) = jmprel(p+len, target);
  /* Do not patch parent exit for a stack check. Skip beyond vmstate update. */
  for (; p < pe; p++)
    if (*(uint32_t *)(p+(LJ_64 ? 3 : 2)) == stateaddr && p[0] == XI_MOVmi) {
      p += LJ_64 ? 11 : 10;
      break;
    }
  lua_assert(p < pe);
  for (; p < pe; p++) {
    if ((*(uint16_t *)p & 0xf0ff) == 0x800f && p + *(int32_t *)(p+2) == px) {
      *(int32_t *)(p+2) = jmprel(p+6, target);
      p += 5;
    }
  }
  lj_mcode_patch(J, mcarea, 1);
  VG_INVALIDATE(T->mcode, T->szmcode);
}

#undef IR

#endif
