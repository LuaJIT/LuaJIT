/*
** SPLIT: Split 64 bit IR instructions into 32 bit IR instructions.
** Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
*/

#define lj_opt_split_c
#define LUA_CORE

#include "lj_obj.h"

#if LJ_HASJIT && LJ_HASFFI && LJ_32

#include "lj_err.h"
#include "lj_str.h"
#include "lj_ir.h"
#include "lj_jit.h"
#include "lj_iropt.h"
#include "lj_vm.h"

/* SPLIT pass:
**
** This pass splits up 64 bit IR instructions into multiple 32 bit IR
** instructions. It's only active for 32 bit CPUs which lack native 64 bit
** operations. The FFI is currently the only emitter for 64 bit
** instructions, so this pass is disabled if the FFI is disabled.
**
** Splitting the IR in a separate pass keeps each 32 bit IR assembler
** backend simple. Only a small amount of extra functionality needs to be
** implemented. This is much easier than adding support for allocating
** register pairs to each backend (believe me, I tried). A few simple, but
** important optimizations can be performed by the SPLIT pass, which would
** be tedious to do in the backend.
**
** The basic idea is to replace each 64 bit IR instruction with its 32 bit
** equivalent plus an extra HIOP instruction. The splitted IR is not passed
** through FOLD or any other optimizations, so each HIOP is guaranteed to
** immediately follow it's counterpart. The actual functionality of HIOP is
** inferred from the previous instruction.
**
** The operands of HIOP hold the hiword input references. The output of HIOP
** is the hiword output reference, which is also used to hold the hiword
** register or spill slot information. The register allocator treats this
** instruction independent of any other instruction, which improves code
** quality compared to using fixed register pairs.
**
** It's easier to split up some instructions into two regular 32 bit
** instructions. E.g. XLOAD is split up into two XLOADs with two different
** addresses. Obviously 64 bit constants need to be split up into two 32 bit
** constants, too. Some hiword instructions can be entirely omitted, e.g.
** when zero-extending a 32 bit value to 64 bits.
**
** Here's the IR and x64 machine code for 'x.b = x.a + 1' for a struct with
** two int64_t fields:
**
** 0100    p32 ADD    base  +8
** 0101    i64 XLOAD  0100
** 0102    i64 ADD    0101  +1
** 0103    p32 ADD    base  +16
** 0104    i64 XSTORE 0103  0102
**
**         mov rax, [esi+0x8]
**         add rax, +0x01
**         mov [esi+0x10], rax
**
** Here's the transformed IR and the x86 machine code after the SPLIT pass:
**
** 0100    p32 ADD    base  +8
** 0101    int XLOAD  0100
** 0102    p32 ADD    base  +12
** 0103    int XLOAD  0102
** 0104    int ADD    0101  +1
** 0105    int HIOP   0103  +0
** 0106    p32 ADD    base  +16
** 0107    int XSTORE 0106  0104
** 0108    p32 ADD    base  +20
** 0109    int XSTORE 0108  0105
**
**         mov eax, [esi+0x8]
**         mov ecx, [esi+0xc]
**         add eax, +0x01
**         adc ecx, +0x00
**         mov [esi+0x10], eax
**         mov [esi+0x14], ecx
**
** You may notice the reassociated hiword address computation, which is
** later fused into the mov operands by the assembler.
*/

/* Some local macros to save typing. Undef'd at the end. */
#define IR(ref)		(&J->cur.ir[(ref)])

/* Directly emit the transformed IR without updating chains etc. */
static IRRef split_emit(jit_State *J, uint16_t ot, IRRef1 op1, IRRef1 op2)
{
  IRRef nref = lj_ir_nextins(J);
  IRIns *ir = IR(nref);
  ir->ot = ot;
  ir->op1 = op1;
  ir->op2 = op2;
  return nref;
}

/* Emit a CALLN with two split 64 bit arguments. */
static IRRef split_call64(jit_State *J, IRRef1 *hisubst, IRIns *oir,
			  IRIns *ir, IRCallID id)
{
  IRRef tmp, op1 = ir->op1, op2 = ir->op2;
  J->cur.nins--;
#if LJ_LE
  tmp = split_emit(J, IRT(IR_CARG, IRT_NIL), oir[op1].prev, hisubst[op1]);
  tmp = split_emit(J, IRT(IR_CARG, IRT_NIL), tmp, oir[op2].prev);
  tmp = split_emit(J, IRT(IR_CARG, IRT_NIL), tmp, hisubst[op2]);
#else
  tmp = split_emit(J, IRT(IR_CARG, IRT_NIL), hisubst[op1], oir[op1].prev);
  tmp = split_emit(J, IRT(IR_CARG, IRT_NIL), tmp, hisubst[op2]);
  tmp = split_emit(J, IRT(IR_CARG, IRT_NIL), tmp, oir[op2].prev);
#endif
  ir->prev = tmp = split_emit(J, IRTI(IR_CALLN), tmp, id);
  return split_emit(J, IRTI(IR_HIOP), tmp, tmp);
}

/* Get a pointer to the other 32 bit word (LE: hiword, BE: loword). */
static IRRef split_ptr(jit_State *J, IRRef ref)
{
  IRIns *ir = IR(ref);
  int32_t ofs = 4;
  if (ir->o == IR_ADD && irref_isk(ir->op2)) {  /* Reassociate address. */
    ofs += IR(ir->op2)->i;
    ref = ir->op1;
    if (ofs == 0) return ref;
  }
  return split_emit(J, IRTI(IR_ADD), ref, lj_ir_kint(J, ofs));
}

/* Transform the old IR to the new IR. */
static void split_ir(jit_State *J)
{
  IRRef nins = J->cur.nins, nk = J->cur.nk;
  MSize irlen = nins - nk;
  MSize need = (irlen+1)*(sizeof(IRIns) + sizeof(IRRef1));
  IRIns *oir = (IRIns *)lj_str_needbuf(J->L, &G(J->L)->tmpbuf, need);
  IRRef1 *hisubst;
  IRRef ref;

  /* Copy old IR to buffer. */
  memcpy(oir, IR(nk), irlen*sizeof(IRIns));
  /* Bias hiword substitution table and old IR. Loword kept in field prev. */
  hisubst = (IRRef1 *)&oir[irlen] - nk;
  oir -= nk;

  /* Remove all IR instructions, but retain IR constants. */
  J->cur.nins = REF_FIRST;

  /* Process constants and fixed references. */
  for (ref = nk; ref <= REF_BASE; ref++) {
    IRIns *ir = &oir[ref];
    if (ir->o == IR_KINT64) {  /* Split up 64 bit constant. */
      TValue tv = *ir_k64(ir);
      ir->prev = lj_ir_kint(J, (int32_t)tv.u32.lo);
      hisubst[ref] = lj_ir_kint(J, (int32_t)tv.u32.hi);
    } else {
      ir->prev = ref;  /* Identity substitution for loword. */
      hisubst[ref] = 0;
    }
  }

  /* Process old IR instructions. */
  for (ref = REF_FIRST; ref < nins; ref++) {
    IRIns *ir = &oir[ref];
    IRRef nref = lj_ir_nextins(J);
    IRIns *nir = IR(nref);
    IRRef hi = 0;

    /* Copy-substitute old instruction to new instruction. */
    nir->op1 = ir->op1 < nk ? ir->op1 : oir[ir->op1].prev;
    nir->op2 = ir->op2 < nk ? ir->op2 : oir[ir->op2].prev;
    ir->prev = nref;  /* Loword substitution. */
    nir->o = ir->o;
    nir->t.irt = ir->t.irt & ~(IRT_MARK|IRT_ISPHI);
    hisubst[ref] = 0;

    /* Split 64 bit instructions. */
    if (irt_isint64(ir->t)) {
      IRRef hiref = hisubst[ir->op1];
      nir->t.irt = IRT_INT | (nir->t.irt & IRT_GUARD);  /* Turn into INT op. */
      switch (ir->o) {
      case IR_ADD:
      case IR_SUB:
	/* Use plain op for hiword if loword cannot produce a carry/borrow. */
	if (irref_isk(nir->op2) && IR(nir->op2)->i == 0) {
	  ir->prev = nir->op1;  /* Pass through loword. */
	  nir->op1 = hiref; nir->op2 = hisubst[ir->op2];
	  hi = nref;
	  break;
	}
	/* fallthrough */
      case IR_NEG:
	hi = split_emit(J, IRTI(IR_HIOP), hiref, hisubst[ir->op2]);
	break;
      case IR_MUL:
	hi = split_call64(J, hisubst, oir, ir, IRCALL_lj_carith_mul64);
	break;
      case IR_DIV:
	hi = split_call64(J, hisubst, oir, ir,
			  irt_isi64(ir->t) ? IRCALL_lj_carith_divi64 :
					     IRCALL_lj_carith_divu64);
	break;
      case IR_MOD:
	hi = split_call64(J, hisubst, oir, ir,
			  irt_isi64(ir->t) ? IRCALL_lj_carith_modi64 :
					     IRCALL_lj_carith_modu64);
	break;
      case IR_POW:
	hi = split_call64(J, hisubst, oir, ir,
			  irt_isi64(ir->t) ? IRCALL_lj_carith_powi64 :
					     IRCALL_lj_carith_powu64);
	break;
      case IR_FLOAD:
	lua_assert(ir->op2 == IRFL_CDATA_INT64);
	hi = split_emit(J, IRTI(IR_FLOAD), nir->op1, IRFL_CDATA_INT64HI);
#if LJ_BE
	ir->prev = hi; hi = nref;
#endif
	break;
      case IR_XLOAD:
	hi = split_emit(J, IRTI(IR_XLOAD), split_ptr(J, nir->op1), ir->op2);
#if LJ_BE
	ir->prev = hi; hi = nref;
#endif
	break;
      case IR_XSTORE:
#if LJ_LE
	hiref = hisubst[ir->op2];
#else
	hiref = nir->op2; nir->op2 = hisubst[ir->op2];
#endif
	split_emit(J, IRTI(IR_XSTORE), split_ptr(J, nir->op1), hiref);
	break;
      case IR_CONV: {  /* Conversion to 64 bit integer. Others handled below. */
	IRType st = (IRType)(ir->op2 & IRCONV_SRCMASK);
	if (st == IRT_NUM || st == IRT_FLOAT) {  /* FP to 64 bit int conv. */
	  hi = split_emit(J, IRTI(IR_HIOP), nir->op1, nref);
	} else if (st == IRT_I64 || st == IRT_U64) {  /* 64/64 bit cast. */
	  /* Drop cast, since assembler doesn't care. */
	  goto fwdlo;
	} else if ((ir->op2 & IRCONV_SEXT)) {  /* Sign-extend to 64 bit. */
	  IRRef k31 = lj_ir_kint(J, 31);
	  nir = IR(nref);  /* May have been reallocated. */
	  ir->prev = nir->op1;  /* Pass through loword. */
	  nir->o = IR_BSAR;  /* hi = bsar(lo, 31). */
	  nir->op2 = k31;
	  hi = nref;
	} else {  /* Zero-extend to 64 bit. */
	  hi = lj_ir_kint(J, 0);
	  goto fwdlo;
	}
	break;
	}
      case IR_CALLXS:
	goto split_call;
      case IR_PHI: {
	IRRef hiref2;
	if ((irref_isk(nir->op1) && irref_isk(nir->op2)) ||
	    nir->op1 == nir->op2)
	  J->cur.nins--;  /* Drop useless PHIs. */
	hiref2 = hisubst[ir->op2];
	if (!((irref_isk(hiref) && irref_isk(hiref2)) || hiref == hiref2))
	  split_emit(J, IRTI(IR_PHI), hiref, hiref2);
	break;
	}
      default:
	lua_assert(ir->o <= IR_NE);  /* Comparisons. */
	split_emit(J, IRTGI(IR_HIOP), hiref, hisubst[ir->op2]);
	break;
      }
    } else if (ir->o == IR_CONV) {  /* See above, too. */
      IRType st = (IRType)(ir->op2 & IRCONV_SRCMASK);
      if (st == IRT_I64 || st == IRT_U64) {  /* Conversion from 64 bit int. */
	if (irt_isfp(ir->t)) {  /* 64 bit integer to FP conversion. */
	  ir->prev = split_emit(J, IRT(IR_HIOP, irt_type(ir->t)),
				hisubst[ir->op1], nref);
	} else {  /* Truncate to lower 32 bits. */
	fwdlo:
	  ir->prev = nir->op1;  /* Forward loword. */
	  /* Replace with NOP to avoid messing up the snapshot logic. */
	  nir->ot = IRT(IR_NOP, IRT_NIL);
	  nir->op1 = nir->op2 = 0;
	}
      }
    } else if (ir->o == IR_CALLXS) {
      IRRef hiref;
    split_call:
      hiref = hisubst[ir->op1];
      if (hiref) {
	IROpT ot = nir->ot;
	IRRef op2 = nir->op2;
	nir->ot = IRT(IR_CARG, IRT_NIL);
#if LJ_LE
	nir->op2 = hiref;
#else
	nir->op2 = nir->op1; nir->op1 = hiref;
#endif
	ir->prev = nref = split_emit(J, ot, nref, op2);
      }
      if (irt_isint64(ir->t))
	hi = split_emit(J, IRTI(IR_HIOP), nref, nref);
    } else if (ir->o == IR_CARG) {
      IRRef hiref = hisubst[ir->op1];
      if (hiref) {
	IRRef op2 = nir->op2;
#if LJ_LE
	nir->op2 = hiref;
#else
	nir->op2 = nir->op1; nir->op1 = hiref;
#endif
	ir->prev = nref = split_emit(J, IRT(IR_CARG, IRT_NIL), nref, op2);
	nir = IR(nref);
      }
      hiref = hisubst[ir->op2];
      if (hiref) {
#if LJ_BE
	IRRef tmp = nir->op2; nir->op2 = hiref; hiref = tmp;
#endif
	ir->prev = split_emit(J, IRT(IR_CARG, IRT_NIL), nref, hiref);
      }
    } else if (ir->o == IR_CNEWI) {
      if (hisubst[ir->op2])
	split_emit(J, IRT(IR_HIOP, IRT_NIL), nref, hisubst[ir->op2]);
    } else if (ir->o == IR_LOOP) {
      J->loopref = nref;  /* Needed by assembler. */
    }
    hisubst[ref] = hi;  /* Store hiword substitution. */
  }

  /* Add PHI marks. */
  for (ref = J->cur.nins-1; ref >= REF_FIRST; ref--) {
    IRIns *ir = IR(ref);
    if (ir->o != IR_PHI) break;
    if (!irref_isk(ir->op1)) irt_setphi(IR(ir->op1)->t);
    if (ir->op2 > J->loopref) irt_setphi(IR(ir->op2)->t);
  }

  /* Substitute snapshot maps. */
  oir[nins].prev = J->cur.nins;  /* Substitution for last snapshot. */
  {
    SnapNo i, nsnap = J->cur.nsnap;
    for (i = 0; i < nsnap; i++) {
      SnapShot *snap = &J->cur.snap[i];
      SnapEntry *map = &J->cur.snapmap[snap->mapofs];
      MSize n, nent = snap->nent;
      snap->ref = oir[snap->ref].prev;
      for (n = 0; n < nent; n++) {
	SnapEntry sn = map[n];
	map[n] = ((sn & 0xffff0000) | oir[snap_ref(sn)].prev);
      }
    }
  }
}

/* Protected callback for split pass. */
static TValue *cpsplit(lua_State *L, lua_CFunction dummy, void *ud)
{
  jit_State *J = (jit_State *)ud;
  split_ir(J);
  UNUSED(L); UNUSED(dummy);
  return NULL;
}

#ifdef LUA_USE_ASSERT
/* Slow, but sure way to check whether a SPLIT pass is needed. */
static int split_needsplit(jit_State *J)
{
  IRIns *ir, *irend;
  IRRef ref;
  for (ir = IR(REF_FIRST), irend = IR(J->cur.nins); ir < irend; ir++)
    if (irt_isint64(ir->t))
      return 1;
  for (ref = J->chain[IR_CONV]; ref; ref = IR(ref)->prev)
    if ((IR(ref)->op2 & IRCONV_SRCMASK) == IRT_I64 ||
	(IR(ref)->op2 & IRCONV_SRCMASK) == IRT_U64)
      return 1;
  return 0;  /* Nope. */
}
#endif

/* SPLIT pass. */
void lj_opt_split(jit_State *J)
{
  lua_assert(J->needsplit >= split_needsplit(J));  /* Verify flag. */
  if (J->needsplit) {
    int errcode = lj_vm_cpcall(J->L, NULL, J, cpsplit);
    if (errcode) {
      /* Completely reset the trace to avoid inconsistent dump on abort. */
      J->cur.nins = J->cur.nk = REF_BASE;
      J->cur.nsnap = 0;
      lj_err_throw(J->L, errcode);  /* Propagate errors. */
    }
  }
}

#undef IR

#endif
