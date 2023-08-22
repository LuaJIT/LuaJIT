/*
** SINK: Allocation Sinking and Store Sinking.
** Copyright (C) 2005-2023 Mike Pall. See Copyright Notice in luajit.h
*/

#define lj_opt_sink_c
#define LUA_CORE

#include "lj_obj.h"

#if LJ_HASJIT

#include "lj_ir.h"
#include "lj_jit.h"
#include "lj_iropt.h"
#include "lj_target.h"
#include "lj_dispatch.h"

/* Some local macros to save typing. Undef'd at the end. */
#define IR(ref)		(&J->cur.ir[(ref)])

/* Check whether the store ref points to an eligible allocation. */
static IRIns *sink_checkalloc(jit_State *J, IRIns *irs)
{
  IRIns *ir = IR(irs->op1);
  if (!irref_isk(ir->op2))
    return NULL;  /* Non-constant key. */
  if (ir->o == IR_HREFK || ir->o == IR_AREF)
    ir = IR(ir->op1);
  else if (!(ir->o == IR_HREF || ir->o == IR_NEWREF ||
	     ir->o == IR_FREF || ir->o == IR_ADD))
    return NULL;  /* Unhandled reference type (for XSTORE). */
  ir = IR(ir->op1);
  if (!(ir->o == IR_TNEW || ir->o == IR_TDUP || ir->o == IR_CNEW))
    return NULL;  /* Not an allocation. */
  return ir;  /* Return allocation. */
}

/* Recursively check whether a value depends on a PHI. */
static int sink_phidep(jit_State *J, IRRef ref, int *workp)
{
  IRIns *ir = IR(ref);
  if (!*workp) return 1;  /* Give up and pretend it does. */
  (*workp)--;
  if (irt_isphi(ir->t)) return 1;
  if (ir->op1 >= REF_FIRST && sink_phidep(J, ir->op1, workp)) return 1;
  if (ir->op2 >= REF_FIRST && sink_phidep(J, ir->op2, workp)) return 1;
  return 0;
}

/* Check whether a value is a sinkable PHI or loop-invariant. */
static int sink_checkphi(jit_State *J, IRIns *ira, IRRef ref)
{
  if (ref >= REF_FIRST) {
    IRIns *ir = IR(ref);
    if (irt_isphi(ir->t) || (ir->o == IR_CONV && ir->op2 == IRCONV_NUM_INT &&
			     irt_isphi(IR(ir->op1)->t))) {
      if ((ira->prev & 0x1FFF) == 0x1FFF)
  return 0; /* This would cause an overflow, just force the allocation to not be sunken. */
      ira->prev++;
      return 1;  /* Sinkable PHI. */
    }
    /* Otherwise the value must be loop-invariant. */
    if (ref < J->loopref) {
      /* Check for PHI dependencies, but give up after reasonable effort. */
      int work = 64;
      return !sink_phidep(J, ref, &work);
    } else {
      return 0;  /* Loop-variant. */
    }
  }
  return 1;  /* Constant (non-PHI). */
}

/* Set prev of all instructions to 0. */
static void sink_prepare(jit_State *J) {
  IRIns *ir, *irlast = IR(J->cur.nins-1);
  for (ir = irlast ; ir->o != IR_BASE; ir--) {
    ir->prev = 0;
  }
}

/* Mark non-sinkable allocations using single-pass backward propagation.
**
** Roots for the marking process are:
** - Some PHIs or snapshots (see below).
** - Non-PHI, non-constant values stored to PHI allocations.
** - All guards.
** - Any remaining loads not eliminated by store-to-load forwarding.
** - Stores with non-constant keys.
** - All stored values.
*/
static int sink_mark_ins(jit_State *J, int lightsink)
{
  int remark = 0;
  int heavysinks = 0;
  IRIns *ir, *irlast = IR(J->cur.nins-1);
  for (ir = irlast ; ; ir--) {
    switch (ir->o) {
    case IR_BASE:
      if (!remark)
        return heavysinks;
      ir = irlast + 1;
      remark = 0;
      heavysinks = 0;
      break;
    case IR_ALOAD: case IR_HLOAD: case IR_XLOAD: case IR_TBAR: case IR_ALEN:
      irt_setmark(IR(ir->op1)->t);  /* Mark ref for remaining loads. */
      break;
    case IR_FLOAD:
      if (irt_ismarked(ir->t) || ir->op2 == IRFL_TAB_META)
	irt_setmark(IR(ir->op1)->t);  /* Mark table for remaining loads. */
      break;
    case IR_ASTORE: case IR_HSTORE: case IR_FSTORE: case IR_XSTORE: {
      IRIns *ira = sink_checkalloc(J, ir);
      IRIns *irv = IR(ir->op2);
      if (!ira || (irt_isphi(ira->t) && !sink_checkphi(J, ira, ir->op2)) || (irt_ismarked(ira->t))) {
	irt_setmark(IR(ir->op1)->t);  /* Mark ineligible ref. */
  irt_setmark(irv->t);  /* Mark stored value. */
      } else if (lightsink || (irv->o != IR_TNEW && irv->o != IR_TDUP && irv->o != IR_CNEW)) {
  irt_setmark(irv->t);
      } else {
  ira->prev |= 0x2000;  /* For this allocation is a store that assumes it is sinkable. */
  irv->prev |= 0x4000;  /* The sunken allocation is required for a other sunken allocation. It requires a global index. */
      }
      break;
      }
#if LJ_HASFFI
    case IR_CNEWI:
      if (irt_isphi(ir->t) &&
	  (!sink_checkphi(J, ir, ir->op2) ||
	   (LJ_32 && ir+1 < irlast && (ir+1)->o == IR_HIOP &&
	    !sink_checkphi(J, ir, (ir+1)->op2))))
	irt_setmark(ir->t);  /* Mark ineligible allocation. */
#endif
      /* fallthrough */
    case IR_USTORE:
      irt_setmark(IR(ir->op2)->t);  /* Mark stored value. */
      break;
#if LJ_HASFFI
    case IR_CALLXS:
#endif
    case IR_CALLS:
      irt_setmark(IR(ir->op1)->t);  /* Mark (potentially) stored values. */
      break;
    case IR_PHI: {
      IRIns *irl = IR(ir->op1), *irr = IR(ir->op2);
      irl->prev &= 0xC000;
      irr->prev &= 0xC000;  /* Clear PHI value counts. */
      if (irl->o == irr->o &&
	  (irl->o == IR_TNEW || irl->o == IR_TDUP ||
	   (LJ_HASFFI && (irl->o == IR_CNEW || irl->o == IR_CNEWI))))
	break;
      irt_setmark(irl->t);
      irt_setmark(irr->t);
      break;
      }
    case IR_TNEW: case IR_TDUP: case IR_CNEW:
      if ((ir->prev & 0x2000) && irt_ismarked(ir->t)) {
  ir->prev &= ~0x2000;
  remark = 1;  /* There is an store that assumed that this allocation can be sunken, but it can't. We need to redo the whool process so that this store can mark it's value. */
      }
      if (!irt_ismarked(ir->t) && (ir->prev & 0x4000)) {
  ir->prev |= 0x8000;
  heavysinks++;
      } else {
  ir->prev &= ~0x8000;
      }
      ir->prev &= ~0x4000;
      if (!irt_isphi(ir->t))
  ir->prev &= ~0x2000;
      /* fallthrough */
    default:
      if (irt_ismarked(ir->t) || irt_isguard(ir->t)) {  /* Propagate mark. */
	if (ir->op1 >= REF_FIRST) irt_setmark(IR(ir->op1)->t);
	if (ir->op2 >= REF_FIRST) irt_setmark(IR(ir->op2)->t);
      }
      break;
    }
  }
}

/* Mark all instructions referenced by a snapshot. */
static void sink_mark_snap(jit_State *J, SnapShot *snap)
{
  SnapEntry *map = &J->cur.snapmap[snap->mapofs];
  MSize n, nent = snap->nent;
  for (n = 0; n < nent; n++) {
    IRRef ref = snap_ref(map[n]);
    if (!irref_isk(ref))
      irt_setmark(IR(ref)->t);
  }
}

/* Iteratively remark PHI refs with differing marks or PHI value counts. */
static int sink_remark_phi(jit_State *J)
{
  IRIns *ir;
  int remark;
  int require_remark = 0;
  do {
    remark = 0;
    for (ir = IR(J->cur.nins-1); ir->o == IR_PHI; ir--) {
      IRIns *irl = IR(ir->op1), *irr = IR(ir->op2);
      if (!((irl->t.irt ^ irr->t.irt) & IRT_MARK) && (irl->prev & 0x1FFF) == (irr->prev & 0x1FFF))
	continue;
      remark |= (~(irl->t.irt & irr->t.irt) & IRT_MARK);
      if ((IR(ir->op1)->prev & 0x2000) || (IR(ir->op2)->prev & 0x2000)) {
  IR(ir->op1)->prev &= ~0x2000;
  IR(ir->op2)->prev &= ~0x2000;
  require_remark |= (~(irl->t.irt & irr->t.irt) & IRT_MARK);
      }
      irt_setmark(IR(ir->op1)->t);
      irt_setmark(IR(ir->op2)->t);
    }
  } while (remark);
  return require_remark;
}

/* Sweep instructions and tag sunken allocations and stores. */
static void sink_sweep_ins(jit_State *J)
{
  int index = 0;
  IRIns *ir, *irbase = IR(REF_BASE);
  for (ir = IR(J->cur.nins-1) ; ir >= irbase; ir--) {
    switch (ir->o) {
    case IR_ASTORE: case IR_HSTORE: case IR_FSTORE: case IR_XSTORE: {
      IRIns *ira = sink_checkalloc(J, ir);
      if (ira && !irt_ismarked(ira->t)) {
	int delta = (int)(ir - ira);
	ir->prev = REGSP(RID_SINK, delta > 255 ? 255 : delta);
      } else {
	ir->prev = REGSP_INIT;
      }
      break;
      }
    case IR_NEWREF:
      if (!irt_ismarked(IR(ir->op1)->t)) {
	ir->prev = REGSP(RID_SINK, 0);
      } else {
	irt_clearmark(ir->t);
	ir->prev = REGSP_INIT;
      }
      break;
#if LJ_HASFFI
    case IR_CNEW: case IR_CNEWI:
#endif
    case IR_TNEW: case IR_TDUP:
      if (!irt_ismarked(ir->t)) {
  if (ir->prev & 0x8000) {
    index++;  /* A sunken store requires this for unsinking. */
    lj_assertJ(index <= 0xFF, "Too many heavy sinks");
    ir->prev = REGSP(RID_SINK, index);
  } else {
    ir->prev = REGSP(RID_SINK, 0);
  }
	ir->t.irt &= ~IRT_GUARD;
	J->cur.sinktags = 1;  /* Signal present SINK tags to assembler. */
      } else {
	irt_clearmark(ir->t);
	ir->prev = REGSP_INIT;
      }
      break;
    case IR_PHI: {
      IRIns *ira = IR(ir->op2);
      if (!irt_ismarked(ira->t) &&
	  (ira->o == IR_TNEW || ira->o == IR_TDUP ||
	   (LJ_HASFFI && (ira->o == IR_CNEW || ira->o == IR_CNEWI)))) {
	ir->prev = REGSP(RID_SINK, 0);
      } else {
	ir->prev = REGSP_INIT;
      }
      break;
      }
    default:
      irt_clearmark(ir->t);
      ir->prev = REGSP_INIT;
      break;
    }
  }
  for (ir = IR(J->cur.nk); ir < irbase; ir++) {
    irt_clearmark(ir->t);
    ir->prev = REGSP_INIT;
    /* The false-positive of irt_is64() for ASMREF_L (REF_NIL) is OK here. */
    if (irt_is64(ir->t) && ir->o != IR_KNULL)
      ir++;
  }
}

/* Allocation sinking and store sinking.
**
** 1. Mark all non-sinkable allocations.
** 2. Then sink all remaining allocations and the related stores.
*/
void lj_opt_sink(jit_State *J)
{
  const uint32_t need = (JIT_F_OPT_SINK|JIT_F_OPT_FWD|
			 JIT_F_OPT_DCE|JIT_F_OPT_CSE|JIT_F_OPT_FOLD);
  if ((J->flags & need) == need &&
      (J->chain[IR_TNEW] || J->chain[IR_TDUP] ||
       (LJ_HASFFI && (J->chain[IR_CNEW] || J->chain[IR_CNEWI])))) {
    sink_prepare(J);
    if (!J->loopref)
      sink_mark_snap(J, &J->cur.snap[J->cur.nsnap-1]);
    int heavysinks;
    int dolightsink = 0;
    do {
      heavysinks = sink_mark_ins(J, dolightsink);
      dolightsink |= heavysinks >= 0xFF;
    } while ((J->loopref && sink_remark_phi(J)) || heavysinks >= 0xFF);
    sink_sweep_ins(J);
  }
}

#undef IR

#endif
