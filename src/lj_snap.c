/*
** Snapshot handling.
** Copyright (C) 2005-2009 Mike Pall. See Copyright Notice in luajit.h
*/

#define lj_snap_c
#define LUA_CORE

#include "lj_obj.h"

#if LJ_HASJIT

#include "lj_gc.h"
#include "lj_state.h"
#include "lj_frame.h"
#include "lj_ir.h"
#include "lj_jit.h"
#include "lj_iropt.h"
#include "lj_trace.h"
#include "lj_snap.h"
#include "lj_target.h"

/* Some local macros to save typing. Undef'd at the end. */
#define IR(ref)		(&J->cur.ir[(ref)])

/* -- Snapshot generation ------------------------------------------------- */

/* NYI: Snapshots are in need of a redesign. The current storage model for
** snapshot maps is too wasteful. They could be compressed (1D or 2D) and
** made more flexible at the same time. Iterators should no longer need to
** skip unmodified slots. IR_FRAME should be eliminated, too.
*/

/* Add all modified slots to the snapshot. */
static void snapshot_slots(jit_State *J, IRRef2 *map, BCReg nslots)
{
  BCReg s;
  for (s = 0; s < nslots; s++) {
    IRRef ref = tref_ref(J->slot[s]);
    if (ref) {
      IRIns *ir = IR(ref);
      if (ir->o == IR_SLOAD && ir->op1 == s && !(ir->op2 & IRSLOAD_INHERIT))
	ref = 0;
    }
    map[s] = (IRRef2)ref;
  }
}

/* Add frame links at the end of the snapshot. */
static MSize snapshot_framelinks(jit_State *J, IRRef2 *map)
{
  cTValue *frame = J->L->base - 1;
  cTValue *lim = J->L->base - J->baseslot;
  MSize f = 0;
  map[f++] = u32ptr(J->pc);
  while (frame > lim) {
    if (frame_islua(frame)) {
      map[f++] = u32ptr(frame_pc(frame));
      frame = frame_prevl(frame);
    } else if (frame_ispcall(frame)) {
      map[f++] = (uint32_t)frame_ftsz(frame);
      frame = frame_prevd(frame);
    } else if (frame_iscont(frame)) {
      map[f++] = (uint32_t)frame_ftsz(frame);
      map[f++] = u32ptr(frame_contpc(frame));
      frame = frame_prevd(frame);
    } else {
      lua_assert(0);
    }
  }
  return f;
}

/* Take a snapshot of the current stack. */
static void snapshot_stack(jit_State *J, SnapShot *snap, MSize nsnapmap)
{
  BCReg nslots = J->baseslot + J->maxslot;
  MSize nsm, nframelinks;
  IRRef2 *p;
  /* Conservative estimate. Continuation frames need 2 slots. */
  nsm = nsnapmap + nslots + (uint32_t)J->framedepth*2+1;
  if (LJ_UNLIKELY(nsm > J->sizesnapmap)) {  /* Need to grow snapshot map? */
    if (nsm < 2*J->sizesnapmap)
      nsm = 2*J->sizesnapmap;
    else if (nsm < 64)
      nsm = 64;
    J->snapmapbuf = (IRRef2 *)lj_mem_realloc(J->L, J->snapmapbuf,
		      J->sizesnapmap*sizeof(IRRef2), nsm*sizeof(IRRef2));
    J->cur.snapmap = J->snapmapbuf;
    J->sizesnapmap = nsm;
  }
  p = &J->cur.snapmap[nsnapmap];
  snapshot_slots(J, p, nslots);
  nframelinks = snapshot_framelinks(J, p + nslots);
  J->cur.nsnapmap = (uint16_t)(nsnapmap + nslots + nframelinks);
  snap->mapofs = (uint16_t)nsnapmap;
  snap->ref = (IRRef1)J->cur.nins;
  snap->nslots = (uint8_t)nslots;
  snap->nframelinks = (uint8_t)nframelinks;
  snap->count = 0;
}

/* Add or merge a snapshot. */
void lj_snap_add(jit_State *J)
{
  MSize nsnap = J->cur.nsnap;
  MSize nsnapmap = J->cur.nsnapmap;
  /* Merge if no ins. inbetween or if requested and no guard inbetween. */
  if (J->mergesnap ? !irt_isguard(J->guardemit) :
      (nsnap > 0 && J->cur.snap[nsnap-1].ref == J->cur.nins)) {
    nsnapmap = J->cur.snap[--nsnap].mapofs;
  } else {
    /* Need to grow snapshot buffer? */
    if (LJ_UNLIKELY(nsnap >= J->sizesnap)) {
      MSize maxsnap = (MSize)J->param[JIT_P_maxsnap];
      if (nsnap >= maxsnap)
	lj_trace_err(J, LJ_TRERR_SNAPOV);
      lj_mem_growvec(J->L, J->snapbuf, J->sizesnap, maxsnap, SnapShot);
      J->cur.snap = J->snapbuf;
    }
    J->cur.nsnap = (uint16_t)(nsnap+1);
  }
  J->mergesnap = 0;
  J->guardemit.irt = 0;
  snapshot_stack(J, &J->cur.snap[nsnap], nsnapmap);
}

/* Shrink last snapshot. */
void lj_snap_shrink(jit_State *J)
{
  BCReg nslots = J->baseslot + J->maxslot;
  SnapShot *snap = &J->cur.snap[J->cur.nsnap-1];
  IRRef2 *oflinks = &J->cur.snapmap[snap->mapofs + snap->nslots];
  IRRef2 *nflinks = &J->cur.snapmap[snap->mapofs + nslots];
  uint32_t s, nframelinks = snap->nframelinks;
  lua_assert(nslots < snap->nslots);
  snap->nslots = (uint8_t)nslots;
  J->cur.nsnapmap = (uint16_t)(snap->mapofs + nslots + nframelinks);
  for (s = 0; s < nframelinks; s++)  /* Move frame links down. */
    nflinks[s] = oflinks[s];
}

/* -- Snapshot access ----------------------------------------------------- */

/* Initialize a Bloom Filter with all renamed refs.
** There are very few renames (often none), so the filter has
** very few bits set. This makes it suitable for negative filtering.
*/
static BloomFilter snap_renamefilter(Trace *T, SnapNo lim)
{
  BloomFilter rfilt = 0;
  IRIns *ir;
  for (ir = &T->ir[T->nins-1]; ir->o == IR_RENAME; ir--)
    if (ir->op2 <= lim)
      bloomset(rfilt, ir->op1);
  return rfilt;
}

/* Process matching renames to find the original RegSP. */
static RegSP snap_renameref(Trace *T, SnapNo lim, IRRef ref, RegSP rs)
{
  IRIns *ir;
  for (ir = &T->ir[T->nins-1]; ir->o == IR_RENAME; ir--)
    if (ir->op1 == ref && ir->op2 <= lim)
      rs = ir->prev;
  return rs;
}

/* Convert a snapshot into a linear slot -> RegSP map. */
void lj_snap_regspmap(uint16_t *rsmap, Trace *T, SnapNo snapno)
{
  SnapShot *snap = &T->snap[snapno];
  BCReg s, nslots = snap->nslots;
  IRRef2 *map = &T->snapmap[snap->mapofs];
  BloomFilter rfilt = snap_renamefilter(T, snapno);
  for (s = 0; s < nslots; s++) {
    IRRef ref = snap_ref(map[s]);
    if (!irref_isk(ref)) {
      IRIns *ir = &T->ir[ref];
      uint32_t rs = ir->prev;
      if (bloomtest(rfilt, ref))
	rs = snap_renameref(T, snapno, ref, rs);
      rsmap[s] = (uint16_t)rs;
    }
  }
}

/* Restore interpreter state from exit state with the help of a snapshot. */
void lj_snap_restore(jit_State *J, void *exptr)
{
  ExitState *ex = (ExitState *)exptr;
  SnapNo snapno = J->exitno;  /* For now, snapno == exitno. */
  Trace *T = J->trace[J->parent];
  SnapShot *snap = &T->snap[snapno];
  BCReg s, nslots = snap->nslots;
  IRRef2 *map = &T->snapmap[snap->mapofs];
  IRRef2 *flinks = map + nslots + snap->nframelinks;
  TValue *o, *newbase, *ntop;
  BloomFilter rfilt = snap_renamefilter(T, snapno);
  lua_State *L = J->L;

  /* Make sure the stack is big enough for the slots from the snapshot. */
  if (L->base + nslots >= L->maxstack) {
    L->top = curr_topL(L);
    lj_state_growstack(L, nslots - curr_proto(L)->framesize);
  }

  /* Fill stack slots with data from the registers and spill slots. */
  newbase = NULL;
  ntop = L->base;
  for (s = 0, o = L->base-1; s < nslots; s++, o++) {
    IRRef ref = snap_ref(map[s]);
    if (ref) {
      IRIns *ir = &T->ir[ref];
      if (irref_isk(ref)) {  /* Restore constant slot. */
	lj_ir_kvalue(L, o, ir);
      } else {
	IRType1 t = ir->t;
	RegSP rs = ir->prev;
	if (LJ_UNLIKELY(bloomtest(rfilt, ref)))
	  rs = snap_renameref(T, snapno, ref, rs);
	if (ra_hasspill(regsp_spill(rs))) {  /* Restore from spill slot. */
	  int32_t *sps = &ex->spill[regsp_spill(rs)];
	  if (irt_isinteger(t)) {
	    setintV(o, *sps);
	  } else if (irt_isnum(t)) {
	    o->u64 = *(uint64_t *)sps;
	  } else {
	    lua_assert(!irt_ispri(t));  /* PRI refs never have a spill slot. */
	    setgcrefi(o->gcr, *sps);
	    setitype(o, irt_toitype(t));
	  }
	} else if (ra_hasreg(regsp_reg(rs))) {  /* Restore from register. */
	  Reg r = regsp_reg(rs);
	  if (irt_isinteger(t)) {
	    setintV(o, ex->gpr[r-RID_MIN_GPR]);
	  } else if (irt_isnum(t)) {
	    setnumV(o, ex->fpr[r-RID_MIN_FPR]);
	  } else {
	    if (!irt_ispri(t))
	      setgcrefi(o->gcr, ex->gpr[r-RID_MIN_GPR]);
	    setitype(o, irt_toitype(t));
	  }
	} else {  /* Restore frame slot. */
	  lua_assert(ir->o == IR_FRAME);
	  /* This works for both PTR and FUNC IR_FRAME. */
	  setgcrefp(o->fr.func, mref(T->ir[ir->op2].ptr, void));
	  if (s != 0)  /* Do not overwrite link to previous frame. */
	    o->fr.tp.ftsz = (int32_t)*--flinks;
	  if (irt_isfunc(ir->t)) {
	    GCfunc *fn = gco2func(gcref(T->ir[ir->op2].gcr));
	    if (isluafunc(fn)) {
	      TValue *fs;
	      fs = o+1 + funcproto(fn)->framesize;
	      if (fs > ntop) ntop = fs; /* Update top for newly added frames. */
	      if (s != 0) newbase = o+1;
	    }
	  }
	}
      }
    } else if (newbase) {
      setnilV(o);  /* Clear unreferenced slots of newly added frames. */
    }
  }
  if (newbase) L->base = newbase;
  if (ntop >= L->maxstack) {  /* Need to grow the stack again. */
    MSize need = (MSize)(ntop - o);
    L->top = o;
    lj_state_growstack(L, need);
    o = L->top;
    ntop = o + need;
  }
  L->top = curr_topL(L);
  for (; o < ntop; o++)  /* Clear remainder of newly added frames. */
    setnilV(o);
  lua_assert(map + nslots == flinks-1);
  J->pc = (const BCIns *)(uintptr_t)(*--flinks);
}

#undef IR

#endif
