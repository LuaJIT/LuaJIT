/*
** Snapshot handling.
** Copyright (C) 2005-2010 Mike Pall. See Copyright Notice in luajit.h
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

/* -- Snapshot buffer allocation ------------------------------------------ */

/* Grow snapshot buffer. */
void lj_snap_grow_buf_(jit_State *J, MSize need)
{
  MSize maxsnap = (MSize)J->param[JIT_P_maxsnap];
  if (need > maxsnap)
    lj_trace_err(J, LJ_TRERR_SNAPOV);
  lj_mem_growvec(J->L, J->snapbuf, J->sizesnap, maxsnap, SnapShot);
  J->cur.snap = J->snapbuf;
}

/* Grow snapshot map buffer. */
void lj_snap_grow_map_(jit_State *J, MSize need)
{
  if (need < 2*J->sizesnapmap)
    need = 2*J->sizesnapmap;
  else if (need < 64)
    need = 64;
  J->snapmapbuf = (SnapEntry *)lj_mem_realloc(J->L, J->snapmapbuf,
		    J->sizesnapmap*sizeof(SnapEntry), need*sizeof(SnapEntry));
  J->cur.snapmap = J->snapmapbuf;
  J->sizesnapmap = need;
}

/* -- Snapshot generation ------------------------------------------------- */

/* NYI: IR_FRAME should be eliminated, too. */

/* Add all modified slots to the snapshot. */
static MSize snapshot_slots(jit_State *J, SnapEntry *map, BCReg nslots)
{
  BCReg s;
  MSize n = 0;
  for (s = 0; s < nslots; s++) {
    IRRef ref = tref_ref(J->slot[s]);
    if (ref) {
      IRIns *ir = IR(ref);
      if (!(ir->o == IR_SLOAD && ir->op1 == s &&
	    !(ir->op2 & IRSLOAD_INHERIT)))
	map[n++] = SNAP(s, ir->o == IR_FRAME ? SNAP_FRAME : 0, ref);
    }
  }
  return n;
}

/* Add frame links at the end of the snapshot. */
static MSize snapshot_framelinks(jit_State *J, SnapEntry *map)
{
  cTValue *frame = J->L->base - 1;
  cTValue *lim = J->L->base - J->baseslot;
  MSize f = 0;
  map[f++] = SNAP_MKPC(J->pc);  /* The current PC is always the first entry. */
  while (frame > lim) {  /* Backwards traversal of all frames above base. */
    if (frame_islua(frame)) {
      map[f++] = SNAP_MKPC(frame_pc(frame));
      frame = frame_prevl(frame);
    } else if (frame_ispcall(frame)) {
      map[f++] = SNAP_MKFTSZ(frame_ftsz(frame));
      frame = frame_prevd(frame);
    } else if (frame_iscont(frame)) {
      map[f++] = SNAP_MKFTSZ(frame_ftsz(frame));
      map[f++] = SNAP_MKPC(frame_contpc(frame));
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
  MSize nent, nframelinks;
  SnapEntry *p;
  /* Conservative estimate. Continuation frames need 2 slots. */
  lj_snap_grow_map(J, nsnapmap + nslots + (MSize)J->framedepth*2+1);
  p = &J->cur.snapmap[nsnapmap];
  nent = snapshot_slots(J, p, nslots);
  nframelinks = snapshot_framelinks(J, p + nent);
  J->cur.nsnapmap = (uint16_t)(nsnapmap + nent + nframelinks);
  snap->mapofs = (uint16_t)nsnapmap;
  snap->ref = (IRRef1)J->cur.nins;
  snap->nent = (uint8_t)nent;
  snap->nframelinks = (uint8_t)nframelinks;
  snap->nslots = (uint8_t)nslots;
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
    lj_snap_grow_buf(J, nsnap+1);
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
  SnapEntry *map = &J->cur.snapmap[snap->mapofs];
  MSize nent = snap->nent;
  lua_assert(nslots < snap->nslots);
  snap->nslots = (uint8_t)nslots;
  if (nent > 0 && snap_slot(map[nent-1]) >= nslots) {
    MSize s, delta, nframelinks = snap->nframelinks;
    for (nent--; nent > 0 && snap_slot(map[nent-1]) >= nslots; nent--)
      ;
    delta = snap->nent - nent;
    snap->nent = (uint8_t)nent;
    J->cur.nsnapmap = (uint16_t)(snap->mapofs + nent + nframelinks);
    map += nent;
    for (s = 0; s < nframelinks; s++)  /* Move frame links down. */
      map[s] = map[s+delta];
  }
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

/* Convert a snapshot into a linear slot -> RegSP map.
** Note: unused slots are not initialized!
*/
void lj_snap_regspmap(uint16_t *rsmap, Trace *T, SnapNo snapno)
{
  SnapShot *snap = &T->snap[snapno];
  MSize n, nent = snap->nent;
  SnapEntry *map = &T->snapmap[snap->mapofs];
  BloomFilter rfilt = snap_renamefilter(T, snapno);
  for (n = 0; n < nent; n++) {
    SnapEntry sn = map[n];
    IRRef ref = snap_ref(sn);
    if (!irref_isk(ref)) {
      IRIns *ir = &T->ir[ref];
      uint32_t rs = ir->prev;
      if (bloomtest(rfilt, ref))
	rs = snap_renameref(T, snapno, ref, rs);
      rsmap[snap_slot(sn)] = (uint16_t)rs;
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
  MSize n, nent = snap->nent;
  SnapEntry *map = &T->snapmap[snap->mapofs];
  SnapEntry *flinks = map + nent + snap->nframelinks;
  BCReg nslots = snap->nslots;
  TValue *frame;
  BloomFilter rfilt = snap_renamefilter(T, snapno);
  lua_State *L = J->L;

  /* Make sure the stack is big enough for the slots from the snapshot. */
  if (LJ_UNLIKELY(L->base + nslots > L->maxstack)) {
    L->top = curr_topL(L);
    lj_state_growstack(L, nslots - curr_proto(L)->framesize);
  }

  /* Fill stack slots with data from the registers and spill slots. */
  frame = L->base-1;
  for (n = 0; n < nent; n++) {
    IRRef ref = snap_ref(map[n]);
    BCReg s = snap_slot(map[n]);
    TValue *o = &frame[s];  /* Stack slots are relative to start frame. */
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
	    MSize framesize = funcproto(fn)->framesize;
	    TValue *fs;
	    L->base = ++o;
	    if (LJ_UNLIKELY(o + framesize > L->maxstack)) {  /* Grow again? */
	      ptrdiff_t fsave = savestack(L, frame);
	      L->top = o;
	      lj_state_growstack(L, framesize);
	      frame = restorestack(L, fsave);
	      o = L->top;
	    }
	    fs = o + framesize;
	    if (s == 0)  /* Only partially clear tail call frame at #0. */
	      o = &frame[nslots];
	    while (o < fs)  /* Clear slots of newly added frames. */
	      setnilV(o++);
	  }
	}
      }
    }
  }
  L->top = curr_topL(L);
  J->pc = snap_pc(*--flinks);
  lua_assert(map + nent == flinks);
}

#undef IR

#endif
