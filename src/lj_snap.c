/*
** Snapshot handling.
** Copyright (C) 2005-2012 Mike Pall. See Copyright Notice in luajit.h
*/

#define lj_snap_c
#define LUA_CORE

#include "lj_obj.h"

#if LJ_HASJIT

#include "lj_gc.h"
#include "lj_state.h"
#include "lj_frame.h"
#include "lj_bc.h"
#include "lj_ir.h"
#include "lj_jit.h"
#include "lj_iropt.h"
#include "lj_trace.h"
#include "lj_snap.h"
#include "lj_target.h"

/* Some local macros to save typing. Undef'd at the end. */
#define IR(ref)		(&J->cur.ir[(ref)])

/* Emit raw IR without passing through optimizations. */
#define emitir_raw(ot, a, b)	(lj_ir_set(J, (ot), (a), (b)), lj_ir_emit(J))

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

/* Add all modified slots to the snapshot. */
static MSize snapshot_slots(jit_State *J, SnapEntry *map, BCReg nslots)
{
  IRRef retf = J->chain[IR_RETF];  /* Limits SLOAD restore elimination. */
  BCReg s;
  MSize n = 0;
  for (s = 0; s < nslots; s++) {
    TRef tr = J->slot[s];
    IRRef ref = tref_ref(tr);
    if (ref) {
      SnapEntry sn = SNAP_TR(s, tr);
      IRIns *ir = IR(ref);
      if (!(sn & (SNAP_CONT|SNAP_FRAME)) &&
	  ir->o == IR_SLOAD && ir->op1 == s && ref > retf) {
	/* No need to snapshot unmodified non-inherited slots. */
	if (!(ir->op2 & IRSLOAD_INHERIT))
	  continue;
	/* No need to restore readonly slots and unmodified non-parent slots. */
	if (!(LJ_DUALNUM && (ir->op2 & IRSLOAD_CONVERT)) &&
	    (ir->op2 & (IRSLOAD_READONLY|IRSLOAD_PARENT)) != IRSLOAD_PARENT)
	  sn |= SNAP_NORESTORE;
      }
      if (LJ_SOFTFP && irt_isnum(ir->t))
	sn |= SNAP_SOFTFPNUM;
      map[n++] = sn;
    }
  }
  return n;
}

/* Add frame links at the end of the snapshot. */
static BCReg snapshot_framelinks(jit_State *J, SnapEntry *map)
{
  cTValue *frame = J->L->base - 1;
  cTValue *lim = J->L->base - J->baseslot;
  cTValue *ftop = frame + funcproto(frame_func(frame))->framesize;
  MSize f = 0;
  map[f++] = SNAP_MKPC(J->pc);  /* The current PC is always the first entry. */
  while (frame > lim) {  /* Backwards traversal of all frames above base. */
    if (frame_islua(frame)) {
      map[f++] = SNAP_MKPC(frame_pc(frame));
      frame = frame_prevl(frame);
      if (frame + funcproto(frame_func(frame))->framesize > ftop)
	ftop = frame + funcproto(frame_func(frame))->framesize;
    } else if (frame_iscont(frame)) {
      map[f++] = SNAP_MKFTSZ(frame_ftsz(frame));
      map[f++] = SNAP_MKPC(frame_contpc(frame));
      frame = frame_prevd(frame);
    } else {
      lua_assert(!frame_isc(frame));
      map[f++] = SNAP_MKFTSZ(frame_ftsz(frame));
      frame = frame_prevd(frame);
    }
  }
  lua_assert(f == (MSize)(1 + J->framedepth));
  return (BCReg)(ftop - lim);
}

/* Take a snapshot of the current stack. */
static void snapshot_stack(jit_State *J, SnapShot *snap, MSize nsnapmap)
{
  BCReg nslots = J->baseslot + J->maxslot;
  MSize nent;
  SnapEntry *p;
  /* Conservative estimate. */
  lj_snap_grow_map(J, nsnapmap + nslots + (MSize)J->framedepth+1);
  p = &J->cur.snapmap[nsnapmap];
  nent = snapshot_slots(J, p, nslots);
  snap->topslot = (uint8_t)snapshot_framelinks(J, p + nent);
  snap->mapofs = (uint16_t)nsnapmap;
  snap->ref = (IRRef1)J->cur.nins;
  snap->nent = (uint8_t)nent;
  snap->nslots = (uint8_t)nslots;
  snap->count = 0;
  J->cur.nsnapmap = (uint16_t)(nsnapmap + nent + 1 + J->framedepth);
}

/* Add or merge a snapshot. */
void lj_snap_add(jit_State *J)
{
  MSize nsnap = J->cur.nsnap;
  MSize nsnapmap = J->cur.nsnapmap;
  /* Merge if no ins. inbetween or if requested and no guard inbetween. */
  if (J->mergesnap ? !irt_isguard(J->guardemit) :
      (nsnap > 0 && J->cur.snap[nsnap-1].ref == J->cur.nins)) {
    if (nsnap == 1 && J->parent == 0) {
      /* But preserve snap #0 PC for root traces. */
      emitir_raw(IRT(IR_NOP, IRT_NIL), 0, 0);
      goto nomerge;
    }
    nsnapmap = J->cur.snap[--nsnap].mapofs;
  } else {
  nomerge:
    lj_snap_grow_buf(J, nsnap+1);
    J->cur.nsnap = (uint16_t)(nsnap+1);
  }
  J->mergesnap = 0;
  J->guardemit.irt = 0;
  snapshot_stack(J, &J->cur.snap[nsnap], nsnapmap);
}

/* -- Snapshot modification ----------------------------------------------- */

#define SNAP_USEDEF_SLOTS	(LJ_MAX_JSLOTS+LJ_STACK_EXTRA)

/* Find unused slots with reaching-definitions bytecode data-flow analysis. */
static BCReg snap_usedef(jit_State *J, uint8_t *udf,
			 const BCIns *pc, BCReg maxslot)
{
  BCReg s;
  GCobj *o;

  if (maxslot == 0) return 0;
#ifdef LUAJIT_USE_VALGRIND
  /* Avoid errors for harmless reads beyond maxslot. */
  memset(udf, 1, SNAP_USEDEF_SLOTS);
#else
  memset(udf, 1, maxslot);
#endif

  /* Treat open upvalues as used. */
  o = gcref(J->L->openupval);
  while (o) {
    if (uvval(gco2uv(o)) < J->L->base) break;
    udf[uvval(gco2uv(o)) - J->L->base] = 0;
    o = gcref(o->gch.nextgc);
  }

#define USE_SLOT(s)		udf[(s)] &= ~1
#define DEF_SLOT(s)		udf[(s)] *= 3

  /* Scan through following bytecode and check for uses/defs. */
  lua_assert(pc >= proto_bc(J->pt) && pc < proto_bc(J->pt) + J->pt->sizebc);
  for (;;) {
    BCIns ins = *pc++;
    BCOp op = bc_op(ins);
    switch (bcmode_b(op)) {
    case BCMvar: USE_SLOT(bc_b(ins)); break;
    default: break;
    }
    switch (bcmode_c(op)) {
    case BCMvar: USE_SLOT(bc_c(ins)); break;
    case BCMrbase:
      lua_assert(op == BC_CAT);
      for (s = bc_b(ins); s <= bc_c(ins); s++) USE_SLOT(s);
      for (; s < maxslot; s++) DEF_SLOT(s);
      break;
    case BCMjump:
    handle_jump: {
      BCReg minslot = bc_a(ins);
      if (op >= BC_FORI && op <= BC_JFORL) minslot += FORL_EXT;
      else if (op >= BC_ITERL && op <= BC_JITERL) minslot += bc_b(pc[-2])-1;
      else if (op == BC_UCLO) { pc += bc_j(ins); break; }
      for (s = minslot; s < maxslot; s++) DEF_SLOT(s);
      return minslot < maxslot ? minslot : maxslot;
      }
    case BCMlit:
      if (op == BC_JFORL || op == BC_JITERL || op == BC_JLOOP) {
	goto handle_jump;
      } else if (bc_isret(op)) {
	BCReg top = op == BC_RETM ? maxslot : (bc_a(ins) + bc_d(ins)-1);
	for (s = 0; s < bc_a(ins); s++) DEF_SLOT(s);
	for (; s < top; s++) USE_SLOT(s);
	for (; s < maxslot; s++) DEF_SLOT(s);
	return 0;
      }
      break;
    case BCMfunc: return maxslot;  /* NYI: will abort, anyway. */
    default: break;
    }
    switch (bcmode_a(op)) {
    case BCMvar: USE_SLOT(bc_a(ins)); break;
    case BCMdst:
       if (!(op == BC_ISTC || op == BC_ISFC)) DEF_SLOT(bc_a(ins));
       break;
    case BCMbase:
      if (op >= BC_CALLM && op <= BC_VARG) {
	BCReg top = (op == BC_CALLM || op == BC_CALLMT || bc_c(ins) == 0) ?
		    maxslot : (bc_a(ins) + bc_c(ins));
	s = bc_a(ins) - ((op == BC_ITERC || op == BC_ITERN) ? 3 : 0);
	for (; s < top; s++) USE_SLOT(s);
	for (; s < maxslot; s++) DEF_SLOT(s);
	if (op == BC_CALLT || op == BC_CALLMT) {
	  for (s = 0; s < bc_a(ins); s++) DEF_SLOT(s);
	  return 0;
	}
      } else if (op == BC_KNIL) {
	for (s = bc_a(ins); s <= bc_d(ins); s++) DEF_SLOT(s);
      } else if (op == BC_TSETM) {
	for (s = bc_a(ins)-1; s < maxslot; s++) USE_SLOT(s);
      }
      break;
    default: break;
    }
    lua_assert(pc >= proto_bc(J->pt) && pc < proto_bc(J->pt) + J->pt->sizebc);
  }

#undef USE_SLOT
#undef DEF_SLOT

  return 0;  /* unreachable */
}

/* Purge dead slots before the next snapshot. */
void lj_snap_purge(jit_State *J)
{
  uint8_t udf[SNAP_USEDEF_SLOTS];
  BCReg maxslot = J->maxslot;
  BCReg s = snap_usedef(J, udf, J->pc, maxslot);
  for (; s < maxslot; s++)
    if (udf[s] != 0)
      J->base[s] = 0;  /* Purge dead slots. */
}

/* Shrink last snapshot. */
void lj_snap_shrink(jit_State *J)
{
  SnapShot *snap = &J->cur.snap[J->cur.nsnap-1];
  SnapEntry *map = &J->cur.snapmap[snap->mapofs];
  MSize n, m, nlim, nent = snap->nent;
  uint8_t udf[SNAP_USEDEF_SLOTS];
  BCReg maxslot = J->maxslot;
  BCReg minslot = snap_usedef(J, udf, snap_pc(map[nent]), maxslot);
  BCReg baseslot = J->baseslot;
  maxslot += baseslot;
  minslot += baseslot;
  snap->nslots = (uint8_t)maxslot;
  for (n = m = 0; n < nent; n++) {  /* Remove unused slots from snapshot. */
    BCReg s = snap_slot(map[n]);
    if (s < minslot || (s < maxslot && udf[s-baseslot] == 0))
      map[m++] = map[n];  /* Only copy used slots. */
  }
  snap->nent = (uint8_t)m;
  nlim = J->cur.nsnapmap - snap->mapofs - 1;
  while (n <= nlim) map[m++] = map[n++];  /* Move PC + frame links down. */
  J->cur.nsnapmap = (uint16_t)(snap->mapofs + m);  /* Free up space in map. */
}

/* -- Snapshot access ----------------------------------------------------- */

/* Initialize a Bloom Filter with all renamed refs.
** There are very few renames (often none), so the filter has
** very few bits set. This makes it suitable for negative filtering.
*/
static BloomFilter snap_renamefilter(GCtrace *T, SnapNo lim)
{
  BloomFilter rfilt = 0;
  IRIns *ir;
  for (ir = &T->ir[T->nins-1]; ir->o == IR_RENAME; ir--)
    if (ir->op2 <= lim)
      bloomset(rfilt, ir->op1);
  return rfilt;
}

/* Process matching renames to find the original RegSP. */
static RegSP snap_renameref(GCtrace *T, SnapNo lim, IRRef ref, RegSP rs)
{
  IRIns *ir;
  for (ir = &T->ir[T->nins-1]; ir->o == IR_RENAME; ir--)
    if (ir->op1 == ref && ir->op2 <= lim)
      rs = ir->prev;
  return rs;
}

/* Copy RegSP from parent snapshot to the parent links of the IR. */
IRIns *lj_snap_regspmap(GCtrace *T, SnapNo snapno, IRIns *ir)
{
  SnapShot *snap = &T->snap[snapno];
  SnapEntry *map = &T->snapmap[snap->mapofs];
  BloomFilter rfilt = snap_renamefilter(T, snapno);
  MSize n = 0;
  IRRef ref = 0;
  for ( ; ; ir++) {
    uint32_t rs;
    if (ir->o == IR_SLOAD) {
      if (!(ir->op2 & IRSLOAD_PARENT)) break;
      for ( ; ; n++) {
	lua_assert(n < snap->nent);
	if (snap_slot(map[n]) == ir->op1) {
	  ref = snap_ref(map[n++]);
	  break;
	}
      }
    } else if (LJ_SOFTFP && ir->o == IR_HIOP) {
      ref++;
    } else {
      break;
    }
    rs = T->ir[ref].prev;
    if (bloomtest(rfilt, ref))
      rs = snap_renameref(T, snapno, ref, rs);
    ir->prev = (uint16_t)rs;
    lua_assert(regsp_used(rs));
  }
  return ir;
}

/* -- Snapshot replay ----------------------------------------------------- */

/* Replay constant from parent trace. */
static TRef snap_replay_const(jit_State *J, IRIns *ir)
{
  /* Only have to deal with constants that can occur in stack slots. */
  switch ((IROp)ir->o) {
  case IR_KPRI: return TREF_PRI(irt_type(ir->t));
  case IR_KINT: return lj_ir_kint(J, ir->i);
  case IR_KGC: return lj_ir_kgc(J, ir_kgc(ir), irt_t(ir->t));
  case IR_KNUM: return lj_ir_k64(J, IR_KNUM, ir_knum(ir));
  case IR_KINT64: return lj_ir_k64(J, IR_KINT64, ir_kint64(ir));
  case IR_KPTR: return lj_ir_kptr(J, ir_kptr(ir));  /* Continuation. */
  default: lua_assert(0); return TREF_NIL; break;
  }
}

/* Replay snapshot state to setup side trace. */
void lj_snap_replay(jit_State *J, GCtrace *T)
{
  SnapShot *snap = &T->snap[J->exitno];
  SnapEntry *map = &T->snapmap[snap->mapofs];
  MSize n, nent = snap->nent;
  BloomFilter seen = 0;
  J->framedepth = 0;
  /* Emit IR for slots inherited from parent snapshot. */
  for (n = 0; n < nent; n++) {
    SnapEntry sn = map[n];
    BCReg s = snap_slot(sn);
    IRRef ref = snap_ref(sn);
    IRIns *ir = &T->ir[ref];
    TRef tr;
    /* The bloom filter avoids O(nent^2) overhead for de-duping slots. */
    if (bloomtest(seen, ref)) {
      MSize j;
      for (j = 0; j < n; j++)
	if (snap_ref(map[j]) == ref) {
	  tr = J->slot[snap_slot(map[j])];
	  goto setslot;
	}
    }
    bloomset(seen, ref);
    if (irref_isk(ref)) {
      tr = snap_replay_const(J, ir);
    } else {
      IRType t = irt_type(ir->t);
      uint32_t mode = IRSLOAD_INHERIT|IRSLOAD_PARENT;
      lua_assert(regsp_used(ir->prev));
      if (LJ_SOFTFP && (sn & SNAP_SOFTFPNUM)) t = IRT_NUM;
      if (ir->o == IR_SLOAD) mode |= (ir->op2 & IRSLOAD_READONLY);
      tr = emitir_raw(IRT(IR_SLOAD, t), s, mode);
    }
  setslot:
    J->slot[s] = tr | (sn&(SNAP_CONT|SNAP_FRAME));  /* Same as TREF_* flags. */
    J->framedepth += ((sn & (SNAP_CONT|SNAP_FRAME)) && s);
    if ((sn & SNAP_FRAME))
      J->baseslot = s+1;
  }
  J->base = J->slot + J->baseslot;
  J->maxslot = snap->nslots - J->baseslot;
}

/* -- Snapshot restore ---------------------------------------------------- */

/* Restore a value from the trace exit state. */
static void snap_restoreval(jit_State *J, GCtrace *T, ExitState *ex,
			    SnapNo snapno, BloomFilter rfilt,
			    IRRef ref, TValue *o)
{
  IRIns *ir = &T->ir[ref];
  IRType1 t = ir->t;
  RegSP rs = ir->prev;
  if (irref_isk(ref)) {  /* Restore constant slot. */
    lj_ir_kvalue(J->L, o, ir);
    return;
  }
  if (LJ_UNLIKELY(bloomtest(rfilt, ref)))
    rs = snap_renameref(T, snapno, ref, rs);
  if (ra_hasspill(regsp_spill(rs))) {  /* Restore from spill slot. */
    int32_t *sps = &ex->spill[regsp_spill(rs)];
    if (irt_isinteger(t)) {
      setintV(o, *sps);
#if !LJ_SOFTFP
    } else if (irt_isnum(t)) {
      o->u64 = *(uint64_t *)sps;
#endif
    } else if (LJ_64 && irt_islightud(t)) {
      /* 64 bit lightuserdata which may escape already has the tag bits. */
      o->u64 = *(uint64_t *)sps;
    } else {
      lua_assert(!irt_ispri(t));  /* PRI refs never have a spill slot. */
      setgcrefi(o->gcr, *sps);
      setitype(o, irt_toitype(t));
    }
  } else {  /* Restore from register. */
    Reg r = regsp_reg(rs);
    lua_assert(ra_hasreg(r));
    if (irt_isinteger(t)) {
      setintV(o, (int32_t)ex->gpr[r-RID_MIN_GPR]);
#if !LJ_SOFTFP
    } else if (irt_isnum(t)) {
      setnumV(o, ex->fpr[r-RID_MIN_FPR]);
#endif
    } else if (LJ_64 && irt_islightud(t)) {
      /* 64 bit lightuserdata which may escape already has the tag bits. */
      o->u64 = ex->gpr[r-RID_MIN_GPR];
    } else {
      if (!irt_ispri(t))
	setgcrefi(o->gcr, ex->gpr[r-RID_MIN_GPR]);
      setitype(o, irt_toitype(t));
    }
  }
}

/* Restore interpreter state from exit state with the help of a snapshot. */
const BCIns *lj_snap_restore(jit_State *J, void *exptr)
{
  ExitState *ex = (ExitState *)exptr;
  SnapNo snapno = J->exitno;  /* For now, snapno == exitno. */
  GCtrace *T = traceref(J, J->parent);
  SnapShot *snap = &T->snap[snapno];
  MSize n, nent = snap->nent;
  SnapEntry *map = &T->snapmap[snap->mapofs];
  SnapEntry *flinks = &T->snapmap[snap_nextofs(T, snap)-1];
  int32_t ftsz0;
  TValue *frame;
  BloomFilter rfilt = snap_renamefilter(T, snapno);
  const BCIns *pc = snap_pc(map[nent]);
  lua_State *L = J->L;

  /* Set interpreter PC to the next PC to get correct error messages. */
  setcframe_pc(cframe_raw(L->cframe), pc+1);

  /* Make sure the stack is big enough for the slots from the snapshot. */
  if (LJ_UNLIKELY(L->base + snap->topslot >= tvref(L->maxstack))) {
    L->top = curr_topL(L);
    lj_state_growstack(L, snap->topslot - curr_proto(L)->framesize);
  }

  /* Fill stack slots with data from the registers and spill slots. */
  frame = L->base-1;
  ftsz0 = frame_ftsz(frame);  /* Preserve link to previous frame in slot #0. */
  for (n = 0; n < nent; n++) {
    SnapEntry sn = map[n];
    if (!(sn & SNAP_NORESTORE)) {
      TValue *o = &frame[snap_slot(sn)];
      snap_restoreval(J, T, ex, snapno, rfilt, snap_ref(sn), o);
      if (LJ_SOFTFP && (sn & SNAP_SOFTFPNUM) && tvisint(o)) {
	TValue tmp;
	snap_restoreval(J, T, ex, snapno, rfilt, snap_ref(sn)+1, &tmp);
	o->u32.hi = tmp.u32.lo;
      } else if ((sn & (SNAP_CONT|SNAP_FRAME))) {
	/* Overwrite tag with frame link. */
	o->fr.tp.ftsz = snap_slot(sn) != 0 ? (int32_t)*flinks-- : ftsz0;
	L->base = o+1;
      }
    }
  }
  lua_assert(map + nent == flinks);

  /* Compute current stack top. */
  switch (bc_op(*pc)) {
  case BC_CALLM: case BC_CALLMT: case BC_RETM: case BC_TSETM:
    L->top = frame + snap->nslots;
    break;
  default:
    L->top = curr_topL(L);
    break;
  }
  return pc;
}

#undef IR
#undef emitir_raw

#endif
