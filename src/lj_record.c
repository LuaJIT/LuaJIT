/*
** Trace recorder (bytecode -> SSA IR).
** Copyright (C) 2005-2010 Mike Pall. See Copyright Notice in luajit.h
*/

#define lj_record_c
#define LUA_CORE

#include "lj_obj.h"

#if LJ_HASJIT

#include "lj_gc.h"
#include "lj_err.h"
#include "lj_str.h"
#include "lj_tab.h"
#include "lj_state.h"
#include "lj_frame.h"
#include "lj_bc.h"
#include "lj_ff.h"
#include "lj_ir.h"
#include "lj_jit.h"
#include "lj_iropt.h"
#include "lj_trace.h"
#include "lj_record.h"
#include "lj_snap.h"
#include "lj_asm.h"
#include "lj_dispatch.h"
#include "lj_vm.h"

/* Some local macros to save typing. Undef'd at the end. */
#define IR(ref)			(&J->cur.ir[(ref)])

/* Pass IR on to next optimization in chain (FOLD). */
#define emitir(ot, a, b)	(lj_ir_set(J, (ot), (a), (b)), lj_opt_fold(J))

/* Emit raw IR without passing through optimizations. */
#define emitir_raw(ot, a, b)	(lj_ir_set(J, (ot), (a), (b)), lj_ir_emit(J))

/* Context for recording an indexed load/store. */
typedef struct RecordIndex {
  TValue tabv;		/* Runtime value of table (or indexed object). */
  TValue keyv;		/* Runtime value of key. */
  TValue valv;		/* Runtime value of stored value. */
  TValue mobjv;		/* Runtime value of metamethod object. */
  GCtab *mtv;		/* Runtime value of metatable object. */
  cTValue *oldv;	/* Runtime value of previously stored value. */
  TRef tab;		/* Table (or indexed object) reference. */
  TRef key;		/* Key reference. */
  TRef val;		/* Value reference for a store or 0 for a load. */
  TRef mt;		/* Metatable reference. */
  TRef mobj;		/* Metamethod object reference. */
  int idxchain;		/* Index indirections left or 0 for raw lookup. */
} RecordIndex;

/* Forward declarations. */
static int rec_mm_lookup(jit_State *J, RecordIndex *ix, MMS mm);
static TRef rec_idx(jit_State *J, RecordIndex *ix);

/* -- Sanity checks ------------------------------------------------------- */

#ifdef LUA_USE_ASSERT
/* Sanity check the whole IR -- sloooow. */
static void rec_check_ir(jit_State *J)
{
  IRRef i, nins = J->cur.nins, nk = J->cur.nk;
  lua_assert(nk <= REF_BIAS && nins >= REF_BIAS && nins < 65536);
  for (i = nins-1; i >= nk; i--) {
    IRIns *ir = IR(i);
    uint32_t mode = lj_ir_mode[ir->o];
    IRRef op1 = ir->op1;
    IRRef op2 = ir->op2;
    switch (irm_op1(mode)) {
    case IRMnone: lua_assert(op1 == 0); break;
    case IRMref: lua_assert(op1 >= nk);
      lua_assert(i >= REF_BIAS ? op1 < i : op1 > i); break;
    case IRMlit: break;
    case IRMcst: lua_assert(i < REF_BIAS); continue;
    }
    switch (irm_op2(mode)) {
    case IRMnone: lua_assert(op2 == 0); break;
    case IRMref: lua_assert(op2 >= nk);
      lua_assert(i >= REF_BIAS ? op2 < i : op2 > i); break;
    case IRMlit: break;
    case IRMcst: lua_assert(0); break;
    }
    if (ir->prev) {
      lua_assert(ir->prev >= nk);
      lua_assert(i >= REF_BIAS ? ir->prev < i : ir->prev > i);
      lua_assert(IR(ir->prev)->o == ir->o);
    }
  }
}

/* Compare stack slots and frames of the recorder and the VM. */
static void rec_check_slots(jit_State *J)
{
  BCReg s, nslots = J->baseslot + J->maxslot;
  int32_t depth = 0;
  cTValue *base = J->L->base - J->baseslot;
  lua_assert(J->baseslot >= 1 && J->baseslot < LJ_MAX_JSLOTS);
  lua_assert(J->baseslot == 1 || (J->slot[J->baseslot-1] & TREF_FRAME));
  lua_assert(nslots < LJ_MAX_JSLOTS);
  for (s = 0; s < nslots; s++) {
    TRef tr = J->slot[s];
    if (tr) {
      cTValue *tv = &base[s];
      IRRef ref = tref_ref(tr);
      IRIns *ir;
      lua_assert(ref >= J->cur.nk && ref < J->cur.nins);
      ir = IR(ref);
      lua_assert(irt_t(ir->t) == tref_t(tr));
      if (s == 0) {
	lua_assert(tref_isfunc(tr));
      } else if ((tr & TREF_FRAME)) {
	GCfunc *fn = gco2func(frame_gc(tv));
	BCReg delta = (BCReg)(tv - frame_prev(tv));
	lua_assert(tref_isfunc(tr));
	if (tref_isk(tr)) lua_assert(fn == ir_kfunc(ir));
	lua_assert(s > delta ? (J->slot[s-delta] & TREF_FRAME) : (s == delta));
	depth++;
      } else if ((tr & TREF_CONT)) {
	lua_assert(ir_kptr(ir) == gcrefp(tv->gcr, void));
	lua_assert((J->slot[s+1] & TREF_FRAME));
	depth++;
      } else {
	if (tvisnum(tv))
	  lua_assert(tref_isnumber(tr));  /* Could be IRT_INT etc., too. */
	else
	  lua_assert(itype2irt(tv) == tref_type(tr));
	if (tref_isk(tr)) {  /* Compare constants. */
	  TValue tvk;
	  lj_ir_kvalue(J->L, &tvk, ir);
	  lua_assert(lj_obj_equal(tv, &tvk));
	}
      }
    }
  }
  lua_assert(J->framedepth == depth);
}
#endif

/* -- Type handling and specialization ------------------------------------ */

/* Note: these functions return tagged references (TRef). */

/* Specialize a slot to a specific type. Note: slot can be negative! */
static TRef sloadt(jit_State *J, int32_t slot, IRType t, int mode)
{
  /* Caller may set IRT_GUARD in t. */
  TRef ref = emitir_raw(IRT(IR_SLOAD, t), (int32_t)J->baseslot+slot, mode);
  J->base[slot] = ref;
  return ref;
}

/* Specialize a slot to the runtime type. Note: slot can be negative! */
static TRef sload(jit_State *J, int32_t slot)
{
  IRType t = itype2irt(&J->L->base[slot]);
  TRef ref = emitir_raw(IRTG(IR_SLOAD, t), (int32_t)J->baseslot+slot,
			IRSLOAD_TYPECHECK);
  if (irtype_ispri(t)) ref = TREF_PRI(t);  /* Canonicalize primitive refs. */
  J->base[slot] = ref;
  return ref;
}

/* Get TRef from slot. Load slot and specialize if not done already. */
#define getslot(J, s)	(J->base[(s)] ? J->base[(s)] : sload(J, (int32_t)(s)))

/* Get TRef for current function. */
static TRef getcurrf(jit_State *J)
{
  if (J->base[-1])
    return J->base[-1];
  lua_assert(J->baseslot == 1);
  return sloadt(J, -1, IRT_FUNC, IRSLOAD_READONLY);
}

/* Compare for raw object equality.
** Returns 0 if the objects are the same.
** Returns 1 if they are different, but the same type.
** Returns 2 for two different types.
** Comparisons between primitives always return 1 -- no caller cares about it.
*/
static int rec_objcmp(jit_State *J, TRef a, TRef b, cTValue *av, cTValue *bv)
{
  int diff = !lj_obj_equal(av, bv);
  if (!tref_isk2(a, b)) {  /* Shortcut, also handles primitives. */
    IRType ta = tref_isinteger(a) ? IRT_INT : tref_type(a);
    IRType tb = tref_isinteger(b) ? IRT_INT : tref_type(b);
    if (ta != tb) {
      /* Widen mixed number/int comparisons to number/number comparison. */
      if (ta == IRT_INT && tb == IRT_NUM) {
	a = emitir(IRTN(IR_TONUM), a, 0);
	ta = IRT_NUM;
      } else if (ta == IRT_NUM && tb == IRT_INT) {
	b = emitir(IRTN(IR_TONUM), b, 0);
      } else {
	return 2;  /* Two different types are never equal. */
      }
    }
    emitir(IRTG(diff ? IR_NE : IR_EQ, ta), a, b);
  }
  return diff;
}

/* -- Record loop ops ----------------------------------------------------- */

/* Loop event. */
typedef enum {
  LOOPEV_LEAVE,		/* Loop is left or not entered. */
  LOOPEV_ENTER		/* Loop is entered. */
} LoopEvent;

/* Canonicalize slots: convert integers to numbers. */
static void canonicalize_slots(jit_State *J)
{
  BCReg s;
  for (s = J->baseslot+J->maxslot-1; s >= 1; s--) {
    TRef tr = J->slot[s];
    if (tref_isinteger(tr)) {
      IRIns *ir = IR(tref_ref(tr));
      if (!(ir->o == IR_SLOAD && (ir->op2 & IRSLOAD_READONLY)))
	J->slot[s] = emitir(IRTN(IR_TONUM), tr, 0);
    }
  }
}

/* Stop recording. */
static void rec_stop(jit_State *J, TraceNo lnk)
{
  lj_trace_end(J);
  J->cur.link = (uint16_t)lnk;
  /* Looping back at the same stack level? */
  if (lnk == J->curtrace && J->framedepth + J->retdepth == 0) {
    if ((J->flags & JIT_F_OPT_LOOP))  /* Shall we try to create a loop? */
      goto nocanon;  /* Do not canonicalize or we lose the narrowing. */
    if (J->cur.root)  /* Otherwise ensure we always link to the root trace. */
      J->cur.link = J->cur.root;
  }
  canonicalize_slots(J);
nocanon:
  /* Note: all loop ops must set J->pc to the following instruction! */
  lj_snap_add(J);  /* Add loop snapshot. */
  J->needsnap = 0;
  J->mergesnap = 1;  /* In case recording continues. */
}

/* Search bytecode backwards for a int/num constant slot initializer. */
static TRef find_kinit(jit_State *J, const BCIns *endpc, BCReg slot, IRType t)
{
  /* This algorithm is rather simplistic and assumes quite a bit about
  ** how the bytecode is generated. It works fine for FORI initializers,
  ** but it won't necessarily work in other cases (e.g. iterator arguments).
  ** It doesn't do anything fancy, either (like backpropagating MOVs).
  */
  const BCIns *pc, *startpc = proto_bc(J->pt);
  for (pc = endpc-1; pc > startpc; pc--) {
    BCIns ins = *pc;
    BCOp op = bc_op(ins);
    /* First try to find the last instruction that stores to this slot. */
    if (bcmode_a(op) == BCMbase && bc_a(ins) <= slot) {
      return 0;  /* Multiple results, e.g. from a CALL or KNIL. */
    } else if (bcmode_a(op) == BCMdst && bc_a(ins) == slot) {
      if (op == BC_KSHORT || op == BC_KNUM) {  /* Found const. initializer. */
	/* Now try to verify there's no forward jump across it. */
	const BCIns *kpc = pc;
	for ( ; pc > startpc; pc--)
	  if (bc_op(*pc) == BC_JMP) {
	    const BCIns *target = pc+bc_j(*pc)+1;
	    if (target > kpc && target <= endpc)
	      return 0;  /* Conditional assignment. */
	  }
	if (op == BC_KSHORT) {
	  int32_t k = (int32_t)(int16_t)bc_d(ins);
	  return t == IRT_INT ? lj_ir_kint(J, k) : lj_ir_knum(J, cast_num(k));
	} else {
	  lua_Number n = proto_knum(J->pt, bc_d(ins));
	  if (t == IRT_INT) {
	    int32_t k = lj_num2int(n);
	    if (n == cast_num(k))  /* -0 is ok here. */
	      return lj_ir_kint(J, k);
	    return 0;  /* Type mismatch. */
	  } else {
	    return lj_ir_knum(J, n);
	  }
	}
      }
      return 0;  /* Non-constant initializer. */
    }
  }
  return 0;  /* No assignment to this slot found? */
}

/* Peek before FORI to find a const initializer. Otherwise load from slot. */
static TRef fori_arg(jit_State *J, const BCIns *fori, BCReg slot, IRType t)
{
  TRef tr = J->base[slot];
  if (!tr) {
    tr = find_kinit(J, fori, slot, t);
    if (!tr) {
      if (t == IRT_INT)
	t |= IRT_GUARD;
      tr = sloadt(J, (int32_t)slot, t, IRSLOAD_READONLY|IRSLOAD_INHERIT);
    }
  }
  return tr;
}

/* In-place coercion of FORI arguments. */
static lua_Number for_coerce(jit_State *J, TValue *o)
{
  if (!tvisnum(o) && !(tvisstr(o) && lj_str_tonum(strV(o), o)))
    lj_trace_err(J, LJ_TRERR_BADTYPE);
  return numV(o);
}

/* Simulate the runtime behavior of the FOR loop iterator.
** It's important to exactly reproduce the semantics of the interpreter.
*/
static LoopEvent for_iter(jit_State *J, IROp *op, BCReg ra, int isforl)
{
  TValue *forbase = &J->L->base[ra];
  lua_Number stopv = for_coerce(J, &forbase[FORL_STOP]);
  lua_Number idxv = for_coerce(J, &forbase[FORL_IDX]);
  lua_Number stepv = for_coerce(J, &forbase[FORL_STEP]);
  if (isforl)
    idxv += stepv;
  if ((int32_t)forbase[FORL_STEP].u32.hi >= 0) {
    if (idxv <= stopv) { *op = IR_LE; return LOOPEV_ENTER; }
    *op = IR_GT; return LOOPEV_LEAVE;
  } else {
    if (stopv <= idxv) { *op = IR_GE; return LOOPEV_ENTER; }
    *op = IR_LT; return LOOPEV_LEAVE;
  }
}

/* Record FORL/JFORL or FORI/JFORI. */
static LoopEvent rec_for(jit_State *J, const BCIns *fori, int isforl)
{
  BCReg ra = bc_a(*fori);
  IROp op;
  LoopEvent ev = for_iter(J, &op, ra, isforl);
  TRef *tr = &J->base[ra];
  TRef idx, stop;
  IRType t;
  if (isforl) {  /* Handle FORL/JFORL opcodes. */
    TRef step;
    idx = tr[FORL_IDX];
    if (tref_ref(idx) == J->scev.idx) {
      t = J->scev.t.irt;
      stop = J->scev.stop;
      step = J->scev.step;
    } else {
      if (!idx) idx = sloadt(J, (int32_t)(ra+FORL_IDX), IRT_NUM, 0);
      t = tref_type(idx);
      stop = fori_arg(J, fori, ra+FORL_STOP, t);
      step = fori_arg(J, fori, ra+FORL_STEP, t);
    }
    tr[FORL_IDX] = idx = emitir(IRT(IR_ADD, t), idx, step);
  } else {  /* Handle FORI/JFORI opcodes. */
    BCReg i;
    t = IRT_NUM;
    for (i = FORL_IDX; i <= FORL_STEP; i++) {
      lua_assert(J->base[ra+i] != 0);  /* Assumes the slots are already set. */
      tr[i] = lj_ir_tonum(J, J->base[ra+i]);
    }
    idx = tr[FORL_IDX];
    stop = tr[FORL_STOP];
    if (!tref_isk(tr[FORL_STEP]))  /* Non-const step: need direction guard. */
      emitir(IRTG(((op-IR_LT)>>1)+IR_LT, IRT_NUM),
	     tr[FORL_STEP], lj_ir_knum_zero(J));
  }

  tr[FORL_EXT] = idx;
  if (ev == LOOPEV_LEAVE) {
    J->maxslot = ra+FORL_EXT+1;
    J->pc = fori+1;
  } else {
    J->maxslot = ra;
    J->pc = fori+bc_j(*fori)+1;
  }
  lj_snap_add(J);

  emitir(IRTG(op, t), idx, stop);

  if (ev == LOOPEV_LEAVE) {
    J->maxslot = ra;
    J->pc = fori+bc_j(*fori)+1;
  } else {
    J->maxslot = ra+FORL_EXT+1;
    J->pc = fori+1;
  }
  J->needsnap = 1;
  return ev;
}

/* Record ITERL/JITERL. */
static LoopEvent rec_iterl(jit_State *J, const BCIns iterins)
{
  BCReg ra = bc_a(iterins);
  lua_assert(J->base[ra] != 0);
  if (!tref_isnil(J->base[ra])) {  /* Looping back? */
    J->base[ra-1] = J->base[ra];  /* Copy result of ITERC to control var. */
    J->maxslot = ra-1+bc_b(J->pc[-1]);
    J->pc += bc_j(iterins)+1;
    return LOOPEV_ENTER;
  } else {
    J->maxslot = ra-3;
    J->pc++;
    return LOOPEV_LEAVE;
  }
}

/* Record LOOP/JLOOP. Now, that was easy. */
static LoopEvent rec_loop(jit_State *J, BCReg ra)
{
  J->maxslot = ra;
  J->pc++;
  return LOOPEV_ENTER;
}

/* Check if a loop repeatedly failed to trace because it didn't loop back. */
static int innerloopleft(jit_State *J, const BCIns *pc)
{
  ptrdiff_t i;
  for (i = 0; i < PENALTY_SLOTS; i++)
    if (mref(J->penalty[i].pc, const BCIns) == pc) {
      if (J->penalty[i].reason == LJ_TRERR_LLEAVE &&
	  J->penalty[i].val >= 2*PENALTY_MIN)
	return 1;
      break;
    }
  return 0;
}

/* Handle the case when an interpreted loop op is hit. */
static void rec_loop_interp(jit_State *J, const BCIns *pc, LoopEvent ev)
{
  if (J->parent == 0) {
    if (pc == J->startpc && J->framedepth + J->retdepth == 0) {
      /* Same loop? */
      if (ev == LOOPEV_LEAVE)  /* Must loop back to form a root trace. */
	lj_trace_err(J, LJ_TRERR_LLEAVE);
      rec_stop(J, J->curtrace);  /* Root trace forms a loop. */
    } else if (ev != LOOPEV_LEAVE) {  /* Entering inner loop? */
      /* It's usually better to abort here and wait until the inner loop
      ** is traced. But if the inner loop repeatedly didn't loop back,
      ** this indicates a low trip count. In this case try unrolling
      ** an inner loop even in a root trace. But it's better to be a bit
      ** more conservative here and only do it for very short loops.
      */
      if (!innerloopleft(J, pc))
	lj_trace_err(J, LJ_TRERR_LINNER);  /* Root trace hit an inner loop. */
      if ((J->loopref && J->cur.nins - J->loopref > 8) || --J->loopunroll < 0)
	lj_trace_err(J, LJ_TRERR_LUNROLL);  /* Limit loop unrolling. */
      J->loopref = J->cur.nins;
    }
  } else if (ev != LOOPEV_LEAVE) {  /* Side trace enters an inner loop. */
    J->loopref = J->cur.nins;
    if (--J->loopunroll < 0)
      lj_trace_err(J, LJ_TRERR_LUNROLL);  /* Limit loop unrolling. */
  }  /* Side trace continues across a loop that's left or not entered. */
}

/* Handle the case when an already compiled loop op is hit. */
static void rec_loop_jit(jit_State *J, TraceNo lnk, LoopEvent ev)
{
  if (J->parent == 0) {  /* Root trace hit an inner loop. */
    /* Better let the inner loop spawn a side trace back here. */
    lj_trace_err(J, LJ_TRERR_LINNER);
  } else if (ev != LOOPEV_LEAVE) {  /* Side trace enters a compiled loop. */
    J->instunroll = 0;  /* Cannot continue across a compiled loop op. */
    if (J->pc == J->startpc && J->framedepth + J->retdepth == 0)
      lnk = J->curtrace;  /* Can form an extra loop. */
    rec_stop(J, lnk);  /* Link to the loop. */
  }  /* Side trace continues across a loop that's left or not entered. */
}

/* -- Record calls and returns -------------------------------------------- */

/* Record call setup. */
static void rec_call_setup(jit_State *J, BCReg func, ptrdiff_t nargs)
{
  RecordIndex ix;
  TValue *functv = &J->L->base[func];
  TRef trfunc, *fbase = &J->base[func];
  ptrdiff_t i;
  for (i = 0; i <= nargs; i++)
    getslot(J, func+i);  /* Ensure func and all args have a reference. */
  if (!tref_isfunc(fbase[0])) {  /* Resolve __call metamethod. */
    ix.tab = fbase[0];
    copyTV(J->L, &ix.tabv, functv);
    if (!rec_mm_lookup(J, &ix, MM_call) || !tref_isfunc(ix.mobj))
      lj_trace_err(J, LJ_TRERR_NOMM);
    for (i = ++nargs; i > 0; i--)  /* Shift arguments up. */
      fbase[i] = fbase[i-1];
    fbase[0] = ix.mobj;  /* Replace function. */
    functv = &ix.mobjv;
  }

  /* Specialize to the runtime value of the called function. */
  trfunc = lj_ir_kfunc(J, funcV(functv));
  emitir(IRTG(IR_EQ, IRT_FUNC), fbase[0], trfunc);
  fbase[0] = trfunc | TREF_FRAME;
  J->maxslot = (BCReg)nargs;
}

/* Record call. */
static void rec_call(jit_State *J, BCReg func, ptrdiff_t nargs)
{
  rec_call_setup(J, func, nargs);
  /* Bump frame. */
  J->framedepth++;
  J->base += func+1;
  J->baseslot += func+1;
}

/* Record tail call. */
static void rec_tailcall(jit_State *J, BCReg func, ptrdiff_t nargs)
{
  rec_call_setup(J, func, nargs);
  /* Move func + args down. */
  memmove(&J->base[-1], &J->base[func], sizeof(TRef)*(J->maxslot+1));
  /* Note: the new TREF_FRAME is now at J->base[-1] (even for slot #0). */
  /* Tailcalls can form a loop, so count towards the loop unroll limit. */
  if (++J->tailcalled > J->loopunroll)
    lj_trace_err(J, LJ_TRERR_LUNROLL);
}

/* Check unroll limits for down-recursion. */
static int check_downrec_unroll(jit_State *J, GCproto *pt)
{
  IRRef ptref;
  for (ptref = J->chain[IR_KGC]; ptref; ptref = IR(ptref)->prev)
    if (ir_kgc(IR(ptref)) == obj2gco(pt)) {
      int count = 0;
      IRRef ref;
      for (ref = J->chain[IR_RETF]; ref; ref = IR(ref)->prev)
	if (IR(ref)->op1 == ptref)
	  count++;
      if (count) {
	if (J->pc == J->startpc) {
	  if (count + J->tailcalled > J->param[JIT_P_recunroll])
	    return 1;
	} else {
	  lj_trace_err(J, LJ_TRERR_DOWNREC);
	}
      }
    }
  return 0;
}

/* Record return. */
static void rec_ret(jit_State *J, BCReg rbase, ptrdiff_t gotresults)
{
  TValue *frame = J->L->base - 1;
  ptrdiff_t i;
  for (i = 0; i < gotresults; i++)
    getslot(J, rbase+i);  /* Ensure all results have a reference. */
  while (frame_ispcall(frame)) {  /* Immediately resolve pcall() returns. */
    BCReg cbase = (BCReg)frame_delta(frame);
    if (--J->framedepth < 0)
      lj_trace_err(J, LJ_TRERR_NYIRETL);
    lua_assert(J->baseslot > 1);
    gotresults++;
    rbase += cbase;
    J->baseslot -= (BCReg)cbase;
    J->base -= cbase;
    J->base[--rbase] = TREF_TRUE;  /* Prepend true to results. */
    frame = frame_prevd(frame);
  }
  if (frame_islua(frame)) {  /* Return to Lua frame. */
    BCIns callins = *(frame_pc(frame)-1);
    ptrdiff_t nresults = bc_b(callins) ? (ptrdiff_t)bc_b(callins)-1 :gotresults;
    BCReg cbase = bc_a(callins);
    GCproto *pt = funcproto(frame_func(frame - (cbase+1)));
    if (J->framedepth == 0 && J->pt && frame == J->L->base - 1) {
      if (check_downrec_unroll(J, pt)) {
	J->maxslot = (BCReg)(rbase + nresults);
	rec_stop(J, J->curtrace);  /* Down-recursion. */
	return;
      }
      lj_snap_add(J);
    }
    for (i = 0; i < nresults; i++)  /* Adjust results. */
      J->base[i-1] = i < gotresults ? J->base[rbase+i] : TREF_NIL;
    J->maxslot = cbase+(BCReg)nresults;
    if (J->framedepth > 0) {  /* Return to a frame that is part of the trace. */
      J->framedepth--;
      lua_assert(J->baseslot > cbase+1);
      J->baseslot -= cbase+1;
      J->base -= cbase+1;
    } else if (J->parent == 0 && !bc_isret(bc_op(J->cur.startins))) {
      /* Return to lower frame would leave the loop in a root trace. */
      lj_trace_err(J, LJ_TRERR_LLEAVE);
    } else {  /* Return to lower frame. Guard for the target we return to. */
      TRef trpt = lj_ir_kgc(J, obj2gco(pt), IRT_PROTO);
      TRef trpc = lj_ir_kptr(J, (void *)frame_pc(frame));
      emitir(IRTG(IR_RETF, IRT_PTR), trpt, trpc);
      J->retdepth++;
      J->needsnap = 1;
      lua_assert(J->baseslot == 1);
      /* Shift result slots up and clear the slots of the new frame below. */
      memmove(J->base + cbase, J->base-1, sizeof(TRef)*nresults);
      memset(J->base-1, 0, sizeof(TRef)*(cbase+1));
    }
  } else if (frame_iscont(frame)) {  /* Return to continuation frame. */
    ASMFunction cont = frame_contf(frame);
    BCReg cbase = (BCReg)frame_delta(frame);
    if ((J->framedepth -= 2) < 0)
      lj_trace_err(J, LJ_TRERR_NYIRETL);
    J->baseslot -= (BCReg)cbase;
    J->base -= cbase;
    J->maxslot = cbase-2;
    if (cont == lj_cont_ra) {
      /* Copy result to destination slot. */
      BCReg dst = bc_a(*(frame_contpc(frame)-1));
      J->base[dst] = gotresults ? J->base[cbase+rbase] : TREF_NIL;
      if (dst > J->maxslot) J->maxslot = dst+1;
    } else if (cont == lj_cont_nop) {
      /* Nothing to do here. */
    } else if (cont == lj_cont_cat) {
      lua_assert(0);
    } else {
      /* Result type already specialized. */
      lua_assert(cont == lj_cont_condf || cont == lj_cont_condt);
    }
  } else {
    lj_trace_err(J, LJ_TRERR_NYIRETL);  /* NYI: handle return to C frame. */
  }
  lua_assert(J->baseslot >= 1);
}

/* -- Metamethod handling ------------------------------------------------- */

/* Prepare to record call to metamethod. */
static BCReg rec_mm_prep(jit_State *J, ASMFunction cont)
{
  BCReg s, top = curr_proto(J->L)->framesize;
  TRef trcont;
  setcont(&J->L->base[top], cont);
#if LJ_64
  trcont = lj_ir_kptr(J, (void *)((int64_t)cont - (int64_t)lj_vm_asm_begin));
#else
  trcont = lj_ir_kptr(J, (void *)cont);
#endif
  J->base[top] = trcont | TREF_CONT;
  J->framedepth++;
  for (s = J->maxslot; s < top; s++)
    J->base[s] = 0;  /* Clear frame gap to avoid resurrecting previous refs. */
  return top+1;
}

/* Record metamethod lookup. */
static int rec_mm_lookup(jit_State *J, RecordIndex *ix, MMS mm)
{
  RecordIndex mix;
  GCtab *mt;
  if (tref_istab(ix->tab)) {
    mt = tabref(tabV(&ix->tabv)->metatable);
    mix.tab = emitir(IRT(IR_FLOAD, IRT_TAB), ix->tab, IRFL_TAB_META);
  } else if (tref_isudata(ix->tab)) {
    mt = tabref(udataV(&ix->tabv)->metatable);
    mix.tab = emitir(IRT(IR_FLOAD, IRT_TAB), ix->tab, IRFL_UDATA_META);
  } else {
    /* Specialize to base metatable. Must flush mcode in lua_setmetatable(). */
    mt = tabref(basemt_obj(J2G(J), &ix->tabv));
    if (mt == NULL) {
      ix->mt = TREF_NIL;
      return 0;  /* No metamethod. */
    }
    ix->mt = mix.tab = lj_ir_ktab(J, mt);
    goto nocheck;
  }
  ix->mt = mt ? mix.tab : TREF_NIL;
  emitir(IRTG(mt ? IR_NE : IR_EQ, IRT_TAB), mix.tab, lj_ir_knull(J, IRT_TAB));
nocheck:
  if (mt) {
    GCstr *mmstr = strref(J2G(J)->mmname[mm]);
    cTValue *mo = lj_tab_getstr(mt, mmstr);
    if (mo && !tvisnil(mo))
      copyTV(J->L, &ix->mobjv, mo);
    ix->mtv = mt;
    settabV(J->L, &mix.tabv, mt);
    if (isdead(J2G(J), obj2gco(mmstr)))
      flipwhite(obj2gco(mmstr));  /* Need same logic as lj_str_new(). */
    setstrV(J->L, &mix.keyv, mmstr);
    mix.key = lj_ir_kstr(J, mmstr);
    mix.val = 0;
    mix.idxchain = 0;
    ix->mobj = rec_idx(J, &mix);
    return !tref_isnil(ix->mobj);  /* 1 if metamethod found, 0 if not. */
  }
  return 0;  /* No metamethod. */
}

/* Record call to arithmetic metamethod (and MM_len). */
static TRef rec_mm_arith(jit_State *J, RecordIndex *ix, MMS mm)
{
  /* Set up metamethod call first to save ix->tab and ix->tabv. */
  BCReg func = rec_mm_prep(J, lj_cont_ra);
  TRef *base = J->base + func;
  TValue *basev = J->L->base + func;
  base[1] = ix->tab; base[2] = ix->key;
  copyTV(J->L, basev+1, &ix->tabv);
  copyTV(J->L, basev+2, &ix->keyv);
  if (!rec_mm_lookup(J, ix, mm)) {  /* Lookup metamethod on 1st operand. */
    if (mm != MM_len) {
      ix->tab = ix->key;
      copyTV(J->L, &ix->tabv, &ix->keyv);
      if (rec_mm_lookup(J, ix, mm))  /* Lookup metamethod on 2nd operand. */
	goto ok;
    }
    lj_trace_err(J, LJ_TRERR_NOMM);
  }
ok:
  base[0] = ix->mobj;
  copyTV(J->L, basev+0, &ix->mobjv);
  rec_call(J, func, 2);
  return 0;  /* No result yet. */
}

/* Call a comparison metamethod. */
static void rec_mm_callcomp(jit_State *J, RecordIndex *ix, int op)
{
  BCReg func = rec_mm_prep(J, (op&1) ? lj_cont_condf : lj_cont_condt);
  TRef *base = J->base + func;
  TValue *tv = J->L->base + func;
  base[0] = ix->mobj; base[1] = ix->val; base[2] = ix->key;
  copyTV(J->L, tv+0, &ix->mobjv);
  copyTV(J->L, tv+1, &ix->valv);
  copyTV(J->L, tv+2, &ix->keyv);
  rec_call(J, func, 2);
}

/* Record call to equality comparison metamethod (for tab and udata only). */
static void rec_mm_equal(jit_State *J, RecordIndex *ix, int op)
{
  ix->tab = ix->val;
  copyTV(J->L, &ix->tabv, &ix->valv);
  if (rec_mm_lookup(J, ix, MM_eq)) {  /* Lookup metamethod on 1st operand. */
    cTValue *bv;
    TRef mo1 = ix->mobj;
    TValue mo1v;
    copyTV(J->L, &mo1v, &ix->mobjv);
    /* Avoid the 2nd lookup and the objcmp if the metatables are equal. */
    bv = &ix->keyv;
    if (tvistab(bv) && tabref(tabV(bv)->metatable) == ix->mtv) {
      TRef mt2 = emitir(IRT(IR_FLOAD, IRT_TAB), ix->key, IRFL_TAB_META);
      emitir(IRTG(IR_EQ, IRT_TAB), mt2, ix->mt);
    } else if (tvisudata(bv) && tabref(udataV(bv)->metatable) == ix->mtv) {
      TRef mt2 = emitir(IRT(IR_FLOAD, IRT_TAB), ix->key, IRFL_UDATA_META);
      emitir(IRTG(IR_EQ, IRT_TAB), mt2, ix->mt);
    } else {  /* Lookup metamethod on 2nd operand and compare both. */
      ix->tab = ix->key;
      copyTV(J->L, &ix->tabv, bv);
      if (!rec_mm_lookup(J, ix, MM_eq) ||
	  rec_objcmp(J, mo1, ix->mobj, &mo1v, &ix->mobjv))
	return;
    }
    rec_mm_callcomp(J, ix, op);
  }
}

/* Record call to ordered comparison metamethods (for arbitrary objects). */
static void rec_mm_comp(jit_State *J, RecordIndex *ix, int op)
{
  ix->tab = ix->val;
  copyTV(J->L, &ix->tabv, &ix->valv);
  while (1) {
    MMS mm = (op & 2) ? MM_le : MM_lt;  /* Try __le + __lt or only __lt. */
    if (rec_mm_lookup(J, ix, mm)) {  /* Lookup metamethod on 1st operand. */
      cTValue *bv;
      TRef mo1 = ix->mobj;
      TValue mo1v;
      copyTV(J->L, &mo1v, &ix->mobjv);
      /* Avoid the 2nd lookup and the objcmp if the metatables are equal. */
      bv = &ix->keyv;
      if (tvistab(bv) && tabref(tabV(bv)->metatable) == ix->mtv) {
	TRef mt2 = emitir(IRT(IR_FLOAD, IRT_TAB), ix->key, IRFL_TAB_META);
	emitir(IRTG(IR_EQ, IRT_TAB), mt2, ix->mt);
      } else if (tvisudata(bv) && tabref(udataV(bv)->metatable) == ix->mtv) {
	TRef mt2 = emitir(IRT(IR_FLOAD, IRT_TAB), ix->key, IRFL_UDATA_META);
	emitir(IRTG(IR_EQ, IRT_TAB), mt2, ix->mt);
      } else {  /* Lookup metamethod on 2nd operand and compare both. */
	ix->tab = ix->key;
	copyTV(J->L, &ix->tabv, bv);
	if (!rec_mm_lookup(J, ix, mm) ||
	    rec_objcmp(J, mo1, ix->mobj, &mo1v, &ix->mobjv))
	  goto nomatch;
      }
      rec_mm_callcomp(J, ix, op);
      return;
    }
  nomatch:
    /* First lookup failed. Retry with  __lt and swapped operands. */
    if (!(op & 2)) break;  /* Already at __lt. Interpreter will throw. */
    ix->tab = ix->key; ix->key = ix->val; ix->val = ix->tab;
    copyTV(J->L, &ix->tabv, &ix->keyv);
    copyTV(J->L, &ix->keyv, &ix->valv);
    copyTV(J->L, &ix->valv, &ix->tabv);
    op ^= 3;
  }
}

/* -- Indexed access ------------------------------------------------------ */

/* Record bounds-check. */
static void rec_idx_abc(jit_State *J, TRef asizeref, TRef ikey, uint32_t asize)
{
  /* Try to emit invariant bounds checks. */
  if ((J->flags & (JIT_F_OPT_LOOP|JIT_F_OPT_ABC)) ==
      (JIT_F_OPT_LOOP|JIT_F_OPT_ABC)) {
    IRRef ref = tref_ref(ikey);
    IRIns *ir = IR(ref);
    int32_t ofs = 0;
    IRRef ofsref = 0;
    /* Handle constant offsets. */
    if (ir->o == IR_ADD && irref_isk(ir->op2)) {
      ofsref = ir->op2;
      ofs = IR(ofsref)->i;
      ref = ir->op1;
      ir = IR(ref);
    }
    /* Got scalar evolution analysis results for this reference? */
    if (ref == J->scev.idx) {
      int32_t stop;
      lua_assert(irt_isint(J->scev.t) && ir->o == IR_SLOAD);
      stop = lj_num2int(numV(&(J->L->base - J->baseslot)[ir->op1 + FORL_STOP]));
      /* Runtime value for stop of loop is within bounds? */
      if ((int64_t)stop + ofs < (int64_t)asize) {
	/* Emit invariant bounds check for stop. */
	emitir(IRTG(IR_ABC, IRT_PTR), asizeref, ofs == 0 ? J->scev.stop :
	       emitir(IRTI(IR_ADD), J->scev.stop, ofsref));
	/* Emit invariant bounds check for start, if not const or negative. */
	if (!(J->scev.dir && J->scev.start &&
	      (int64_t)IR(J->scev.start)->i + ofs >= 0))
	  emitir(IRTG(IR_ABC, IRT_PTR), asizeref, ikey);
	return;
      }
    }
  }
  emitir(IRTGI(IR_ABC), asizeref, ikey);  /* Emit regular bounds check. */
}

/* Record indexed key lookup. */
static TRef rec_idx_key(jit_State *J, RecordIndex *ix)
{
  TRef key;
  GCtab *t = tabV(&ix->tabv);
  ix->oldv = lj_tab_get(J->L, t, &ix->keyv);  /* Lookup previous value. */

  /* Integer keys are looked up in the array part first. */
  key = ix->key;
  if (tref_isnumber(key)) {
    lua_Number n = numV(&ix->keyv);
    int32_t k = lj_num2int(n);
    lua_assert(tvisnum(&ix->keyv));
    /* Potential array key? */
    if ((MSize)k < LJ_MAX_ASIZE && n == cast_num(k)) {
      TRef asizeref, ikey = key;
      if (!tref_isinteger(ikey))
	ikey = emitir(IRTGI(IR_TOINT), ikey, IRTOINT_INDEX);
      asizeref = emitir(IRTI(IR_FLOAD), ix->tab, IRFL_TAB_ASIZE);
      if ((MSize)k < t->asize) {  /* Currently an array key? */
	TRef arrayref;
	rec_idx_abc(J, asizeref, ikey, t->asize);
	arrayref = emitir(IRT(IR_FLOAD, IRT_PTR), ix->tab, IRFL_TAB_ARRAY);
	return emitir(IRT(IR_AREF, IRT_PTR), arrayref, ikey);
      } else {  /* Currently not in array (may be an array extension)? */
	emitir(IRTGI(IR_ULE), asizeref, ikey);  /* Inv. bounds check. */
	if (k == 0 && tref_isk(key))
	  key = lj_ir_knum_zero(J);  /* Canonicalize 0 or +-0.0 to +0.0. */
	/* And continue with the hash lookup. */
      }
    } else if (!tref_isk(key)) {
      /* We can rule out const numbers which failed the integerness test
      ** above. But all other numbers are potential array keys.
      */
      if (t->asize == 0) {  /* True sparse tables have an empty array part. */
	/* Guard that the array part stays empty. */
	TRef tmp = emitir(IRTI(IR_FLOAD), ix->tab, IRFL_TAB_ASIZE);
	emitir(IRTGI(IR_EQ), tmp, lj_ir_kint(J, 0));
      } else {
	lj_trace_err(J, LJ_TRERR_NYITMIX);
      }
    }
  }

  /* Otherwise the key is located in the hash part. */
  if (tref_isinteger(key))  /* Hash keys are based on numbers, not ints. */
    ix->key = key = emitir(IRTN(IR_TONUM), key, 0);
  if (tref_isk(key)) {
    /* Optimize lookup of constant hash keys. */
    MSize hslot = (MSize)((char *)ix->oldv - (char *)&noderef(t->node)[0].val);
    if (t->hmask > 0 && hslot <= t->hmask*(MSize)sizeof(Node) &&
	hslot <= 65535*(MSize)sizeof(Node)) {
      TRef node, kslot;
      TRef hm = emitir(IRTI(IR_FLOAD), ix->tab, IRFL_TAB_HMASK);
      emitir(IRTGI(IR_EQ), hm, lj_ir_kint(J, (int32_t)t->hmask));
      node = emitir(IRT(IR_FLOAD, IRT_PTR), ix->tab, IRFL_TAB_NODE);
      kslot = lj_ir_kslot(J, key, hslot / sizeof(Node));
      return emitir(IRTG(IR_HREFK, IRT_PTR), node, kslot);
    }
  }
  /* Fall back to a regular hash lookup. */
  return emitir(IRT(IR_HREF, IRT_PTR), ix->tab, key);
}

/* Determine whether a key is NOT one of the fast metamethod names. */
static int nommstr(jit_State *J, TRef key)
{
  if (tref_isstr(key)) {
    if (tref_isk(key)) {
      GCstr *str = ir_kstr(IR(tref_ref(key)));
      uint32_t i;
      for (i = 0; i <= MM_FAST; i++)
	if (strref(J2G(J)->mmname[i]) == str)
	  return 0;  /* MUST be one the fast metamethod names. */
    } else {
      return 0;  /* Variable string key MAY be a metamethod name. */
    }
  }
  return 1;  /* CANNOT be a metamethod name. */
}

/* Record indexed load/store. */
static TRef rec_idx(jit_State *J, RecordIndex *ix)
{
  TRef xref;
  IROp xrefop, loadop;
  cTValue *oldv;

  while (!tref_istab(ix->tab)) { /* Handle non-table lookup. */
    lua_assert(ix->idxchain != 0); /* Never call raw rec_idx() on non-table. */
    if (!rec_mm_lookup(J, ix, ix->val ? MM_newindex : MM_index))
      lj_trace_err(J, LJ_TRERR_NOMM);
  handlemm:
    if (tref_isfunc(ix->mobj)) {  /* Handle metamethod call. */
      BCReg func = rec_mm_prep(J, ix->val ? lj_cont_nop : lj_cont_ra);
      TRef *base = J->base + func;
      TValue *tv = J->L->base + func;
      base[0] = ix->mobj; base[1] = ix->tab; base[2] = ix->key;
      setfuncV(J->L, tv+0, funcV(&ix->mobjv));
      copyTV(J->L, tv+1, &ix->tabv);
      copyTV(J->L, tv+2, &ix->keyv);
      if (ix->val) {
	base[3] = ix->val;
	copyTV(J->L, tv+3, &ix->valv);
	rec_call(J, func, 3);  /* mobj(tab, key, val) */
	return 0;
      } else {
	rec_call(J, func, 2);  /* res = mobj(tab, key) */
	return 0;  /* No result yet. */
      }
    }
    /* Otherwise retry lookup with metaobject. */
    ix->tab = ix->mobj;
    copyTV(J->L, &ix->tabv, &ix->mobjv);
    if (--ix->idxchain == 0)
      lj_trace_err(J, LJ_TRERR_IDXLOOP);
  }

  /* First catch nil and NaN keys for tables. */
  if (tvisnil(&ix->keyv) || (tvisnum(&ix->keyv) && tvisnan(&ix->keyv))) {
    if (ix->val)  /* Better fail early. */
      lj_trace_err(J, LJ_TRERR_STORENN);
    if (tref_isk(ix->key)) {
      if (ix->idxchain && rec_mm_lookup(J, ix, MM_index))
	goto handlemm;
      return TREF_NIL;
    }
  }

  /* Record the key lookup. */
  xref = rec_idx_key(J, ix);
  xrefop = IR(tref_ref(xref))->o;
  loadop = xrefop == IR_AREF ? IR_ALOAD : IR_HLOAD;
  oldv = ix->oldv;

  if (ix->val == 0) {  /* Indexed load */
    IRType t = itype2irt(oldv);
    TRef res;
    if (oldv == niltvg(J2G(J))) {
      emitir(IRTG(IR_EQ, IRT_PTR), xref, lj_ir_kptr(J, niltvg(J2G(J))));
      res = TREF_NIL;
    } else {
      res = emitir(IRTG(loadop, t), xref, 0);
    }
    if (t == IRT_NIL && ix->idxchain && rec_mm_lookup(J, ix, MM_index))
      goto handlemm;
    if (irtype_ispri(t)) res = TREF_PRI(t);  /* Canonicalize primitives. */
    return res;
  } else {  /* Indexed store. */
    GCtab *mt = tabref(tabV(&ix->tabv)->metatable);
    if (tvisnil(oldv)) {  /* Previous value was nil? */
      /* Need to duplicate the hasmm check for the early guards. */
      int hasmm = 0;
      if (ix->idxchain && mt) {
	cTValue *mo = lj_tab_getstr(mt, strref(J2G(J)->mmname[MM_newindex]));
	hasmm = mo && !tvisnil(mo);
      }
      if (hasmm)
	emitir(IRTG(loadop, IRT_NIL), xref, 0);  /* Guard for nil value. */
      else if (xrefop == IR_HREF)
	emitir(IRTG(oldv == niltvg(J2G(J)) ? IR_EQ : IR_NE, IRT_PTR), xref, lj_ir_kptr(J, niltvg(J2G(J))));
      if (ix->idxchain && rec_mm_lookup(J, ix, MM_newindex)) { /* Metamethod? */
	lua_assert(hasmm);
	goto handlemm;
      }
      lua_assert(!hasmm);
      if (oldv == niltvg(J2G(J))) {  /* Need to insert a new key. */
	TRef key = ix->key;
	if (tref_isinteger(key))  /* NEWREF needs a TValue as a key. */
	  key = emitir(IRTN(IR_TONUM), key, 0);
	xref = emitir(IRT(IR_NEWREF, IRT_PTR), ix->tab, key);
      }
    } else if (!lj_opt_fwd_wasnonnil(J, loadop, tref_ref(xref))) {
      /* Cannot derive that the previous value was non-nil, must do checks. */
      if (xrefop == IR_HREF)  /* Guard against store to niltv. */
	emitir(IRTG(IR_NE, IRT_PTR), xref, lj_ir_kptr(J, niltvg(J2G(J))));
      if (ix->idxchain) {  /* Metamethod lookup required? */
	/* A check for NULL metatable is cheaper (hoistable) than a load. */
	if (!mt) {
	  TRef mtref = emitir(IRT(IR_FLOAD, IRT_TAB), ix->tab, IRFL_TAB_META);
	  emitir(IRTG(IR_EQ, IRT_TAB), mtref, lj_ir_knull(J, IRT_TAB));
	} else {
	  IRType t = itype2irt(oldv);
	  emitir(IRTG(loadop, t), xref, 0);  /* Guard for non-nil value. */
	}
      }
    }
    if (tref_isinteger(ix->val))  /* Convert int to number before storing. */
      ix->val = emitir(IRTN(IR_TONUM), ix->val, 0);
    emitir(IRT(loadop+IRDELTA_L2S, tref_type(ix->val)), xref, ix->val);
    if (tref_isgcv(ix->val))
      emitir(IRT(IR_TBAR, IRT_NIL), ix->tab, 0);
    /* Invalidate neg. metamethod cache for stores with certain string keys. */
    if (!nommstr(J, ix->key)) {
      TRef fref = emitir(IRT(IR_FREF, IRT_PTR), ix->tab, IRFL_TAB_NOMM);
      emitir(IRT(IR_FSTORE, IRT_U8), fref, lj_ir_kint(J, 0));
    }
    J->needsnap = 1;
    return 0;
  }
}

/* -- Upvalue access ------------------------------------------------------ */

/* Shrink disambiguation hash into an 8 bit value. */
static uint32_t shrink_dhash(uint32_t lo, uint32_t hi)
{
  lo ^= hi; hi = lj_rol(hi, 14);
  lo -= hi; hi = lj_rol(hi, 5);
  hi ^= lo; hi -= lj_rol(lo, 27);
  return (hi & 0xff);
}

/* Record upvalue load/store. */
static TRef rec_upvalue(jit_State *J, uint32_t uv, TRef val)
{
  GCupval *uvp = &gcref(J->fn->l.uvptr[uv])->uv;
  TRef fn = getcurrf(J);
  IRRef uref;
  int needbarrier = 0;
  /* Note: this effectively limits LJ_MAX_UPVAL to 127. */
  uv = (uv << 8) | shrink_dhash(uvp->dhash, uvp->dhash-0x04c11db7);
  if (!uvp->closed) {
    /* In current stack? */
    if (uvval(uvp) >= J->L->stack && uvval(uvp) < J->L->maxstack) {
      int32_t slot = (int32_t)(uvval(uvp) - (J->L->base - J->baseslot));
      if (slot >= 0) {  /* Aliases an SSA slot? */
	slot -= (int32_t)J->baseslot;  /* Note: slot number may be negative! */
	/* NYI: add IR to guard that it's still aliasing the same slot. */
	if (val == 0) {
	  return getslot(J, slot);
	} else {
	  J->base[slot] = val;
	  if (slot >= (int32_t)J->maxslot) J->maxslot = (BCReg)(slot+1);
	  return 0;
	}
      }
    }
    uref = tref_ref(emitir(IRTG(IR_UREFO, IRT_PTR), fn, uv));
  } else {
    needbarrier = 1;
    uref = tref_ref(emitir(IRTG(IR_UREFC, IRT_PTR), fn, uv));
  }
  if (val == 0) {  /* Upvalue load */
    IRType t = itype2irt(uvval(uvp));
    TRef res = emitir(IRTG(IR_ULOAD, t), uref, 0);
    if (irtype_ispri(t)) res = TREF_PRI(t);  /* Canonicalize primitive refs. */
    return res;
  } else {  /* Upvalue store. */
    if (tref_isinteger(val))  /* Convert int to number before storing. */
      val = emitir(IRTN(IR_TONUM), val, 0);
    emitir(IRT(IR_USTORE, tref_type(val)), uref, val);
    if (needbarrier && tref_isgcv(val))
      emitir(IRT(IR_OBAR, IRT_NIL), uref, val);
    J->needsnap = 1;
    return 0;
  }
}

/* -- Fast function recording handlers ------------------------------------ */

/* Conventions for fast function call handlers:
**
** The argument slots start at J->base[0]. All of them are guaranteed to be
** valid and type-specialized references. J->base[J->maxslot] is set to 0
** as a sentinel. The runtime argument values start at rd->argv[0].
**
** In general fast functions should check for presence of all of their
** arguments and for the correct argument types. Some simplifications
** are allowed if the interpreter throws instead. But even if recording
** is aborted, the generated IR must be consistent (no zero-refs).
**
** The number of results in rd->nres is set to 1. Handlers that return
** a different number of results need to override it. A negative value
** prevents return processing (e.g. for pending calls).
**
** Results need to be stored starting at J->base[0]. Return processing
** moves them to the right slots later.
**
** The per-ffid auxiliary data is the value of the 2nd part of the
** LJLIB_REC() annotation. This allows handling similar functionality
** in a common handler.
*/

/* Data used by handlers to record a fast function. */
typedef struct RecordFFData {
  TValue *argv;		/* Runtime argument values. */
  ptrdiff_t nres;	/* Number of returned results (defaults to 1). */
  uint32_t data;	/* Per-ffid auxiliary data (opcode, literal etc.). */
} RecordFFData;

/* Type of handler to record a fast function. */
typedef void (LJ_FASTCALL *RecordFunc)(jit_State *J, RecordFFData *rd);

/* Get runtime value of int argument. */
static int32_t argv2int(jit_State *J, TValue *o)
{
  if (!tvisnum(o) && !(tvisstr(o) && lj_str_tonum(strV(o), o)))
    lj_trace_err(J, LJ_TRERR_BADTYPE);
  return lj_num2bit(numV(o));
}

/* Get runtime value of string argument. */
static GCstr *argv2str(jit_State *J, TValue *o)
{
  if (LJ_LIKELY(tvisstr(o))) {
    return strV(o);
  } else {
    GCstr *s;
    if (!tvisnum(o))
      lj_trace_err(J, LJ_TRERR_BADTYPE);
    s = lj_str_fromnum(J->L, &o->n);
    setstrV(J->L, o, s);
    return s;
  }
}

/* Return number of results wanted by caller. */
static ptrdiff_t results_wanted(jit_State *J)
{
  TValue *frame = J->L->base-1;
  if (frame_islua(frame))
    return (ptrdiff_t)bc_b(frame_pc(frame)[-1]) - 1;
  else
    return -1;
}

/* Throw error for unsupported variant of fast function. */
LJ_NORET static void recff_nyiu(jit_State *J)
{
  setfuncV(J->L, &J->errinfo, J->fn);
  lj_trace_err_info(J, LJ_TRERR_NYIFFU);
}

/* Fallback handler for all fast functions that are not recorded (yet). */
static void LJ_FASTCALL recff_nyi(jit_State *J, RecordFFData *rd)
{
  setfuncV(J->L, &J->errinfo, J->fn);
  lj_trace_err_info(J, LJ_TRERR_NYIFF);
  UNUSED(rd);
}

/* C functions can have arbitrary side-effects and are not recorded (yet). */
static void LJ_FASTCALL recff_c(jit_State *J, RecordFFData *rd)
{
  setfuncV(J->L, &J->errinfo, J->fn);
  lj_trace_err_info(J, LJ_TRERR_NYICF);
  UNUSED(rd);
}

/* -- Base library fast functions ----------------------------------------- */

static void LJ_FASTCALL recff_assert(jit_State *J, RecordFFData *rd)
{
  /* Arguments already specialized. The interpreter throws for nil/false. */
  rd->nres = J->maxslot;  /* Pass through all arguments. */
}

static void LJ_FASTCALL recff_type(jit_State *J, RecordFFData *rd)
{
  /* Arguments already specialized. Result is a constant string. Neat, huh? */
  IRType t = tref_isinteger(J->base[0]) ? IRT_NUM : tref_type(J->base[0]);
  J->base[0] = lj_ir_kstr(J, strV(&J->fn->c.upvalue[t]));
  UNUSED(rd);
}

static void LJ_FASTCALL recff_getmetatable(jit_State *J, RecordFFData *rd)
{
  TRef tr = J->base[0];
  if (tr) {
    RecordIndex ix;
    ix.tab = tr;
    copyTV(J->L, &ix.tabv, &rd->argv[0]);
    if (rec_mm_lookup(J, &ix, MM_metatable))
      J->base[0] = ix.mobj;
    else
      J->base[0] = ix.mt;
  }  /* else: Interpreter will throw. */
}

static void LJ_FASTCALL recff_setmetatable(jit_State *J, RecordFFData *rd)
{
  TRef tr = J->base[0];
  TRef mt = J->base[1];
  if (tref_istab(tr) && (tref_istab(mt) || (mt && tref_isnil(mt)))) {
    TRef fref, mtref;
    RecordIndex ix;
    ix.tab = tr;
    copyTV(J->L, &ix.tabv, &rd->argv[0]);
    rec_mm_lookup(J, &ix, MM_metatable); /* Guard for no __metatable field. */
    fref = emitir(IRT(IR_FREF, IRT_PTR), tr, IRFL_TAB_META);
    mtref = tref_isnil(mt) ? lj_ir_knull(J, IRT_TAB) : mt;
    emitir(IRT(IR_FSTORE, IRT_TAB), fref, mtref);
    if (!tref_isnil(mt))
      emitir(IRT(IR_TBAR, IRT_TAB), tr, 0);
    J->base[0] = tr;
    J->needsnap = 1;
  }  /* else: Interpreter will throw. */
}

static void LJ_FASTCALL recff_rawget(jit_State *J, RecordFFData *rd)
{
  RecordIndex ix;
  ix.tab = J->base[0]; ix.key = J->base[1];
  if (tref_istab(ix.tab) && ix.key) {
    ix.val = 0; ix.idxchain = 0;
    settabV(J->L, &ix.tabv, tabV(&rd->argv[0]));
    copyTV(J->L, &ix.keyv, &rd->argv[1]);
    J->base[0] = rec_idx(J, &ix);
  }  /* else: Interpreter will throw. */
}

static void LJ_FASTCALL recff_rawset(jit_State *J, RecordFFData *rd)
{
  RecordIndex ix;
  ix.tab = J->base[0]; ix.key = J->base[1]; ix.val = J->base[2];
  if (tref_istab(ix.tab) && ix.key && ix.val) {
    ix.idxchain = 0;
    settabV(J->L, &ix.tabv, tabV(&rd->argv[0]));
    copyTV(J->L, &ix.keyv, &rd->argv[1]);
    copyTV(J->L, &ix.valv, &rd->argv[2]);
    rec_idx(J, &ix);
    /* Pass through table at J->base[0] as result. */
  }  /* else: Interpreter will throw. */
}

static void LJ_FASTCALL recff_rawequal(jit_State *J, RecordFFData *rd)
{
  TRef tra = J->base[0];
  TRef trb = J->base[1];
  if (tra && trb) {
    int diff = rec_objcmp(J, tra, trb, &rd->argv[0], &rd->argv[1]);
    J->base[0] = diff ? TREF_FALSE : TREF_TRUE;
  }  /* else: Interpreter will throw. */
}

static void LJ_FASTCALL recff_tonumber(jit_State *J, RecordFFData *rd)
{
  TRef tr = J->base[0];
  if (tref_isnumber_str(tr)) {
    TRef base = J->base[1];
    if (base) {
      base = lj_ir_toint(J, base);
      if (!tref_isk(base) || IR(tref_ref(base))->i != 10)
	recff_nyiu(J);
    }
    if (tref_isstr(tr)) {
      TValue tmp;
      if (!lj_str_tonum(strV(&rd->argv[0]), &tmp))
	recff_nyiu(J);  /* Would need an inverted STRTO for this case. */
      tr = emitir(IRTG(IR_STRTO, IRT_NUM), tr, 0);
    }
  } else {
    tr = TREF_NIL;
  }
  J->base[0] = tr;
  UNUSED(rd);
}

static TValue *recff_tostring_cp(lua_State *L, lua_CFunction dummy, void *ud)
{
  jit_State *J = (jit_State *)ud;
  rec_tailcall(J, 0, 1);
  UNUSED(L); UNUSED(dummy);
  return NULL;
}

static void LJ_FASTCALL recff_tostring(jit_State *J, RecordFFData *rd)
{
  TRef tr = J->base[0];
  if (tref_isstr(tr)) {
    /* Ignore __tostring in the string base metatable. */
    /* Pass on result in J->base[0]. */
  } else {
    RecordIndex ix;
    ix.tab = tr;
    copyTV(J->L, &ix.tabv, &rd->argv[0]);
    if (rec_mm_lookup(J, &ix, MM_tostring)) {  /* Has __tostring metamethod? */
      int errcode;
      /* Temporarily insert metamethod below object. */
      J->base[1] = tr;
      J->base[0] = ix.mobj;
      copyTV(J->L, &rd->argv[1], &rd->argv[0]);
      copyTV(J->L, &rd->argv[0], &ix.mobjv);
      /* Need to protect rec_tailcall because it may throw. */
      errcode = lj_vm_cpcall(J->L, NULL, J, recff_tostring_cp);
      /* Always undo Lua stack changes to avoid confusing the interpreter. */
      copyTV(J->L, &rd->argv[0], &rd->argv[1]);
      if (errcode)
	lj_err_throw(J->L, errcode);  /* Propagate errors. */
      rd->nres = -1;  /* Pending call. */
    } else if (tref_isnumber(tr)) {
      J->base[0] = emitir(IRT(IR_TOSTR, IRT_STR), tr, 0);
    } else if (tref_ispri(tr)) {
      J->base[0] = lj_ir_kstr(J, strV(&J->fn->c.upvalue[tref_type(tr)]));
    } else {
      recff_nyiu(J);
    }
  }
}

static void LJ_FASTCALL recff_ipairs_aux(jit_State *J, RecordFFData *rd)
{
  RecordIndex ix;
  ix.tab = J->base[0];
  if (tref_istab(ix.tab)) {
    if (!tvisnum(&rd->argv[1]))  /* No support for string coercion. */
      lj_trace_err(J, LJ_TRERR_BADTYPE);
    setnumV(&ix.keyv, numV(&rd->argv[1])+(lua_Number)1);
    settabV(J->L, &ix.tabv, tabV(&rd->argv[0]));
    ix.val = 0; ix.idxchain = 0;
    ix.key = lj_ir_toint(J, J->base[1]);
    J->base[0] = ix.key = emitir(IRTI(IR_ADD), ix.key, lj_ir_kint(J, 1));
    J->base[1] = rec_idx(J, &ix);
    rd->nres = tref_isnil(J->base[1]) ? 0 : 2;
  }  /* else: Interpreter will throw. */
}

static void LJ_FASTCALL recff_ipairs(jit_State *J, RecordFFData *rd)
{
  TRef tab = J->base[0];
  if (tref_istab(tab)) {
    J->base[0] = lj_ir_kfunc(J, funcV(&J->fn->c.upvalue[0]));
    J->base[1] = tab;
    J->base[2] = lj_ir_kint(J, 0);
    rd->nres = 3;
  }  /* else: Interpreter will throw. */
}

static void LJ_FASTCALL recff_pcall(jit_State *J, RecordFFData *rd)
{
  if (J->maxslot >= 1) {
    rec_call(J, 0, J->maxslot - 1);
    rd->nres = -1;  /* Pending call. */
  }  /* else: Interpreter will throw. */
}

static TValue *recff_xpcall_cp(lua_State *L, lua_CFunction dummy, void *ud)
{
  jit_State *J = (jit_State *)ud;
  rec_call(J, 1, J->maxslot - 2);
  UNUSED(L); UNUSED(dummy);
  return NULL;
}

static void LJ_FASTCALL recff_xpcall(jit_State *J, RecordFFData *rd)
{
  if (J->maxslot >= 2) {
    TValue argv0, argv1;
    TRef tmp;
    int errcode;
    /* Swap function and traceback. */
    tmp = J->base[0]; J->base[0] = J->base[1]; J->base[1] = tmp;
    copyTV(J->L, &argv0, &rd->argv[0]);
    copyTV(J->L, &argv1, &rd->argv[1]);
    copyTV(J->L, &rd->argv[0], &argv1);
    copyTV(J->L, &rd->argv[1], &argv0);
    /* Need to protect rec_call because it may throw. */
    errcode = lj_vm_cpcall(J->L, NULL, J, recff_xpcall_cp);
    /* Always undo Lua stack swap to avoid confusing the interpreter. */
    copyTV(J->L, &rd->argv[0], &argv0);
    copyTV(J->L, &rd->argv[1], &argv1);
    if (errcode)
      lj_err_throw(J->L, errcode);  /* Propagate errors. */
    rd->nres = -1;  /* Pending call. */
  }  /* else: Interpreter will throw. */
}

/* -- Math library fast functions ----------------------------------------- */

static void LJ_FASTCALL recff_math_abs(jit_State *J, RecordFFData *rd)
{
  TRef tr = lj_ir_tonum(J, J->base[0]);
  J->base[0] = emitir(IRTN(IR_ABS), tr, lj_ir_knum_abs(J));
  UNUSED(rd);
}

/* Record rounding functions math.floor and math.ceil. */
static void LJ_FASTCALL recff_math_round(jit_State *J, RecordFFData *rd)
{
  if (!tref_isinteger(J->base[0]))  /* Pass through integers unmodified. */
    J->base[0] = emitir(IRTN(IR_FPMATH), lj_ir_tonum(J, J->base[0]), rd->data);
  /* Note: result is integral (or NaN/Inf), but may not fit into an integer. */
}

/* Record unary math.* functions, mapped to IR_FPMATH opcode. */
static void LJ_FASTCALL recff_math_unary(jit_State *J, RecordFFData *rd)
{
  J->base[0] = emitir(IRTN(IR_FPMATH), lj_ir_tonum(J, J->base[0]), rd->data);
}

/* Record binary math.* functions math.atan2 and math.ldexp. */
static void LJ_FASTCALL recff_math_binary(jit_State *J, RecordFFData *rd)
{
  TRef tr = lj_ir_tonum(J, J->base[0]);
  J->base[0] = emitir(IRTN(rd->data), tr, lj_ir_tonum(J, J->base[1]));
}

/* Record math.asin, math.acos, math.atan. */
static void LJ_FASTCALL recff_math_atrig(jit_State *J, RecordFFData *rd)
{
  TRef y = lj_ir_tonum(J, J->base[0]);
  TRef x = lj_ir_knum_one(J);
  uint32_t ffid = rd->data;
  if (ffid != FF_math_atan) {
    TRef tmp = emitir(IRTN(IR_MUL), y, y);
    tmp = emitir(IRTN(IR_SUB), x, tmp);
    tmp = emitir(IRTN(IR_FPMATH), tmp, IRFPM_SQRT);
    if (ffid == FF_math_asin) { x = tmp; } else { x = y; y = tmp; }
  }
  J->base[0] = emitir(IRTN(IR_ATAN2), y, x);
}

static void LJ_FASTCALL recff_math_htrig(jit_State *J, RecordFFData *rd)
{
  TRef tr = lj_ir_tonum(J, J->base[0]);
  J->base[0] = lj_ir_call(J, rd->data, tr);
}

static void LJ_FASTCALL recff_math_modf(jit_State *J, RecordFFData *rd)
{
  TRef tr = J->base[0];
  if (tref_isinteger(tr)) {
    J->base[0] = tr;
    J->base[1] = lj_ir_kint(J, 0);
  } else {
    TRef trt;
    tr = lj_ir_tonum(J, tr);
    trt = emitir(IRTN(IR_FPMATH), tr, IRFPM_TRUNC);
    J->base[0] = trt;
    J->base[1] = emitir(IRTN(IR_SUB), tr, trt);
  }
  rd->nres = 2;
}

static void LJ_FASTCALL recff_math_degrad(jit_State *J, RecordFFData *rd)
{
  TRef tr = lj_ir_tonum(J, J->base[0]);
  TRef trm = lj_ir_knum(J, numV(&J->fn->c.upvalue[0]));
  J->base[0] = emitir(IRTN(IR_MUL), tr, trm);
  UNUSED(rd);
}

static void LJ_FASTCALL recff_math_pow(jit_State *J, RecordFFData *rd)
{
  TRef tr = lj_ir_tonum(J, J->base[0]);
  if (!tref_isnumber_str(J->base[1]))
    lj_trace_err(J, LJ_TRERR_BADTYPE);
  J->base[0] = lj_opt_narrow_pow(J, tr, J->base[1], &rd->argv[1]);
  UNUSED(rd);
}

static void LJ_FASTCALL recff_math_minmax(jit_State *J, RecordFFData *rd)
{
  TRef tr = lj_ir_tonum(J, J->base[0]);
  uint32_t op = rd->data;
  BCReg i;
  for (i = 1; J->base[i] != 0; i++)
    tr = emitir(IRTN(op), tr, lj_ir_tonum(J, J->base[i]));
  J->base[0] = tr;
}

static void LJ_FASTCALL recff_math_random(jit_State *J, RecordFFData *rd)
{
  GCudata *ud = udataV(&J->fn->c.upvalue[0]);
  TRef tr, one;
  lj_ir_kgc(J, obj2gco(ud), IRT_UDATA);  /* Prevent collection. */
  tr = lj_ir_call(J, IRCALL_lj_math_random_step, lj_ir_kptr(J, uddata(ud)));
  one = lj_ir_knum_one(J);
  tr = emitir(IRTN(IR_SUB), tr, one);
  if (J->base[0]) {
    TRef tr1 = lj_ir_tonum(J, J->base[0]);
    if (J->base[1]) {  /* d = floor(d*(r2-r1+1.0)) + r1 */
      TRef tr2 = lj_ir_tonum(J, J->base[1]);
      tr2 = emitir(IRTN(IR_SUB), tr2, tr1);
      tr2 = emitir(IRTN(IR_ADD), tr2, one);
      tr = emitir(IRTN(IR_MUL), tr, tr2);
      tr = emitir(IRTN(IR_FPMATH), tr, IRFPM_FLOOR);
      tr = emitir(IRTN(IR_ADD), tr, tr1);
    } else {  /* d = floor(d*r1) + 1.0 */
      tr = emitir(IRTN(IR_MUL), tr, tr1);
      tr = emitir(IRTN(IR_FPMATH), tr, IRFPM_FLOOR);
      tr = emitir(IRTN(IR_ADD), tr, one);
    }
  }
  J->base[0] = tr;
  UNUSED(rd);
}

/* -- Bit library fast functions ------------------------------------------ */

/* Record unary bit.tobit, bit.bnot, bit.bswap. */
static void LJ_FASTCALL recff_bit_unary(jit_State *J, RecordFFData *rd)
{
  TRef tr = lj_ir_tobit(J, J->base[0]);
  J->base[0] = (rd->data == IR_TOBIT) ? tr : emitir(IRTI(rd->data), tr, 0);
}

/* Record N-ary bit.band, bit.bor, bit.bxor. */
static void LJ_FASTCALL recff_bit_nary(jit_State *J, RecordFFData *rd)
{
  TRef tr = lj_ir_tobit(J, J->base[0]);
  uint32_t op = rd->data;
  BCReg i;
  for (i = 1; J->base[i] != 0; i++)
    tr = emitir(IRTI(op), tr, lj_ir_tobit(J, J->base[i]));
  J->base[0] = tr;
}

/* Record bit shifts. */
static void LJ_FASTCALL recff_bit_shift(jit_State *J, RecordFFData *rd)
{
  TRef tr = lj_ir_tobit(J, J->base[0]);
  TRef tsh = lj_ir_tobit(J, J->base[1]);
#if !LJ_TARGET_MASKEDSHIFT
  if (!tref_isk(tsh))
    tsh = emitir(IRTI(IR_BAND), tsh, lj_ir_kint(J, 31));
#endif
  J->base[0] = emitir(IRTI(rd->data), tr, tsh);
}

/* -- String library fast functions --------------------------------------- */

static void LJ_FASTCALL recff_string_len(jit_State *J, RecordFFData *rd)
{
  J->base[0] = emitir(IRTI(IR_FLOAD), lj_ir_tostr(J, J->base[0]), IRFL_STR_LEN);
  UNUSED(rd);
}

/* Handle string.byte (rd->data = 0) and string.sub (rd->data = 1). */
static void LJ_FASTCALL recff_string_range(jit_State *J, RecordFFData *rd)
{
  TRef trstr = lj_ir_tostr(J, J->base[0]);
  TRef trlen = emitir(IRTI(IR_FLOAD), trstr, IRFL_STR_LEN);
  TRef tr0 = lj_ir_kint(J, 0);
  TRef trstart, trend;
  GCstr *str = argv2str(J, &rd->argv[0]);
  int32_t start, end;
  if (rd->data) {  /* string.sub(str, start [,end]) */
    start = argv2int(J, &rd->argv[1]);
    trstart = lj_ir_toint(J, J->base[1]);
    trend = J->base[2];
    if (tref_isnil(trend)) {
      trend = lj_ir_kint(J, -1);
      end = -1;
    } else {
      trend = lj_ir_toint(J, trend);
      end = argv2int(J, &rd->argv[2]);
    }
  } else {  /* string.byte(str, [,start [,end]]) */
    if (J->base[1]) {
      start = argv2int(J, &rd->argv[1]);
      trstart = lj_ir_toint(J, J->base[1]);
      trend = J->base[2];
      if (tref_isnil(trend)) {
	trend = trstart;
	end = start;
      } else {
	trend = lj_ir_toint(J, trend);
	end = argv2int(J, &rd->argv[2]);
      }
    } else {
      trend = trstart = lj_ir_kint(J, 1);
      end = start = 1;
    }
  }
  if (end < 0) {
    emitir(IRTGI(IR_LT), trend, tr0);
    trend = emitir(IRTI(IR_ADD), emitir(IRTI(IR_ADD), trlen, trend),
		   lj_ir_kint(J, 1));
    end = end+(int32_t)str->len+1;
  } else if ((MSize)end <= str->len) {
    emitir(IRTGI(IR_ULE), trend, trlen);
  } else {
    emitir(IRTGI(IR_GT), trend, trlen);
    end = (int32_t)str->len;
    trend = trlen;
  }
  if (start < 0) {
    emitir(IRTGI(IR_LT), trstart, tr0);
    trstart = emitir(IRTI(IR_ADD), trlen, trstart);
    start = start+(int32_t)str->len;
    emitir(start < 0 ? IRTGI(IR_LT) : IRTGI(IR_GE), trstart, tr0);
    if (start < 0) {
      trstart = tr0;
      start = 0;
    }
  } else {
    if (start == 0) {
      emitir(IRTGI(IR_EQ), trstart, tr0);
      trstart = tr0;
    } else {
      trstart = emitir(IRTI(IR_ADD), trstart, lj_ir_kint(J, -1));
      emitir(IRTGI(IR_GE), trstart, tr0);
      start--;
    }
  }
  if (rd->data) {  /* Return string.sub result. */
    if (end - start >= 0) {
      /* Also handle empty range here, to avoid extra traces. */
      TRef trptr, trslen = emitir(IRTI(IR_SUB), trend, trstart);
      emitir(IRTGI(IR_GE), trslen, tr0);
      trptr = emitir(IRT(IR_STRREF, IRT_PTR), trstr, trstart);
      J->base[0] = emitir(IRT(IR_SNEW, IRT_STR), trptr, trslen);
    } else {  /* Range underflow: return empty string. */
      emitir(IRTGI(IR_LT), trend, trstart);
      J->base[0] = lj_ir_kstr(J, lj_str_new(J->L, strdata(str), 0));
    }
  } else {  /* Return string.byte result(s). */
    ptrdiff_t i, len = end - start;
    if (len > 0) {
      TRef trslen = emitir(IRTI(IR_SUB), trend, trstart);
      emitir(IRTGI(IR_EQ), trslen, lj_ir_kint(J, (int32_t)len));
      if (J->baseslot + len > LJ_MAX_JSLOTS)
	lj_trace_err_info(J, LJ_TRERR_STACKOV);
      rd->nres = len;
      for (i = 0; i < len; i++) {
	TRef tmp = emitir(IRTI(IR_ADD), trstart, lj_ir_kint(J, (int32_t)i));
	tmp = emitir(IRT(IR_STRREF, IRT_PTR), trstr, tmp);
	J->base[i] = emitir(IRT(IR_XLOAD, IRT_U8), tmp, IRXLOAD_READONLY);
      }
    } else {  /* Empty range or range underflow: return no results. */
      emitir(IRTGI(IR_LE), trend, trstart);
      rd->nres = 0;
    }
  }
}

/* -- Table library fast functions ---------------------------------------- */

static void LJ_FASTCALL recff_table_getn(jit_State *J, RecordFFData *rd)
{
  if (tref_istab(J->base[0]))
    J->base[0] = lj_ir_call(J, IRCALL_lj_tab_len, J->base[0]);
  /* else: Interpreter will throw. */
  UNUSED(rd);
}

static void LJ_FASTCALL recff_table_remove(jit_State *J, RecordFFData *rd)
{
  TRef tab = J->base[0];
  rd->nres = 0;
  if (tref_istab(tab)) {
    if (!J->base[1] || tref_isnil(J->base[1])) {  /* Simple pop: t[#t] = nil */
      TRef trlen = lj_ir_call(J, IRCALL_lj_tab_len, tab);
      GCtab *t = tabV(&rd->argv[0]);
      MSize len = lj_tab_len(t);
      emitir(IRTGI(len ? IR_NE : IR_EQ), trlen, lj_ir_kint(J, 0));
      if (len) {
	RecordIndex ix;
	ix.tab = tab;
	ix.key = trlen;
	settabV(J->L, &ix.tabv, t);
	setintV(&ix.keyv, len);
	ix.idxchain = 0;
	if (results_wanted(J) != 0) {  /* Specialize load only if needed. */
	  ix.val = 0;
	  J->base[0] = rec_idx(J, &ix);  /* Load previous value. */
	  rd->nres = 1;
	  /* Assumes ix.key/ix.tab is not modified for raw rec_idx(). */
	}
	ix.val = TREF_NIL;
	rec_idx(J, &ix);  /* Remove value. */
      }
    } else {  /* Complex case: remove in the middle. */
      recff_nyiu(J);
    }
  }  /* else: Interpreter will throw. */
}

static void LJ_FASTCALL recff_table_insert(jit_State *J, RecordFFData *rd)
{
  RecordIndex ix;
  ix.tab = J->base[0];
  ix.val = J->base[1];
  rd->nres = 0;
  if (tref_istab(ix.tab) && ix.val) {
    if (!J->base[2]) {  /* Simple push: t[#t+1] = v */
      TRef trlen = lj_ir_call(J, IRCALL_lj_tab_len, ix.tab);
      GCtab *t = tabV(&rd->argv[0]);
      ix.key = emitir(IRTI(IR_ADD), trlen, lj_ir_kint(J, 1));
      settabV(J->L, &ix.tabv, t);
      setintV(&ix.keyv, lj_tab_len(t) + 1);
      ix.idxchain = 0;
      rec_idx(J, &ix);  /* Set new value. */
    } else {  /* Complex case: insert in the middle. */
      recff_nyiu(J);
    }
  }  /* else: Interpreter will throw. */
}

/* -- I/O library fast functions ------------------------------------------ */

/* Get FILE* for I/O function. Any I/O error aborts recording, so there's
** no need to encode the alternate cases for any of the guards.
*/
static TRef recff_io_fp(jit_State *J, uint32_t id)
{
  TRef tr, ud, fp;
  if (id) {  /* io.func() */
    tr = lj_ir_kptr(J, &J2G(J)->gcroot[id]);
    ud = emitir(IRT(IR_XLOAD, IRT_UDATA), tr, 0);
  } else {  /* fp:method() */
    ud = J->base[0];
    if (!tref_isudata(ud))
      lj_trace_err(J, LJ_TRERR_BADTYPE);
    tr = emitir(IRT(IR_FLOAD, IRT_U8), ud, IRFL_UDATA_UDTYPE);
    emitir(IRTGI(IR_EQ), tr, lj_ir_kint(J, UDTYPE_IO_FILE));
  }
  fp = emitir(IRT(IR_FLOAD, IRT_LIGHTUD), ud, IRFL_UDATA_FILE);
  emitir(IRTG(IR_NE, IRT_LIGHTUD), fp, lj_ir_knull(J, IRT_LIGHTUD));
  return fp;
}

static void LJ_FASTCALL recff_io_write(jit_State *J, RecordFFData *rd)
{
  TRef fp = recff_io_fp(J, rd->data);
  TRef zero = lj_ir_kint(J, 0);
  TRef one = lj_ir_kint(J, 1);
  ptrdiff_t i = rd->data == 0 ? 1 : 0;
  for (; J->base[i]; i++) {
    TRef str = lj_ir_tostr(J, J->base[i]);
    TRef buf = emitir(IRT(IR_STRREF, IRT_PTR), str, zero);
    TRef len = emitir(IRTI(IR_FLOAD), str, IRFL_STR_LEN);
    if (tref_isk(len) && IR(tref_ref(len))->i == 1) {
      TRef tr = emitir(IRT(IR_XLOAD, IRT_U8), buf, IRXLOAD_READONLY);
      tr = lj_ir_call(J, IRCALL_fputc, tr, fp);
      if (results_wanted(J) != 0)  /* Check result only if not ignored. */
	emitir(IRTGI(IR_NE), tr, lj_ir_kint(J, -1));
    } else {
      TRef tr = lj_ir_call(J, IRCALL_fwrite, buf, one, len, fp);
      if (results_wanted(J) != 0)  /* Check result only if not ignored. */
	emitir(IRTGI(IR_EQ), tr, len);
    }
  }
  J->base[0] = TREF_TRUE;
}

static void LJ_FASTCALL recff_io_flush(jit_State *J, RecordFFData *rd)
{
  TRef fp = recff_io_fp(J, rd->data);
  TRef tr = lj_ir_call(J, IRCALL_fflush, fp);
  if (results_wanted(J) != 0)  /* Check result only if not ignored. */
    emitir(IRTGI(IR_EQ), tr, lj_ir_kint(J, 0));
  J->base[0] = TREF_TRUE;
}

/* -- Record calls to fast functions -------------------------------------- */

#include "lj_recdef.h"

static uint32_t recdef_lookup(GCfunc *fn)
{
  if (fn->c.ffid < sizeof(recff_idmap)/sizeof(recff_idmap[0]))
    return recff_idmap[fn->c.ffid];
  else
    return 0;
}

/* Record entry to a fast function or C function. */
static void rec_func_ff(jit_State *J)
{
  RecordFFData rd;
  uint32_t m = recdef_lookup(J->fn);
  rd.data = m & 0xff;
  rd.nres = 1;  /* Default is one result. */
  rd.argv = J->L->base;
  J->base[J->maxslot] = 0;  /* Mark end of arguments. */
  (recff_func[m >> 8])(J, &rd);  /* Call recff_* handler. */
  if (rd.nres >= 0)
    rec_ret(J, 0, rd.nres);
}

/* -- Record calls to Lua functions --------------------------------------- */

/* Check unroll limits for calls. */
static void check_call_unroll(jit_State *J)
{
  IRRef fref = tref_ref(J->base[-1]);
  int32_t count = 0;
  BCReg s;
  for (s = J->baseslot - 1; s > 0; s--)
    if ((J->slot[s] & TREF_FRAME) && tref_ref(J->slot[s]) == fref)
      count++;
  if (J->pc == J->startpc) {
    if (count + J->tailcalled > J->param[JIT_P_recunroll]) {
      J->pc++;
      rec_stop(J, J->curtrace);  /* Up-recursion or tail-recursion. */
    }
  } else {
    if (count > J->param[JIT_P_callunroll])
      lj_trace_err(J, LJ_TRERR_CUNROLL);
  }
}

/* Record Lua function setup. */
static void rec_func_setup(jit_State *J)
{
  GCproto *pt = J->pt;
  BCReg s, numparams = pt->numparams;
  if ((pt->flags & PROTO_NO_JIT))
    lj_trace_err(J, LJ_TRERR_CJITOFF);
  lua_assert(!(pt->flags & PROTO_IS_VARARG));
  if (J->baseslot + pt->framesize >= LJ_MAX_JSLOTS)
    lj_trace_err(J, LJ_TRERR_STACKOV);
  /* Fill up missing parameters with nil. */
  for (s = J->maxslot; s < numparams; s++)
    J->base[s] = TREF_NIL;
  /* The remaining slots should never be read before they are written. */
  J->maxslot = numparams;
}

/* Record entry to a Lua function. */
static void rec_func_lua(jit_State *J)
{
  rec_func_setup(J);
  check_call_unroll(J);
}

/* Record entry to an already compiled function. */
static void rec_func_jit(jit_State *J, TraceNo lnk)
{
  rec_func_setup(J);
  J->instunroll = 0;  /* Cannot continue across a compiled function. */
  if (J->pc == J->startpc && J->framedepth + J->retdepth == 0)
    lnk = J->curtrace;  /* Can form an extra tail-recursive loop. */
  rec_stop(J, lnk);  /* Link to the function. */
}

/* -- Record allocations -------------------------------------------------- */

static TRef rec_tnew(jit_State *J, uint32_t ah)
{
  uint32_t asize = ah & 0x7ff;
  uint32_t hbits = ah >> 11;
  if (asize == 0x7ff) asize = 0x801;
  return emitir(IRT(IR_TNEW, IRT_TAB), asize, hbits);
}

/* -- Record bytecode ops ------------------------------------------------- */

/* Optimize state after comparison. */
static void optstate_comp(jit_State *J, int cond)
{
  BCIns jmpins = J->pc[1];
  const BCIns *npc = J->pc + 2 + (cond ? bc_j(jmpins) : 0);
  SnapShot *snap = &J->cur.snap[J->cur.nsnap-1];
  /* Avoid re-recording the comparison in side traces. */
  J->cur.snapmap[snap->mapofs + snap->nent] = SNAP_MKPC(npc);
  J->needsnap = 1;
  /* Shrink last snapshot if possible. */
  if (bc_a(jmpins) < J->maxslot) {
    J->maxslot = bc_a(jmpins);
    lj_snap_shrink(J);
  }
}

/* Record the next bytecode instruction (_before_ it's executed). */
void lj_record_ins(jit_State *J)
{
  cTValue *lbase;
  RecordIndex ix;
  const BCIns *pc;
  BCIns ins;
  BCOp op;
  TRef ra, rb, rc;

  /* Need snapshot before recording next bytecode (e.g. after a store). */
  if (J->needsnap) {
    J->needsnap = 0;
    lj_snap_add(J);
    J->mergesnap = 1;
  }

  /* Record only closed loops for root traces. */
  pc = J->pc;
  if (J->framedepth == 0 &&
     (MSize)((char *)pc - (char *)J->bc_min) >= J->bc_extent)
    lj_trace_err(J, LJ_TRERR_LLEAVE);

#ifdef LUA_USE_ASSERT
  rec_check_slots(J);
  rec_check_ir(J);
#endif

  /* Keep a copy of the runtime values of var/num/str operands. */
#define rav	(&ix.valv)
#define rbv	(&ix.tabv)
#define rcv	(&ix.keyv)

  lbase = J->L->base;
  ins = *pc;
  op = bc_op(ins);
  ra = bc_a(ins);
  ix.val = 0;
  switch (bcmode_a(op)) {
  case BCMvar:
    copyTV(J->L, rav, &lbase[ra]); ix.val = ra = getslot(J, ra); break;
  default: break;  /* Handled later. */
  }
  rb = bc_b(ins);
  rc = bc_c(ins);
  switch (bcmode_b(op)) {
  case BCMnone: rb = 0; rc = bc_d(ins); break;  /* Upgrade rc to 'rd'. */
  case BCMvar:
    copyTV(J->L, rbv, &lbase[rb]); ix.tab = rb = getslot(J, rb); break;
  case BCMnum: { lua_Number n = proto_knum(J->pt, rb);
    setnumV(rbv, n); ix.tab = rb = lj_ir_knumint(J, n); } break;
  default: break;  /* Handled later. */
  }
  switch (bcmode_c(op)) {
  case BCMvar:
    copyTV(J->L, rcv, &lbase[rc]); ix.key = rc = getslot(J, rc); break;
  case BCMpri: setitype(rcv, (int32_t)~rc); rc = TREF_PRI(IRT_NIL+rc); break;
  case BCMnum: { lua_Number n = proto_knum(J->pt, rc);
    setnumV(rcv, n); ix.key = rc = lj_ir_knumint(J, n); } break;
  case BCMstr: { GCstr *s = gco2str(proto_kgc(J->pt, ~(ptrdiff_t)rc));
    setstrV(J->L, rcv, s); ix.key = rc = lj_ir_kstr(J, s); } break;
  default: break;  /* Handled later. */
  }

  switch (op) {

  /* -- Comparison ops ---------------------------------------------------- */

  case BC_ISLT: case BC_ISGE: case BC_ISLE: case BC_ISGT:
    /* Emit nothing for two numeric or string consts. */
    if (!(tref_isk2(ra,rc) && tref_isnumber_str(ra) && tref_isnumber_str(rc))) {
      IRType ta = tref_isinteger(ra) ? IRT_INT : tref_type(ra);
      IRType tc = tref_isinteger(rc) ? IRT_INT : tref_type(rc);
      int irop;
      if (ta != tc) {
	/* Widen mixed number/int comparisons to number/number comparison. */
	if (ta == IRT_INT && tc == IRT_NUM) {
	  ra = emitir(IRTN(IR_TONUM), ra, 0);
	  ta = IRT_NUM;
	} else if (ta == IRT_NUM && tc == IRT_INT) {
	  rc = emitir(IRTN(IR_TONUM), rc, 0);
	} else if (!((ta == IRT_FALSE || ta == IRT_TRUE) &&
		     (tc == IRT_FALSE || tc == IRT_TRUE))) {
	  break;  /* Interpreter will throw for two different types. */
	}
      }
      lj_snap_add(J);
      irop = (int)op - (int)BC_ISLT + (int)IR_LT;
      if (ta == IRT_NUM) {
	if ((irop & 1)) irop ^= 4;  /* ISGE/ISGT are unordered. */
	if (!lj_ir_numcmp(numV(rav), numV(rcv), (IROp)irop)) irop ^= 5;
      } else if (ta == IRT_INT) {
	if (!lj_ir_numcmp(numV(rav), numV(rcv), (IROp)irop)) irop ^= 1;
      } else if (ta == IRT_STR) {
	if (!lj_ir_strcmp(strV(rav), strV(rcv), (IROp)irop)) irop ^= 1;
	ra = lj_ir_call(J, IRCALL_lj_str_cmp, ra, rc);
	rc = lj_ir_kint(J, 0);
	ta = IRT_INT;
      } else {
	rec_mm_comp(J, &ix, (int)op);
	break;
      }
      emitir(IRTG(irop, ta), ra, rc);
      optstate_comp(J, ((int)op ^ irop) & 1);
    }
    break;

  case BC_ISEQV: case BC_ISNEV:
  case BC_ISEQS: case BC_ISNES:
  case BC_ISEQN: case BC_ISNEN:
  case BC_ISEQP: case BC_ISNEP:
    /* Emit nothing for two non-table, non-udata consts. */
    if (!(tref_isk2(ra, rc) && !(tref_istab(ra) || tref_isudata(ra)))) {
      int diff;
      lj_snap_add(J);
      diff = rec_objcmp(J, ra, rc, rav, rcv);
      if (diff == 1 && (tref_istab(ra) || tref_isudata(ra))) {
	/* Only check __eq if different, but the same type (table or udata). */
	rec_mm_equal(J, &ix, (int)op);
	break;
      }
      optstate_comp(J, ((int)op & 1) == !diff);
    }
    break;

  /* -- Unary test and copy ops ------------------------------------------- */

  case BC_ISTC: case BC_ISFC:
    if ((op & 1) == tref_istruecond(rc))
      rc = 0;  /* Don't store if condition is not true. */
    /* fallthrough */
  case BC_IST: case BC_ISF:  /* Type specialization suffices. */
    if (bc_a(pc[1]) < J->maxslot)
      J->maxslot = bc_a(pc[1]);  /* Shrink used slots. */
    break;

  /* -- Unary ops --------------------------------------------------------- */

  case BC_NOT:
    /* Type specialization already forces const result. */
    rc = tref_istruecond(rc) ? TREF_FALSE : TREF_TRUE;
    break;

  case BC_LEN:
    if (tref_isstr(rc)) {
      rc = emitir(IRTI(IR_FLOAD), rc, IRFL_STR_LEN);
    } else if (tref_istab(rc)) {
      rc = lj_ir_call(J, IRCALL_lj_tab_len, rc);
    } else {
      ix.tab = rc;
      copyTV(J->L, &ix.tabv, &ix.keyv);
      ix.key = TREF_NIL;
      setnilV(&ix.keyv);
      rc = rec_mm_arith(J, &ix, MM_len);
    }
    break;

  /* -- Arithmetic ops ---------------------------------------------------- */

  case BC_UNM:
    if (tref_isnumber_str(rc)) {
      rc = lj_ir_tonum(J, rc);
      rc = emitir(IRTN(IR_NEG), rc, lj_ir_knum_neg(J));
    } else {
      ix.tab = rc;
      copyTV(J->L, &ix.tabv, &ix.keyv);
      rc = rec_mm_arith(J, &ix, MM_unm);
    }
    break;

  case BC_ADDNV: case BC_SUBNV: case BC_MULNV: case BC_DIVNV: case BC_MODNV:
    ix.tab = rc; ix.key = rc = rb; rb = ix.tab;
    copyTV(J->L, &ix.valv, &ix.tabv);
    copyTV(J->L, &ix.tabv, &ix.keyv);
    copyTV(J->L, &ix.keyv, &ix.valv);
    if (op == BC_MODNV)
      goto recmod;
    /* fallthrough */
  case BC_ADDVN: case BC_SUBVN: case BC_MULVN: case BC_DIVVN:
  case BC_ADDVV: case BC_SUBVV: case BC_MULVV: case BC_DIVVV: {
    MMS mm = bcmode_mm(op);
    if (tref_isnumber_str(rb) && tref_isnumber_str(rc)) {
      rb = lj_ir_tonum(J, rb);
      rc = lj_ir_tonum(J, rc);
      rc = emitir(IRTN((int)mm - (int)MM_add + (int)IR_ADD), rb, rc);
    } else {
      rc = rec_mm_arith(J, &ix, mm);
    }
    break;
    }

  case BC_MODVN: case BC_MODVV:
  recmod:
    if (tref_isnumber_str(rb) && tref_isnumber_str(rc))
      rc = lj_opt_narrow_mod(J, rb, rc);
    else
      rc = rec_mm_arith(J, &ix, MM_mod);
    break;

  case BC_POW:
    if (tref_isnumber_str(rb) && tref_isnumber_str(rc))
      rc = lj_opt_narrow_pow(J, lj_ir_tonum(J, rb), rc, rcv);
    else
      rc = rec_mm_arith(J, &ix, MM_pow);
    break;

  /* -- Constant and move ops --------------------------------------------- */

  case BC_MOV:
    /* Clear gap of method call to avoid resurrecting previous refs. */
    if (ra > J->maxslot) J->base[ra-1] = 0;
    break;
  case BC_KSTR: case BC_KNUM: case BC_KPRI:
    break;
  case BC_KSHORT:
    rc = lj_ir_kint(J, (int32_t)(int16_t)rc);
    break;
  case BC_KNIL:
    while (ra <= rc)
      J->base[ra++] = TREF_NIL;
    if (rc >= J->maxslot) J->maxslot = rc+1;
    break;

  /* -- Upvalue and function ops ------------------------------------------ */

  case BC_UGET:
    rc = rec_upvalue(J, rc, 0);
    break;
  case BC_USETV: case BC_USETS: case BC_USETN: case BC_USETP:
    rec_upvalue(J, ra, rc);
    break;

  /* -- Table ops --------------------------------------------------------- */

  case BC_GGET: case BC_GSET:
    settabV(J->L, &ix.tabv, tabref(J->fn->l.env));
    ix.tab = emitir(IRT(IR_FLOAD, IRT_TAB), getcurrf(J), IRFL_FUNC_ENV);
    ix.idxchain = LJ_MAX_IDXCHAIN;
    rc = rec_idx(J, &ix);
    break;

  case BC_TGETB: case BC_TSETB:
    setintV(&ix.keyv, (int32_t)rc);
    ix.key = lj_ir_kint(J, (int32_t)rc);
    /* fallthrough */
  case BC_TGETV: case BC_TGETS: case BC_TSETV: case BC_TSETS:
    ix.idxchain = LJ_MAX_IDXCHAIN;
    rc = rec_idx(J, &ix);
    break;

  case BC_TNEW:
    rc = rec_tnew(J, rc);
    break;
  case BC_TDUP:
    rc = emitir(IRT(IR_TDUP, IRT_TAB),
		lj_ir_ktab(J, gco2tab(proto_kgc(J->pt, ~(ptrdiff_t)rc))), 0);
    break;

  /* -- Calls and vararg handling ----------------------------------------- */

  case BC_ITERC:
    J->base[ra] = getslot(J, ra-3);
    J->base[ra+1] = getslot(J, ra-2);
    J->base[ra+2] = getslot(J, ra-1);
    { /* Have to do the actual copy now because rec_call needs the values. */
      TValue *b = &J->L->base[ra];
      copyTV(J->L, b, b-3);
      copyTV(J->L, b+1, b-2);
      copyTV(J->L, b+2, b-1);
    }
    rec_call(J, ra, (ptrdiff_t)rc-1);
    break;

  /* L->top is set to L->base+ra+rc+NARGS-1+1. See lj_dispatch_ins(). */
  case BC_CALLM:
    rc = (BCReg)(J->L->top - J->L->base) - ra;
    /* fallthrough */
  case BC_CALL:
    rec_call(J, ra, (ptrdiff_t)rc-1);
    break;

  case BC_CALLMT:
    rc = (BCReg)(J->L->top - J->L->base) - ra;
    /* fallthrough */
  case BC_CALLT:
    rec_tailcall(J, ra, (ptrdiff_t)rc-1);
    break;

  /* -- Returns ----------------------------------------------------------- */

  case BC_RETM:
    /* L->top is set to L->base+ra+rc+NRESULTS-1, see lj_dispatch_ins(). */
    rc = (BCReg)(J->L->top - J->L->base) - ra + 1;
    /* fallthrough */
  case BC_RET: case BC_RET0: case BC_RET1:
    rec_ret(J, ra, (ptrdiff_t)rc-1);
    break;

  /* -- Loops and branches ------------------------------------------------ */

  case BC_FORI:
    if (rec_for(J, pc, 0) != LOOPEV_LEAVE)
      J->loopref = J->cur.nins;
    break;
  case BC_JFORI:
    lua_assert(bc_op(pc[(ptrdiff_t)rc-BCBIAS_J]) == BC_JFORL);
    if (rec_for(J, pc, 0) != LOOPEV_LEAVE)  /* Link to existing loop. */
      rec_stop(J, bc_d(pc[(ptrdiff_t)rc-BCBIAS_J]));
    /* Continue tracing if the loop is not entered. */
    break;

  case BC_FORL:
    rec_loop_interp(J, pc, rec_for(J, pc+((ptrdiff_t)rc-BCBIAS_J), 1));
    break;
  case BC_ITERL:
    rec_loop_interp(J, pc, rec_iterl(J, *pc));
    break;
  case BC_LOOP:
    rec_loop_interp(J, pc, rec_loop(J, ra));
    break;

  case BC_JFORL:
    rec_loop_jit(J, rc, rec_for(J, pc+bc_j(J->trace[rc]->startins), 1));
    break;
  case BC_JITERL:
    rec_loop_jit(J, rc, rec_iterl(J, J->trace[rc]->startins));
    break;
  case BC_JLOOP:
    rec_loop_jit(J, rc, rec_loop(J, ra));
    break;

  case BC_IFORL:
  case BC_IITERL:
  case BC_ILOOP:
  case BC_IFUNCF:
  case BC_IFUNCV:
    lj_trace_err(J, LJ_TRERR_BLACKL);
    break;

  case BC_JMP:
    if (ra < J->maxslot)
      J->maxslot = ra;  /* Shrink used slots. */
    break;

  /* -- Function headers -------------------------------------------------- */

  case BC_FUNCF:
    rec_func_lua(J);
    break;
  case BC_JFUNCF:
    rec_func_jit(J, rc);
    break;

  case BC_FUNCV:
  case BC_JFUNCV:
    lj_trace_err(J, LJ_TRERR_NYIVF);
    break;

  case BC_FUNCC:
  case BC_FUNCCW:
    rec_func_ff(J);
    break;

  default:
    if (op >= BC__MAX) {
      rec_func_ff(J);
      break;
    }
    /* fallthrough */
  case BC_CAT:
  case BC_UCLO:
  case BC_FNEW:
  case BC_TSETM:
  case BC_VARG:
    setintV(&J->errinfo, (int32_t)op);
    lj_trace_err_info(J, LJ_TRERR_NYIBC);
    break;
  }

  /* rc == 0 if we have no result yet, e.g. pending __index metamethod call. */
  if (bcmode_a(op) == BCMdst && rc) {
    J->base[ra] = rc;
    if (ra >= J->maxslot) J->maxslot = ra+1;
  }

#undef rav
#undef rbv
#undef rcv

  /* Limit the number of recorded IR instructions. */
  if (J->cur.nins > REF_FIRST+(IRRef)J->param[JIT_P_maxrecord])
    lj_trace_err(J, LJ_TRERR_TRACEOV);
}

/* -- Recording setup ----------------------------------------------------- */

/* Setup recording for a FORL loop. */
static void rec_setup_forl(jit_State *J, const BCIns *fori)
{
  BCReg ra = bc_a(*fori);
  cTValue *forbase = &J->L->base[ra];
  IRType t = (J->flags & JIT_F_OPT_NARROW) ? lj_opt_narrow_forl(forbase)
					   : IRT_NUM;
  TRef stop = fori_arg(J, fori, ra+FORL_STOP, t);
  TRef step = fori_arg(J, fori, ra+FORL_STEP, t);
  int dir = (0 <= numV(&forbase[FORL_STEP]));
  lua_assert(bc_op(*fori) == BC_FORI || bc_op(*fori) == BC_JFORI);
  J->scev.t.irt = t;
  J->scev.dir = dir;
  J->scev.stop = tref_ref(stop);
  J->scev.step = tref_ref(step);
  if (!tref_isk(step)) {
    /* Non-constant step: need a guard for the direction. */
    TRef zero = (t == IRT_INT) ? lj_ir_kint(J, 0) : lj_ir_knum_zero(J);
    emitir(IRTG(dir ? IR_GE : IR_LT, t), step, zero);
    /* Add hoistable overflow checks for a narrowed FORL index. */
    if (t == IRT_INT) {
      if (tref_isk(stop)) {
	/* Constant stop: optimize check away or to a range check for step. */
	int32_t k = IR(tref_ref(stop))->i;
	if (dir) {
	  if (k > 0)
	    emitir(IRTGI(IR_LE), step, lj_ir_kint(J, (int32_t)0x7fffffff-k));
	} else {
	  if (k < 0)
	    emitir(IRTGI(IR_GE), step, lj_ir_kint(J, (int32_t)0x80000000-k));
	}
      } else {
	/* Stop+step variable: need full overflow check (with dead result). */
	emitir(IRTGI(IR_ADDOV), step, stop);
      }
    }
  } else if (t == IRT_INT && !tref_isk(stop)) {
    /* Constant step: optimize overflow check to a range check for stop. */
    int32_t k = IR(tref_ref(step))->i;
    k = (int32_t)(dir ? 0x7fffffff : 0x80000000) - k;
    emitir(IRTGI(dir ? IR_LE : IR_GE), stop, lj_ir_kint(J, k));
  }
  J->scev.start = tref_ref(find_kinit(J, fori, ra+FORL_IDX, IRT_INT));
  if (t == IRT_INT && !J->scev.start)
    t |= IRT_GUARD;
  J->base[ra+FORL_EXT] = sloadt(J, (int32_t)(ra+FORL_IDX), t, IRSLOAD_INHERIT);
  J->scev.idx = tref_ref(J->base[ra+FORL_EXT]);
  J->maxslot = ra+FORL_EXT+1;
}

/* Setup recording for a root trace started by a hot loop. */
static const BCIns *rec_setup_root(jit_State *J)
{
  /* Determine the next PC and the bytecode range for the loop. */
  const BCIns *pcj, *pc = J->pc;
  BCIns ins = *pc;
  BCReg ra = bc_a(ins);
  switch (bc_op(ins)) {
  case BC_FORL:
    J->bc_extent = (MSize)(-bc_j(ins))*sizeof(BCIns);
    pc += 1+bc_j(ins);
    J->bc_min = pc;
    break;
  case BC_ITERL:
    lua_assert(bc_op(pc[-1]) == BC_ITERC);
    J->maxslot = ra + bc_b(pc[-1]) - 1;
    J->bc_extent = (MSize)(-bc_j(ins))*sizeof(BCIns);
    pc += 1+bc_j(ins);
    lua_assert(bc_op(pc[-1]) == BC_JMP);
    J->bc_min = pc;
    break;
  case BC_LOOP:
    /* Only check BC range for real loops, but not for "repeat until true". */
    pcj = pc + bc_j(ins);
    ins = *pcj;
    if (bc_op(ins) == BC_JMP && bc_j(ins) < 0) {
      J->bc_min = pcj+1 + bc_j(ins);
      J->bc_extent = (MSize)(-bc_j(ins))*sizeof(BCIns);
    }
    J->maxslot = ra;
    pc++;
    break;
  case BC_RET:
  case BC_RET0:
  case BC_RET1:
    /* No bytecode range check for down-recursive root traces. */
    J->maxslot = ra + bc_d(ins);
    break;
  case BC_FUNCF:
    /* No bytecode range check for root traces started by a hot call. */
    J->maxslot = J->pt->numparams;
    pc++;
    break;
  default:
    lua_assert(0);
    break;
  }
  return pc;
}

/* Setup recording for a side trace. */
static void rec_setup_side(jit_State *J, Trace *T)
{
  SnapShot *snap = &T->snap[J->exitno];
  SnapEntry *map = &T->snapmap[snap->mapofs];
  MSize n, nent = snap->nent;
  BloomFilter seen = 0;
  /* Emit IR for slots inherited from parent snapshot. */
  for (n = 0; n < nent; n++) {
    SnapEntry sn = map[n];
    IRRef ref = snap_ref(sn);
    BCReg s = snap_slot(sn);
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
    switch ((IROp)ir->o) {
    /* Only have to deal with constants that can occur in stack slots. */
    case IR_KPRI: tr = TREF_PRI(irt_type(ir->t)); break;
    case IR_KINT: tr = lj_ir_kint(J, ir->i); break;
    case IR_KGC:  tr = lj_ir_kgc(J, ir_kgc(ir), irt_t(ir->t)); break;
    case IR_KNUM: tr = lj_ir_knum_addr(J, ir_knum(ir)); break;
    case IR_KPTR:  tr = lj_ir_kptr(J, ir_kptr(ir)); break;  /* Continuation. */
    /* Inherited SLOADs don't need a guard or type check. */
    case IR_SLOAD:
      tr = emitir_raw(ir->ot & ~IRT_GUARD, s,
	     (ir->op2&IRSLOAD_READONLY) | IRSLOAD_INHERIT|IRSLOAD_PARENT);
      break;
    /* Parent refs are already typed and don't need a guard. */
    default:
      tr = emitir_raw(IRT(IR_SLOAD, irt_type(ir->t)), s,
		      IRSLOAD_INHERIT|IRSLOAD_PARENT);
      break;
    }
  setslot:
    J->slot[s] = tr | (sn&(SNAP_CONT|SNAP_FRAME));  /* Same as TREF_* flags. */
    if ((sn & SNAP_FRAME))
      J->baseslot = s+1;
  }
  J->base = J->slot + J->baseslot;
  J->maxslot = snap->nslots - J->baseslot;
  J->framedepth = snap->depth;
  lj_snap_add(J);
}

/* Setup for recording a new trace. */
void lj_record_setup(jit_State *J)
{
  uint32_t i;

  /* Initialize state related to current trace. */
  memset(J->slot, 0, sizeof(J->slot));
  memset(J->chain, 0, sizeof(J->chain));
  memset(J->bpropcache, 0, sizeof(J->bpropcache));
  J->scev.idx = REF_NIL;

  J->baseslot = 1;  /* Invoking function is at base[-1]. */
  J->base = J->slot + J->baseslot;
  J->maxslot = 0;
  J->framedepth = 0;
  J->retdepth = 0;

  J->instunroll = J->param[JIT_P_instunroll];
  J->loopunroll = J->param[JIT_P_loopunroll];
  J->tailcalled = 0;
  J->loopref = 0;

  J->bc_min = NULL;  /* Means no limit. */
  J->bc_extent = ~(MSize)0;

  /* Emit instructions for fixed references. Also triggers initial IR alloc. */
  emitir_raw(IRT(IR_BASE, IRT_PTR), J->parent, J->exitno);
  for (i = 0; i <= 2; i++) {
    IRIns *ir = IR(REF_NIL-i);
    ir->i = 0;
    ir->t.irt = (uint8_t)(IRT_NIL+i);
    ir->o = IR_KPRI;
    ir->prev = 0;
  }
  J->cur.nk = REF_TRUE;

  setgcref(J->cur.startpt, obj2gco(J->pt));
  J->startpc = J->pc;
  if (J->parent) {  /* Side trace. */
    Trace *T = J->trace[J->parent];
    TraceNo root = T->root ? T->root : J->parent;
    J->cur.root = (uint16_t)root;
    J->cur.startins = BCINS_AD(BC_JMP, 0, 0);
    /* Check whether we could at least potentially form an extra loop. */
    if (J->exitno == 0 && T->snap[0].nent == 0) {
      /* We can narrow a FORL for some side traces, too. */
      if (J->pc > proto_bc(J->pt) && bc_op(J->pc[-1]) == BC_JFORI &&
	  bc_d(J->pc[bc_j(J->pc[-1])-1]) == root) {
	lj_snap_add(J);
	rec_setup_forl(J, J->pc-1);
	goto sidecheck;
      }
    } else {
      J->startpc = NULL;  /* Prevent forming an extra loop. */
    }
    rec_setup_side(J, T);
  sidecheck:
    if (J->trace[J->cur.root]->nchild >= J->param[JIT_P_maxside] ||
	T->snap[J->exitno].count >= J->param[JIT_P_hotexit] +
				    J->param[JIT_P_tryside])
      rec_stop(J, TRACE_INTERP);
  } else {  /* Root trace. */
    J->cur.root = 0;
    J->cur.startins = *J->pc;
    J->pc = rec_setup_root(J);
    /* Note: the loop instruction itself is recorded at the end and not
    ** at the start! So snapshot #0 needs to point to the *next* instruction.
    */
    lj_snap_add(J);
    if (bc_op(J->cur.startins) == BC_FORL)
      rec_setup_forl(J, J->pc-1);
    if (1 + J->pt->framesize >= LJ_MAX_JSLOTS)
      lj_trace_err(J, LJ_TRERR_STACKOV);
  }
}

#undef IR
#undef emitir_raw
#undef emitir

#endif
