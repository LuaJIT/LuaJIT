/*
** Trace management.
** Copyright (C) 2005-2009 Mike Pall. See Copyright Notice in luajit.h
*/

#define lj_trace_c
#define LUA_CORE

#include "lj_obj.h"

#if LJ_HASJIT

#include "lj_gc.h"
#include "lj_err.h"
#include "lj_str.h"
#include "lj_frame.h"
#include "lj_state.h"
#include "lj_bc.h"
#include "lj_ir.h"
#include "lj_jit.h"
#include "lj_iropt.h"
#include "lj_mcode.h"
#include "lj_trace.h"
#include "lj_snap.h"
#include "lj_gdbjit.h"
#include "lj_record.h"
#include "lj_asm.h"
#include "lj_dispatch.h"
#include "lj_vm.h"
#include "lj_vmevent.h"
#include "lj_target.h"

/* -- Error handling ------------------------------------------------------ */

/* Synchronous abort with error message. */
void lj_trace_err(jit_State *J, TraceError e)
{
  setnilV(&J->errinfo);  /* No error info. */
  setintV(J->L->top++, (int32_t)e);
  lj_err_throw(J->L, LUA_ERRRUN);
}

/* Synchronous abort with error message and error info. */
void lj_trace_err_info(jit_State *J, TraceError e)
{
  setintV(J->L->top++, (int32_t)e);
  lj_err_throw(J->L, LUA_ERRRUN);
}

/* -- Trace management ---------------------------------------------------- */

/* The current trace is first assembled in J->cur. The variable length
** arrays point to shared, growable buffers (J->irbuf etc.). The trace is
** kept in this state until a new trace needs to be created. Then the current
** trace and its data structures are copied to a new (compact) Trace object.
*/

/* Find a free trace number. */
static TraceNo trace_findfree(jit_State *J)
{
  MSize osz, lim;
  if (J->freetrace == 0)
    J->freetrace = 1;
  for (; J->freetrace < J->sizetrace; J->freetrace++)
    if (J->trace[J->freetrace] == NULL)
      return J->freetrace++;
  /* Need to grow trace array. */
  lim = (MSize)J->param[JIT_P_maxtrace] + 1;
  if (lim < 2) lim = 2; else if (lim > 65535) lim = 65535;
  osz = J->sizetrace;
  if (osz >= lim)
    return 0;  /* Too many traces. */
  lj_mem_growvec(J->L, J->trace, J->sizetrace, lim, Trace *);
  while (osz < J->sizetrace)
    J->trace[osz++] = NULL;
  return J->freetrace;
}

#define TRACE_COPYELEM(field, szfield, tp) \
  T2->field = (tp *)p; \
  memcpy(p, T->field, T->szfield*sizeof(tp)); \
  p += T->szfield*sizeof(tp);

/* Save a trace by copying and compacting it. */
static Trace *trace_save(jit_State *J, Trace *T)
{
  size_t sztr = ((sizeof(Trace)+7)&~7);
  size_t szins = (T->nins-T->nk)*sizeof(IRIns);
  size_t sz = sztr + szins +
	      T->nsnap*sizeof(SnapShot) +
	      T->nsnapmap*sizeof(IRRef2);
  Trace *T2 = lj_mem_newt(J->L, (MSize)sz, Trace);
  char *p = (char *)T2 + sztr;
  memcpy(T2, T, sizeof(Trace));
  T2->ir = (IRIns *)p - T->nk;
  memcpy(p, T->ir+T->nk, szins);
  p += szins;
  TRACE_COPYELEM(snap, nsnap, SnapShot)
  TRACE_COPYELEM(snapmap, nsnapmap, IRRef2)
  lj_gc_barriertrace(J2G(J), T);
  return T2;
}

/* Free a trace. */
static void trace_free(jit_State *J, TraceNo traceno)
{
  lua_assert(traceno != 0);
  if (traceno < J->freetrace)
    J->freetrace = traceno;
  lj_gdbjit_deltrace(J, J->trace[traceno]);
  if (traceno == J->curtrace) {
    lua_assert(J->trace[traceno] == &J->cur);
    J->trace[traceno] = NULL;
    J->curtrace = 0;
  } else {
    Trace *T = J->trace[traceno];
    lua_assert(T != NULL && T != &J->cur);
    J->trace[traceno] = NULL;
    lj_mem_free(J2G(J), T,
      ((sizeof(Trace)+7)&~7) + (T->nins-T->nk)*sizeof(IRIns) +
      T->nsnap*sizeof(SnapShot) + T->nsnapmap*sizeof(IRRef2));
  }
}

/* Free all traces associated with a prototype. No unpatching needed. */
void lj_trace_freeproto(global_State *g, GCproto *pt)
{
  jit_State *J = G2J(g);
  TraceNo traceno;
  /* Free all root traces. */
  for (traceno = pt->trace; traceno != 0; ) {
    TraceNo side, nextroot = J->trace[traceno]->nextroot;
    /* Free all side traces. */
    for (side = J->trace[traceno]->nextside; side != 0; ) {
      TraceNo next = J->trace[side]->nextside;
      trace_free(J, side);
      side = next;
    }
    /* Now free the trace itself. */
    trace_free(J, traceno);
    traceno = nextroot;
  }
}

/* Re-enable compiling a prototype by unpatching any modified bytecode. */
void lj_trace_reenableproto(GCproto *pt)
{
  if ((pt->flags & PROTO_HAS_ILOOP)) {
    BCIns *bc = pt->bc;
    BCPos i, sizebc = pt->sizebc;;
    pt->flags &= ~PROTO_HAS_ILOOP;
    for (i = 0; i < sizebc; i++) {
      BCOp op = bc_op(bc[i]);
      if (op == BC_IFORL || op == BC_IITERL || op == BC_ILOOP)
	setbc_op(&bc[i], (int)op+(int)BC_LOOP-(int)BC_ILOOP);
    }
  }
}

/* Unpatch the bytecode modified by a root trace. */
static void trace_unpatch(jit_State *J, Trace *T)
{
  BCOp op = bc_op(T->startins);
  uint32_t pcofs = T->snap[0].mapofs + T->snap[0].nslots;
  BCIns *pc = ((BCIns *)(uintptr_t)T->snapmap[pcofs]) - 1;
  switch (op) {
  case BC_FORL:
    lua_assert(bc_op(*pc) == BC_JFORI);
    setbc_op(pc, BC_FORI);  /* Unpatch JFORI, too. */
    pc += bc_j(*pc);
    lua_assert(bc_op(*pc) == BC_JFORL && J->trace[bc_d(*pc)] == T);
    *pc = T->startins;
    break;
  case BC_LOOP:
    lua_assert(bc_op(*pc) == BC_JLOOP && J->trace[bc_d(*pc)] == T);
    *pc = T->startins;
    break;
  case BC_ITERL:
    lua_assert(bc_op(*pc) == BC_JMP);
    pc += bc_j(*pc)+2;
    lua_assert(bc_op(*pc) == BC_JITERL && J->trace[bc_d(*pc)] == T);
    *pc = T->startins;
    break;
  case BC_CALL:
    lj_trace_err(J, LJ_TRERR_NYILNKF);
    break;
  case BC_JMP:  /* No need to unpatch branches in parent traces (yet). */
  default:
    lua_assert(0);
    break;
  }
}

/* Free a root trace and any attached side traces. */
static void trace_freeroot(jit_State *J, Trace *T, TraceNo traceno)
{
  GCproto *pt = &gcref(T->startpt)->pt;
  TraceNo side;
  lua_assert(T->root == 0 && pt != NULL);
  /* First unpatch any modified bytecode. */
  trace_unpatch(J, T);
  /* Unlink root trace from chain anchored in prototype. */
  if (pt->trace == traceno) {  /* Trace is first in chain. Easy. */
    pt->trace = T->nextroot;
  } else {  /* Otherwise search in chain of root traces. */
    Trace *T2 = J->trace[pt->trace];
    while (T2->nextroot != traceno) {
      lua_assert(T2->nextroot != 0);
      T2 = J->trace[T2->nextroot];
    }
    T2->nextroot = T->nextroot;  /* Unlink from chain. */
  }
  /* Free all side traces. */
  for (side = T->nextside; side != 0; ) {
    TraceNo next = J->trace[side]->nextside;
    trace_free(J, side);
    side = next;
  }
  /* Now free the trace itself. */
  trace_free(J, traceno);
}

/* Flush a root trace + side traces, if there are no links to it. */
int lj_trace_flush(jit_State *J, TraceNo traceno)
{
  if (traceno > 0 && traceno < J->sizetrace) {
    Trace *T = J->trace[traceno];
    if (T && T->root == 0) {
      ptrdiff_t i;
      for (i = (ptrdiff_t)J->sizetrace-1; i > 0; i--)
	if (i != (ptrdiff_t)traceno && J->trace[i] &&
	    J->trace[i]->root != traceno && J->trace[i]->link == traceno)
	  return 0;  /* Failed: existing link to trace. */
      trace_freeroot(J, T, traceno);
      return 1;  /* Ok. */
    }
  }
  return 0;  /* Failed. */
}

/* Flush all traces associated with a prototype. */
void lj_trace_flushproto(global_State *g, GCproto *pt)
{
  while (pt->trace != 0)
    trace_freeroot(G2J(g), G2J(g)->trace[pt->trace], pt->trace);
}

/* Flush all traces. */
int lj_trace_flushall(lua_State *L)
{
  jit_State *J = L2J(L);
  ptrdiff_t i;
  if ((J2G(J)->hookmask & HOOK_GC))
    return 1;
  for (i = (ptrdiff_t)J->sizetrace-1; i > 0; i--) {
    Trace *T = J->trace[i];
    if (T && T->root == 0)
      trace_freeroot(J, T, (TraceNo)i);
  }
#ifdef LUA_USE_ASSERT
  for (i = 0; i < (ptrdiff_t)J->sizetrace; i++)
    lua_assert(J->trace[i] == NULL);
#endif
  J->freetrace = 0;
  /* Free the whole machine code and invalidate all exit stub groups. */
  lj_mcode_free(J);
  memset(J->exitstubgroup, 0, sizeof(J->exitstubgroup));
  lj_vmevent_send(L, TRACE,
    setstrV(L, L->top++, lj_str_newlit(L, "flush"));
  );
  return 0;
}

/* Free everything associated with the JIT compiler state. */
void lj_trace_freestate(global_State *g)
{
  jit_State *J = G2J(g);
#ifdef LUA_USE_ASSERT
  {  /* This assumes all traces have already been freed. */
    ptrdiff_t i;
    for (i = 0; i < (ptrdiff_t)J->sizetrace; i++)
      lua_assert(J->trace[i] == NULL);
  }
#endif
  lj_mcode_free(J);
  lj_ir_knum_freeall(J);
  lj_mem_freevec(g, J->snapmapbuf, J->sizesnapmap, IRRef2);
  lj_mem_freevec(g, J->snapbuf, J->sizesnap, SnapShot);
  lj_mem_freevec(g, J->irbuf + J->irbotlim, J->irtoplim - J->irbotlim, IRIns);
  lj_mem_freevec(g, J->trace, J->sizetrace, Trace *);
}

/* -- Trace compiler state machine ---------------------------------------- */

/* Penalize a bytecode instruction by bumping its hot counter. */
static void hotpenalty(jit_State *J, const BCIns *pc, TraceError e)
{
  uint32_t i, val = HOTCOUNT_MIN_PENALTY;
  for (i = 0; i < PENALTY_SLOTS; i++)
    if (J->penalty[i].pc == pc) {
      val = ((uint32_t)J->penalty[i].val << 1) + 1;
      if (val > HOTCOUNT_MAX_PENALTY) val = HOTCOUNT_MAX_PENALTY;
      goto setpenalty;
    }
  i = J->penaltyslot;
  J->penaltyslot = (J->penaltyslot + 1) & (PENALTY_SLOTS-1);
  J->penalty[i].pc = pc;
setpenalty:
  J->penalty[i].val = (uint16_t)val;
  J->penalty[i].reason = e;
  hotcount_set(J2GG(J), pc+1, val);
}

/* Start tracing. */
static void trace_start(jit_State *J)
{
  lua_State *L;

  if (J->curtrace != 0 && J->trace[J->curtrace] == &J->cur) {
    J->trace[J->curtrace] = trace_save(J, &J->cur);  /* Save current trace. */
    J->curtrace = 0;
  }

  if ((J->pt->flags & PROTO_NO_JIT)) {  /* JIT disabled for this proto? */
    if (J->parent == 0) {
      if (J->pc >= J->pt->bc) {
	/* Lazy bytecode patching to disable hotcount events. */
	setbc_op(J->pc, (int)bc_op(*J->pc)+(int)BC_ILOOP-(int)BC_LOOP);
	J->pt->flags |= PROTO_HAS_ILOOP;
      } else {
	/* NYI: lazy closure patching to disable hotcall events. */
	lua_assert(0);
      }
    }
    J->state = LJ_TRACE_IDLE;  /* Silently ignored. */
    return;
  }

  /* Get a new trace number. */
  J->curtrace = trace_findfree(J);
  if (LJ_UNLIKELY(J->curtrace == 0)) {  /* No free trace? */
    lua_assert((J2G(J)->hookmask & HOOK_GC) == 0);
    lj_trace_flushall(J->L);
    J->state = LJ_TRACE_IDLE;  /* Silently ignored. */
    return;
  }
  J->trace[J->curtrace] = &J->cur;

  /* Setup enough of the current trace to be able to send the vmevent. */
  memset(&J->cur, 0, sizeof(Trace));
  J->cur.nins = J->cur.nk = REF_BASE;
  J->cur.ir = J->irbuf;
  J->cur.snap = J->snapbuf;
  J->cur.snapmap = J->snapmapbuf;
  /* J->cur.nsnapmap = 0; */
  J->mergesnap = 0;
  J->needsnap = 0;
  J->guardemit.irt = 0;

  L = J->L;
  lj_vmevent_send(L, TRACE,
    setstrV(L, L->top++, lj_str_newlit(L, "start"));
    setintV(L->top++, J->curtrace);
    setfuncV(L, L->top++, J->fn);
    setintV(L->top++, J->pc - J->pt->bc + 1);
    if (J->parent) {
      setintV(L->top++, J->parent);
      setintV(L->top++, J->exitno);
    }
  );
  lj_record_setup(J);
}

/* Stop tracing. */
static void trace_stop(jit_State *J)
{
  BCIns *pc = (BCIns *)J->startpc;  /* Not const here. */
  BCOp op = bc_op(J->cur.startins);
  GCproto *pt = &gcref(J->cur.startpt)->pt;
  lua_State *L;

  switch (op) {
  case BC_FORL:
    setbc_op(pc+bc_j(J->cur.startins), BC_JFORI);  /* Patch FORI, too. */
    /* fallthrough */
  case BC_LOOP:
  case BC_ITERL:
    /* Patch bytecode of starting instruction in root trace. */
    setbc_op(pc, (int)op+(int)BC_JLOOP-(int)BC_LOOP);
    setbc_d(pc, J->curtrace);
    /* Add to root trace chain in prototype. */
    J->cur.nextroot = pt->trace;
    pt->trace = (TraceNo1)J->curtrace;
    break;
  case BC_CALL:
    lj_trace_err(J, LJ_TRERR_NYILNKF);
    break;
  case BC_JMP:
    /* Patch exit branch in parent to side trace entry. */
    lua_assert(J->parent != 0 && J->cur.root != 0);
    lj_asm_patchexit(J, J->trace[J->parent], J->exitno, J->cur.mcode);
    /* Avoid compiling a side trace twice (stack resizing uses parent exit). */
    J->trace[J->parent]->snap[J->exitno].count = SNAPCOUNT_DONE;
    /* Add to side trace chain in root trace. */
    {
      Trace *root = J->trace[J->cur.root];
      root->nchild++;
      J->cur.nextside = root->nextside;
      root->nextside = (TraceNo1)J->curtrace;
    }
    break;
  default:
    lua_assert(0);
    break;
  }

  /* Commit new mcode only after all patching is done. */
  lj_mcode_commit(J, J->cur.mcode);
  lj_gdbjit_addtrace(J, &J->cur, J->curtrace);

  L = J->L;
  lj_vmevent_send(L, TRACE,
    setstrV(L, L->top++, lj_str_newlit(L, "stop"));
    setintV(L->top++, J->curtrace);
  );
}

/* Abort tracing. */
static int trace_abort(jit_State *J)
{
  lua_State *L = J->L;
  TraceError e = LJ_TRERR_RECERR;
  lj_mcode_abort(J);
  if (tvisnum(L->top-1))
    e = (TraceError)lj_num2int(numV(L->top-1));
  if (e == LJ_TRERR_MCODELM) {
    J->state = LJ_TRACE_ASM;
    return 1;  /* Retry ASM with new MCode area. */
  }
  if (J->parent == 0)
    hotpenalty(J, J->startpc, e);  /* Penalize starting instruction. */
  if (J->curtrace) {  /* Is there anything to abort? */
    ptrdiff_t errobj = savestack(L, L->top-1);  /* Stack may be resized. */
    lj_vmevent_send(L, TRACE,
      setstrV(L, L->top++, lj_str_newlit(L, "abort"));
      setintV(L->top++, J->curtrace);
      setfuncV(L, L->top++, J->fn);
      setintV(L->top++, J->pc - J->pt->bc + 1);
      copyTV(L, L->top++, restorestack(L, errobj));
      copyTV(L, L->top++, &J->errinfo);
    );
    /* Drop aborted trace after the vmevent (which may still access it). */
    J->trace[J->curtrace] = NULL;
    if (J->curtrace < J->freetrace)
      J->freetrace = J->curtrace;
    J->curtrace = 0;
  }
  L->top--;  /* Remove error object */
  if (e == LJ_TRERR_MCODEAL)
    lj_trace_flushall(L);
  return 0;
}

/* State machine for the trace compiler. Protected callback. */
static TValue *trace_state(lua_State *L, lua_CFunction dummy, void *ud)
{
  jit_State *J = (jit_State *)ud;
  UNUSED(dummy);
  do {
    switch (J->state) {
    case LJ_TRACE_START:
      J->state = LJ_TRACE_RECORD;  /* trace_start() may change state. */
      trace_start(J);
      lj_dispatch_update(J2G(J));
      break;

    case LJ_TRACE_RECORD:
      setvmstate(J2G(J), RECORD);
      lj_vmevent_send(L, RECORD,
	setintV(L->top++, J->curtrace);
	setfuncV(L, L->top++, J->fn);
	setintV(L->top++, J->pc - J->pt->bc + 1);
	setintV(L->top++, J->framedepth);
	if (bcmode_mm(bc_op(*J->pc)) == MM_call) {
	  cTValue *o = &L->base[bc_a(*J->pc)];
	  if (bc_op(*J->pc) == BC_ITERC) o -= 3;
	  copyTV(L, L->top++, o);
	}
      );
      lj_record_ins(J);
      break;

    case LJ_TRACE_END:
      J->loopref = 0;
      if ((J->flags & JIT_F_OPT_LOOP) && J->cur.link == J->curtrace) {
	setvmstate(J2G(J), OPT);
	lj_opt_dce(J);
	if (lj_opt_loop(J)) {  /* Loop optimization failed? */
	  J->loopref = J->cur.nins;
	  J->state = LJ_TRACE_RECORD;  /* Try to continue recording. */
	  break;
	}
	J->loopref = J->chain[IR_LOOP];  /* Needed by assembler. */
      }
      J->state = LJ_TRACE_ASM;
      break;

    case LJ_TRACE_ASM:
      setvmstate(J2G(J), ASM);
      lj_asm_trace(J, &J->cur);
      trace_stop(J);
      setvmstate(J2G(J), INTERP);
      J->state = LJ_TRACE_IDLE;
      lj_dispatch_update(J2G(J));
      return NULL;

    default:  /* Trace aborted asynchronously. */
      setintV(L->top++, (int32_t)LJ_TRERR_RECERR);
      /* fallthrough */
    case LJ_TRACE_ERR:
      if (trace_abort(J))
	break;  /* Retry. */
      setvmstate(J2G(J), INTERP);
      J->state = LJ_TRACE_IDLE;
      lj_dispatch_update(J2G(J));
      return NULL;
    }
  } while (J->state > LJ_TRACE_RECORD);
  return NULL;
}

/* -- Event handling ------------------------------------------------------ */

/* A bytecode instruction is about to be executed. Record it. */
void lj_trace_ins(jit_State *J)
{
  while (lj_vm_cpcall(J->L, NULL, (void *)J, trace_state) != 0)
    J->state = LJ_TRACE_ERR;
}

/* Start recording a new trace. */
static void trace_new(jit_State *J)
{
  /* Only start a new trace if not inside __gc call or vmevent. */
  if (!(J2G(J)->hookmask & (HOOK_GC|HOOK_VMEVENT))) {
    lua_assert(J->state == LJ_TRACE_IDLE);
    J->state = LJ_TRACE_START;
    J->fn = curr_func(J->L);
    J->pt = funcproto(J->fn);
    lj_trace_ins(J);
  }
}

/* A hotcount triggered. Start recording a root trace. */
void lj_trace_hot(jit_State *J, const BCIns *pc)
{
  lua_State *L = J->L;
  L->top = curr_topL(L);  /* Only called from Lua and NRESULTS is not used. */
  hotcount_set(J2GG(J), pc, J->param[JIT_P_hotloop]+1);  /* Reset hotcount. */
  J->parent = 0;  /* Root trace. */
  J->exitno = 0;
  J->pc = pc-1;  /* The interpreter bytecode PC is offset by 1. */
  trace_new(J);
}

/* A trace exited. Restore interpreter state and check for hot exits. */
void *lj_trace_exit(jit_State *J, void *exptr)
{
  lua_State *L = J->L;
  void *cf;

  /* Restore interpreter state. */
  lj_snap_restore(J, exptr);
  cf = cframe_raw(L->cframe);
  cframe_pc(cf) = J->pc;

  lj_vmevent_send(L, TEXIT,
    ExitState *ex = (ExitState *)exptr;
    uint32_t i;
    lj_state_checkstack(L, 4+RID_NUM_GPR+RID_NUM_FPR+LUA_MINSTACK);
    setintV(L->top++, J->parent);
    setintV(L->top++, J->exitno);
    setintV(L->top++, RID_NUM_GPR);
    setintV(L->top++, RID_NUM_FPR);
    for (i = 0; i < RID_NUM_GPR; i++)
      setintV(L->top++, ex->gpr[i]);
    for (i = 0; i < RID_NUM_FPR; i++) {
      setnumV(L->top, ex->fpr[i]);
      if (LJ_UNLIKELY(tvisnan(L->top)))
	setnanV(L->top);
      L->top++;
    }
  );

  {  /* Check for a hot exit. */
    SnapShot *snap = &J->trace[J->parent]->snap[J->exitno];
    if (snap->count != SNAPCOUNT_DONE &&
	++snap->count >= J->param[JIT_P_hotexit])
      trace_new(J);  /* Start recording a side trace. */
  }

  return cf;  /* Return the interpreter C frame. */
}

#endif
