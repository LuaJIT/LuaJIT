#define LUA_CORE

#include "lj_jit.h"
#include "lj_trace.h"
#include "lj_vmevent.h"
#include "lj_debug.h"
#include "luajit.h"
#include "lauxlib.h"

#include <stdio.h>

typedef struct JITLogContext {
  int i;
  GCfunc *startfunc;
  GCproto *lastpt;
  BCPos lastpc;
  GCfunc *lastlua;
  GCfunc *lastfunc;
} JITLogContext;

JITLogContext context = { 0 };

static uint32_t getlastline(JITLogContext *context, GCproto *pt, BCPos pc)
{
  if(!pt) {
    pt = funcproto(context->lastlua);
    pc = context->lastpc;
  }

  return lj_debug_line(pt, pc);
}

static int isloopbc(BCOp op)
{
  return op == BC_LOOP || op == BC_FORI  || op == BC_FORL || op == BC_ITERN;
}

static void vmevent_tracestart(jit_State *J, void *t)
{
  BCPos pc = J->pt ? (int32_t)proto_bcpos(J->pt, J->pc) : -1;
  GCproto *pt = J->pt;
  GCtrace *T = &J->cur;
  const char *start = "root";
  lua_assert(J->pt);

  if(J->pc && isloopbc(bc_op(*J->pc))) {
    start = "loop";
  }

  context.startfunc = J->fn;
  context.lastfunc = context.lastlua = J->fn;

  if (J->parent == 0) {
    printf("START(%d) %s %s %d\n", T->traceno, start,  proto_chunknamestr(pt), lj_debug_line(pt, pc));
  } else {
    printf("START(%d) side %s %d\n", T->traceno, proto_chunknamestr(pt), lj_debug_line(pt, pc));
  }
}

static void vmevent_tracestop(jit_State *J, GCtrace *T)
{
  GCfunc *fn = J->fn;
  GCproto *pt = J->pt;
  BCPos pc;

  if(!pt) {
    pt = funcproto(context.lastlua);
    pc = context.lastpc;
  } else {
    pc = proto_bcpos(pt, J->pc);
  }

  if(J->cur.linktype == LJ_TRLINK_INTERP) {
   printf("STOP(%d) Compiled exit to %s %d\n", T->traceno, proto_chunknamestr(pt), getlastline(&context, pt, pc));
  } else {
   printf("STOP(%d) %s %d\n", T->traceno, proto_chunknamestr(pt), getlastline(&context, pt, pc));
  }
}

static void vmevent_tracebc(jit_State *J, GCtrace *T)
{
  GCfunc *fn = J->fn;
  GCproto *pt = J->pt;
  BCPos pc = J->pt ? (int32_t)proto_bcpos(J->pt, J->pc) : -1;

  if (context.lastfunc != J->fn) {
    context.lastfunc = J->fn;
  }

  if (pt && J->fn != context.lastlua) {
    lua_assert(pc != -1 && isluafunc(J->fn));
    context.lastlua = J->fn;
  }

  if (pc != -1) {
    context.lastpc = pc;
    context.lastpt = pt;
  }
}

static void vmevent_exit(jit_State *J, void* exitState)
{
  GCtrace *T = traceref(J, J->parent);
  int gcexit = J2G(J)->gc.state == GCSatomic || J2G(J)->gc.state == GCSfinalize;

  printf("EXIT: trace %d, exit %d\n", J->parent, J->exitno);

  if(J->exitno != -1) {
     //J->exitno
  }
}

static void vmevent_protobl(jit_State *J, void* protopc)
{
  GCproto *pt = ((void**)protopc)[0];
  BCIns *pc = ((void**)protopc)[1];
  BCPos pos = proto_bcpos(pt, pc);
  BCOp op = bc_op(*pc);

  printf("Proto blacklisted: %s:%d %s\n", proto_chunknamestr(pt), pt->firstline, op == BC_FUNCF ? "header" : "loop");
}

static void vmevent_traceabort(jit_State * J, void* t)
{
  GCproto *pt = J->pt;
  BCPos pc;
  GCtrace *T = &J->cur;

  if(!pt) {
    pt = funcproto(context.lastlua);
    pc = context.lastpc;
  } else {
    pc = proto_bcpos(pt, J->pc);
  }

  if (J->parent == 0) {
    printf("ABORT(%d) %s %d\n", T->traceno, proto_chunknamestr(pt), getlastline(&context, pt, pc));
  } else {
    printf("ABORT(%d) side %s %d\n", T->traceno, proto_chunknamestr(pt), getlastline(&context, pt, pc));
  }
}

static void vmevent_traceflush(jit_State *J, void *eventdata)
{
  printf("TRACEFLUSH mcode total %d\n", (uint32_t)J->szallmcarea);
}

const char *getgcsname(int gcs)
{
  switch (gcs)
  {
  case GCSpause:
    return "Pause";
  case GCSpropagate:
    return "Propagate";
  case GCSatomic:
    return "Atomic";
  case GCSsweepstring:
    return "SweepString";
  case GCSsweep:
    return "Sweep";
  case GCSfinalize:
    return "Finalize";
  default:
    return "?";
    break;
  }
}

static void vmevent_gcstate(jit_State *J, void *eventdata)
{
  global_State *g = J2G(J);
  int newstate = (int)(uintptr_t)eventdata;
  printf("GCSTATE: %s, totalmem %llukb, strings %d\n", getgcsname(newstate), (uint64_t)g->gc.total/1024, g->strnum);
}

LUA_API void vmevent_printer_shutdown(lua_State *L);

static void vmevent_callback(void *data, lua_State *L, int eventid, void *eventdata)
{
  VMEvent2 event = (VMEvent2)eventid;
  jit_State *J = L2J(L);

  switch (event) {
    case VMEVENT_TRACE_START:
      vmevent_tracestart(J, eventdata);
      break;
    case VMEVENT_TRACE_STOP:
      vmevent_tracestop(J, eventdata);
      break;
    case VMEVENT_TRACE_ABORT:
      vmevent_traceabort(J, eventdata);
      break;
    case VMEVENT_TRACE_EXIT:
      vmevent_exit(J, eventdata);
      break;
    case VMEVENT_PROTO_BLACKLISTED:
      vmevent_protobl(J, eventdata);
      break;
    case VMEVENT_TRACE_FLUSH:
      vmevent_traceflush(J, eventdata);
      break;
    case VMEVENT_GC_STATECHANGE:
      vmevent_gcstate(J, eventdata);
      break;
    case VMEVENT_SHUTDOWN:
      vmevent_printer_shutdown(L);
      break;
    default:
      break;
  }
}

LUA_API void vmevent_printer_start(lua_State *L)
{
  luaJIT_vmevent_sethook(L, vmevent_callback, &context);
}

LUA_API void vmevent_printer_shutdown(lua_State *L)
{
  luaJIT_vmevent_sethook(L, NULL, NULL);
}
