/*
** Trace profiling.
** Copyright (C) 2016 Luke Gorrie. See Copyright Notice in luajit.h
*/

#define lj_traceprofile_c
#define LUA_CORE

#ifdef LUAJIT_TRACEPROFILE

#define _GNU_SOURCE 1
#include <stdio.h>
#include <assert.h>
#include <sys/time.h>
#include <stdint.h>
#include <signal.h>
#include <ucontext.h>
#undef _GNU_SOURCE

#include "lj_obj.h"
#include "lj_dispatch.h"
#include "lj_jit.h"
#include "lj_trace.h"
#include "lj_traceprofile.h"

typedef struct TraceProfileState {
  global_State *g;                  /* VM state that started the profiler. */
  struct sigaction oldsa;
  uint64_t vmstate[LJ_TRACEPROF_VMST__MAX]; /* Counter per VM state */
} TraceProfileState;

static TraceProfileState state;

static void traceprofile_signal(int sig, siginfo_t *si, void *data)
{
  global_State *g = state.g;
  intptr_t ip = (intptr_t)((ucontext_t*)data)->uc_mcontext.gregs[REG_RIP];
  int st = g->vmstate;
  state.vmstate[LJ_TRACEPROF_VMST_TOTAL]++;
  assert((st >= 0) || (~st < LJ_VMST__MAX));
  if (st >= 0) {
    lua_State *L = gco2th(gcref(g->cur_L));
    TraceNo tr = (TraceNo)st;
    jit_State *J = L2J(L);
    GCtrace *T = traceref(J, tr);
    ptrdiff_t rel_ip = ip - (intptr_t)T->mcode;
    if ((rel_ip >= 0) && (rel_ip < T->szmcode)) {
      if (rel_ip < T->mcloop) {
	T->prof.nonloop++;	/* Sample is in non-loop mcode. */
	state.vmstate[LJ_TRACEPROF_VMST_TRACE_NONLOOP]++;
      } else {
	T->prof.loop++;		/* Sample is in loop mcode. */
	state.vmstate[LJ_TRACEPROF_VMST_TRACE_LOOP]++;
      }
    } else {
      T->prof.other++;		/* Sample is outside the trace mcode. */
      state.vmstate[LJ_TRACEPROF_VMST_TRACE_OTHER]++;
    }
  } else {
    state.vmstate[~st]++;
  }
}

static void traceprofile_start_timer(int interval)
{
  struct itimerval tm;
  struct sigaction sa;
  tm.it_value.tv_sec = tm.it_interval.tv_sec = interval / 1000;
  tm.it_value.tv_usec = tm.it_interval.tv_usec = (interval % 1000) * 1000;
  setitimer(ITIMER_PROF, &tm, NULL);
  sa.sa_flags = SA_SIGINFO | SA_RESTART;
  sa.sa_sigaction = traceprofile_signal;
  sigemptyset(&sa.sa_mask);
  sigaction(SIGPROF, &sa, &state.oldsa);
}

static void traceprofile_stop_timer()
{
  struct itimerval tm;
  tm.it_value.tv_sec = tm.it_interval.tv_sec = 0;
  tm.it_value.tv_usec = tm.it_interval.tv_usec = 0;
  setitimer(ITIMER_PROF, &tm, NULL);
  sigaction(SIGPROF, NULL, &state.oldsa);
}

LUA_API void luaJIT_traceprofile_start(lua_State *L, int interval)
{
  memset(&state, 0, sizeof(state));
  state.g = G(L);
  traceprofile_start_timer(interval);
}

LUA_API void luaJIT_traceprofile_stop(lua_State *L)
{
  traceprofile_stop_timer();
}

LUA_API uint64_t *luaJIT_traceprofile_vmstats(lua_State *L)
{
  if (state.g && L == gco2th(gcref(state.g->cur_L))) {
    return state.vmstate;
  } else {
    return NULL;
  }
}

#endif

