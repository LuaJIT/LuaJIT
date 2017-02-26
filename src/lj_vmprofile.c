/*
** VM profiling.
** Copyright (C) 2016 Luke Gorrie. See Copyright Notice in luajit.h
*/

#define lj_vmprofile_c
#define LUA_CORE

#ifdef LUAJIT_VMPROFILE

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
#include "lj_vmprofile.h"

static struct {
  global_State *g;
  struct sigaction oldsa;
} state;

/* -- State that the application can manage via FFI ----------------------- */

static VMProfile *profile;      /* Current counters */

/* How much memory to allocate for profiler counters. */
int vmprofile_get_profile_size() {
  return sizeof(VMProfile);
}

/* Set the memory where the next samples will be counted. 
   Size of the memory must match vmprofile_get_profile_size(). */
void vmprofile_set_profile(void *counters) {
  profile = (VMProfile*)counters;
  profile->magic = 0x1d50f007;
  profile->major = 1;
  profile->minor = 0;
}

/* -- Signal handler ------------------------------------------------------ */

/* Signal handler: bumps one counter. */
static void vmprofile_signal(int sig, siginfo_t *si, void *data)
{
  if (profile != NULL) {
    lua_State *L = gco2th(gcref(state.g->cur_L));
    int vmstate = state.g->vmstate;
    /* Not in a trace */
    if (vmstate < 0) {
      profile->vm[~vmstate]++;
    } else {
      int bucket = vmstate > LJ_VMPROFILE_TRACE_MAX ? 0 : vmstate;
      VMProfileTraceCount *count = &profile->trace[bucket];
      GCtrace *T = traceref(L2J(L), (TraceNo)vmstate);
      intptr_t ip = (intptr_t)((ucontext_t*)data)->uc_mcontext.gregs[REG_RIP];
      ptrdiff_t mcposition = ip - (intptr_t)T->mcode;
      if ((mcposition < 0) || (mcposition >= T->szmcode)) {
        count->other++;
      } else if ((T->mcloop != 0) && (mcposition >= T->mcloop)) {
        count->loop++;
      } else {
        count->head++;
      }
    }
  }
}

static void start_timer(int interval)
{
  struct itimerval tm;
  struct sigaction sa;
  tm.it_value.tv_sec = tm.it_interval.tv_sec = interval / 1000;
  tm.it_value.tv_usec = tm.it_interval.tv_usec = (interval % 1000) * 1000;
  setitimer(ITIMER_PROF, &tm, NULL);
  sa.sa_flags = SA_SIGINFO | SA_RESTART;
  sa.sa_sigaction = vmprofile_signal;
  sigemptyset(&sa.sa_mask);
  sigaction(SIGPROF, &sa, &state.oldsa);
}

static void stop_timer()
{
  struct itimerval tm;
  tm.it_value.tv_sec = tm.it_interval.tv_sec = 0;
  tm.it_value.tv_usec = tm.it_interval.tv_usec = 0;
  setitimer(ITIMER_PROF, &tm, NULL);
  sigaction(SIGPROF, NULL, &state.oldsa);
}

/* -- Lua API ------------------------------------------------------------- */

LUA_API void luaJIT_vmprofile_start(lua_State *L)
{
  memset(&state, 0, sizeof(state));
  state.g = G(L);
  start_timer(1);               /* Sample every 1ms */
}

LUA_API void luaJIT_vmprofile_stop(lua_State *L)
{
  stop_timer();
}

#endif
