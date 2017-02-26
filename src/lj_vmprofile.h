/*
** Virtual machine profiling.
** Copyright (C) 2017 Luke Gorrie. See Copyright Notice in luajit.h
*/

#ifndef _LJ_VMPROFILE_H
#define _LJ_VMPROFILE_H


/* Counters are 64-bit to avoid overflow even in long running processes. */
typedef uint64_t VMProfileCount;

/* Maximum trace number for distinct counter buckets. Traces with
   higher numbers will be counted together in bucket zero. */
#define LJ_VMPROFILE_TRACE_MAX 4096

/* Traces have separate counters for different machine code regions. */
typedef struct VMProfileTraceCount {
  VMProfileCount head;          /* Head of the trace (non-looping part) */
  VMProfileCount loop;          /* Loop of the trace */
  VMProfileCount other;         /* Outside the trace mcode (unidentified) */
} VMProfileTraceCount;

/* Complete set of counters for VM and traces. */
typedef struct VMProfile {
  uint32_t magic;               /* 0x1d50f007 */
  uint16_t major, minor;        /* 1, 0 */
  VMProfileCount vm[LJ_VMST__MAX];
  VMProfileTraceCount trace[LJ_VMPROFILE_TRACE_MAX+1];
} VMProfile;

/* Functions that should be accessed via FFI. */

void vmprofile_set_profile(void *counters);
int vmprofile_get_profile_size();

#endif
