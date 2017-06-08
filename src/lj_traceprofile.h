/*
** Trace profiling.
** Copyright (C) 2016 Luke Gorrie. See Copyright Notice in luajit.h
*/

#ifndef _LJ_TRACEPROFILE_H
#define _LJ_TRACEPROFILE_H

/* vmstate extended to represent running in a trace (any one) */
typedef enum {
  LJ_TRACEPROF_VMST_TRACE_NONLOOP = LJ_VMST__MAX,
  LJ_TRACEPROF_VMST_TRACE_LOOP,
  LJ_TRACEPROF_VMST_TRACE_OTHER,
  LJ_TRACEPROF_VMST_TOTAL,
  LJ_TRACEPROF_VMST__MAX
} TraceProfileVMState;

typedef struct TraceProfile {
  uint64_t nonloop;	/* Samples taken in non-loop mcode */
  uint64_t loop;	/* Samples taken in loop mcode */
  uint64_t other;	/* Samples taken outside trace mcode */
} TraceProfile;

#endif
