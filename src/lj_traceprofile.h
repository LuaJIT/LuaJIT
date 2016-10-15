/*
** Trace profiling.
** Copyright (C) 2016 Luke Gorrie. See Copyright Notice in luajit.h
*/

#ifndef _LJ_TRACEPROFILE_H
#define _LJ_TRACEPROFILE_H

typedef struct TraceProfile {
  uint64_t nonloop;	/* Samples taken in non-loop mcode */
  uint64_t loop;	/* Samples taken in loop mcode */
  uint64_t other;	/* Samples taken outside trace mcode */
} TraceProfile;

#endif
