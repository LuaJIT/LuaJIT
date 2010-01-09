/*
** Client for the GDB JIT API.
** Copyright (C) 2005-2010 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_GDBJIT_H
#define _LJ_GDBJIT_H

#include "lj_obj.h"
#include "lj_jit.h"

#if LJ_HASJIT && defined(LUAJIT_USE_GDBJIT)

LJ_FUNC void lj_gdbjit_addtrace(jit_State *J, Trace *T, TraceNo traceno);
LJ_FUNC void lj_gdbjit_deltrace(jit_State *J, Trace *T);

#else
#define lj_gdbjit_addtrace(J, T, tn)	UNUSED(T)
#define lj_gdbjit_deltrace(J, T)	UNUSED(T)
#endif

#endif
