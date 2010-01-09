/*
** Trace recorder (bytecode -> SSA IR).
** Copyright (C) 2005-2010 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_RECORD_H
#define _LJ_RECORD_H

#include "lj_obj.h"
#include "lj_jit.h"

#if LJ_HASJIT
LJ_FUNC void lj_record_ins(jit_State *J);
LJ_FUNC void lj_record_setup(jit_State *J);
#endif

#endif
