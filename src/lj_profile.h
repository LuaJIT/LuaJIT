/*
** Low-overhead profiling.
** Copyright (C) 2005-2013 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_PROFILE_H
#define _LJ_PROFILE_H

#include "lj_obj.h"

#if LJ_HASPROFILE

LJ_FUNC void LJ_FASTCALL lj_profile_interpreter(lua_State *L);

#endif

#endif
