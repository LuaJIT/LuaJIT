/*
** Snapshot handling.
** Copyright (C) 2005-2010 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_SNAP_H
#define _LJ_SNAP_H

#include "lj_obj.h"
#include "lj_jit.h"

#if LJ_HASJIT
LJ_FUNC void lj_snap_add(jit_State *J);
LJ_FUNC void lj_snap_shrink(jit_State *J);
LJ_FUNC void lj_snap_regspmap(uint16_t *rsmap, Trace *T, SnapNo snapno);
LJ_FUNC void lj_snap_restore(jit_State *J, void *exptr);
#endif

#endif
