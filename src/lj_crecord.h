/*
** Trace recorder for C data operations.
** Copyright (C) 2005-2010 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_CRECORD_H
#define _LJ_CRECORD_H

#include "lj_obj.h"
#include "lj_jit.h"
#include "lj_ffrecord.h"

#if LJ_HASJIT && LJ_HASFFI
LJ_FUNC void LJ_FASTCALL recff_cdata_index(jit_State *J, RecordFFData *rd);
#else
#define recff_cdata_index	recff_nyi
#endif

#endif
