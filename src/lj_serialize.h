/*
** Object de/serialization.
** Copyright (C) 2005-2021 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_SERIALIZE_H
#define _LJ_SERIALIZE_H

#include "lj_obj.h"
#include "lj_buf.h"

#if LJ_HASBUFFER

#define LJ_SERIALIZE_DEPTH	100	/* Default depth. */

LJ_FUNC void LJ_FASTCALL lj_serialize_dict_prep(lua_State *L, GCtab *dict);
LJ_FUNC SBufExt * LJ_FASTCALL lj_serialize_put(SBufExt *sbx, cTValue *o);
LJ_FUNC SBufExt * LJ_FASTCALL lj_serialize_get(SBufExt *sbx, TValue *o);

#endif

#endif
