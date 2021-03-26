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

LJ_FUNC StrBuf * LJ_FASTCALL lj_serialize_put(StrBuf *sb, cTValue *o);
LJ_FUNC StrBuf * LJ_FASTCALL lj_serialize_get(StrBuf *sb, TValue *o);

#endif

#endif
