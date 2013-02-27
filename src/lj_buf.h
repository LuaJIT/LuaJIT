/*
** Buffer handling.
** Copyright (C) 2005-2013 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_BUF_H
#define _LJ_BUF_H

#include "lj_obj.h"

/* Resizable string buffers. Struct definition in lj_obj.h. */
LJ_FUNC char *lj_buf_tmp(lua_State *L, MSize sz);
LJ_FUNC void lj_buf_grow(lua_State *L, SBuf *sb, MSize sz);
LJ_FUNC void lj_buf_shrink(lua_State *L, SBuf *sb);

#define lj_buf_init(sb)		((sb)->buf = NULL, (sb)->sz = 0)
#define lj_buf_reset(sb)	((sb)->n = 0)
#define lj_buf_free(g, sb)	lj_mem_free(g, (void *)(sb)->buf, (sb)->sz)

static LJ_AINLINE char *lj_buf_need(lua_State *L, SBuf *sb, MSize sz)
{
  if (LJ_UNLIKELY(sz > sb->sz))
    lj_buf_grow(L, sb, sz);
  return sb->buf;
}

#endif
