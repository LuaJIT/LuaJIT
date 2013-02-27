/*
** Buffer handling.
** Copyright (C) 2005-2013 Mike Pall. See Copyright Notice in luajit.h
*/

#include <stdio.h>

#define lj_buf_c
#define LUA_CORE

#include "lj_obj.h"
#include "lj_gc.h"
#include "lj_err.h"
#include "lj_buf.h"

LJ_NOINLINE void lj_buf_grow(lua_State *L, SBuf *sb, MSize sz)
{
  MSize bsz = sb->sz * 2;
  if (LJ_UNLIKELY(sz > LJ_MAX_MEM))
    lj_err_mem(L);
  if (bsz < LJ_MIN_SBUF) bsz = LJ_MIN_SBUF;
  while (bsz < sz) bsz += bsz;
  sb->buf = lj_mem_realloc(L, sb->buf, sb->sz, bsz);
  sb->sz = bsz;
}

char *lj_buf_tmp(lua_State *L, MSize sz)
{
  return lj_buf_need(L, &G(L)->tmpbuf, sz);
}

void lj_buf_shrink(lua_State *L, SBuf *sb)
{
  MSize sz = sb->sz;
  if (sz > 2*LJ_MIN_SBUF) {
    sb->buf = lj_mem_realloc(L, sb->buf, sz, (sz >> 1));
    sb->sz = (sz >> 1);
  }
}

