/*
** Buffer handling.
** Copyright (C) 2005-2013 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_BUF_H
#define _LJ_BUF_H

#include "lj_obj.h"
#include "lj_gc.h"
#include "lj_str.h"

/* Resizable string buffers. Struct definition in lj_obj.h. */
#define sbufB(sb)	(mref((sb)->b, char))
#define sbufP(sb)	(mref((sb)->p, char))
#define sbufE(sb)	(mref((sb)->e, char))
#define sbufsz(sb)	((MSize)(sbufE((sb)) - sbufB((sb))))
#define sbuflen(sb)	((MSize)(sbufP((sb)) - sbufB((sb))))
#define setsbufP(sb, q)	(setmref((sb)->p, (q)))

LJ_FUNC char *lj_buf_tmp(lua_State *L, MSize sz);
LJ_FUNC void lj_buf_grow(lua_State *L, SBuf *sb, char *en);
LJ_FUNC void lj_buf_shrink(lua_State *L, SBuf *sb);

LJ_FUNC char *lj_buf_wmem(char *p, const void *q, MSize len);
LJ_FUNC void lj_buf_putmem(lua_State *L, SBuf *sb, const void *q, MSize len);
LJ_FUNC uint32_t lj_buf_ruleb128(const char **pp);
LJ_FUNC char *lj_buf_wuleb128(char *p, uint32_t v);

static LJ_AINLINE void lj_buf_init(SBuf *sb)
{
  setmref(sb->p, NULL); setmref(sb->e, NULL); setmref(sb->b, NULL);
}

static LJ_AINLINE void lj_buf_reset(SBuf *sb)
{
  setmrefr(sb->p, sb->b);
}

static LJ_AINLINE void lj_buf_free(global_State *g, SBuf *sb)
{
  lj_mem_free(g, sbufB(sb), sbufsz(sb));
}

static LJ_AINLINE GCstr *lj_buf_str(lua_State *L, SBuf *sb)
{
  return lj_str_new(L, sbufB(sb), sbuflen(sb));
}

static LJ_AINLINE char *lj_buf_need(lua_State *L, SBuf *sb, MSize sz)
{
  char *en = sbufB(sb) + sz;
  if (LJ_UNLIKELY(en > sbufE(sb)))
    lj_buf_grow(L, sb, en);
  return sbufB(sb);
}

static LJ_AINLINE char *lj_buf_more(lua_State *L, SBuf *sb, MSize sz)
{
  char *en = sbufP(sb) + sz;
  if (LJ_UNLIKELY(en > sbufE(sb)))
    lj_buf_grow(L, sb, en);
  return sbufP(sb);
}

static LJ_AINLINE void lj_buf_putb(lua_State *L, SBuf *sb, int c)
{
  char *p = lj_buf_more(L, sb, 1);
  *p++ = (char)c;
  setsbufP(sb, p);
}

#endif
