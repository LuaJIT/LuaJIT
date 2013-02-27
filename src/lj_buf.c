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

LJ_NOINLINE void lj_buf_grow(lua_State *L, SBuf *sb, char *en)
{
  char *b = sbufB(sb);
  MSize sz = (MSize)(en - b);
  MSize osz = (MSize)(sbufE(sb) - b), nsz = osz;
  MSize n = (MSize)(sbufP(sb) - b);
  if (LJ_UNLIKELY(sz > LJ_MAX_MEM))
    lj_err_mem(L);
  if (nsz < LJ_MIN_SBUF) nsz = LJ_MIN_SBUF;
  while (nsz < sz) nsz += nsz;
  b = (char *)lj_mem_realloc(L, b, osz, nsz);
  setmref(sb->b, b);
  setmref(sb->p, b + n);
  setmref(sb->e, b + nsz);
}

char *lj_buf_tmp(lua_State *L, MSize sz)
{
  return lj_buf_need(L, &G(L)->tmpbuf, sz);
}

void lj_buf_shrink(lua_State *L, SBuf *sb)
{
  char *b = sbufB(sb);
  MSize osz = (MSize)(sbufE(sb) - b);
  if (osz > 2*LJ_MIN_SBUF) {
    MSize n = (MSize)(sbufP(sb) - b);
    b = lj_mem_realloc(L, b, osz, (osz >> 1));
    setmref(sb->b, b);
    setmref(sb->p, b + n);
    setmref(sb->e, b + (osz >> 1));
  }
}

char *lj_buf_wmem(char *p, const void *q, MSize len)
{
  const char *s = (const char *)q, *e = s + len;
  while (s < e) *p++ = *s++;
  return p;
}

void lj_buf_putmem(lua_State *L, SBuf *sb, const void *q, MSize len)
{
  char *p = lj_buf_more(L, sb, len);
  p = lj_buf_wmem(p, q, len);
  setsbufP(sb, p);
}

uint32_t lj_buf_ruleb128(const char **pp)
{
  const uint8_t *p = (const uint8_t *)*pp;
  uint32_t v = *p++;
  if (LJ_UNLIKELY(v >= 0x80)) {
    int sh = 0;
    v &= 0x7f;
    do { v |= ((*p & 0x7f) << (sh += 7)); } while (*p++ >= 0x80);
  }
  *pp = (const char *)p;
  return v;
}

char *lj_buf_wuleb128(char *p, uint32_t v)
{
  for (; v >= 0x80; v >>= 7)
    *p++ = (char)((v & 0x7f) | 0x80);
  *p++ = (char)v;
  return p;
}

