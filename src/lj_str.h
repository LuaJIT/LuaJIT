/*
** String handling.
** Copyright (C) 2005-2016 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_STR_H
#define _LJ_STR_H

#include <stdarg.h>

#include "lj_obj.h"

/* String helpers. */
LJ_FUNC int32_t LJ_FASTCALL lj_str_cmp(GCstr *a, GCstr *b);
LJ_FUNC const char *lj_str_find(const char *s, const char *f,
				MSize slen, MSize flen);
LJ_FUNC int lj_str_haspattern(GCstr *s);

/* String interning. */
LJ_FUNC void lj_str_resize(lua_State *L, MSize newmask);
LJ_FUNCA GCstr *lj_str_new(lua_State *L, const char *str, size_t len);
LJ_FUNC void LJ_FASTCALL lj_str_free(global_State *g, GCstr *s);

#define lj_str_newz(L, s)	(lj_str_new(L, s, strlen(s)))
#define lj_str_newlit(L, s)	(lj_str_new(L, "" s, sizeof(s)-1))

#if LUAJIT_SMART_STRINGS
#define strsmartbit     (1<<(sizeof(MSize)*8-1))
#define strsmart(s)     ((s)->hash & strsmartbit)
#define strbloombits0(h)  ((h)>>(sizeof(h)*8-1-BLOOM_LOG*2))
#define strbloombits1(h)  ((h)>>(sizeof(h)*8-1-BLOOM_LOG))
static LJ_AINLINE MSize lj_str_fast_hash(GCstr* s)
{
  const char *str = strdata(s);
  MSize len = s->len;
  MSize a, b, h = len;
  if (!strsmart(s)) {
    return s->hash;
  }
  if (len >= 4) {  /* Caveat: unaligned access! */
    a = lj_getu32(str);
    h ^= lj_getu32(str+len-4);
    b = lj_getu32(str+(len>>1)-2);
    h ^= b; h -= lj_rol(b, 14);
    b += lj_getu32(str+(len>>2)-1);
  } else if (len > 0) {
    a = *(const uint8_t *)str;
    h ^= *(const uint8_t *)(str+len-1);
    b = *(const uint8_t *)(str+(len>>1));
    h ^= b; h -= lj_rol(b, 14);
  } else {
    return s->hash;;
  }
  a ^= h; a -= lj_rol(h, 11);
  b ^= a; b -= lj_rol(a, 25);
  h ^= b; h -= lj_rol(b, 16);
  return h & ~strsmartbit;
}
#else
#define lj_str_fast_hash(s) ((s)->hash)
#endif
#endif
