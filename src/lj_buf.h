/*
** Buffer handling.
** Copyright (C) 2005-2021 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_BUF_H
#define _LJ_BUF_H

#include "lj_obj.h"
#include "lj_gc.h"
#include "lj_str.h"

/* Resizable string buffers. SBuf struct definition in lj_obj.h. */
#define sbufsz(sb)	((MSize)((sb)->e - (sb)->b))
#define sbuflen(sb)	((MSize)((sb)->w - (sb)->b))
#define sbufleft(sb)	((MSize)((sb)->e - (sb)->w))

#define sbufL(sb)	(mref((sb)->L, lua_State))
#define setsbufL(sb, l)	(setmref((sb)->L, (l)))

/* Buffer management */
LJ_FUNC char *LJ_FASTCALL lj_buf_need2(SBuf *sb, MSize sz);
LJ_FUNC char *LJ_FASTCALL lj_buf_more2(SBuf *sb, MSize sz);
LJ_FUNC void LJ_FASTCALL lj_buf_shrink(lua_State *L, SBuf *sb);
LJ_FUNC char * LJ_FASTCALL lj_buf_tmp(lua_State *L, MSize sz);

static LJ_AINLINE void lj_buf_init(lua_State *L, SBuf *sb)
{
  setsbufL(sb, L);
  sb->w = sb->e = sb->b = NULL;
}

static LJ_AINLINE void lj_buf_reset(SBuf *sb)
{
  sb->w = sb->b;
}

static LJ_AINLINE SBuf *lj_buf_tmp_(lua_State *L)
{
  SBuf *sb = &G(L)->tmpbuf;
  setsbufL(sb, L);
  lj_buf_reset(sb);
  return sb;
}

static LJ_AINLINE void lj_buf_free(global_State *g, SBuf *sb)
{
  lj_mem_free(g, sb->b, sbufsz(sb));
}

static LJ_AINLINE char *lj_buf_need(SBuf *sb, MSize sz)
{
  if (LJ_UNLIKELY(sz > sbufsz(sb)))
    return lj_buf_need2(sb, sz);
  return sb->b;
}

static LJ_AINLINE char *lj_buf_more(SBuf *sb, MSize sz)
{
  if (LJ_UNLIKELY(sz > sbufleft(sb)))
    return lj_buf_more2(sb, sz);
  return sb->w;
}

/* Low-level buffer put operations */
LJ_FUNC SBuf *lj_buf_putmem(SBuf *sb, const void *q, MSize len);
LJ_FUNC SBuf * LJ_FASTCALL lj_buf_putchar(SBuf *sb, int c);
LJ_FUNC SBuf * LJ_FASTCALL lj_buf_putstr(SBuf *sb, GCstr *s);

static LJ_AINLINE char *lj_buf_wmem(char *p, const void *q, MSize len)
{
  return (char *)memcpy(p, q, len) + len;
}

static LJ_AINLINE void lj_buf_putb(SBuf *sb, int c)
{
  char *w = lj_buf_more(sb, 1);
  *w++ = (char)c;
  sb->w = w;
}

/* High-level buffer put operations */
LJ_FUNCA SBuf * LJ_FASTCALL lj_buf_putstr_reverse(SBuf *sb, GCstr *s);
LJ_FUNCA SBuf * LJ_FASTCALL lj_buf_putstr_lower(SBuf *sb, GCstr *s);
LJ_FUNCA SBuf * LJ_FASTCALL lj_buf_putstr_upper(SBuf *sb, GCstr *s);
LJ_FUNC SBuf *lj_buf_putstr_rep(SBuf *sb, GCstr *s, int32_t rep);
LJ_FUNC SBuf *lj_buf_puttab(SBuf *sb, GCtab *t, GCstr *sep,
			    int32_t i, int32_t e);

/* Miscellaneous buffer operations */
LJ_FUNCA GCstr * LJ_FASTCALL lj_buf_tostr(SBuf *sb);
LJ_FUNC GCstr *lj_buf_cat2str(lua_State *L, GCstr *s1, GCstr *s2);
LJ_FUNC uint32_t LJ_FASTCALL lj_buf_ruleb128(const char **pp);

static LJ_AINLINE GCstr *lj_buf_str(lua_State *L, SBuf *sb)
{
  return lj_str_new(L, sb->b, sbuflen(sb));
}

/* Interim user-accessible string buffer. */
typedef struct StrBuf {
  SBuf *sb;		/* Pointer to system buffer. */
  char *r;		/* String buffer read pointer. */
  int depth;		/* Remaining recursion depth. */
} StrBuf;

#endif
