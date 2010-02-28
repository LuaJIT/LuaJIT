/*
** String handling.
** Copyright (C) 2005-2010 Mike Pall. See Copyright Notice in luajit.h
**
** Portions taken verbatim or adapted from the Lua interpreter.
** Copyright (C) 1994-2008 Lua.org, PUC-Rio. See Copyright Notice in lua.h
*/

#include <stdio.h>

#define lj_str_c
#define LUA_CORE

#include "lj_obj.h"
#include "lj_gc.h"
#include "lj_err.h"
#include "lj_str.h"
#include "lj_state.h"
#include "lj_ctype.h"

/* -- String interning ---------------------------------------------------- */

/* Ordered compare of strings. Assumes string data is 4-byte aligned. */
int32_t LJ_FASTCALL lj_str_cmp(GCstr *a, GCstr *b)
{
  MSize i, n = a->len > b->len ? b->len : a->len;
  for (i = 0; i < n; i += 4) {
    /* Note: innocuous access up to end of string + 3. */
    uint32_t va = *(const uint32_t *)(strdata(a)+i);
    uint32_t vb = *(const uint32_t *)(strdata(b)+i);
    if (va != vb) {
#if LJ_ARCH_ENDIAN == LUAJIT_LE
      va = lj_bswap(va); vb = lj_bswap(vb);
#endif
      i -= n;
      if ((int32_t)i >= -3) {
	va >>= 32+(i<<3); vb >>= 32+(i<<3);
	if (va == vb) break;
      }
      return va < vb ? -1 : 1;
    }
  }
  return (int32_t)(a->len - b->len);
}

/* Resize the string hash table (grow and shrink). */
void lj_str_resize(lua_State *L, MSize newmask)
{
  global_State *g = G(L);
  GCRef *newhash;
  MSize i;
  if (g->gc.state == GCSsweepstring || newmask >= LJ_MAX_STRTAB-1)
    return;  /* No resizing during GC traversal or if already too big. */
  newhash = lj_mem_newvec(L, newmask+1, GCRef);
  memset(newhash, 0, (newmask+1)*sizeof(GCRef));
  for (i = g->strmask; i != ~(MSize)0; i--) {  /* Rehash old table. */
    GCobj *p = gcref(g->strhash[i]);
    while (p) {  /* Follow each hash chain and reinsert all strings. */
      MSize h = gco2str(p)->hash & newmask;
      GCobj *next = gcnext(p);
      /* NOBARRIER: The string table is a GC root. */
      setgcrefr(p->gch.nextgc, newhash[h]);
      setgcref(newhash[h], p);
      p = next;
    }
  }
  lj_mem_freevec(g, g->strhash, g->strmask+1, GCRef);
  g->strmask = newmask;
  g->strhash = newhash;
}

/* Intern a string and return string object. */
GCstr *lj_str_new(lua_State *L, const char *str, size_t lenx)
{
  global_State *g;
  GCstr *s;
  GCobj *o;
  MSize len = (MSize)lenx;
  MSize h = len;
  MSize step = (len>>5)+1;  /* Partial hash. */
  MSize l1;
  if (lenx >= LJ_MAX_STR)
    lj_err_msg(L, LJ_ERR_STROV);
  for (l1 = len; l1 >= step; l1 -= step)  /* Compute string hash. */
    h = h ^ ((h<<5)+(h>>2)+cast(unsigned char, str[l1-1]));
  /* Check if the string has already been interned. */
  g = G(L);
  for (o = gcref(g->strhash[h & g->strmask]); o != NULL; o = gcnext(o)) {
    GCstr *tso = gco2str(o);
    if (tso->len == len && (memcmp(str, strdata(tso), len) == 0)) {
      if (isdead(g, o)) flipwhite(o);  /* Resurrect if dead. */
      return tso;  /* Return existing string. */
    }
  }
  /* Nope, create a new string. */
  s = lj_mem_newt(L, sizeof(GCstr)+len+1, GCstr);
  newwhite(g, s);
  s->gct = ~LJ_TSTR;
  s->len = len;
  s->hash = h;
  s->reserved = 0;
  memcpy(strdatawr(s), str, len);
  strdatawr(s)[len] = '\0';  /* Zero-terminate string. */
  /* Add it to string hash table. */
  h &= g->strmask;
  s->nextgc = g->strhash[h];
  /* NOBARRIER: The string table is a GC root. */
  setgcref(g->strhash[h], obj2gco(s));
  if (g->strnum++ > g->strmask)  /* Allow a 100% load factor. */
    lj_str_resize(L, (g->strmask<<1)+1);  /* Grow string table. */
  return s;  /* Return newly interned string. */
}

void LJ_FASTCALL lj_str_free(global_State *g, GCstr *s)
{
  g->strnum--;
  lj_mem_free(g, s, sizestring(s));
}

/* -- Type conversions ---------------------------------------------------- */

/* Convert string object to number. */
int LJ_FASTCALL lj_str_tonum(GCstr *str, TValue *n)
{
  return lj_str_numconv(strdata(str), n);
}

/* Convert string to number. */
int LJ_FASTCALL lj_str_numconv(const char *s, TValue *n)
{
  lua_Number sign = 1;
  const uint8_t *p = (const uint8_t *)s;
  while (lj_ctype_isspace(*p)) p++;
  if (*p == '-') { p++; sign = -1; } else if (*p == '+') { p++; }
  if ((uint32_t)(*p - '0') < 10) {
    uint32_t k = (uint32_t)(*p++ - '0');
    if (k == 0 && ((*p & ~0x20) == 'X')) {
      p++;
      while (lj_ctype_isxdigit(*p)) {
	if (k >= 0x10000000) goto parsedbl;
	k = (k << 4) + (*p & 15u);
	if (!lj_ctype_isdigit(*p)) k += 9;
	p++;
      }
    } else {
      while ((uint32_t)(*p - '0') < 10) {
	if (k >= 0x19999999) goto parsedbl;
	k = k * 10u + (uint32_t)(*p++ - '0');
      }
    }
    while (LJ_UNLIKELY(lj_ctype_isspace(*p))) p++;
    if (LJ_LIKELY(*p == '\0')) {
      setnumV(n, sign * cast_num(k));
      return 1;
    }
  }
parsedbl:
  {
    TValue tv;
    char *endptr;
    setnumV(&tv, lua_str2number(s, &endptr));
    if (endptr == s) return 0;  /* conversion failed */
    if (LJ_UNLIKELY(*endptr != '\0')) {
      while (lj_ctype_isspace((uint8_t)*endptr)) endptr++;
      if (*endptr != '\0') return 0;  /* invalid trailing characters? */
    }
    if (LJ_LIKELY(!tvisnan(&tv)))
      setnumV(n, numV(&tv));
    else
      setnanV(n); /* Canonicalize injected NaNs. */
    return 1;
  }
}

/* Convert number to string. */
GCstr * LJ_FASTCALL lj_str_fromnum(lua_State *L, const lua_Number *np)
{
  char s[LUAI_MAXNUMBER2STR];
  lua_Number n = *np;
  size_t len = (size_t)lua_number2str(s, n);
  return lj_str_new(L, s, len);
}

#if LJ_HASJIT
/* Convert integer to string. */
GCstr * LJ_FASTCALL lj_str_fromint(lua_State *L, int32_t k)
{
  char s[1+10];
  char *p = s+sizeof(s);
  uint32_t i = (uint32_t)(k < 0 ? -k : k);
  do { *--p = (char)('0' + i % 10); } while (i /= 10);
  if (k < 0) *--p = '-';
  return lj_str_new(L, p, (size_t)(s+sizeof(s)-p));
}
#endif

/* -- String formatting --------------------------------------------------- */

static void addstr(lua_State *L, SBuf *sb, const char *str, MSize len)
{
  char *p;
  MSize i;
  if (sb->n + len > sb->sz) {
    MSize sz = sb->sz * 2;
    while (sb->n + len > sz) sz = sz * 2;
    lj_str_resizebuf(L, sb, sz);
  }
  p = sb->buf + sb->n;
  sb->n += len;
  for (i = 0; i < len; i++) p[i] = str[i];
}

static void addchar(lua_State *L, SBuf *sb, int c)
{
  if (sb->n + 1 > sb->sz) {
    MSize sz = sb->sz * 2;
    lj_str_resizebuf(L, sb, sz);
  }
  sb->buf[sb->n++] = cast(char, c);
}

/* Push formatted message as a string object to Lua stack. va_list variant. */
const char *lj_str_pushvf(lua_State *L, const char *fmt, va_list argp)
{
  SBuf *sb = &G(L)->tmpbuf;
  lj_str_needbuf(L, sb, (MSize)strlen(fmt));
  lj_str_resetbuf(sb);
  for (;;) {
    const char *e = strchr(fmt, '%');
    if (e == NULL) break;
    addstr(L, sb, fmt, (MSize)(e-fmt));
    /* This function only handles %s, %c, %d, %f and %p formats. */
    switch (e[1]) {
    case 's': {
      const char *s = va_arg(argp, char *);
      if (s == NULL) s = "(null)";
      addstr(L, sb, s, (MSize)strlen(s));
      break;
      }
    case 'c':
      addchar(L, sb, va_arg(argp, int));
      break;
    case 'd': {
      char buff[1+10];
      char *p = buff+sizeof(buff);
      int32_t k = va_arg(argp, int32_t);
      uint32_t i = (uint32_t)(k < 0 ? -k : k);
      do { *--p = (char)('0' + i % 10); } while (i /= 10);
      if (k < 0) *--p = '-';
      addstr(L, sb, p, (MSize)(buff+sizeof(buff)-p));
      break;
      }
    case 'f': {
      char buff[LUAI_MAXNUMBER2STR];
      lua_Number n = cast_num(va_arg(argp, LUAI_UACNUMBER));
      MSize len = (MSize)lua_number2str(buff, n);
      addstr(L, sb, buff, len);
      break;
      }
    case 'p': {
#define FMTP_CHARS	(2*sizeof(ptrdiff_t))
      char buff[2+FMTP_CHARS];
      ptrdiff_t p = (ptrdiff_t)(va_arg(argp, void *));
      ptrdiff_t i, lasti = 2+FMTP_CHARS;
#if LJ_64
      if ((p >> 32) == 0)  /* Shorten output for true 32 bit pointers. */
	lasti = 2+2*4;
#endif
      buff[0] = '0';
      buff[1] = 'x';
      for (i = lasti-1; i >= 2; i--, p >>= 4)
	buff[i] = "0123456789abcdef"[(p & 15)];
      addstr(L, sb, buff, (MSize)lasti);
      break;
      }
    case '%':
      addchar(L, sb, '%');
      break;
    default:
      addchar(L, sb, '%');
      addchar(L, sb, e[1]);
      break;
    }
    fmt = e+2;
  }
  addstr(L, sb, fmt, (MSize)strlen(fmt));
  setstrV(L, L->top, lj_str_new(L, sb->buf, sb->n));
  incr_top(L);
  return strVdata(L->top - 1);
}

/* Push formatted message as a string object to Lua stack. Vararg variant. */
const char *lj_str_pushf(lua_State *L, const char *fmt, ...)
{
  const char *msg;
  va_list argp;
  va_start(argp, fmt);
  msg = lj_str_pushvf(L, fmt, argp);
  va_end(argp);
  return msg;
}

/* -- Buffer handling ----------------------------------------------------- */

char *lj_str_needbuf(lua_State *L, SBuf *sb, MSize sz)
{
  if (sz > sb->sz) {
    if (sz < LJ_MIN_SBUF) sz = LJ_MIN_SBUF;
    lj_str_resizebuf(L, sb, sz);
  }
  return sb->buf;
}

