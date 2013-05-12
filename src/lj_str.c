/*
** String handling.
** Copyright (C) 2005-2013 Mike Pall. See Copyright Notice in luajit.h
*/

#include <stdio.h>

#define lj_str_c
#define LUA_CORE

#include "lj_obj.h"
#include "lj_gc.h"
#include "lj_err.h"
#include "lj_buf.h"
#include "lj_str.h"
#include "lj_char.h"

/* -- String helpers ------------------------------------------------------ */

/* Ordered compare of strings. Assumes string data is 4-byte aligned. */
int32_t LJ_FASTCALL lj_str_cmp(GCstr *a, GCstr *b)
{
  MSize i, n = a->len > b->len ? b->len : a->len;
  for (i = 0; i < n; i += 4) {
    /* Note: innocuous access up to end of string + 3. */
    uint32_t va = *(const uint32_t *)(strdata(a)+i);
    uint32_t vb = *(const uint32_t *)(strdata(b)+i);
    if (va != vb) {
#if LJ_LE
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

/* Fast string data comparison. Caveat: unaligned access to 1st string! */
static LJ_AINLINE int str_fastcmp(const char *a, const char *b, MSize len)
{
  MSize i = 0;
  lua_assert(len > 0);
  lua_assert((((uintptr_t)a+len-1) & (LJ_PAGESIZE-1)) <= LJ_PAGESIZE-4);
  do {  /* Note: innocuous access up to end of string + 3. */
    uint32_t v = lj_getu32(a+i) ^ *(const uint32_t *)(b+i);
    if (v) {
      i -= len;
#if LJ_LE
      return (int32_t)i >= -3 ? (v << (32+(i<<3))) : 1;
#else
      return (int32_t)i >= -3 ? (v >> (32+(i<<3))) : 1;
#endif
    }
    i += 4;
  } while (i < len);
  return 0;
}

/* Find fixed string p inside string s. */
const char *lj_str_find(const char *s, const char *p, MSize slen, MSize plen)
{
  if (plen <= slen) {
    if (plen == 0) {
      return s;
    } else {
      int c = *(const uint8_t *)p++;
      plen--; slen -= plen;
      while (slen) {
	const char *q = (const char *)memchr(s, c, slen);
	if (!q) break;
	if (memcmp(q+1, p, plen) == 0) return q;
	q++; slen -= (MSize)(q-s); s = q;
      }
    }
  }
  return NULL;
}

/* Check whether a string has a pattern matching character. */
int lj_str_haspattern(GCstr *s)
{
  const char *p = strdata(s), *q = p + s->len;
  while (p < q) {
    int c = *(const uint8_t *)p++;
    if (lj_char_ispunct(c) && strchr("^$*+?.([%-", c))
      return 1;  /* Found a pattern matching char. */
  }
  return 0;  /* No pattern matching chars found. */
}

/* -- String interning ---------------------------------------------------- */

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
  MSize a, b, h = len;
  if (lenx >= LJ_MAX_STR)
    lj_err_msg(L, LJ_ERR_STROV);
  g = G(L);
  /* Compute string hash. Constants taken from lookup3 hash by Bob Jenkins. */
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
    return &g->strempty;
  }
  a ^= h; a -= lj_rol(h, 11);
  b ^= a; b -= lj_rol(a, 25);
  h ^= b; h -= lj_rol(b, 16);
  /* Check if the string has already been interned. */
  o = gcref(g->strhash[h & g->strmask]);
  if (LJ_LIKELY((((uintptr_t)str+len-1) & (LJ_PAGESIZE-1)) <= LJ_PAGESIZE-4)) {
    while (o != NULL) {
      GCstr *sx = gco2str(o);
      if (sx->len == len && str_fastcmp(str, strdata(sx), len) == 0) {
	/* Resurrect if dead. Can only happen with fixstring() (keywords). */
	if (isdead(g, o)) flipwhite(o);
	return sx;  /* Return existing string. */
      }
      o = gcnext(o);
    }
  } else {  /* Slow path: end of string is too close to a page boundary. */
    while (o != NULL) {
      GCstr *sx = gco2str(o);
      if (sx->len == len && memcmp(str, strdata(sx), len) == 0) {
	/* Resurrect if dead. Can only happen with fixstring() (keywords). */
	if (isdead(g, o)) flipwhite(o);
	return sx;  /* Return existing string. */
      }
      o = gcnext(o);
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

/* Print number to buffer. Canonicalizes non-finite values. */
char * LJ_FASTCALL lj_str_bufnum(char *p, cTValue *o)
{
  if (LJ_LIKELY((o->u32.hi << 1) < 0xffe00000)) {  /* Finite? */
#if __BIONIC__
    if (tvismzero(o)) { *p++ = '-'; *p++ = '0'; return p; }
#endif
    return p + lua_number2str(p, o->n);
  } else if (((o->u32.hi & 0x000fffff) | o->u32.lo) != 0) {
    *p++ = 'n'; *p++ = 'a'; *p++ = 'n';
  } else if ((o->u32.hi & 0x80000000) == 0) {
    *p++ = 'i'; *p++ = 'n'; *p++ = 'f';
  } else {
    *p++ = '-'; *p++ = 'i'; *p++ = 'n'; *p++ = 'f';
  }
  return p;
}

#define STR_BUFINT_R(x, sh, sc) \
  { uint32_t d = (x*(((1<<sh)+sc-1)/sc))>>sh; x -= d*sc; *p++ = (char)('0'+d); }

/* Print integer to buffer. */
char * LJ_FASTCALL lj_str_bufint(char *p, int32_t k)
{
  uint32_t u = (uint32_t)k;
  if (k < 0) { u = (uint32_t)-k; *p++ = '-'; }
  if (u < 10000) {
    if (u < 10) goto dig1; if (u < 100) goto dig2; if (u < 1000) goto dig3;
  } else {
    uint32_t v = u / 10000; u -= v * 10000;
    if (v < 10000) {
      if (v < 10) goto dig5; if (v < 100) goto dig6; if (v < 1000) goto dig7;
    } else {
      uint32_t w = v / 10000; v -= w * 10000;
      if (w >= 10) STR_BUFINT_R(w, 10, 10)
      *p++ = (char)('0'+w);
    }
    STR_BUFINT_R(v, 23, 1000)
    dig7: STR_BUFINT_R(v, 12, 100)
    dig6: STR_BUFINT_R(v, 10, 10)
    dig5: *p++ = (char)('0'+v);
  }
  STR_BUFINT_R(u, 23, 1000)
  dig3: STR_BUFINT_R(u, 12, 100)
  dig2: STR_BUFINT_R(u, 10, 10)
  dig1: *p++ = (char)('0'+u);
  return p;
}

/* Print pointer to buffer. */
char * LJ_FASTCALL lj_str_bufptr(char *p, const void *v)
{
  ptrdiff_t x = (ptrdiff_t)v;
  MSize i, n = LJ_STR_PTRBUF;
  if (x == 0) {
    *p++ = 'N'; *p++ = 'U'; *p++ = 'L'; *p++ = 'L';
    return p;
  }
#if LJ_64
  /* Shorten output for 64 bit pointers. */
  n = 2+2*4+((x >> 32) ? 2+2*(lj_fls((uint32_t)(x >> 32))>>3) : 0);
#endif
  p[0] = '0';
  p[1] = 'x';
  for (i = n-1; i >= 2; i--, x >>= 4)
    p[i] = "0123456789abcdef"[(x & 15)];
  return p+n;
}

/* Print TValue to buffer (only for numbers) and return pointer to start. */
const char *lj_str_buftv(char *buf, cTValue *o, MSize *lenp)
{
  if (tvisstr(o)) {
    *lenp = strV(o)->len;
    return strVdata(o);
  } else if (tvisint(o)) {
    *lenp = (MSize)(lj_str_bufint(buf, intV(o)) - buf);
    return buf;
  } else if (tvisnum(o)) {
    *lenp = (MSize)(lj_str_bufnum(buf, o) - buf);
    return buf;
  } else {
    return NULL;
  }
}

/* Convert number to string. */
GCstr * LJ_FASTCALL lj_str_fromnum(lua_State *L, const lua_Number *np)
{
  char buf[LJ_STR_NUMBUF];
  MSize len = (MSize)(lj_str_bufnum(buf, (TValue *)np) - buf);
  return lj_str_new(L, buf, len);
}

/* Convert integer to string. */
GCstr * LJ_FASTCALL lj_str_fromint(lua_State *L, int32_t k)
{
  char buf[LJ_STR_INTBUF];
  MSize len = (MSize)(lj_str_bufint(buf, k) - buf);
  return lj_str_new(L, buf, len);
}

GCstr * LJ_FASTCALL lj_str_fromnumber(lua_State *L, cTValue *o)
{
  return tvisint(o) ? lj_str_fromint(L, intV(o)) : lj_str_fromnum(L, &o->n);
}

/* Convert char value to string. */
GCstr * LJ_FASTCALL lj_str_fromchar(lua_State *L, int c)
{
  char buf[1];
  buf[0] = c;
  return lj_str_new(L, buf, 1);
}

