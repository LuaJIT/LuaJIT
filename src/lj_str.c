/*
** String handling.
** Copyright (C) 2005-2016 Mike Pall. See Copyright Notice in luajit.h
*/

#define lj_str_c
#define LUA_CORE

#include "lj_obj.h"
#include "lj_gc.h"
#include "lj_err.h"
#include "lj_str.h"
#include "lj_char.h"
#if LUAJIT_SMART_STRINGS == 2
#if LJ_TARGET_POSIX
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/time.h>
#elif LJ_TARGET_WINDOWS
#include <windows.h>
#pragma comment(lib, "advapi32.dll")
#endif
#endif

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

#if LUAJIT_SMART_STRINGS==2
#if LJ_TARGET_WINDOWS
static void lj_init_strkey(struct global_State* g) {
  FILETIME ft;
  HCRYPTPROV hProvider = 0;
  const DWORD dwLength = 8;
  BYTE pbBuffer[dwLength] = {};

  if (CryptAcquireContextW(&hProvider, 0, 0, PROV_RSA_FULL, CRYPT_VERIFYCONTEXT | CRYPT_SILENT)) {
    CryptGenRandom(hProvider, sizeof(g->str_rand_key), &g->str_rand_key);
    CryptReleaseContext(hProvider, 0);
  }

  GetSystemTimeAsFileTime(&ft);
  g->str_rand_key[0] ^= (uint32_t)ft.dwLowDateTime;
  g->str_rand_key[1] ^= (uint32_t)ft.dwHighDateTime;
  if (g->str_rand_key[0] == 0 && g->str_rand_key[1] == 0)
          g->str_rand_key[0] = 1;
}
#else
static void lj_init_strkey(struct global_State* g) {
  int fd = open("/dev/urandom", O_RDONLY);
  if (fd != -1) {
    (void)read(fd, &g->str_rand_key, sizeof(g->str_rand_key));
    (void)close(fd);
  }
  struct timeval tv;
  gettimeofday(&tv, NULL);
  g->str_rand_key[0] ^= (uint32_t)tv.tv_sec;
  g->str_rand_key[1] ^= (uint32_t)tv.tv_usec;
  if (g->str_rand_key[0] == 0 && g->str_rand_key[1] == 0)
          g->str_rand_key[0] = 1;
}
#endif

// 32bit cousin to SipHash
#define HALF_ROUND(a,b,c,d,s,t)			\
  a += b; c += d;				\
  b = lj_rol(b, s) ^ a;			\
  d = lj_rol(d, t) ^ c;			\
  a = lj_rol(a, 16);

#define ROUND(v0,v1,v2,v3)		\
  HALF_ROUND(v0,v1,v2,v3,5,8);		\
  HALF_ROUND(v2,v1,v0,v3,7,13);

static MSize lj_saphash(struct global_State* g, const char *str, MSize len)
{
  uint32_t b = len << 24;
  uint32_t v0, v1, v2, v3;
  uint8_t* m;

  if ((g->str_rand_key[0] | g->str_rand_key[1])== 0)
    lj_init_strkey(g);

  v0 = g->str_rand_key[0] ^ 0x736f6d65UL;
  v1 = g->str_rand_key[1] ^ 0x326f7261UL;
  v2 = g->str_rand_key[0] ^ 0x6c796765UL;
  v3 = g->str_rand_key[1] ^ 0x74653262UL;

  while (len >= 4) {
    uint32_t mi = lj_getu32(str);
    str += 4; len -= 4;
    v3 ^= mi;
    ROUND(v0,v1,v2,v3);
    v0 ^= mi;
  }

  m = (uint8_t *)str;
  switch (len) {
    case 3: b |= m[2]<<16;
    case 2: b |= m[1]<<8;
    case 1: b |= m[0];
  }

  v3 ^= b;
  ROUND(v0,v1,v2,v3);
  v0 ^= b; v2 ^= 0xff;
  ROUND(v0,v1,v2,v3);
  ROUND(v0,v1,v2,v3);
  ROUND(v0,v1,v2,v3);
  return (v0 ^ v1) ^ (v2 ^ v3);
}
#endif

/* Intern a string and return string object. */
GCstr *lj_str_new(lua_State *L, const char *str, size_t lenx)
{
  global_State *g;
  GCstr *s;
  GCobj *o;
  MSize len = (MSize)lenx;
  MSize a, b, h = len;
#if LUAJIT_SMART_STRINGS
  int collisions = 0;
#endif
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
#if LUAJIT_SMART_STRINGS
  h &= ~strsmartbit;
#endif
  /* Check if the string has already been interned. */
  o = gcref(g->strhash[h & g->strmask]);
  if (LJ_LIKELY((((uintptr_t)str+len-1) & (LJ_PAGESIZE-1)) <= LJ_PAGESIZE-4)) {
#if LUAJIT_SMART_STRINGS
#define inc_collision_hard() (collisions+=8, 1)
#define inc_collision_soft() (collisions++)
#define max_collisions 17
#else
#define inc_collision_hard() (1)
#define inc_collision_soft()
#endif
    while (o != NULL) {
      GCstr *sx = gco2str(o);
      if (sx->hash == h && sx->len == len && inc_collision_hard() &&
                      str_fastcmp(str, strdata(sx), len) == 0) {
	/* Resurrect if dead. Can only happen with fixstring() (keywords). */
	if (isdead(g, o)) flipwhite(o);
	return sx;  /* Return existing string. */
      }
      o = gcnext(o);
      inc_collision_soft();
    }
  } else {  /* Slow path: end of string is too close to a page boundary. */
    while (o != NULL) {
      GCstr *sx = gco2str(o);
      if (sx->hash == h && sx->len == len && inc_collision_hard() &&
                      memcmp(str, strdata(sx), len) == 0) {
	/* Resurrect if dead. Can only happen with fixstring() (keywords). */
	if (isdead(g, o)) flipwhite(o);
	return sx;  /* Return existing string. */
      }
      o = gcnext(o);
      inc_collision_soft();
    }
  }
#if LUAJIT_SMART_STRINGS
#if LUAJIT_SMART_STRINGS==1
  if (len > 12)
#endif
  {
  int need_fullh = 0, search_fullh = 0;
  search_fullh = bloomtest(g->strbloom.cur[0], strbloombits0(h)) &&
    bloomtest(g->strbloom.cur[1], strbloombits1(h));
  need_fullh = search_fullh || collisions > max_collisions;
  if (LJ_UNLIKELY(need_fullh)) {
    MSize fh;
#if LUAJIT_SMART_STRINGS==1
    const char *ss = str;
    MSize i = (len-1)/8;
    fh = h ^ len;
    a = lj_getu32(str + len - 4);
    b = lj_getu32(str + len - 8);
    for (; i; i--, ss+=8) {
      fh = lj_rol(fh ^ a, 17) + (b ^ 0xdeadbeef);
      a = lj_rol(a, 13); a -= lj_getu32(ss);
      b = lj_rol(a, 11); b -= lj_getu32(ss+4);
    }
    fh = lj_rol(fh ^ a, 17) + (b ^ 0xdeadbeef);
    a ^= fh; a -= lj_rol(fh, 11);
    b ^= a;  b -= lj_rol(a, 25);
    fh ^= b; fh -= lj_rol(b, 16);
#elif LUAJIT_SMART_STRINGS==2
    fh = lj_saphash(g, str, len);
#endif
    fh |= strsmartbit;
    if (search_fullh) {
      /* Recheck if the string has already been interned with "harder" hash. */
      o = gcref(g->strhash[fh & g->strmask]);
      if (LJ_LIKELY((((uintptr_t)str+len-1) & (LJ_PAGESIZE-1)) <= LJ_PAGESIZE-4)) {
	while (o != NULL) {
	  GCstr *sx = gco2str(o);
	  if (sx->hash == fh && sx->len == len && str_fastcmp(str, strdata(sx), len) == 0) {
	    /* Resurrect if dead. Can only happen with fixstring() (keywords). */
	    if (isdead(g, o)) flipwhite(o);
	    return sx;  /* Return existing string. */
	  }
	  o = gcnext(o);
	}
      } else {  /* Slow path: end of string is too close to a page boundary. */
	while (o != NULL) {
	  GCstr *sx = gco2str(o);
	  if (sx->hash == fh && sx->len == len && memcmp(str, strdata(sx), len) == 0) {
	    /* Resurrect if dead. Can only happen with fixstring() (keywords). */
	    if (isdead(g, o)) flipwhite(o);
	    return sx;  /* Return existing string. */
	  }
	  o = gcnext(o);
	}
      }
    }
    if (collisions > 10) {
      bloomset(g->strbloom.cur[0], strbloombits0(h));
      bloomset(g->strbloom.new[0], strbloombits0(h));
      bloomset(g->strbloom.cur[1], strbloombits1(h));
      bloomset(g->strbloom.new[1], strbloombits1(h));
      h = fh;
    }
  }
  }
#endif
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

