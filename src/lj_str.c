/*
** String handling.
** Copyright (C) 2005-2023 Mike Pall. See Copyright Notice in luajit.h
*/

#define lj_str_c
#define LUA_CORE

#include "lj_obj.h"
#include "lj_gc.h"
#include "lj_err.h"
#include "lj_str.h"
#include "lj_char.h"
#include "lj_prng.h"
#include "lj_intrin.h"

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

/* -- String hashing ------------------------------------------------------ */

/* Keyed sparse ARX string hash. Constant time. */
static StrHash hash_sparse(uint64_t seed, const char *str, MSize len)
{
  /* Constants taken from lookup3 hash by Bob Jenkins. */
  StrHash a, b, h = len ^ (StrHash)seed;
  if (len >= 4) {  /* Caveat: unaligned access! */
    a = lj_getu32(str);
    h ^= lj_getu32(str+len-4);
    b = lj_getu32(str+(len>>1)-2);
    h ^= b; h -= lj_rol(b, 14);
    b += lj_getu32(str+(len>>2)-1);
  } else {
    a = *(const uint8_t *)str;
    h ^= *(const uint8_t *)(str+len-1);
    b = *(const uint8_t *)(str+(len>>1));
    h ^= b; h -= lj_rol(b, 14);
  }
  a ^= h; a -= lj_rol(h, 11);
  b ^= a; b -= lj_rol(a, 25);
  h ^= b; h -= lj_rol(b, 16);
  return h;
}

#if LUAJIT_SECURITY_STRHASH
/* Keyed dense ARX string hash. Linear time. */
static LJ_NOINLINE StrHash hash_dense(uint64_t seed, StrHash h,
				      const char *str, MSize len)
{
  StrHash b = lj_bswap(lj_rol(h ^ (StrHash)(seed >> 32), 4));
  if (len > 12) {
    StrHash a = (StrHash)seed;
    const char *pe = str+len-12, *p = pe, *q = str;
    do {
      a += lj_getu32(p);
      b += lj_getu32(p+4);
      h += lj_getu32(p+8);
      p = q; q += 12;
      h ^= b; h -= lj_rol(b, 14);
      a ^= h; a -= lj_rol(h, 11);
      b ^= a; b -= lj_rol(a, 25);
    } while (p < pe);
    h ^= b; h -= lj_rol(b, 16);
    a ^= h; a -= lj_rol(h, 4);
    b ^= a; b -= lj_rol(a, 14);
  }
  return b;
}
#endif

/* -- String interning ---------------------------------------------------- */

#define LJ_STR_MAXCOLL         32
#define LJ_STR_MAXCHAIN        32

static uint32_t lj_str_get_free(StrTab *st)
{
#if LJ_64
  I256 a, b, c, d, z;
  uint32_t t1, t2;
  I256_ZERO(z);
  I256_LOADA(a, &st->strs[0]);
  I256_LOADA(b, &st->strs[4]);
  I256_LOADA(c, &st->strs[8]);
  I256_LOADA(d, &st->strs[12]);
  t1 = I256_EQ_64_MASK(a, z) | (I256_EQ_64_MASK(b, z) << 4);
  t2 = (I256_EQ_64_MASK(c, z) << 8) | (I256_EQ_64_MASK(d, z) << 12);
  return t1 | t2;
#else
  I256 x, z;
  I256_LOADA(x, &st->strs[0]);
  ret = I256_EQ_64_MASK(x, z);
  I256_LOADA(x, &st->strs[8]);
  return ret | I256_EQ_64_MASK(x, z) << 8;
#endif
}

static void lj_str_insert(lua_State *L, GCstr *s, StrHash hash, int hashalg)
{
  global_State *g = G(L);
  uint32_t index = hash & g->str.mask;
  uint32_t hid = (index << 4) | 0xFC000000;
  StrTab *st = &mref(g->str.tab, StrTab)[index];
#if LUAJIT_SECURITY_STRHASH
  /* Check for algorithm mismatch, sparse into dense list */
  if ((st->prev_len & LJ_STR_SECONDARY) && !hashalg) {
    hashalg = 1;
    hash = hash_dense(g->str.seed, hash, strdata(s), s->len);
    index = hash & g->str.mask;
    hid = (index << 4) | 0xFC000000;
    st = &mref(g->str.tab, StrTab)[index];
  }
#endif
  while(1) {
    if((st->prev_len & 0xF) < 15) {
      uint32_t i = tzcount32(lj_str_get_free(st));
      lj_assertG(!gcrefu(st->strs[i]), "bad stringtable index, occupied");
      lj_assertG(i == 0 || gcrefu(st->strs[i-1]), "bad stringtable index, nonsequential");
      st->hashes[i] = hash;
      /* NOBARRIER: string table is cleared on demand */
      setgcrefp(st->strs[i], (uintptr_t)s | hashalg);
      st->prev_len++;
      if (!hid) {
        /* There are three options here
         * 1. Directly compute it from the arena header
         * 2. Find an existing string and reuse it's hid
         * 3. Use the prev_len value of ->next
         * But since next isn't guaranteed to be populated and
         * while we *should* have a valid entry we'd have to find it,
         * it's best to just compute it directly.
         */
        GCAstrtab *a = gcat(st, GCAstrtab);
        hid =
            ((uint32_t)a->index << 13) | ((uint32_t)(st - &a->entries[0]) << 4);
      }
      s->hid = hid | i;
      return;
    }
    if(!st->next) {
      StrTab *next = lj_mem_allocstrtab(L, &s->hid);
      st->next = next;
      next->hashes[0] = hash;
      /* NOBARRIER: string table is cleared on demand */
      setgcrefp(next->strs[0], (uintptr_t)s | hashalg);
      /* We know all strings are valid but we don't easily know the ID going
       * forwards, however every string will contain it in hid. String 1 is
       * guaranteed to hold one with the correct value.
       */
      next->prev_len = st_ref(st->strs[1])->hid;
      return;
    }
    st = st->next;
    hid = 0;
  }
}

/* Resize the string interning hash table (grow and shrink). */
void lj_str_resize(lua_State *L, MSize newmask)
{
  global_State *g = G(L);
  StrTab *newtab, *oldtab = mref(g->str.tab, StrTab);
  MSize i, j;
  MSize oldmask = g->str.mask;
  StrTab *tab;

  /* No resizing if already too big. */
  if (newmask >= LJ_MAX_STRTAB-1)
    return;

  newtab = (StrTab *)lj_mem_newpages(g, (newmask + 1) * sizeof(StrTab));
  /* Already zeroed */

#if LUAJIT_SECURITY_STRHASH
  /* Check which chains need secondary hashes. */
  if (g->str.second) {
    uint32_t newsecond = 0;
    /* Compute primary chain lengths. */
    for (i = oldmask; i != ~(MSize)0; i--) {
      StrTab *tab = (StrTab *)&oldtab[i];
      do {
        for (j = 0; j < 15; j++) {
          GCstr *s = st_ref(tab->strs[j]);
          MSize hash = st_alg(tab->strs[j])
                           ? hash_sparse(g->str.seed, strdata(s), s->len)
                           : tab->hashes[j];
          newtab[hash & newmask].prev_len++;
        }
        tab = tab->next;
      } while (tab);
    }
    /* Mark secondary chains. */
    for (i = newmask; i != ~(MSize)0; i--) {
      StrTab *tab = (StrTab *)&newtab[i];
      tab->prev_len =
          (tab->prev_len > LJ_STR_MAXCOLL * 15) ? LJ_STR_SECONDARY : 0;
      newsecond |= tab->prev_len;
    }
    g->str.second = newsecond;
  }
#endif

  /* Install new table */
  setmref(g->str.tab, newtab);
  g->str.mask = newmask;

  /* Reinsert all strings from the old table into the new table. */
  for (i = 0; i < oldmask+1; i++) {
    tab = &oldtab[i];
    do {
      StrTab *old = tab;
      for (j = 0; j < 15; j++) {
        GCstr *s = st_ref(tab->strs[j]);
        if (s) {
          lj_str_insert(L, s, tab->hashes[j], st_alg(tab->strs[j]));
        }
      }
      tab = tab->next;
      if (old != &oldtab[i])
        lj_mem_freestrtab(g, old);
    } while (tab);
  }

  /* Free old table. */
  lj_mem_freepages(g, oldtab, (oldmask + 1) * sizeof(StrTab));
}

#if LUAJIT_SECURITY_STRHASH
/* Rehash and rechain all strings in a chain. */
static LJ_NOINLINE GCstr *lj_str_rehash_chain(lua_State *L, StrHash hashc,
					      const char *str, MSize len)
{
  global_State *g = G(L);
  MSize strmask = g->str.mask;
  StrTab *tab = &mref(g->str.tab, StrTab)[hashc & strmask];
  StrTab *base = tab;
  uint32_t i;

  g->str.second = 1;
  tab->prev_len |= LJ_STR_SECONDARY;
  do {
    StrTab *old = tab;
    for (i = 0; i < 15; i++) {
      if (!st_alg(tab->strs[i])) {
        GCstr *s = st_ref(tab->strs[i]);
        if (s) {
          setgcrefnull(tab->strs[i]);
          tab->prev_len--;
          lj_str_insert(
              L, s, hash_dense(g->str.seed, tab->hashes[i], strdata(s), s->len),
              1);
        }
      }
    }
    tab = tab->next;
    if (old != base && !(old->prev_len & 0xF)) {
      lj_mem_freechainedstrtab(g, old);
    }
  } while (tab);

  /* Try to insert the pending string again. */
  return lj_str_new(L, str, len);
}
#endif

/* Reseed String ID from PRNG after random interval < 2^bits. */
#if LUAJIT_SECURITY_STRID == 1
#define STRID_RESEED_INTERVAL	8
#elif LUAJIT_SECURITY_STRID == 2
#define STRID_RESEED_INTERVAL	4
#elif LUAJIT_SECURITY_STRID >= 3
#define STRID_RESEED_INTERVAL	0
#endif

/* Allocate a new string and add to string interning table. */
static GCstr *lj_str_alloc(lua_State *L, const char *str, MSize len,
			   StrHash hash, int hashalg)
{
  GCstr *s = lj_mem_allocstr(L, len);
  global_State *g = G(L);
  newwhite(s);
  s->gct = ~LJ_TSTR;
  s->len = len;
#ifndef STRID_RESEED_INTERVAL
  s->sid = g->str.id++;
#elif STRID_RESEED_INTERVAL
  if (!g->str.idreseed--) {
    uint64_t r = lj_prng_u64(&g->prng);
    g->str.id = (StrID)r;
    g->str.idreseed = (uint8_t)(r >> (64 - STRID_RESEED_INTERVAL));
  }
  s->sid = g->str.id++;
#else
  s->sid = (StrID)lj_prng_u64(&g->prng);
#endif
  s->reserved = 0;
  /* Clear last 4 bytes of allocated memory. Implies zero-termination, too. */
  *(uint32_t *)(strdatawr(s)+(len & ~(MSize)3)) = 0;
  memcpy(strdatawr(s), str, len);
  /* Add to string hash table. */
  lj_str_insert(L, s, hash, hashalg);
  if (g->str.num++ > g->str.mask * 15)        /* Allow a 100% load factor. */
    lj_str_resize(L, (g->str.mask << 1) + 1); /* Grow string table. */
  return s; /* Return newly interned string. */
}

/* Intern a string and return string object. */
GCstr *lj_str_new(lua_State *L, const char *str, size_t lenx)
{
  global_State *g = G(L);
  if (lenx-1 < LJ_MAX_STR-1) {
    I256 h0, h1, cmp;
    MSize len = (MSize)lenx;
    StrHash hash = hash_sparse(g->str.seed, str, len);
    MSize coll = 0;
    uint32_t chain = 0;
    int hashalg = 0;
    /* Check if the string has already been interned. */
    StrTab *st = &mref(g->str.tab, StrTab)[hash & g->str.mask];
    StrTab *root;
#if LUAJIT_SECURITY_STRHASH
    if (LJ_UNLIKELY(st->prev_len & LJ_STR_SECONDARY)) {  /* Secondary hash for this chain? */
      hashalg = 1;
      hash = hash_dense(g->str.seed, hash, str, len);
      st = &mref(g->str.tab, StrTab)[hash & g->str.mask];
    }
#endif
    root = st;
    I256_BCAST_32(cmp, hash);
    do {
      I256_LOADA(h0, &st->hashes[0]);
      I256_LOADA(h1, &st->hashes[8]);
      uint32_t eq = (I256_EQ_32_MASK(h0, cmp) | ((I256_EQ_32_MASK(h1, cmp) & 0x7F) << 8));

      while (eq != 0) {
        GCstr *sx = st_ref(st->strs[tzcount32(eq)]);
        eq = reset_lowest32(eq);
        if (LJ_UNLIKELY(!sx))
          continue;
        if (len == sx->len && memcmp(str, strdata(sx), len) == 0) {
          maybe_resurrect_str(g, sx);
          return sx;  /* Return existing string. */
        }
        coll++;
      }
      chain++;
      st = st->next;
    } while (st != NULL);
    if(LJ_UNLIKELY(chain > 0x3FFFFFF))
        chain = 0x3FFFFFF;
    root->prev_len = (root->prev_len & 0x1F) | (chain << 5);
#if LUAJIT_SECURITY_STRHASH
    /* Rehash chain if there are too many collisions. */
    if (LJ_UNLIKELY(coll > LJ_STR_MAXCOLL) && !hashalg) {
      return lj_str_rehash_chain(L, hash, str, len);
    }
#endif
    /* Otherwise allocate a new string. */

    return lj_str_alloc(L, str, len, hash, hashalg);
  } else {
    if (lenx)
      lj_err_msg(L, LJ_ERR_STROV);
    return g->strempty;
  }
}

void LJ_FASTCALL lj_str_init(lua_State *L)
{
  global_State *g = G(L);
  g->str.seed = lj_prng_u64(&g->prng);
  g->str.secondary_arena_free_head = -1;
  g->str.secondary_list_capacity = 8;
  g->str.secondary_slot_free_head = g->str.secondary_list_capacity - 1;
  g->str.secondary_list = lj_mem_newvec(L, g->str.secondary_list_capacity, MRef);
  for(uint32_t i = 1; i < g->str.secondary_list_capacity; i++)
    setmrefu(g->str.secondary_list[i], i-1);
  setmrefu(g->str.secondary_list[0], ~0ull);

  g->strempty = lj_mem_allocstr(L, 0);
  memset(g->strempty, 0, 32);
  g->strempty->gct = ~LJ_TSTR;
  fixstring(g->strempty);

  g->str.mask = LJ_MIN_STRTAB - 1;
  setmref(g->str.tab, lj_mem_newpages(g, LJ_MIN_STRTAB * sizeof(StrTab)));
}

void lj_str_freetab(global_State *g)
{
  lj_mem_freepages(g, mref(g->str.tab, void), (g->str.mask + 1) * sizeof(StrTab));
}
