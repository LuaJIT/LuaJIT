/*
** Garbage collector.
** Copyright (C) 2005-2023 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_GC_H
#define _LJ_GC_H

#include "lj_obj.h"
#include "lj_jit.h"
#include "lj_intrin.h"

/* Garbage collector states. Order matters. */
enum {
  GCSpause, GCSpropagate, GCSatomic, GCSsweepstring, GCSsweep,
  GCSsweep_blob, GCSsweep_func, GCSsweep_tab, GCSsweep_fintab,
  GCSsweep_uv, GCSsweep_udata, GCSfinalize_arena, GCSfinalize
};

/* Bitmasks for marked field of GCobj. */
#define LJ_GC_BLACK0	0x01
#define LJ_GC_BLACK1	0x02
#define LJ_GC_GRAY	0x04
#define LJ_GC_FINALIZED	0x08
#define LJ_GC_WEAKKEY	0x10
#define LJ_GC_WEAKVAL	0x08
#define LJ_GC_CDATA_FIN	0x10
#define LJ_GC_FIXED	0x20
#define LJ_GC_SFIXED	0x40

#define LJ_GC_MARK_MASK 0xE0

#define size2flags(n) (((1u << (n - 1)) - 1) << 5)
#define flags2bitmask(o, bit) (((bitmap_t)(o)->gch.gcflags >> 5) << ((bit) + 1))

#define LJ_GCMODE_MINORSWEEP 0x01
#define LJ_GCMODE_ENABLE_MINORSWEEP 0x02

#define LJ_GC_BLACKS (LJ_GC_BLACK0 | LJ_GC_BLACK1)
#define LJ_GC_COLORS (LJ_GC_BLACKS | LJ_GC_GRAY)
#define LJ_GC_WEAK	(LJ_GC_WEAKKEY | LJ_GC_WEAKVAL)

#define LJ_GC_SWEEP0 0x01
#define LJ_GC_SWEEP1 0x02
#define LJ_GC_SWEEPS (LJ_GC_SWEEP0 | LJ_GC_SWEEP1)

/* Macros to test and set GCobj colors. */
#define iswhite(g, x)	(!((x)->gch.gcflags & (g)->gc.currentblackgray))
#define isblack(g, x)	(((x)->gch.gcflags & LJ_GC_COLORS) == (g)->gc.currentblack)
#define isgray(x)	((x)->gch.gcflags & LJ_GC_GRAY)
#define tviswhite(g, x)	(tvisgcv(x) && iswhite(g, gcV(x)))

/* Death checking should only be done in asserts */
LJ_FUNC int checkdead(global_State *g, GCobj *o);

#define curwhite(g)	((g)->gc.currentwhite & LJ_GC_WHITES)
#define newwhite(x)	(obj2gco(x)->gch.gcflags = 0)
#define makewhite(x) \
  ((x)->gch.gcflags = ((x)->gch.gcflags & (uint8_t)~LJ_GC_COLORS))
#define black2gray(x)	((x)->gch.gcflags |= (uint8_t)LJ_GC_GRAY)
#define fixstring(s)	((s)->gcflags |= LJ_GC_FIXED)
#define markfinalized(x)	((x)->gch.gcflags |= LJ_GC_FINALIZED)

#define maybe_resurrect_str(g, s)                                              \
  if (LJ_UNLIKELY(iswhite(g, obj2gco(s)) && (g)->gc.state == GCSsweepstring &&           \
                  ((s)->hash & (g)->str.mask) >= (g)->gc.sweepstr))                    \
    {                                                                          \
      (s)->gcflags |= (g)->gc.currentblack;                                       \
    }

#define isminor(g) (g->gc.gcmode & LJ_GCMODE_MINORSWEEP)

/* Collector. */
LJ_FUNC void lj_gc_separateudata(global_State *g);
LJ_FUNC void lj_gc_finalize_udata(lua_State *L);
#if LJ_HASFFI
LJ_FUNC void lj_gc_finalize_cdata(lua_State *L);
#else
#define lj_gc_finalize_cdata(L)		UNUSED(L)
#endif
LJ_FUNC void lj_gc_freeall(global_State *g);
LJ_FUNCA int LJ_FASTCALL lj_gc_step(lua_State *L);
LJ_FUNCA void LJ_FASTCALL lj_gc_step_fixtop(lua_State *L);
#if LJ_HASJIT
LJ_FUNC int LJ_FASTCALL lj_gc_step_jit(global_State *g, MSize steps);
#endif
LJ_FUNC void lj_gc_fullgc(lua_State *L, int maximal);

/* GC check: drive collector forward if the GC threshold has been reached. */
#define lj_gc_check(L) \
  { if (LJ_UNLIKELY(G(L)->gc.total >= G(L)->gc.threshold)) \
      lj_gc_step(L); }
#define lj_gc_check_fixtop(L) \
  { if (LJ_UNLIKELY(G(L)->gc.total >= G(L)->gc.threshold)) \
      lj_gc_step_fixtop(L); }

/* Write barriers. */
LJ_FUNC void lj_gc_barrierf(global_State *g, GCobj *o, GCobj *v);
LJ_FUNCA void LJ_FASTCALL lj_gc_barrieruv(global_State *g, TValue *tv);
#if LJ_HASJIT
LJ_FUNC void lj_gc_barriertrace(global_State *g, uint32_t traceno);
#endif

/* Move the GC propagation frontier back for tables (make it gray again). */
static LJ_AINLINE void lj_gc_barrierback(global_State *g, GCtab *t)
{
  GCobj *o = obj2gco(t);
  lj_assertG(isblack(g, o) && !checkdead(g, o),
	     "bad object states for backward barrier");
  lj_assertG(g->gc.state != GCSfinalize && g->gc.state != GCSpause,
	     "bad GC state");
  black2gray(o);
  setgcrefr(t->gclist, g->gc.grayagain);
  setgcref(g->gc.grayagain, o);
}

/* Barrier for stores to table objects. TValue and GCobj variant. */
#define lj_gc_anybarriert(L, t)  \
  { if (LJ_UNLIKELY(isblack(G(L), obj2gco(t)))) lj_gc_barrierback(G(L), (t)); }
#define lj_gc_barriert(L, t, tv) \
  { if (tviswhite(G(L), tv) && isblack(G(L), obj2gco(t))) \
      lj_gc_barrierback(G(L), (t)); }
#define lj_gc_objbarriert(L, t, o)  \
  { if (iswhite(G(L), obj2gco(o)) && isblack(G(L), obj2gco(t))) \
      lj_gc_barrierback(G(L), (t)); }

/* Barrier for stores to any other object. TValue and GCobj variant. */
#define lj_gc_barrier(L, p, tv) \
  { if (tviswhite(G(L), tv) && isblack(G(L), obj2gco(p))) \
      lj_gc_barrierf(G(L), obj2gco(p), gcV(tv)); }
#define lj_gc_objbarrier(L, p, o) \
  { if (iswhite(G(L), obj2gco(o)) && isblack(G(L), obj2gco(p))) \
      lj_gc_barrierf(G(L), obj2gco(p), obj2gco(o)); }

/* Allocator. */
LJ_FUNC void *lj_mem_realloc(lua_State *L, void *p, GCSize osz, GCSize nsz);
LJ_FUNC void * LJ_FASTCALL lj_mem_newgco(lua_State *L, GCSize size);
LJ_FUNC void *lj_mem_grow(lua_State *L, void *p,
			  MSize *szp, MSize lim, MSize esz);

#define lj_mem_new(L, s)	lj_mem_realloc(L, NULL, 0, (s))

static LJ_AINLINE void lj_mem_free(global_State *g, void *p, size_t osize)
{
  g->gc.total -= (GCSize)osize;
  g->gc.malloc -= (GCSize)osize;
  g->allocf(g->allocd, p, osize, 0);
}

#define lj_mem_newvec(L, n, t) ((t *)lj_mem_new(L, (GCSize)((n) * sizeof(t))))
#define lj_mem_reallocvec(L, p, on, n, t)                                      \
  ((p) = (t *)lj_mem_realloc(L, p, (on) * sizeof(t), (GCSize)((n) * sizeof(t))))
#define lj_mem_growvec(L, p, n, m, t)                                          \
  ((p) = (t *)lj_mem_grow(L, (p), &(n), (m), (MSize)sizeof(t)))
#define lj_mem_freevec(g, p, n, t) lj_mem_free(g, (p), (n) * sizeof(t))

#define lj_mem_newv(L, n, t) ((t *)lj_mem_newblob(L, (MSize)((n) * sizeof(t))))
#define lj_mem_reallocv(L, p, on, n, t)                                        \
  (t *)lj_mem_reallocblob(L, p, (on) * sizeof(t),                       \
                                 (MSize)((n) * sizeof(t)))

#define lj_mem_newobj(L, t) ((t *)lj_mem_newgco(L, sizeof(t)))
#define lj_mem_newt(L, s, t)	((t *)lj_mem_new(L, (s)))
#define lj_mem_freet(g, p)	lj_mem_free(g, (p), sizeof(*(p)))

/* New GC */

LJ_FUNC GCtab *lj_mem_alloctab(lua_State *L, uint32_t asize);
LJ_FUNC GCtab *lj_mem_alloctabempty_gc(lua_State *L);
LJ_FUNC GCstr *lj_mem_allocstr(lua_State *L, MSize len);

LJ_FUNC GCupval *lj_mem_allocuv(lua_State *L);
LJ_FUNC GCudata *lj_mem_allocudata(lua_State *L, MSize bytes);
LJ_FUNC GCfunc *lj_mem_allocfunc(lua_State *L, MSize bytes);


LJ_FUNC void *lj_mem_newblob(lua_State *L, MSize sz);
LJ_FUNC void *lj_mem_reallocblob(lua_State *L, void *p, MSize osz, MSize nsz);

LJ_FUNC void lj_mem_registergc_udata(lua_State *L, GCudata *ud);

#define lj_gc_markblob(L, blob, size) \
  G(L)->gc.bloblist_usage[gcablob(blob)->id] += size

typedef uint64_t bitmap_t;
#define WORD_BITS 64
#define SIMD_BITS 256
#define BLOB_REAP_THRESHOLD (ARENA_SIZE / 3)

/* Basic requirement is that each bitmap have an increment of 256 and that each
 * area be 32-byte aligned. ELEMENTS_MAX is the maximum index allowed.
 * WORDS_FOR_TYPE is the number of 64-bit words in each header bitmap
 * WORDS_FOR_TYPE_UNROUNDED is WORDS_FOR_TYPE but not rounded up to 256 bits
 * ELEMENTS_OCCUPIED is the number of elements occupied by the header
 * HIGH_ELEMENTS_OCCUPIED is the number of elements in the last word
 */
#define ELEMENTS_MAX(type) (ARENA_SIZE / sizeof(type))
#define WORDS_FOR_TYPE(type)                                                   \
  (((ELEMENTS_MAX(type) + SIMD_BITS - 1) / SIMD_BITS) * (SIMD_BITS / WORD_BITS))
#define WORDS_FOR_SZ(sz) ((((ARENA_SIZE / sz) + SIMD_BITS - 1) / SIMD_BITS) * 4)
#define SIMD_WORDS_FOR_TYPE(type)                                              \
  ((ELEMENTS_MAX(type) + SIMD_BITS - 1) / SIMD_BITS)
#define SIMD_MULTIPLIER (sizeof(I256) / sizeof(bitmap_t))
#define WORDS_FOR_TYPE_UNROUNDED(type)                                         \
  ((ELEMENTS_MAX(type) + WORD_BITS - 1) / WORD_BITS)
#define HIGH_ELEMENTS_OCCUPIED(type)                                           \
  ((ELEMENTS_MAX(type)) & (WORD_BITS - 1))
#define ELEMENTS_OCCUPIED(hdr, type)                                           \
  ((sizeof(hdr) + sizeof(type) - 1) / sizeof(type))
#define ELEMENTS_AVAILABLE(atype, otype) (ELEMENTS_MAX(otype) - ELEMENTS_OCCUPIED(atype, otype))

#define FREE_EXTRA_MASK(type) (~0ull >> (WORD_BITS - WORDS_FOR_TYPE(type)))
#define FREE_MASK(type) (~0ull >> (WORD_BITS - WORDS_FOR_TYPE_UNROUNDED(type)))
#define FREE_LOW(atype, type) ~0ull << ELEMENTS_OCCUPIED(atype, type)
/* The else branch of the ternary is incorrect and must be guarded against,
 * but it eliminates UB an a warning. It should be resolved at compile time */
#define FREE_HIGH(type)                                                        \
  (~0ull >>                                                                    \
   HIGH_ELEMENTS_OCCUPIED(type) ? (WORD_BITS - HIGH_ELEMENTS_OCCUPIED(type)) : 1)
#define FREE_HIGH_INDEX(type) (WORDS_FOR_TYPE_UNROUNDED(type) - 1)

#define MAX_BMARRAY_SIZE (ARENA_SIZE / 16 / WORD_BITS)
LJ_STATIC_ASSERT(MAX_BMARRAY_SIZE <= WORD_BITS);

#define arena(p) ((GCAcommon *)((uintptr_t)(p)&ARENA_MASK))
#define gcat(p, t) ((t *)((uintptr_t)(p)&ARENA_MASK))
#define gcablob(p) gcat(p, GCAblob)
#define aobj(base, type, index) ((type *)(base) + (index))
#define objmask(p) ((uintptr_t)(p)&ARENA_OMASK)
/* Any good compiler should be able to turn this divide into a multiply */
#define aidx(p) (objmask(p) / sizeof(*p))
#define aidxl(i) ((i)&63)
#define aidxh(i) ((i) >> 6)
#define abit(i) (1ull << (i))

#define free_enq(a, h)                                                         \
  do {                                                                         \
    (a)->freenext = h;                                                         \
    (a)->freeprev = NULL;                                                      \
    if (h)                                                                     \
      (h)->freeprev = a;                                                       \
    h = a;                                                                     \
  } while (0)

#define do_arena_init(a, g, id, atype, otype)                                  \
  memset(a, 0, sizeof(atype));                                                 \
  a->hdr.obj_type = id;                                                        \
  a->hdr.flags = g->gc.currentsweep;                                           \
  a->free_h = FREE_MASK(otype);                                                \
  for (uint32_t i = 0; i < WORDS_FOR_TYPE_UNROUNDED(otype); i++) {             \
    a->free[i] = ~0ull;                                                        \
  }                                                                            \
  a->free[0] = FREE_LOW(atype, otype);                                         \
  if (HIGH_ELEMENTS_OCCUPIED(otype) != 0)                                      \
    a->free[FREE_HIGH_INDEX(otype)] = FREE_HIGH(otype)

typedef struct GCAcommon {
  GCArenaHdr hdr;
  bitmap_t unspecified;
  bitmap_t gray_h;
  bitmap_t mark[MAX_BMARRAY_SIZE];
  bitmap_t gray[MAX_BMARRAY_SIZE];
} GCAcommon;

/* The requirement is that the offset between gray and mark is identical,
 * but the extra space is unused so we can stuff the other arrays in there.
 * This works because tables are relatively large objects.
 */
typedef struct GCAtab {
  GCArenaHdr hdr;
  bitmap_t free_h;
  bitmap_t gray_h;
  bitmap_t mark[WORDS_FOR_TYPE(GCtab)];
  bitmap_t free[WORDS_FOR_TYPE(GCtab)];
  /*bitmap_t padding[MAX_BMARRAY_SIZE - 4 * WORDS_FOR_TYPE(GCtab)];*/
  bitmap_t weak[WORDS_FOR_TYPE(GCtab)];
  bitmap_t fin[WORDS_FOR_TYPE(GCtab)];
  bitmap_t gray[WORDS_FOR_TYPE(GCtab)];
  struct GCAtab *fin_next;
} GCAtab;

LJ_STATIC_ASSERT(sizeof(GCtab) == 64);

typedef struct GCAudata {
  GCArenaHdr hdr;
  /* This works because we cannot have a bit set in free4 if that same bit is
   * cleared in free, so the general case will stop at the set bit in free
   * first. */
  LJ_ENDIAN_LOHI(uint32_t free_h;, uint32_t free4_h);
  bitmap_t gray_h;
  bitmap_t mark[WORDS_FOR_TYPE(GCudata)];
  bitmap_t free[WORDS_FOR_TYPE(GCudata)];
  bitmap_t padding[MAX_BMARRAY_SIZE - 2 * WORDS_FOR_TYPE(GCudata)];
  bitmap_t gray[WORDS_FOR_TYPE(GCudata)];
  bitmap_t fin[WORDS_FOR_TYPE(GCudata)];
  bitmap_t fin_req[WORDS_FOR_TYPE(GCudata)];
} GCAudata;

LJ_STATIC_ASSERT(sizeof(GCudata) >= sizeof(PRNGState));

typedef struct GCAupval {
  GCArenaHdr hdr;
  bitmap_t free_h;
  bitmap_t gray_h;
  bitmap_t mark[WORDS_FOR_TYPE(GCupval)];
  bitmap_t padding[MAX_BMARRAY_SIZE - WORDS_FOR_TYPE(GCupval)];
  bitmap_t gray[WORDS_FOR_TYPE(GCupval)];
  bitmap_t free[WORDS_FOR_TYPE(GCupval)];
} GCAupval;

typedef struct GCAfunc {
  GCArenaHdr hdr;
  bitmap_t free_h;
  bitmap_t gray_h;
  bitmap_t mark[WORDS_FOR_TYPE(GCfunc)];
  bitmap_t free[WORDS_FOR_TYPE(GCfunc)];
  /*bitmap_t padding[MAX_BMARRAY_SIZE - 2 * WORDS_FOR_TYPE(GCfunc)];*/
  bitmap_t gray[WORDS_FOR_TYPE(GCfunc)];
} GCAfunc;

/* All offsets must match the common arena */
LJ_STATIC_ASSERT(offsetof(GCAtab, gray) == offsetof(GCAcommon, gray));
LJ_STATIC_ASSERT(offsetof(GCAtab, mark) == offsetof(GCAcommon, mark));
LJ_STATIC_ASSERT(offsetof(GCAudata, gray) == offsetof(GCAcommon, gray));
LJ_STATIC_ASSERT(offsetof(GCAudata, mark) == offsetof(GCAcommon, mark));
LJ_STATIC_ASSERT(offsetof(GCAupval, gray) == offsetof(GCAcommon, gray));
LJ_STATIC_ASSERT(offsetof(GCAupval, mark) == offsetof(GCAcommon, mark));
LJ_STATIC_ASSERT(offsetof(GCAfunc, gray) == offsetof(GCAcommon, gray));
LJ_STATIC_ASSERT(offsetof(GCAfunc, mark) == offsetof(GCAcommon, mark));

#if LJ_HASJIT
typedef struct GCAtrace {
  GCArenaHdr hdr;
  bitmap_t free_h;
  bitmap_t gray_h;
  bitmap_t mark[WORDS_FOR_TYPE(GCtrace)];
  bitmap_t free[WORDS_FOR_TYPE(GCtrace)];
  bitmap_t padding[MAX_BMARRAY_SIZE - 2 * WORDS_FOR_TYPE(GCtrace)];
  bitmap_t gray[WORDS_FOR_TYPE(GCtrace)];
} GCAtrace;
LJ_STATIC_ASSERT(offsetof(GCAtrace, gray) == offsetof(GCAcommon, gray));
LJ_STATIC_ASSERT(offsetof(GCAtrace, mark) == offsetof(GCAcommon, mark));
#endif

#endif
