/* To get the mremap prototype. Must be defined before any system includes. */
#if defined(__linux__) && !defined(_GNU_SOURCE)
#define _GNU_SOURCE
#endif

#include "lj_def.h"
#include "lj_arena.h"
#include "lj_gc.h"
#include "lj_obj.h"

#include <stdlib.h>
#include <string.h>

/* unmap() any misaligned huge pages */
#define LJ_ALLOC_FIXUP 1

#if LJ_TARGET_LINUX
#define LJ_ALLOC_MREMAP 1
#endif

/* COMMITLESS trades increased memory usage for many fewer syscalls. Very recommended */
#define COMMITLESS

#if LJ_TARGET_POSIX
#include <sys/mman.h>
#include <pthread.h>

#define RESERVE_PAGES(h, n)                                                    \
  mmap(h, n, PROT_WRITE | PROT_READ, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0)
#define UNRESERVE_PAGES(p, n) munmap(p, n)
#define COMMIT_PAGES(p, n)

#ifdef COMMITLESS
#define UNCOMMIT_PAGES(p, n)
#else
#define UNCOMMIT_PAGES(p, n) madvise(p, n, MADV_DONTNEED)
#endif

#define RESERVE_AND_COMMIT_PAGES(n) RESERVE_PAGES(0, n)

#elif LJ_TARGET_WINDOWS

#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

#define UNRESERVE_PAGES(p, n) VirtualFree(p, 0, MEM_RELEASE)
#define RESERVE_AND_COMMIT_PAGES(n)                                            \
  LJ_WIN_VALLOC(0, n, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE)
#ifdef COMMITLESS
#define RESERVE_PAGES(h, n) LJ_WIN_VALLOC(h, n, MEM_RESERVE|MEM_COMMIT, PAGE_READWRITE)
#define COMMIT_PAGES(p, n)
#define UNCOMMIT_PAGES(p, n)
#else
#define RESERVE_PAGES(h, n) LJ_WIN_VALLOC(h, n, MEM_RESERVE, PAGE_READWRITE)
#define COMMIT_PAGES(p, n) LJ_WIN_VALLOC(p, n, MEM_COMMIT, PAGE_READWRITE)
#define UNCOMMIT_PAGES(p, n) VirtualFree(p, n, MEM_DECOMMIT)
#endif

#else
#error "No page allocation OS support!"
#endif

#define RESERVE_SIZE 8 * 1024 * 1024
/* output >= input */
#define PTRALIGN_UP(x, y)                                                      \
  (void *)(((uintptr_t)(x) + ((y)-1)) & ~(uintptr_t)((y)-1))
/* output <= input */
#define PTRALIGN_DOWN(x, y) (void *)(((uintptr_t)(x)) & ~(uintptr_t)((y)-1))

typedef struct arena_alloc {
  void **freelist;
  uint32_t freelist_sz;
  uint32_t freelist_at;

  void **ptrs;
  uint32_t ptrs_sz;
  uint32_t ptrs_at;

  uint8_t *at;
  uint8_t *end;

  void *hint;

  global_State *g;
} arena_alloc;

int lj_arena_newchunk(arena_alloc *arenas)
{
  if (arenas->ptrs_at == arenas->ptrs_sz) {
    size_t oldsz = sizeof(void *) * arenas->ptrs_sz;
    arenas->ptrs_sz *= 2;
    void *newp = arenas->g->allocf(arenas->g->allocd, arenas->ptrs,
                                   oldsz, sizeof(void *) * arenas->ptrs_sz);
    if (!newp) {
      return 0;
    }
    arenas->ptrs = newp;
  }
  void *p = RESERVE_PAGES(arenas->hint, RESERVE_SIZE);
  if (!p) {
    p = RESERVE_PAGES(NULL, RESERVE_SIZE);
    if (!p) {
      return 0;
    }
  }
  arenas->ptrs[arenas->ptrs_at++] = p;

  arenas->at = (uint8_t *)PTRALIGN_UP(p, ARENA_SIZE);
  arenas->end = (uint8_t *)PTRALIGN_DOWN((uint8_t*)p + RESERVE_SIZE, ARENA_SIZE);
  arenas->hint = PTRALIGN_UP((uint8_t *)p + RESERVE_SIZE, ARENA_SIZE);
  return 1;
}

static void lj_arena_api_freepages(void *ud, void **pages, unsigned n)
{
  arena_alloc *arenas = (arena_alloc*)ud;
  if (LJ_UNLIKELY(!pages)) {
    for (uint32_t i = 0; i < arenas->ptrs_at; i++)
      UNRESERVE_PAGES(arenas->ptrs[i], RESERVE_SIZE);
    arenas->g->allocf(arenas->g->allocd, arenas->freelist,
                      arenas->freelist_sz * sizeof(void *), 0);
    arenas->g->allocf(arenas->g->allocd, arenas->ptrs,
                      arenas->ptrs_sz * sizeof(void*), 0);
    /* This deletes the context so must be last */
    arenas->g->allocf(arenas->g->allocd, ud, sizeof(arena_alloc), 0);
  } else {
    if (arenas->freelist_at + n > arenas->freelist_sz) {
      size_t oldsz = sizeof(void *) * arenas->ptrs_sz;
      arenas->freelist_sz *= 2;
      void *newp = arenas->g->allocf(arenas->g->allocd, arenas->freelist, oldsz,
                                     sizeof(void *) * arenas->freelist_sz);
      if (!newp) {
        /* Well this is awkward. We are freeing memory but can't allocate enough
         * for the freelist. Maybe a smaller realloc will work? */
        arenas->freelist_sz = arenas->freelist_at + n;
        newp = arenas->g->allocf(arenas->g->allocd, arenas->freelist, oldsz,
                                 sizeof(void *) * arenas->freelist_sz);
        if (!newp) {
          /* Just leak it, which does not help our OOM situation... */
          /* TODO: use arenas for the freelist, maybe chained? Pretty wasteful
           * for most scenarios that don't have gigabytes freed. Maybe release
           * pages back to the OS? */
          return;
        }
      }
      arenas->freelist = (void**)newp;
    }

    for (uint32_t i = 0; i < n; i++) {
      arenas->freelist[arenas->freelist_at++] = pages[i];
      UNCOMMIT_PAGES(pages[i], ARENA_SIZE);
    }
  }
}

static unsigned lj_arena_api_allocpages(void *ud, void **pages, unsigned n)
{
  arena_alloc *arenas = (arena_alloc*)ud;
  if (arenas->freelist_at >= n) {
    for (uint32_t i = 0; i < n; i++) {
      pages[i] = arenas->freelist[--arenas->freelist_at];
      COMMIT_PAGES(pages[i], ARENA_SIZE);
    }
  } else if (arenas->end - arenas->at >= n * ARENA_SIZE) {
    COMMIT_PAGES(arenas->at, n * ARENA_SIZE);
    for (uint32_t i = 0; i < n; i++) {
      pages[i] = arenas->at;
      arenas->at += ARENA_SIZE;
    }
  } else {
    for (uint32_t i = 0; i < n; i++) {
      if (arenas->at == arenas->end) {
        lj_arena_newchunk(arenas);
      }
      COMMIT_PAGES(arenas->at, ARENA_SIZE);
      pages[i] = arenas->at;
      arenas->at += ARENA_SIZE;
    }
  }
  return n;
}

struct posix_huge_arena
{
  void *base;
  size_t size;
};

static void *allochuge(size_t sz)
{
#if LJ_TARGET_WINDOWS
  return RESERVE_AND_COMMIT_PAGES(sz);
#else
  void *p = RESERVE_AND_COMMIT_PAGES(sz + ARENA_SIZE);
  void *s = PTRALIGN_UP(p, ARENA_SIZE);
  struct posix_huge_arena *a;
#if LJ_ALLOC_FIXUP
  if (s != p)
    UNRESERVE_PAGES(p, (uint8_t *)s - (uint8_t *)p);
  a = (struct posix_huge_arena *)s;
  a->base = s;
  a->size = sz + ARENA_SIZE - ((uint8_t *)s - (uint8_t *)p);
#else
  a->base = p;
  a->size = sz + ARENA_SIZE;
#endif
  return s;
#endif
}

static void *lj_arena_api_reallochuge(void *ud, void *p, size_t osz, size_t nsz)
{
  void *newp;
  if (!p)
    return allochuge(nsz);
#if LJ_TARGET_WINDOWS
  if (!nsz) {
    UNRESERVE_PAGES(p, sz);
    return NULL;
  }

  newp = RESERVE_AND_COMMIT_PAGES(nsz);
  if (LJ_LIKELY(newp))
    memcpy(newp, p, osz);
  UNRESERVE_PAGES(p, osz);
#else
  struct posix_huge_arena *a;
  a = (struct posix_huge_arena *)p;

  if (!nsz) {
    UNRESERVE_PAGES(a->base, a->size);
    return NULL;
  }

  if (nsz <= a->size)
    return p;

#if LJ_ALLOC_MREMAP
  /* We can use mremap, but we also require a 64k base address.
   * We could just try it and adjust but you can't move a mapping
   * to an overlapping range. But mremap overwrites existing mappings,
   * so we can use mmap to query a suitable range. */
  void *rawp = mmap(NULL, nsz + ARENA_SIZE, PROT_WRITE | PROT_READ,
                    MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  newp = PTRALIGN_UP(rawp, ARENA_SIZE);
  if (rawp != newp)
    UNRESERVE_PAGES(rawp, (uint8_t *)newp - (uint8_t *)rawp);

  /* Can either unmap or adopt the extra memory. This is one fewer syscall */
  nsz += ARENA_SIZE - ((uint8_t *)newp - (uint8_t *)rawp);
  if (mremap(a->base, a->size, nsz, MREMAP_MAYMOVE | MREMAP_FIXED,
             newp) != newp) {
    UNRESERVE_PAGES(rawp, nsz + ARENA_SIZE);
    return NULL;
  }
  a = (struct posix_huge_arena *)newp;
  a->base = newp;
  a->size = nsz;
#else
  newp = allochuge(nsz);
  memcpy(newp, p, osz);
  UNRESERVE_PAGES(a->base, a->size);
#endif
#endif
  return newp;
}

#define lj_arena_firstalloc(g, arena, id, atype, type)                         \
  {                                                                            \
    atype *a = (atype *)lj_arena_alloc(&g->gc.ctx);                            \
    if (!a)                                                                    \
      return 0;                                                                \
    arena = &a->hdr;                                                           \
    do_arena_init(a, g, id, atype, type);                                      \
  }

static int lj_blob_firstalloc(global_State *g, GCAblob **h)
{
  GCAblob *a = (GCAblob *)lj_arena_alloc(&g->gc.ctx);
  if (!a)
    return 0;
  g->gc.bloblist[0] = a;
  g->gc.bloblist_usage[0] = 0;
  a->alloc = sizeof(GCAblob);
  a->flags = LJ_GC_SWEEP0;
  a->id = 0;
  *h = a;
  return 1;
}

int lj_arena_init(struct global_State *g, luaJIT_allocpages allocp,
                  luaJIT_freepages freep, luaJIT_reallochuge realloch,
                  void *page_ud)
{
  g->gc.bloblist_alloc = 32;
  g->gc.bloblist =
      (GCAblob **)g->allocf(g->allocd, NULL, 0,
                            g->gc.bloblist_alloc * sizeof(void *));
  if (!g->gc.bloblist)
    return 0;
  g->gc.bloblist_usage = (uint32_t *)g->allocf(
      g->allocd, g->gc.bloblist_usage, 0,
      g->gc.bloblist_alloc * sizeof(uint32_t));
  if (!g->gc.bloblist_usage)
    return 0;
  g->gc.bloblist_wr = 1;

  /* All must be provided to override */
  if (allocp && freep && realloch) {
    g->gc.ctx.allocpages = allocp;
    g->gc.ctx.freepages = freep;
    g->gc.ctx.reallochuge = realloch;
    g->gc.ctx.pageud = page_ud;
  } else {
    arena_alloc *arenas = (arena_alloc*)g->allocf(g->allocd, NULL, 0, sizeof(arena_alloc));
    if (!arenas)
      return 0;
    memset(arenas, 0, sizeof(arena_alloc));
    g->gc.ctx.allocpages = &lj_arena_api_allocpages;
    g->gc.ctx.freepages = &lj_arena_api_freepages;
    g->gc.ctx.reallochuge = &lj_arena_api_reallochuge;
    g->gc.ctx.pageud = arenas;

    arenas->g = g;
    arenas->freelist_sz = 32;
    arenas->freelist = (void **)g->allocf(g->allocd, NULL, 0,
                                          arenas->freelist_sz * sizeof(void *));
    arenas->ptrs_sz = 8;
    arenas->ptrs = (void **)g->allocf(g->allocd, NULL, 0,
                                      arenas->ptrs_sz * sizeof(void *));
    if (!arenas->freelist || !arenas->ptrs || !lj_arena_newchunk(arenas)) {
      return 0;
    }
  }

  /* Allocate all arenas */
  lj_arena_firstalloc(g, g->gc.tab, ~LJ_TTAB, GCAtab, GCtab);
  lj_arena_firstalloc(g, g->gc.fintab, ~LJ_TTAB, GCAtab, GCtab);
  lj_arena_firstalloc(g, g->gc.uv, ~LJ_TUPVAL, GCAupval, GCupval);
  lj_arena_firstalloc(g, g->gc.func, ~LJ_TFUNC, GCAfunc, GCfunc);
  lj_arena_firstalloc(g, g->gc.udata, ~LJ_TUDATA, GCAudata, GCudata);

  ((GCAudata *)g->gc.udata)->free4_h = ((GCAudata *)g->gc.udata)->free_h;

  return lj_blob_firstalloc(g, &g->gc.blob_generic);
}

static void release_one_arena(struct global_State *g, void *a)
{
  lj_arena_free(&g->gc.ctx, a);
}

static void release_chain_arena(struct global_State *g, GCArenaHdr *a)
{
  while (a) {
    GCArenaHdr *n = a->next;
    release_one_arena(g, a);
    a = n;
  }
}

static void release_all_arenas(struct global_State *g)
{
  release_chain_arena(g, g->gc.tab);
  release_chain_arena(g, g->gc.fintab);
  release_chain_arena(g, g->gc.uv);
  release_chain_arena(g, g->gc.func);
  release_chain_arena(g, g->gc.udata);
  for (uint32_t i = 0; i < g->gc.bloblist_wr; i++) {
    GCAblob *a = g->gc.bloblist[i];
    if (a->flags & GCA_BLOB_HUGE)
      lj_arena_freehuge(&g->gc.ctx, a, a->alloc);
    else
      release_one_arena(g, a);
  }
}

void lj_arena_cleanup(struct global_State *g)
{
  release_all_arenas(g);
  g->allocf(g->allocd, g->gc.bloblist, g->gc.bloblist_alloc * sizeof(void *), 0);
  g->allocf(g->allocd, g->gc.bloblist_usage, g->gc.bloblist_alloc * sizeof(uint32_t), 0);

  /* Early failures may not initialize this. release_all_arenas will not have
   * any actual arenas to release, thus it is safe. */
  if (!g->gc.ctx.freepages)
    return;

  g->gc.ctx.mem_commit -= g->gc.ctx.freelist_at;
  if (g->gc.ctx.freelist_at > 0) {
    g->gc.ctx.freepages(g->gc.ctx.pageud, g->gc.ctx.freelist,
                        g->gc.ctx.freelist_at);
  }
  /* Special notice to shut down */
  g->gc.ctx.freepages(g->gc.ctx.pageud, NULL, 0);
  g->gc.ctx.freelist_at = 0;
}
