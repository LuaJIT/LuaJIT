#ifndef _LJ_ARENA_H
#define _LJ_ARENA_H

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "lj_arch.h"
#include "lj_def.h"

typedef unsigned (*luaJIT_allocpages)(void *ud, void **pages, unsigned n);
typedef void (*luaJIT_freepages)(void *ud, void **pages, unsigned n);
typedef void *(*luaJIT_reallochuge)(void *ud, void *p, size_t osz, size_t nsz);

#define ARENA_SHIFT 16
#define ARENA_SIZE (1u << ARENA_SHIFT)
#define ARENA_HUGE_THRESHOLD 16000
#define ARENA_MASK (~0ull << ARENA_SHIFT)
#define ARENA_OMASK (ARENA_SIZE - 1)

#define ARENA_FREELIST_SIZE 32
#define ARENA_FREELIST_CHUNK (ARENA_FREELIST_SIZE / 2)
#define CHUNK_SIZE (ARENA_SIZE * ARENA_FREELIST_CHUNK)
#define MINIMUM_QUOTA (32 * ARENA_SIZE)

struct global_State;

/* These are here so obj.h can see them */
/* Common header for all bitmap arenas */
typedef struct GCArenaHdr {
  struct GCArenaHdr *prev, *next;
  struct GCArenaHdr *freeprev, *freenext;
  struct GCArenaHdr *gray;
  uint32_t obj_type;
  uint32_t flags;
} GCArenaHdr;

/* If set this arena is being moved from */
#define GCA_BLOB_REAP 0x08
#define GCA_BLOB_HUGE 0x10

/* Blob arenas don't hold objects but instead data pointed to by objects.
 * All blob arenas are solely bump allocated and compacted as needed.
 */
typedef struct GCAblob {
  uint8_t reserved_for_impl[20];
  uint32_t id;
  uint32_t alloc;  /* Next byte to be allocated from */
  uint32_t flags;
} GCAblob;

LJ_STATIC_ASSERT(sizeof(GCAblob) % 16 == 0);

typedef struct arena_context {
  /* In arenas */
  uint32_t mem_commit;
  uint32_t mem_watermark;
  /* Bytes */
  uint64_t mem_huge;

  /* The free cache represents the "working memory" of the state */
  uint32_t freelist_at;
  void *freelist[ARENA_FREELIST_SIZE];

  luaJIT_allocpages allocpages;
  luaJIT_freepages freepages;
  luaJIT_reallochuge reallochuge;
  void *pageud;
} arena_context;

int lj_arena_init(struct global_State *g, luaJIT_allocpages allocp,
                  luaJIT_freepages freep, luaJIT_reallochuge realloch,
                  void *page_ud);
void lj_arena_cleanup(struct global_State *g);

/* Add ARENA_FREELIST_CHUNK free arenas */
inline void *lj_arena_alloc(arena_context *ctx)
{
  if (!ctx->freelist_at) {
    ctx->freelist_at =
        ctx->allocpages(ctx->pageud, ctx->freelist, ARENA_FREELIST_CHUNK);
    ctx->mem_commit += ctx->freelist_at;
    if (ctx->mem_commit > ctx->mem_watermark)
      ctx->mem_watermark = ctx->mem_commit;
    if (!ctx->freelist_at)
      return NULL;
  }
  void *p = ctx->freelist[--ctx->freelist_at];
  return p;
}

/* Free the last ARENA_FREELIST_CHUNK arenas */
inline void lj_arena_free(arena_context *ctx, void *p)
{
  if (ctx->freelist_at == ARENA_FREELIST_SIZE) {
    ctx->freepages(ctx->pageud,
                   ctx->freelist +
                       (ARENA_FREELIST_SIZE - ARENA_FREELIST_CHUNK),
                   ARENA_FREELIST_CHUNK);
    ctx->freelist_at -= ARENA_FREELIST_CHUNK;
    ctx->mem_commit -= ARENA_FREELIST_CHUNK;
  }
  ctx->freelist[ctx->freelist_at++] = p;
}

inline void *lj_arena_allochuge(arena_context *ctx, size_t sz)
{
  ctx->mem_huge += sz;
  return ctx->reallochuge(ctx->pageud, NULL, 0, sz);
}

inline void lj_arena_freehuge(arena_context *ctx, void *p, size_t sz)
{
  ctx->mem_huge -= sz;
  ctx->reallochuge(ctx->pageud, p, sz, 0);
}

inline void *lj_arena_reallochuge(arena_context *ctx, void *p, size_t osz, size_t nsz)
{
  ctx->mem_huge = ctx->mem_huge - osz + nsz;
  return ctx->reallochuge(ctx->pageud, p, osz, nsz);
}

#endif
