/*
** Garbage collector.
** Copyright (C) 2005-2023 Mike Pall. See Copyright Notice in luajit.h
**
** Major portions taken verbatim or adapted from the Lua interpreter.
** Copyright (C) 1994-2008 Lua.org, PUC-Rio. See Copyright Notice in lua.h
*/

#define lj_gc_c
#define LUA_CORE

#include "lj_obj.h"
#include "lj_gc.h"
#include "lj_err.h"
#include "lj_buf.h"
#include "lj_str.h"
#include "lj_tab.h"
#include "lj_func.h"
#include "lj_udata.h"
#include "lj_meta.h"
#include "lj_state.h"
#include "lj_frame.h"
#if LJ_HASFFI
#include "lj_ctype.h"
#include "lj_cdata.h"
#endif
#include "lj_trace.h"
#include "lj_dispatch.h"
#include "lj_vm.h"
#include "lj_vmevent.h"
#include "lj_intrin.h"

#define GCSTEPSIZE	1024u
#define GCSWEEPMAX	40
#define GCSWEEPCOST	10
#define GCFINALIZECOST	100

/* Macros to set GCobj colors and flags. */
#define white2gray(x)		((x)->gch.gcflags |= (uint8_t)LJ_GC_GRAY)
#define gray2black(g, x)                                                       \
  ((x)->gch.gcflags = (((x)->gch.gcflags & (uint8_t)~LJ_GC_COLORS) | (g)->gc.currentblack))
#define isfinalized(u)		((u)->gcflags & LJ_GC_FINALIZED)

#define lj_huge_str_size(len) \
  len + 1 + offsetof(GCAstr, mark[2]) + sizeof(GCstr)

/* -- Mark phase ---------------------------------------------------------- */

#define gray_enq(a, g)                                                         \
  do {                                                                         \
    a->hdr.gray = NULL;                                                        \
    if (LJ_LIKELY(g->gc.gray_head)) {                                          \
      g->gc.gray_tail->gray = &a->hdr;                                         \
    } else {                                                                   \
      g->gc.gray_head = &a->hdr;                                               \
    }                                                                          \
    g->gc.gray_tail = &a->hdr;                                                 \
  } while (0)

/* Mark a TValue (if needed). */
#define gc_marktv(g, tv) \
  { lj_assertG(!tvisgcv(tv) || (~itype(tv) == gcval(tv)->gch.gct), \
	       "TValue and GC type mismatch %d vs %d", ~itype(tv),             \
               gcval(tv)->gch.gct); \
    if (tviswhite(g, tv)) gc_mark_type(g, gcV(tv), ~itype(tv)); }

/* Mark a GCobj (if needed). */
#define gc_markobj(g, o) \
  { if (iswhite(g, obj2gco(o))) gc_mark_type(g, obj2gco(o), obj2gco(o)->gch.gct); }

/* Mark a string object. */
#define gc_mark_str(g, s)		((s)->gcflags |= (g)->gc.currentblack)

static void *lj_mem_newblob_g(global_State *g, MSize sz);
static void gc_presweep_udata(global_State *g, GCAudata *a);

static LJ_NOINLINE uintptr_t move_blob(global_State *g, uintptr_t src, MSize sz)
{
  void *newp = lj_mem_newblob_g(g, sz);
  g->gc.bloblist_usage[gcablob(newp)->id] += sz;
  memcpy(newp, (void *)src, sz);
  return (uintptr_t)newp;
}

#define mark_blob(g, b, sz)                                                    \
  do {                                                                         \
    GCAblob *a = gcablob(b);                                                   \
    if (LJ_UNLIKELY((a->flags & GCA_BLOB_REAP))) {                             \
      b = move_blob(g, b, sz);                                                 \
    } else {                                                                   \
      g->gc.bloblist_usage[a->id] += sz;                                       \
    }                                                                          \
  } while (0)

#define maybe_mark_blob(g, b, sz)                                              \
  if (sz > 0) {                                                                \
    mark_blob(g, b, sz);                                                       \
  }

/* We only need to divide a small range and never need to divide
 * anything with a remainder. An assert checks this is correct.
 * Shifts of 16 to 52 work. Shift of 16 results in smaller
 * constants. At 32 x86 might schedule edx:eax mul for
 * shift-free access
 */
#define MULTIPLICATIVE_INVERSE_SHIFT 32
#define MULTIPLICATIVE_INVERSE(x) \
    (uint32_t)(1 + (1ull << MULTIPLICATIVE_INVERSE_SHIFT) / x)

/* ORDER LJ_T */
const uint32_t kInverseDividers[~LJ_TNUMX] = {
    0, 0, 0, 0,
    MULTIPLICATIVE_INVERSE(sizeof(GCstr)),
    MULTIPLICATIVE_INVERSE(sizeof(GCupval)),
    0, 0,
    MULTIPLICATIVE_INVERSE(sizeof(GCfunc)),
    0, 0,
    MULTIPLICATIVE_INVERSE(sizeof(GCtab)),
    MULTIPLICATIVE_INVERSE(sizeof(GCudata)),
};
const uint32_t kDividers[~LJ_TNUMX] = {
    0, 0, 0, 0,
    sizeof(GCstr),
    sizeof(GCupval),
    0, 0,
    sizeof(GCfunc),
    0, 0,
    sizeof(GCtab),
    sizeof(GCudata),
};

#define is_arena_obj(t) (kInverseDividers[t] != 0)

/* Mark a white GCobj. */
static void gc_mark_type(global_State *g, GCobj *o, int gct)
{
  lj_assertG(gct == o->gch.gct, "GC type mismatch obj %d / param %d",
             o->gch.gct, gct);
  if (gct == ~LJ_TSTR) {
    /* There is a choice, we can either modify the object here, or we can
     * put it on the gray queue and process it normally. If we mark it black
     * here we can avoid the mark & bit branch below and triggering further
     * barriers and avoid arena traversal. Doing it this way also allows
     * a permanent gray state for fixed objects.
     */
    GCAcommon *a = arena(o);
    uint32_t idx = (uint32_t)(objmask(o) >> 4);
    a->mark[aidxh(idx)] |= abit(aidxl(idx));
    o->gch.gcflags = (o->gch.gcflags & ~LJ_GC_BLACKS) | g->gc.currentblack;
    return;
  }

  if (LJ_LIKELY(kInverseDividers[gct])) {
    /* Generic arena marking */
    GCAcommon *a = arena(o);
    /* mul + shift should be much faster than div on every CPU */
    uint32_t idx = (uint32_t)((objmask(o) * (uintptr_t)kInverseDividers[gct]) >>
                              MULTIPLICATIVE_INVERSE_SHIFT);
    uint32_t h = aidxh(idx);
    uint64_t bit = 1ull << aidxl(idx);
    lj_assertG(idx <= ARENA_SIZE / 16, "index out of range");
    lj_assertG(idx == objmask(o) / kDividers[gct], "bad divider!");
    lj_assertG(!((((uintptr_t)(o)&ARENA_OMASK) % (uintptr_t)(kDividers[gct]))),
               "index not multiple of divisor!");
    lj_assertG(gct == ~LJ_TUPVAL || gct == ~LJ_TSTR || gct == ~LJ_TFUNC ||
                   gct == ~LJ_TTAB || gct == ~LJ_TUDATA,
               "bad GC type %d", gct);
    if (!(a->mark[h] & bit)) {
      if (!a->gray_h) {
        gray_enq(a, g);
      }
      a->gray_h |= 1ull << h;
      a->mark[h] |= bit;
      a->gray[h] |= bit;
    }
    return;
  }
  lj_assertG(iswhite(g, o), "mark of non-white object");
  lj_assertG(!checkdead(g, o), "mark of dead object");
  white2gray(o);
  if (gct != ~LJ_TCDATA) {
    lj_assertG(gct == ~LJ_TTHREAD || gct == ~LJ_TPROTO ||
               gct == ~LJ_TTRACE, "bad GC type %d", gct);
    lj_assertG(o->gch.gcflags & LJ_GC_GRAY, "not gray?");
    setgcrefr(o->gch.gclist, g->gc.gray);
    setgcref(g->gc.gray, o);
  }
}

static void gc_mark_uv(global_State *g, GCupval *o)
{
  GCAupval *a = gcat(o, GCAupval);
  uint32_t idx = aidx(o);
  uint32_t h = aidxh(idx);
  uint64_t bit = 1ull << aidxl(idx);
  lj_assertG(idx >= ELEMENTS_OCCUPIED(GCAupval, GCupval) &&
                 idx < ELEMENTS_MAX(GCupval),
             "bad obj pointer");
  lj_assertG(~LJ_TUPVAL == o->gct, "not a upval");
  if (!(a->mark[h] & bit)) {
    if (!a->gray_h) {
      gray_enq(a, g);
    }
    a->gray_h |= 1ull << h;
    a->mark[h] |= bit;
    a->gray[h] |= bit;
  }
}

static void gc_mark_tab(global_State *g, GCtab *o)
{
  GCAtab *a = gcat(o, GCAtab);
  uint32_t idx = aidx(o);
  uint32_t h = aidxh(idx);
  uint64_t bit = 1ull << aidxl(idx);
  lj_assertG(idx >= ELEMENTS_OCCUPIED(GCAtab, GCtab) &&
                 idx < ELEMENTS_MAX(GCtab),
             "bad obj pointer");
  lj_assertG(~LJ_TTAB == o->gct, "not a table");
  if (!(a->mark[h] & bit)) {
    if (!a->gray_h) {
      gray_enq(a, g);
    }
    a->gray_h |= 1ull << h;
    a->mark[h] |= bit;
    a->gray[h] |= bit;
  }
}

/* Mark GC roots. */
static void gc_mark_gcroot(global_State *g)
{
  ptrdiff_t i;
  for (i = 0; i < GCROOT_MAX; i++)
    if (gcref(g->gcroot[i]) != NULL)
      gc_markobj(g, gcref(g->gcroot[i]));
}

/* Start a GC cycle and mark the root set. */
static void gc_mark_start(global_State *g)
{
  setgcrefnull(g->gc.gray);
  setgcrefnull(g->gc.grayagain);
  setgcrefnull(g->gc.weak);
  gc_markobj(g, mainthread(g));
  gc_mark_tab(g, tabref(mainthread(g)->env));
  gc_marktv(g, &g->registrytv);
  gc_mark_gcroot(g);
#if LJ_HASFFI
  if (ctype_ctsG(g)) gc_markobj(g, ctype_ctsG(g)->finalizer);
#endif
  g->gc.state = GCSpropagate;
  g->gc.accum = 0;
}

/* Separate userdata objects to be finalized to mmudata list. */
void lj_gc_separateudata(global_State *g)
{
  GCArenaHdr *a;
  uint32_t i;
  setgcrefnull(g->gc.fin_list);
  for (a = g->gc.udata; a; a = a->next) {
    GCAudata *ud = (GCAudata *)a;
    for (i = 0; i < WORDS_FOR_TYPE_UNROUNDED(GCudata); i++)
      ud->mark[i] &= ~ud->fin_req[i];
  }
  gc_presweep_udata(g, (GCAudata *)g->gc.udata);
}

/* -- Propagation phase --------------------------------------------------- */

/* Traverse a table. */
static void gc_mark_tab_hash(global_State *g, GCtab *t)
{
  MSize hmask = t->hmask;
  MSize size = (hmask + 1) * sizeof(Node);
  GCAblob *a = gcablob(mrefu(t->node));
  if (LJ_UNLIKELY(((a->flags & GCA_BLOB_REAP) != 0 && !mrefu(g->jit_base)))) {
    /* Rewrite everything to account for the new location */
    ptrdiff_t old_addr = mrefu(t->node);
    ptrdiff_t new_addr = move_blob(g, old_addr, size);
    ptrdiff_t diff = new_addr - old_addr;
    setmrefu(t->node, new_addr);
    for (uint32_t i = 0; i <= hmask; i++) {
      Node *n = mref(t->node, Node) + i;
      if (mrefu(n->next)) {
        setmrefu(n->next, mrefu(n->next) + diff);
      }
    }
    if (mref(t->freetop, Node) != &g->nilnode) {
      setmrefu(t->freetop, mrefu(t->freetop) + diff);
    }
  } else {
    g->gc.bloblist_usage[a->id] += size;
  }
}

static int gc_traverse_tab(global_State *g, GCtab *t)
{
  int weak = 0;
  cTValue *mode;
  GCtab *mt = tabref(t->metatable);
  if (mt)
    gc_mark_tab(g, mt);
  mode = lj_meta_fastg(g, mt, MM_mode);
  if (mode && tvisstr(mode)) {  /* Valid __mode field? */
    const char *modestr = strVdata(mode);
    int c;
    while ((c = *modestr++)) {
      if (c == 'k') weak |= LJ_GC_WEAKKEY;
      else if (c == 'v') weak |= LJ_GC_WEAKVAL;
    }
    if (weak) {  /* Weak tables are cleared in the atomic phase. */
#if LJ_HASFFI
      CTState *cts = ctype_ctsG(g);
      if (cts && cts->finalizer == t) {
	weak = (int)(~0u & ~LJ_GC_WEAKVAL);
      } else
#endif
      {
	t->gcflags = (uint8_t)((t->gcflags & ~LJ_GC_WEAK) | weak);
      }
    }
  }
  if (!(t->gcflags & LJ_GC_MARK_MASK) && mrefu(t->array)) {
    GCAblob *a = gcablob(mref(t->array, void));
    if (LJ_UNLIKELY(((a->flags & GCA_BLOB_REAP) != 0 && !mrefu(g->jit_base)))) {
      setmrefu(t->array, move_blob(g, mrefu(t->array), t->asize * sizeof(TValue)));
    } else {
      g->gc.bloblist_usage[a->id] += t->asize * sizeof(TValue);
    }
  }
  /* Nothing to mark if both keys/values are weak or ephemeron. */
  if (weak > LJ_GC_WEAKVAL)
    return weak;
  /* We can't move table data while on a trace */
  if (!(weak & LJ_GC_WEAKVAL)) {  /* Mark array part. */
    MSize i, asize = t->asize;
    for (i = 0; i < asize; i++)
      gc_marktv(g, arrayslot(t, i));
  }
  if (t->hmask > 0) {  /* Mark hash part. */
    Node *node;
    MSize i, hmask = t->hmask;
    gc_mark_tab_hash(g, t);
    node = noderef(t->node);

    for (i = 0; i <= hmask; i++) {
      Node *n = &node[i];
      if (!tvisnil(&n->val)) { /* Mark non-empty slot. */
        lj_assertG(!tvisnil(&n->key), "mark of nil key in non-empty slot");
        /* TODO this is *only* required for FFI finalizer table */
        if (!(weak & LJ_GC_WEAKKEY)) gc_marktv(g, &n->key);
        if (!(weak & LJ_GC_WEAKVAL)) gc_marktv(g, &n->val);
      }
    }
  }
  return weak;
}

/* Traverse a function. */
static void gc_traverse_func(global_State *g, GCfunc *fn)
{
  gc_mark_tab(g, tabref(fn->c.env));
  if (isluafunc(fn)) {
    uint32_t i;
    lj_assertG(fn->l.nupvalues <= funcproto(fn)->sizeuv,
	       "function upvalues out of range");
    gc_markobj(g, funcproto(fn));
    for (i = 0; i < fn->l.nupvalues; i++)  /* Mark Lua function upvalues. */
      gc_mark_uv(g, gco2uv(gcref(fn->l.uvptr[i])));
  } else {
    uint32_t i;
    for (i = 0; i < fn->c.nupvalues; i++)  /* Mark C function upvalues. */
      gc_marktv(g, &fn->c.data->upvalue[i]);
  }
}

#if LJ_HASJIT
/* Mark a trace. */
static void gc_marktrace(global_State *g, TraceNo traceno)
{
  GCobj *o = obj2gco(traceref(G2J(g), traceno));
  lj_assertG(traceno != G2J(g)->cur.traceno, "active trace escaped");
  if (iswhite(g, o)) {
    white2gray(o);
    lj_assertG(o->gch.gcflags & LJ_GC_GRAY, "not gray?");
    setgcrefr(o->gch.gclist, g->gc.gray);
    setgcref(g->gc.gray, o);
  }
}

/* Traverse a trace. */
static void gc_traverse_trace(global_State *g, GCtrace *T)
{
  IRRef ref;
  if (T->traceno == 0) return;
  for (ref = T->nk; ref < REF_TRUE; ref++) {
    IRIns *ir = &T->ir[ref];
    if (ir->o == IR_KGC)
      gc_markobj(g, ir_kgc(ir));
    if (irt_is64(ir->t) && ir->o != IR_KNULL)
      ref++;
  }
  if (T->link) gc_marktrace(g, T->link);
  if (T->nextroot) gc_marktrace(g, T->nextroot);
  if (T->nextside) gc_marktrace(g, T->nextside);
  gc_markobj(g, gcref(T->startpt));
}

/* The current trace is a GC root while not anchored in the prototype (yet). */
#define gc_traverse_curtrace(g)	gc_traverse_trace(g, &G2J(g)->cur)
#else
#define gc_traverse_curtrace(g)	UNUSED(g)
#endif

/* Traverse a prototype. */
static void gc_traverse_proto(global_State *g, GCproto *pt)
{
  ptrdiff_t i;
  gc_mark_str(g, proto_chunkname(pt));
  for (i = -(ptrdiff_t)pt->sizekgc; i < 0; i++)  /* Mark collectable consts. */
    gc_markobj(g, proto_kgc(pt, i));
#if LJ_HASJIT
  if (pt->trace) gc_marktrace(g, pt->trace);
#endif
}

/* Traverse the frame structure of a stack. */
static MSize gc_traverse_frames(global_State *g, lua_State *th)
{
  TValue *frame, *top = th->top-1, *bot = tvref(th->stack);
  /* Note: extra vararg frame not skipped, marks function twice (harmless). */
  for (frame = th->base-1; frame > bot+LJ_FR2; frame = frame_prev(frame)) {
    GCfunc *fn = frame_func(frame);
    TValue *ftop = frame;
    if (isluafunc(fn)) ftop += funcproto(fn)->framesize;
    if (ftop > top) top = ftop;
    if (!LJ_FR2) gc_markobj(g, fn);  /* Need to mark hidden function (or L). */
  }
  top++;  /* Correct bias of -1 (frame == base-1). */
  if (top > tvref(th->maxstack)) top = tvref(th->maxstack);
  return (MSize)(top - bot);  /* Return minimum needed stack size. */
}

/* Traverse a thread object. */
static void gc_traverse_thread(global_State *g, lua_State *th)
{
  TValue *o, *top = th->top;
  for (o = tvref(th->stack)+1+LJ_FR2; o < top; o++)
    gc_marktv(g, o);
  if (g->gc.state == GCSatomic) {
    top = tvref(th->stack) + th->stacksize;
    for (; o < top; o++)  /* Clear unmarked slots. */
      setnilV(o);
  }
  gc_mark_tab(g, tabref(th->env));
  lj_state_shrinkstack(th, gc_traverse_frames(g, th));
}

static size_t traverse_upvals(global_State *g, GCAupval *a, size_t threshold)
{
  size_t ret = 0;
  for (uint32_t i = tzcount64(a->gray_h); a->gray_h;
       a->gray_h = reset_lowest64(a->gray_h), i = tzcount64(a->gray_h)) {
    /* It is not allowed to synchronously change a->gray[i] */
    uint64_t v = a->gray[i];
    while(v) {
      GCupval *uv = aobj(a, GCupval, (i << 6) + tzcount64(v));
      uv->gcflags = g->gc.currentblack;
      v = reset_lowest64(v);
      ret += sizeof(GCupval);
      gc_marktv(g, uvval(uv));
      if (ret >= threshold) {
        a->gray[i] = v;
        return ret;
      }
    }
    a->gray[i] = 0;
  }
  g->gc.gray_head = a->hdr.gray;
  return ret;
}

static size_t traverse_funcs(global_State *g, GCAfunc *a, size_t threshold)
{
  size_t ret = 0;
  for (uint32_t i = tzcount64(a->gray_h); a->gray_h;
       a->gray_h = reset_lowest64(a->gray_h), i = tzcount64(a->gray_h)) {
    /* It is not allowed to synchronously change a->gray[i] */
    uint64_t v = a->gray[i];
    while (v) {
      uint32_t j = tzcount64(v);
      GCfunc *fn = aobj(a, GCfunc, (i << 6) + j);
      MSize size = isluafunc(fn) ? sizeLfunc((MSize)fn->l.nupvalues)
                                 : sizeCfunc((MSize)fn->c.nupvalues);
      gray2black(g, obj2gco(fn));
      if (!(fn->gen.gcflags & LJ_GC_MARK_MASK)) {
        maybe_mark_blob(g, mrefu(fn->gen.data), size);
      }
      v = reset_lowest64(v);
      ret += sizeof(GCfunc) + size;
      a->mark[i] |= flags2bitmask(obj2gco(fn), j);
      gc_traverse_func(g, fn);
      if (ret >= threshold) {
        a->gray[i] = v;
        return ret;
      }
    }
    a->gray[i] = 0;
  }
  g->gc.gray_head = a->hdr.gray;
  return ret;
}

static size_t traverse_tables(global_State *g, GCAtab *a, size_t threshold)
{
  size_t ret = 0;
  /* Tables could contain other table refs and those refs could be to
   * this arena, so we must handle cases where gc_traverse_tab sets
   * bits in the current or previous words. */
  while (a->gray_h) {
    uint32_t i = tzcount64(a->gray_h);
    for (uint32_t j = tzcount64(a->gray[i]); a->gray[i];
         j = tzcount64(a->gray[i])) {
      GCtab *t = aobj(a, GCtab, (i << 6) + j);
      gray2black(g, obj2gco(t));
      a->gray[i] = reset_lowest64(a->gray[i]);

      ret += sizeof(GCtab) + sizeof(TValue) * t->asize +
             (t->hmask ? sizeof(Node) * (t->hmask + 1) : 0);
      a->mark[i] |= flags2bitmask(obj2gco(t), j);
      if (gc_traverse_tab(g, t) > 0) {
        /* Weak tables go onto the grayagain list */
        t->gcflags |= LJ_GC_GRAY;
        setgcrefr(t->gclist, g->gc.grayagain);
        setgcref(g->gc.grayagain, obj2gco(t));
      }
      if (ret >= threshold)
        return ret;
    }
    a->gray_h ^= 1ull << i;
  }
  g->gc.gray_head = a->hdr.gray;
  return ret;
}

static size_t traverse_udata(global_State *g, GCAudata *a, size_t threshold)
{
  size_t ret = 0;
  for (uint32_t i = tzcount64(a->gray_h); a->gray_h;
       a->gray_h = reset_lowest64(a->gray_h), i = tzcount64(a->gray_h)) {
    /* It is not allowed to synchronously change a->gray[i] */
    uint64_t v = a->gray[i];
    while (v) {
      uint32_t j = tzcount64(v);
      GCudata *ud = aobj(a, GCudata, (i << 6) + j);
      GCtab *mt = tabref(ud->metatable);
      v = reset_lowest64(v);
      gray2black(g, obj2gco(ud));
      a->gray[i] = reset_lowest64(a->gray[i]);
      /* If this occupies multiple slots mark them all */

      a->mark[i] |= flags2bitmask(obj2gco(ud), j);
      if (mt)
        gc_mark_tab(g, mt);
      gc_mark_tab(g, tabref(ud->env));
      if (LJ_HASBUFFER && ud->udtype == UDTYPE_BUFFER) {
        SBufExt *sbx = (SBufExt *)uddata(ud);
        if (sbufiscow(sbx) && gcref(sbx->cowref))
          gc_markobj(g, gcref(sbx->cowref));
        if (gcref(sbx->dict_str))
          gc_mark_tab(g, tabref(sbx->dict_str));
        if (gcref(sbx->dict_mt))
          gc_mark_tab(g, tabref(sbx->dict_mt));
      }
      ret += sizeof(GCudata);
      if (ret >= threshold) {
        a->gray[i] = v;
        return ret;
      }
    }
    a->gray[i] = 0;
  }
  g->gc.gray_head = a->hdr.gray;
  return ret;
}

/* Propagate arena objects */
static size_t propagatemark_arena(global_State *g, size_t threshold)
{
  GCArenaHdr *a = g->gc.gray_head;
  size_t ret = 0;
  switch (a->obj_type) {
  case ~LJ_TUPVAL:
    ret = traverse_upvals(g, (GCAupval *)a, threshold);
    break;
  case ~LJ_TFUNC:
    ret = traverse_funcs(g, (GCAfunc *)a, threshold);
    break;
  case ~LJ_TTAB:
    ret = traverse_tables(g, (GCAtab *)a, threshold);
    break;
  case ~LJ_TUDATA:
    ret = traverse_udata(g, (GCAudata *)a, threshold);
    break;
  default:
    lj_assertG(0, "bad arena type");
    break;
  }
  g->gc.accum += ret;
  return ret;
}

/* Propagate one gray object. Traverse it and turn it black. */
static size_t propagatemark(global_State *g)
{
  GCobj *o = gcref(g->gc.gray);
  int gct = o->gch.gct;
  lj_assertG(isgray(o), "propagation of non-gray object");
  gray2black(g, o);
  setgcrefr(g->gc.gray, o->gch.gclist); /* Remove from gray list. */
  if (LJ_LIKELY(gct == ~LJ_TPROTO)) {
    GCproto *pt = gco2pt(o);
    gc_traverse_proto(g, pt);
    return pt->sizept;
  } else if (LJ_LIKELY(gct == ~LJ_TTHREAD)) {
    lua_State *th = gco2th(o);
    setgcrefr(th->gclist, g->gc.grayagain_th);
    setgcref(g->gc.grayagain_th, o);
    black2gray(o);  /* Threads are never black. */
    gc_traverse_thread(g, th);
    return sizeof(lua_State) + sizeof(TValue) * th->stacksize;
  } else {
#if LJ_HASJIT
    GCtrace *T = gco2trace(o);
    gc_traverse_trace(g, T);
    return ((sizeof(GCtrace) + 7) & ~7) + (T->nins - T->nk) * sizeof(IRIns) +
           T->nsnap * sizeof(SnapShot) + T->nsnapmap * sizeof(SnapEntry);
#else
    lj_assertG(0, "bad GC type %d", gct);
    return 0;
#endif
  }
}

static void sweep_upvals(global_State *g)
{
  int c = g->gc.currentblack;
  for (GCobj *o = gcref(g->gc.grayagain_th); o; o = gcref(o->gch.gclist)) {
    GCobj *uvo;
    GCRef *uvp = &gco2th(o)->openupval;
    GCupval *uv;
    /* Need to sweep dead upvals */
    while ((uvo = gcref(*uvp)) != NULL) {
      uv = &uvo->uv;
      if (uv->gcflags & c) {
        uvp = &uv->next;
      } else {
        setgcrefr(*uvp, uv->next);
      }
    }
  }
}

static void propagatemark_again(global_State *g)
{
  for (GCobj *o = gcref(g->gc.grayagain); o;) {
    GCobj *n = gcref(o->gch.gclist);
    int x;
    lj_assertG(isgray(o), "propagation of non-gray object");
    gray2black(g, o);
    x = gc_traverse_tab(g, gco2tab(o));
    if(x > 0) {
      lj_assertG(o->gch.gcflags & LJ_GC_WEAK, "no weak flags");
      if (x == LJ_GC_WEAKKEY) {
        setgcrefr(o->gch.gclist, g->gc.ephemeron);
        setgcref(g->gc.ephemeron, o);
      } else {
        setgcrefr(o->gch.gclist, g->gc.weak);
        setgcref(g->gc.weak, o);
      }
    }
    o = n;
  }

  for (GCobj *o = gcref(g->gc.grayagain_th); o; o = gcref(o->gch.gclist)) {
    gray2black(g, o);
    gc_traverse_thread(g, gco2th(o));
  }
}

/* Propagate all gray objects. */
static size_t gc_propagate_gray(global_State *g)
{
  size_t m = 0;
  while (gcref(g->gc.gray) != NULL || g->gc.gray_head != NULL) {
    while (gcref(g->gc.gray) != NULL)
      m += propagatemark(g);
    while (g->gc.gray_head != NULL)
      m += propagatemark_arena(g, UINT_MAX);
  }
  return m;
}

static int traverse_ephemeron(global_State *g, GCtab *t)
{
  int ret = 0;
  MSize i, hmask;
  Node *node = mref(t->node, Node);
  hmask = t->hmask;
  for (i = 0; i <= hmask; i++) {
    Node *n = &node[i];
    if (!tvisnil(&n->val) && tviswhite(g, &n->val) && !tviswhite(g, &n->key)) {
      gc_marktv(g, &n->val);
      ret = 1;
    }
  }
  return ret;
}

static void process_ephemerons(global_State *g)
{
  int changed;
  do {
    gc_propagate_gray(g);
    changed = 0;
    for (GCtab *t = tabref(g->gc.ephemeron); t; t = tabref(t->gclist)) {
      changed |= traverse_ephemeron(g, t);
    }
  } while (changed);
}

/* -- Sweep phase --------------------------------------------------------- */

/* Type of GC free functions. */
typedef void (LJ_FASTCALL *GCFreeFunc)(global_State *g, GCobj *o);

/* GC free functions for LJ_TSTR .. LJ_TUDATA. ORDER LJ_T */
static const GCFreeFunc gc_freefunc[] = {
  (GCFreeFunc)0,
  (GCFreeFunc)0,
  (GCFreeFunc)lj_state_free,
  (GCFreeFunc)lj_func_freeproto,
  (GCFreeFunc)0,
#if LJ_HASJIT
  (GCFreeFunc)lj_trace_free,
#else
  (GCFreeFunc)0,
#endif
#if LJ_HASFFI
  (GCFreeFunc)lj_cdata_free,
#else
  (GCFreeFunc)0,
#endif
  (GCFreeFunc)0,
  (GCFreeFunc)0
};

/* Full sweep of a GC list. */
#define gc_fullsweep(g, p)	gc_sweep(g, (p), ~(uint32_t)0)

#ifdef LUA_USE_ASSERT
static int check_not_gray(global_State *g, GCArenaHdr *a)
{
  for (GCArenaHdr *h = g->gc.gray_head; h; h = h->gray)
    if (h == a)
      return 0;
  return 1;
}
#endif

static void gc_free_arena(global_State *g, GCArenaHdr *a)
{
  lj_assertG(check_not_gray(g, a), "arena in gray list while being freed");
  lj_assertG(a->prev != NULL, "freeing list head");
  lj_assertG(a->prev->next == a, "freeing broken chain");
  lj_assertG(!a->freeprev || a->freenext != a->freeprev, "broken freelist");

  a->prev->next = a->next;
  if (a->next) {
    lj_assertG(a->next->prev == a, "freeing broken chain");
    a->next->prev = a->prev;
  }

  if (a->freeprev)
    a->freeprev->freenext = a->freenext;
  if (a->freenext)
    a->freenext->freeprev = a->freeprev;
  lj_arena_free(&g->gc.ctx, a);
}

/* Fixups are required for the first & last words */
#define sweep_fixup(atype, otype)                                              \
  free &= FREE_MASK(otype);                                                    \
  a->free[0] &= FREE_LOW(atype, otype);                                        \
  if (!a->free[0])                                                             \
    free &= ~1ull;                                                             \
  if (HIGH_ELEMENTS_OCCUPIED(otype) != 0) {                                    \
    a->free[FREE_HIGH_INDEX(otype)] &= FREE_HIGH(otype);                       \
    if (!a->free[FREE_HIGH_INDEX(otype)])                                      \
      free &= ~(1ull << FREE_HIGH_INDEX(otype));                               \
  }
#define sweep_fixup2(atype, otype)                                             \
  free &= FREE_MASK(otype) & ~1ull;                                            \
  a->free[0] = 0;                                                              \
  a->free[1] &= FREE_LOW2(atype, otype);                                       \
  if (!a->free[1])                                                             \
    free &= ~2ull;                                                             \
  if (HIGH_ELEMENTS_OCCUPIED(otype) != 0) {                                    \
    a->free[FREE_HIGH_INDEX(otype)] &= FREE_HIGH(otype);                       \
    if (!a->free[FREE_HIGH_INDEX(otype)])                                      \
      free &= ~(1ull << FREE_HIGH_INDEX(otype));                               \
  }

/* The first arena in the list is the primary one. It is being allocated out of
 * and can never be put on the freelist or released */
#define sweep_free(atype, src, freevar, cond, ...)                             \
  if (LJ_LIKELY(g->gc.src != &a->hdr)) {                                       \
    if (LJ_UNLIKELY(I256_EQ_64_MASK(any, zero) == 0xF)) {                      \
      GCArenaHdr *x = &a->hdr;                                                 \
      a = (atype *)a->hdr.next;                                                \
      if (x == g->gc.freevar) {                                                \
        g->gc.freevar = x->freenext;                                           \
      }                                                                        \
      __VA_ARGS__                                                              \
      gc_free_arena(g, x);                                                     \
      continue;                                                                \
    }                                                                          \
    if (LJ_UNLIKELY(cond)) {                                                   \
      free_enq(&a->hdr, g->gc.freevar);                                        \
    }                                                                          \
  }

/* This is pipelined, while a is processing the branchy fixup & free code
 * b is running the SIMD crunchy code. */
static void *gc_sweep_tab_i256(global_State *g, GCAtab *a, uint32_t lim)
{
  GCAtab *b;
  I256 v, v2;
  I256 any, any2;
  I256 zero;
  I256 ones;
  I256_ZERO(any);
  I256_ZERO(zero);
  I256_ONES(ones);
  uint64_t free = ~0ull;
  uint64_t free2;

  if (!a)
    return NULL;
  b = (GCAtab *)a->hdr.next;

  for (uint32_t i = 0; i < SIMD_WORDS_FOR_TYPE(GCtab); i++) {
    I256_LOADA(v, &a->mark[i * SIMD_MULTIPLIER]);
    I256_OR(any, any, v);
    if (!isminor(g))
      I256_STOREA(&a->mark[i * SIMD_MULTIPLIER], zero);
    I256_XOR(v, v, ones);
    I256_STOREA(&a->free[i * SIMD_MULTIPLIER], v);
    free ^= I256_EQ_64_MASK(v, zero) << (SIMD_MULTIPLIER * i);
  }
  for (; b && lim; lim--, a = b, b = (GCAtab*)b->hdr.next, any = any2, v = v2, free = free2) {
    lj_assertG((a->hdr.flags & LJ_GC_SWEEPS) != LJ_GC_SWEEPS,
               "both bits cannot be set!");

    lj_assertG(!(a->hdr.flags & g->gc.currentsweep), "sweeping swept arena");

    I256_ZERO(any2);
    free2 = ~0ull;

    a->hdr.flags ^= LJ_GC_SWEEPS;

    for (uint32_t i = 0; i < SIMD_WORDS_FOR_TYPE(GCtab); i++) {
      I256_LOADA(v2, &b->mark[i * SIMD_MULTIPLIER]);
      I256_OR(any2, any2, v2);
      if (!isminor(g))
        I256_STOREA(&b->mark[i * SIMD_MULTIPLIER], zero);
      I256_XOR(v2, v2, ones);
      I256_STOREA(&b->free[i * SIMD_MULTIPLIER], v2);
      free2 ^= I256_EQ_64_MASK(v2, zero) << (SIMD_MULTIPLIER * i);
    }

    sweep_fixup(GCAtab, GCtab);

    if (LJ_UNLIKELY(I256_EQ_64_MASK(any, zero) == 0xF)) {
      GCArenaHdr *x = &a->hdr;
      a = (GCAtab *)a->hdr.next;
      if (x == g->gc.free_tab) {
        g->gc.free_tab = x->freenext;
      }
      gc_free_arena(g, x);
      continue;
    }
    if (LJ_UNLIKELY(free && !a->free_h)) {
      free_enq(&a->hdr, g->gc.free_tab);
    }

    a->free_h = free;
  }

  sweep_fixup(GCAtab, GCtab);
  a->hdr.flags ^= LJ_GC_SWEEPS;
  if (LJ_UNLIKELY(I256_EQ_64_MASK(any, zero) == 0xF)) {
    GCArenaHdr *x = &a->hdr;
    if (x == g->gc.free_tab) {
      g->gc.free_tab = x->freenext;
      if (a->hdr.freenext)
        a->hdr.freenext->freeprev = NULL;
    }
    gc_free_arena(g, x);
    return b;
  }
  if (LJ_UNLIKELY(free && !a->free_h)) {
    free_enq(&a->hdr, g->gc.free_tab);
  }

  a->free_h = free;

  return b;
}

static void *gc_sweep_tab1_i256(global_State *g, GCAtab *a)
{
  I256 v;
  I256 any;
  I256 zero;
  I256 ones;
  I256_ZERO(any);
  I256_ZERO(zero);
  I256_ONES(ones);
  do {
    uint64_t free = ~0ull;
    lj_assertG((a->hdr.flags & LJ_GC_SWEEPS) != LJ_GC_SWEEPS,
               "both bits cannot be set!");

    lj_assertG(!(a->hdr.flags & g->gc.currentsweep), "sweeping swept arena");

    a->hdr.flags ^= LJ_GC_SWEEPS;
    /* free = ~mark
     * mark = 0 (if major collection)
     */
    for (uint32_t i = 0; i < SIMD_WORDS_FOR_TYPE(GCtab); i++) {
      I256_LOADA(v, &a->mark[i * SIMD_MULTIPLIER]);
      I256_OR(any, any, v);
      if (!isminor(g))
        I256_STOREA(&a->mark[i * SIMD_MULTIPLIER], zero);
      I256_XOR(v, v, ones);
      I256_STOREA(&a->free[i * SIMD_MULTIPLIER], v);
      free ^= I256_EQ_64_MASK(v, zero) << (SIMD_MULTIPLIER * i);
    }

    sweep_fixup(GCAtab, GCtab);

    sweep_free(GCAtab, tab, free_tab, free && !a->free_h);

    a->free_h = free;
    a = (GCAtab *)a->hdr.next;
  } while (0);
  return a;
}

static void *gc_sweep_fintab1_i256(global_State *g, GCAtab *a)
{
  I256 v, f;
  I256 any;
  I256 zero;
  I256 ones;
  I256_ZERO(any);
  I256_ZERO(zero);
  I256_ONES(ones);
  do {
    uint64_t free = ~0ull;
    lj_assertG((a->hdr.flags & LJ_GC_SWEEPS) != LJ_GC_SWEEPS,
               "both bits cannot be set!");

    lj_assertG(!(a->hdr.flags & g->gc.currentsweep), "sweeping swept arena");

    a->hdr.flags ^= LJ_GC_SWEEPS;
    /* free = ~mark
     * fin = fin & mark
     * mark = 0 (if major collection)
     */
    for (uint32_t i = 0; i < SIMD_WORDS_FOR_TYPE(GCtab); i++) {
      I256_LOADA(v, &a->mark[i * SIMD_MULTIPLIER]);
      I256_LOADA(f, &a->fin[i * SIMD_MULTIPLIER]);
      I256_OR(any, any, v);
      if (!isminor(g))
        I256_STOREA(&a->mark[i * SIMD_MULTIPLIER], zero);
      I256_AND(f, f, v);
      I256_XOR(v, v, ones);
      I256_STOREA(&a->free[i * SIMD_MULTIPLIER], v);
      I256_STOREA(&a->fin[i * SIMD_MULTIPLIER], f);
      free ^= I256_EQ_64_MASK(v, zero) << (SIMD_MULTIPLIER * i);
    }

    sweep_fixup(GCAtab, GCtab);

    sweep_free(GCAtab, fintab, free_fintab, free && !a->free_h);

    a->free_h = free;
    a = (GCAtab *)a->hdr.next;
  } while (0);
  return a;
}

static void gc_presweep_process(global_State *g, GCAtab *a, uint32_t i,
                                bitmap_t f)
{
  for (uint32_t j = tzcount64(f); f; f = reset_lowest64(f), j = tzcount64(f)) {
    GCtab *t = aobj(a, GCtab, (i << 6) + j);
    setgcrefr(t->gclist, g->gc.fin_list);
    setgcref(g->gc.fin_list, obj2gco(t));
  }
}

static void gc_presweep_process_ud(global_State *g, GCAudata *a, uint32_t i, bitmap_t f)
{
  for (uint32_t j = tzcount64(f); f; f = reset_lowest64(f), j = tzcount64(f)) {
    GCudata *t = aobj(a, GCudata, (i << 6) + j);
    setgcrefr(t->gclist, g->gc.fin_list);
    setgcref(g->gc.fin_list, obj2gco(t));
  }
}

static void gc_presweep_fintab(global_State *g, GCAtab *a)
{
  /* We could set fin to f directly as f represents "new" finalized objects
   * and this would skip the step of clearing fin bits in sweeping,
   * however this would cause a re-run of a finalizer for an object that had
   * previously been finalized but were referenced by a dead finalized object
   * in the next cycle, because it's fin bit would get cleared here since it
   * still isn't marked.
   * While it's very unlikely this will ever happen in a real program this
   * matches PUC Lua behaviour.
   */
  for (; a; a = (GCAtab *)a->hdr.next) {
    uint32_t i;
    bitmap_t gray_h = 0;
    bitmap_t f =
        ~(a->free[0] | a->fin[0] | a->mark[0]) & FREE_LOW(GCAtab, GCtab);
    a->fin[0] |= f;
    a->gray[0] = f;
    a->mark[0] |= f;
    if (f) {
      gray_h |= 1;
      gc_presweep_process(g, a, 0, f);
    }
    for (i = 1; i < WORDS_FOR_TYPE_UNROUNDED(GCtab) - 1; i++) {
      f = ~(a->free[i] | a->fin[i] | a->mark[i]);
      a->fin[i] |= f;
      a->gray[i] = f;
      a->mark[i] |= f;
      if (f) {
        gray_h |= abit(i);
        gc_presweep_process(g, a, i, f);
      }
    }
    f = ~(a->free[i] | a->fin[i] | a->mark[i]);
    if (HIGH_ELEMENTS_OCCUPIED(GCtab) != 0) {
      f &= FREE_HIGH(GCtab);
    }
    a->fin[i] |= f;
    a->gray[i] = f;
    a->mark[i] |= f;
    if (f) {
      gray_h |= abit(i);
      gc_presweep_process(g, a, i, f);
    }

    a->gray_h = gray_h;
    if (gray_h)
      gray_enq(a, g);
  }
}

static void gc_presweep_udata(global_State *g, GCAudata *a)
{
  for (; a; a = (GCAudata *)a->hdr.next) {
    uint32_t i;
    bitmap_t gray_h = 0;
    bitmap_t f = a->fin_req[0] & ~(a->free[0] | a->fin[0] | a->mark[0]) &
                 FREE_LOW(GCAudata, GCudata);
    a->fin[0] |= f;
    a->gray[0] = f;
    a->mark[0] |= f;
    if (f) {
      gray_h |= 1;
      gc_presweep_process_ud(g, a, 0, f);
    }
    for (i = 1; i < WORDS_FOR_TYPE_UNROUNDED(GCudata) - 1; i++) {
      f = a->fin_req[i] & ~(a->free[i] | a->fin[i] | a->mark[i]);
      a->fin[i] |= f;
      a->gray[i] = f;
      a->mark[i] |= f;
      if (f) {
        gray_h |= abit(i);
        gc_presweep_process_ud(g, a, i, f);
      }
    }
    f = a->fin_req[i] & ~(a->free[i] | a->fin[i] | a->mark[i]);
    if (HIGH_ELEMENTS_OCCUPIED(GCudata) != 0) {
      f &= FREE_HIGH(GCudata);
    }
    a->fin[i] |= f;
    a->gray[i] = f;
    a->mark[i] |= f;
    if (f) {
      gray_h |= abit(i);
      gc_presweep_process_ud(g, a, i, f);
    }

    a->gray_h = gray_h;
    if (gray_h)
      gray_enq(a, g);
  }
}

static void *gc_sweep_func_i256(global_State *g, GCAfunc *a, uint32_t lim)
{
  I256 v;
  I256 any;
  I256 zero;
  I256 ones;
  I256_ZERO(zero);
  I256_ONES(ones);
  for (; a && lim; lim--) {
    uint64_t free = ~0ull;
    I256_ZERO(any);

    lj_assertG((a->hdr.flags & LJ_GC_SWEEPS) != LJ_GC_SWEEPS,
               "both bits cannot be set!");

    lj_assertG(!(a->hdr.flags & g->gc.currentsweep), "sweeping swept arena");
    a->hdr.flags ^= LJ_GC_SWEEPS;

    for (uint32_t i = 0; i < SIMD_WORDS_FOR_TYPE(GCfunc); i++) {
      /* free = ~mark; mark = 0*/
      I256_LOADA(v, &a->mark[i * SIMD_MULTIPLIER]);
      I256_OR(any, any, v);
      if (!isminor(g))
        I256_STOREA(&a->mark[i * SIMD_MULTIPLIER], zero);
      I256_XOR(v, v, ones);
      I256_STOREA(&a->free[i * SIMD_MULTIPLIER], v);
      free ^= I256_EQ_64_MASK(v, zero) << (SIMD_MULTIPLIER * i);
    }

    sweep_fixup(GCAfunc, GCfunc);

    sweep_free(GCAfunc, func, free_func, free && !a->free_h);

    a->free_h = free;
    a = (GCAfunc *)a->hdr.next;
  }
  return a;
}

static void *gc_sweep_uv_i256(global_State *g, GCAupval *a, uint32_t lim)
{
  I256 v;
  I256 any;
  I256 zero;
  I256 ones;
  I256_ZERO(zero);
  I256_ONES(ones);
  for (; a && lim; lim--) {
    uint64_t free = ~0ull;
    I256_ZERO(any);

    lj_assertG((a->hdr.flags & LJ_GC_SWEEPS) != LJ_GC_SWEEPS, "both bits cannot be set!");

    lj_assertG(!(a->hdr.flags & g->gc.currentsweep), "sweeping swept arena");
    a->hdr.flags ^= LJ_GC_SWEEPS;

    for (uint32_t i = 0; i < SIMD_WORDS_FOR_TYPE(GCupval); i++) {
      /* free = ~mark; mark = 0*/
      I256_LOADA(v, &a->mark[i * SIMD_MULTIPLIER]);
      I256_OR(any, any, v);
      if (!isminor(g))
        I256_STOREA(&a->mark[i * SIMD_MULTIPLIER], zero);
      I256_XOR(v, v, ones);
      I256_STOREA(&a->free[i * SIMD_MULTIPLIER], v);
      free ^= I256_EQ_64_MASK(v, zero) << (SIMD_MULTIPLIER * i);
    }

    sweep_fixup(GCAupval, GCupval);

    sweep_free(GCAupval, uv, free_uv, free && !a->free_h);

    a->free_h = free;
    a = (GCAupval *)a->hdr.next;
  }
  return a;
}

static void *gc_sweep_tab(global_State *g, GCAtab *a, uint32_t lim)
{
  return gc_sweep_tab_i256(g, a, lim);
}
static void *gc_sweep_tab1(global_State *g, GCAtab *a)
{
  return gc_sweep_tab1_i256(g, a);
}

static void *gc_sweep_fintab(global_State *g, GCAtab *a, uint32_t lim)
{
  return gc_sweep_fintab1_i256(g, a);
}
static void *gc_sweep_fintab1(global_State *g, GCAtab *a)
{
  return gc_sweep_fintab1_i256(g, a);
}

static void *gc_sweep_func(global_State *g, GCAfunc *a, uint32_t lim)
{
  return gc_sweep_func_i256(g, a, lim);
}
static void *gc_sweep_func1(global_State *g, GCAfunc *a)
{
  return gc_sweep_func_i256(g, a, 1);
}

static void *gc_sweep_uv(global_State *g, GCAupval *a, uint32_t lim)
{
  return gc_sweep_uv_i256(g, a, lim);
}
static void *gc_sweep_uv1(global_State *g, GCAupval *a)
{
  return gc_sweep_uv_i256(g, a, 1);
}

static void gc_sweep_udata_obj(global_State *g, GCAudata *a, uint32_t i,
                               bitmap_t f)
{
  GCudata *base = aobj(a, GCudata, i << 6);
  for (uint32_t j = tzcount64(f); f; f = reset_lowest64(f), j = tzcount64(f)) {
    GCudata *ud = &base[j];
    if (!(ud->gcflags & LJ_GC_MARK_MASK) && ud->len > 0) {
      g->gc.malloc -= ud->len;
      g->allocf(g->allocd, uddata(ud), ud->len, 0);
    }
  }
}

/* Because a lot of these will require individual traversal anyway,
 * it's probably best to do this as scalar code */
static void *gc_sweep_udata1(global_State *g, GCAudata *a)
{
  uint32_t free = 0;
  bitmap_t m;
  bitmap_t f;
  bitmap_t any = 0;
  uint32_t i = 0;

  lj_assertG((a->hdr.flags & LJ_GC_SWEEPS) != LJ_GC_SWEEPS,
             "both bits cannot be set!");

  lj_assertG(!(a->hdr.flags & g->gc.currentsweep), "sweeping swept arena");
  a->hdr.flags ^= LJ_GC_SWEEPS;

  m = a->mark[i];
  any |= m;
  f = ~m & ~a->free[i] & FREE_LOW(GCAudata, GCudata);
  a->free[i] |= f;
  if (!isminor(g))
    a->mark[i] = 0;
  a->fin[i] &= m;
  a->fin_req[i] &= m;
  gc_sweep_udata_obj(g, a, 0, f);
  if (f)
    free = 1;

  for (i = 1; i < WORDS_FOR_TYPE_UNROUNDED(GCudata) - 1; i++) {
    m = a->mark[i];
    any |= m;
    f = ~m & ~a->free[i];
    a->free[i] |= f;
    if (!isminor(g))
      a->mark[i] = 0;
    a->fin[i] &= m;
    a->fin_req[i] &= m;
    gc_sweep_udata_obj(g, a, 0, f);
    if (f)
      free |= 1u << i;
  }

  m = a->mark[i];
  any |= m;
  f = ~m & ~a->free[i];
  if (HIGH_ELEMENTS_OCCUPIED(GCudata) != 0) {
    f &= FREE_HIGH(GCudata);
  }
  a->free[i] |= f;
  if (!isminor(g))
    a->mark[i] = 0;
  a->fin[i] &= m;
  a->fin_req[i] &= m;
  gc_sweep_udata_obj(g, a, 0, f);
  if (f)
    free |= 1u << i;

  if (&a->hdr != g->gc.udata) {
    if (!any) {
      GCArenaHdr *x = &a->hdr;
      a = (GCAudata *)a->hdr.next;
      if (x == g->gc.free_udata) {
        g->gc.free_udata = x->freenext;
      }
      gc_free_arena(g, x);
      return a;
    }

    if (free && !a->free_h) {
      free_enq(&a->hdr, g->gc.free_udata);
    }
  }

  a->free_h |= free;
  return a->hdr.next;
}

StrTab *get_strtab(global_State *g, uint32_t hid)
{
  if (hid >= 0xFC000000) {
    return strtab_primary(g, hid);
  } else {
    return strtab_secondary(g, hid);
  }
}

/* Clear one string table entry.
 * Precondition: the entry referred to by hid must have a matching string
 */
static void gc_clear_strtab(global_State *g, uint32_t hid)
{
  StrTab *st;
  uint32_t i = hid & 0xF;
  g->str.num--;
  /* Primary 111111, (22-bit array index), (4-bit entry index)
   * Secondary (19-bit array index), (9-bit arena index), (4-bit entry index)
   */
  lj_assertG(i != 0xF, "Invalid hid field - low index 15");
  if (hid >= 0xFC000000) { /* Primary always has the top 6 bits set */
    st = strtab_primary(g, hid);
    lj_assertG(((hid & 0x3FFFFFF) >> 4) <= g->str.mask,
               "Invalid hid field - primary list exceeded");
    lj_assertG(gcrefu(st->strs[i]) > 1, "Clearing null string");
    lj_assertG(((GCstr *)(gcrefu(st->strs[i]) & ~(uintptr_t)1))->hid ==
                   hid,
               "Mismatch, str->hid != hid");

    setgcrefnull(st->strs[i]);
    /* By flipping the bits we eliminate matches because the low N bits must
     * match the index of the chain
     */
    st->hashes[i] = ~st->hashes[i];
    st->prev_len--;
    return;
  }
  lj_assertG((hid >> 13) < g->str.secondary_list_capacity,
             "Invalid hid field - secondary list exceeded");
  lj_assertG(((hid >> 4) & 0x1FF) < STRTAB_ENTRIES_PER_ARENA,
             "Invalid hid field - bad arena index");
  st = strtab_secondary(g, hid);
  lj_assertG(gcrefu(st->strs[i]) > 1, "Clearing null string");
  lj_assertG(((GCstr *)(gcrefu(st->strs[i]) & ~(uintptr_t)1))->hid ==
                 hid,
             "Mismatch, str->hid != hid");
  setgcrefnull(st->strs[i]);
  st->hashes[i] = ~st->hashes[i];
  st->prev_len--;
  if(!(st->prev_len & 0xF)) {
    lj_mem_freechainedstrtab(g, st);
  }
}

static void clean_str_small(global_State *g, GCstr *strs, uint64_t mask, uint64_t *free)
{
  do {
    uint32_t i = tzcount64(mask);
    mask = reset_lowest64(mask);
    uint64_t v = free[i];
    do {
      uint32_t j = tzcount64(v);
      v = reset_lowest64(v);

      gc_clear_strtab(g, strs[(i << 6) + j].hid);
    } while (v);
  } while (mask);
}

static void free_str_small(global_State *g, GCArenaHdr *h)
{
  GCstr *s = (GCstr*)h;
  /* If the arena is considered dirty then every element is in use */
  for (uint32_t i = ELEMENTS_OCCUPIED(GCAstr, GCstr);
       i < ARENA_SIZE / sizeof(GCstr); i += 2) {
    gc_clear_strtab(g, s[i].hid);
  }
}

static void *gc_sweep_str_small(global_State *g, GCAstr *a, uint32_t lim)
{
  I256 v;
  I256 x;
  I256 t;
  I256 any;
  I256 zero;
  I256 mask;
  uint64_t temp_buf[67];
  uint64_t *temp = (uint64_t*)(((uintptr_t)temp_buf + 31) & ~31ull);
  I256_ZERO(any);
  I256_ZERO(zero);
  I256_BCAST_8(mask, 0x55);

  for (; a && lim; lim--) {
    uint64_t free = ~0ull;
    uint32_t count = 0;
    uint64_t free_mask = 0;

    lj_assertG((a->hdr.flags & LJ_GC_SWEEPS) != LJ_GC_SWEEPS, "both bits cannot be set!");

    lj_assertG(!(a->hdr.flags & g->gc.currentsweep), "sweeping swept arena");
    a->hdr.flags ^= LJ_GC_SWEEPS;

    for (uint32_t i = 0; i < SIMD_WORDS_FOR_TYPE(GCstr); i++) {
      /*
       * count += popcount(mark)
       * free = ~fixed & ~mark & 0x55 (implemented as (fixed | mark) ^ 0x55)
       * mark = 0
       */
      I256_LOADA(v, &a->mark[i * SIMD_MULTIPLIER]);
      I256_LOADA(x, &a->fixed[i * SIMD_MULTIPLIER]);
      /* compute popcount(mark[i] | (mark[i+1] << 1)) */
      /* This should be slightly faster than doing it in scalar */
      I256_SHL_64(t, v, 1);
      I256_SHUFFLE_64(t, t, 0xF);
      I256_OR(t, t, v);
      count += popcount64(I256_EXTRACT(t, 0)) + popcount64(I256_EXTRACT(t, 2));

      I256_OR(t, v, x);
      I256_OR(any, any, t);
      if (!isminor(g))
        I256_STOREA(&a->mark[i * SIMD_MULTIPLIER], zero);
      I256_XOR(t, t, mask);

      I256_LOADA(v, &a->free[i * SIMD_MULTIPLIER]);
      I256_XOR(v, v, t);
      I256_STOREA(&temp[i * SIMD_MULTIPLIER], v);
      free_mask |= I256_NEQ_64_MASK(v, zero) << (4 * i);
      I256_STOREA(&a->free[i * SIMD_MULTIPLIER], t);
      free ^= I256_EQ_64_MASK(t, zero) << (SIMD_MULTIPLIER * i);
    }

    free_mask &= ~1ull;
    temp[1] &= FREE_LOW2(GCAstr, GCstr);
    if (!temp[1])
      free_mask &= ~2ull;

    sweep_fixup2(GCAstr, GCstr);

    sweep_free(GCAstr, str_small, free_str_small, free && !a->free_h,
               if (a->hdr.flags & LJ_GC_SWEEP_DIRTY) free_str_small(g, x);
               else clean_str_small(g, (GCstr *)a, free_mask, temp););

    g->str.num_small += count;
    if (a->hdr.flags & LJ_GC_SWEEP_DIRTY) {
      g->str.num_dead += ((ARENA_SIZE - sizeof(GCstr)) >> 5) - count;
    } else if(free_mask) {
      /* This isn't a dirty arena, so we must eagerly clean */
      clean_str_small(g, (GCstr*)a, free_mask, temp);
    }
    a->free_h = free;
    a = (GCAstr *)a->hdr.next;
  }
  return a;
}

static void *gc_sweep_str_small1(global_State *g, GCAstr *a)
{
  return gc_sweep_str_small(g, a, 1);
}

/* Rescan this arena, aggregate adjacent free blocks and chain all free blocks
 * together.
 */
static void gc_aggregate_str_freelist(global_State *g, GCAstr *a)
{
  uint32_t *pnext = &a->free_start;
  FreeBlock *prev = NULL;
  FreeBlock *b;
  /* i is the current word, j is the current bit in that word. */
  uint32_t i, j;
  /* at is the current byte offset, walk_at is the byte offset of the
   * next entry in the previously existing freelist */
  uint32_t at, walk_at = a->free_start;
  /* end is one past the end of the chunk starting at 'at' */
  uint32_t end = 0;
  /* free contains a 1 if this starts a free block (mark & ~free) */
  uint64_t free;

  a->in_use = ARENA_SIZE - sizeof(GCAstr);

  /* This arena consists of
   * Free blocks (mark & ~free)
   * Newly freed strings (also mark & ~free)
   * Valid strings (~mark & free)
   * Extents (~mark & ~free)
   *
   * All current free blocks are chained, in-order into the freelist, so
   * we can identify newly freed strings by whether the next free entry
   * is at the expected offset.
   */
  for (i = 1; i < 64; i++) {
    free = ~a->free[i] & a->mark[i];
    while(free) {
      j = tzcount64(free);
      free = reset_lowest64(free);
      at = (i << 10) | (j << 4);

      b = (FreeBlock *)((char *)a + at);
      if (at == walk_at) {
        walk_at = b->next;

        /* If this is the expected entry then continue walking the freelist.
         * This may coalesce with the previous one
         */
        if (at == end) {
          prev->size += b->size;
          end += b->size << 4;
          /* Change to extent */
          a->mark[i] ^= abit(j);
          continue;
        }
      } else {
        /* This is a newly freed thing. */
        GCstr *str = (GCstr *)b;
        uint32_t len = (str->len >> 4) + 2;
        gc_clear_strtab(g, str->hid);
        if (at == end) {
          /* This coalesces with the previous entry. */
          prev->size += len;
          end += len << 4;
          /* Change to extent */
          a->mark[i] ^= abit(j);
          continue;
        }
        /* New entry */
        b->size = len;
      }
      *pnext = at;
      pnext = &b->next;
      prev = b;
      end = at + (b->size << 4);
      a->in_use -= b->size << 4;
    }
  }
  *pnext = 0;
}

/* Allocation arena sweeping
 *
 * Small strings are collected lazily, to make the actual sweeping very fast.
 * Lazy sweeping has a problem with a GC as it will never actually "free"
 * memory unless entire arenas get released. This isn't a problem for other
 * types as the accounting can still be done, and for small strings we
 * can use a cheap popcount to compute the real active consumption, however
 * for allocated strings we have to scan.
 *
 * If we didn't compute space used then allocations would act as a ratchet,
 * new strings would go into freed space and either not increment the total
 * and so not get included in the pacing, or falsely increment it and then
 * either never disappear or disappear incorrectly.
 *
 * The solution is to do a full sweep and eager collection here. We might
 * as well also compact free areas.
 */
static void *gc_sweep_str_med(global_State *g, GCAstr *a, uint32_t lim)
{
  I256 v;
  I256 m;
  I256 f;
  I256 b;
  I256 any;
  I256 new_free;
  I256 zero;
  I256_ZERO(zero);
  for (; a && lim; lim--) {
    I256_ZERO(any);
    I256_ZERO(new_free);
    lj_assertG((a->hdr.flags & LJ_GC_SWEEPS) != LJ_GC_SWEEPS,
               "both bits cannot be set!");

    lj_assertG(!(a->hdr.flags & g->gc.currentsweep), "sweeping swept arena");
    a->hdr.flags ^= LJ_GC_SWEEPS;

    for (uint32_t i = 0; i < SIMD_WORDS_FOR_TYPE(GCstr); i++) {
      /*
       * (fixed, free, mark) -> (free, mark)
       * 111 -> 10
       * 110 -> 10
       * 101 -> INVALID
       * 100 -> INVALID
       * 011 -> 10
       * 010 -> 01
       * 001 -> 01
       * 000 -> 00
       * free = (free & mark) | fixed
       * mark = (free ^ mark) & ~fixed
       */
      I256_LOADA(m, &a->mark[i * SIMD_MULTIPLIER]);
      I256_LOADA(f, &a->fixed[i * SIMD_MULTIPLIER]);
      I256_LOADA(b, &a->free[i * SIMD_MULTIPLIER]);
      I256_AND(v, m, b);
      I256_OR(v, v, f);
      I256_OR(any, any, v);
      I256_STOREA(&a->free[i * SIMD_MULTIPLIER], v);
      I256_ANDNOT(v, b, v);
      I256_OR(new_free, new_free, v);
      I256_XOR(v, m, b);
      I256_ANDNOT(v, v, f);
      I256_STOREA(&a->mark[i * SIMD_MULTIPLIER], v);
    }
    int has_new_free = 0;
    if (I256_EQ_64_MASK(new_free, zero) != 0xF) {
      /* Even in the case where all strings are freed, we still need to remove
       * the newly freed ones from the string table, so this can't be skipped.
       */
      has_new_free = !(a->hdr.flags & LJ_GC_ON_FREE_LIST);
      a->hdr.flags |= LJ_GC_ON_FREE_LIST;
      gc_aggregate_str_freelist(g, a);
    }
    sweep_free(GCAstr, str, free_str, has_new_free);

    g->gc.strings += a->in_use;
    a = (GCAstr *)a->hdr.next;
  }
  return a;
}

static void *gc_sweep_str_med1(global_State *g, GCAstr *a)
{
  return gc_sweep_str_med(g, a, 1);
}

/* Partial sweep of a GC list. */
static GCRef *gc_sweep(global_State *g, GCRef *p, uint32_t lim)
{
  /* Mask with other white and LJ_GC_FIXED. Or LJ_GC_SFIXED on shutdown. */
  int safe = g->gc.safecolor;
  GCobj *o;
  while ((o = gcref(*p)) != NULL && lim-- > 0) {
    if (o->gch.gcflags & safe) { /* Black or current white? */
      p = &o->gch.nextgc;
      makewhite(o);
    } else {  /* Otherwise value is dead, free it. */
      setgcrefr(*p, o->gch.nextgc);
      if (o == gcref(g->gc.root))
	setgcrefr(g->gc.root, o->gch.nextgc);  /* Adjust list anchor. */
      gc_freefunc[o->gch.gct - ~LJ_TSTR](g, o);
    }
  }
  return p;
}

/* Check whether we can clear a key or a value slot from a table. */
static int gc_mayclear(global_State *g, cTValue *o, int val)
{
  if (tvisgcv(o)) {  /* Only collectable objects can be weak references. */
    if (tvisstr(o)) {  /* But strings cannot be used as weak references. */
      gc_mark_str(g, strV(o));  /* And need to be marked. */
      return 0;
    }
    if (iswhite(g, gcV(o)))
      return 1;  /* Object is about to be collected. */
    if (tvisudata(o) && val && isfinalized(udataV(o)))
      return 1;  /* Finalized userdata is dropped only from values. */
  }
  return 0;  /* Cannot clear. */
}

/* Clear collected entries from weak tables. */
static void gc_clearweak(global_State *g, GCobj *o)
{
  UNUSED(g);
  while (o) {
    GCtab *t = gco2tab(o);
    if ((t->gcflags & LJ_GC_WEAK) != LJ_GC_WEAKVAL) {
      /* Need to mark & relocate hash part */
      gc_mark_tab_hash(g, t);
    }
    lj_assertG((t->gcflags & LJ_GC_WEAK), "clear of non-weak table");
    if ((t->gcflags & LJ_GC_WEAKVAL)) {
      MSize i, asize = t->asize;
      for (i = 0; i < asize; i++) {
	/* Clear array slot when value is about to be collected. */
	TValue *tv = arrayslot(t, i);
	if (gc_mayclear(g, tv, 1))
	  setnilV(tv);
      }
    }
    if (t->hmask > 0) {
      Node *node = noderef(t->node);
      MSize i, hmask = t->hmask;
      for (i = 0; i <= hmask; i++) {
	Node *n = &node[i];
	/* Clear hash slot when key or value is about to be collected. */
	if (!tvisnil(&n->val) && (gc_mayclear(g, &n->key, 0) ||
				  gc_mayclear(g, &n->val, 1)))
	  setnilV(&n->val);
      }
    }
    o = gcref(t->gclist);
  }
}

/* Call a userdata or cdata finalizer. */
static void gc_call_finalizer(global_State *g, lua_State *L,
			      cTValue *mo, GCobj *o)
{
  /* Save and restore lots of state around the __gc callback. */
  uint8_t oldh = hook_save(g);
  GCSize oldt = g->gc.threshold;
  int errcode;
  TValue *top;
  lj_trace_abort(g);
  hook_entergc(g);  /* Disable hooks and new traces during __gc. */
  if (LJ_HASPROFILE && (oldh & HOOK_PROFILE)) lj_dispatch_update(g);
  g->gc.threshold = LJ_MAX_MEM;  /* Prevent GC steps. */
  top = L->top;
  copyTV(L, top++, mo);
  if (LJ_FR2) setnilV(top++);
  setgcV(L, top, o, ~o->gch.gct);
  L->top = top+1;
  errcode = lj_vm_pcall(L, top, 1+0, -1);  /* Stack: |mo|o| -> | */
  hook_restore(g, oldh);
  if (LJ_HASPROFILE && (oldh & HOOK_PROFILE)) lj_dispatch_update(g);
  g->gc.threshold = oldt;  /* Restore GC threshold. */
  if (errcode) {
    ptrdiff_t errobj = savestack(L, L->top-1);  /* Stack may be resized. */
    lj_vmevent_send(L, ERRFIN,
      copyTV(L, L->top++, restorestack(L, errobj));
    );
    L->top--;
  }
}

static GCobj *gc_finalize_obj(lua_State *L, GCobj *o)
{
  global_State *g = G(L);
  cTValue *mo;
  lj_assertG(tvref(g->jit_base) == NULL, "finalizer called on trace");
  mo = lj_meta_fastg(g, tabref(o->gch.metatable), MM_gc);
  if (mo)
    gc_call_finalizer(g, L, mo, o);
  return gcref(o->gch.gclist);
}

/* Finalize one userdata or cdata object from the mmudata list. */
static void gc_finalize(lua_State *L)
{
  global_State *g = G(L);
  GCobj *o = gcnext(gcref(g->gc.mmudata));
  cTValue *mo;
  lj_assertG(tvref(g->jit_base) == NULL, "finalizer called on trace");
  /* Unchain from list of userdata to be finalized. */
  if (o == gcref(g->gc.mmudata))
    setgcrefnull(g->gc.mmudata);
  else
    setgcrefr(gcref(g->gc.mmudata)->gch.nextgc, o->gch.nextgc);
#if LJ_HASFFI
  if (o->gch.gct == ~LJ_TCDATA) {
    TValue tmp, *tv;
    /* Add cdata back to the GC list and make it white. */
    setgcrefr(o->gch.nextgc, g->gc.root);
    setgcref(g->gc.root, o);
    o->gch.gcflags &= (uint8_t)~LJ_GC_CDATA_FIN;
    /* Resolve finalizer. */
    setcdataV(L, &tmp, gco2cd(o));
    tv = lj_tab_set(L, ctype_ctsG(g)->finalizer, &tmp);
    if (!tvisnil(tv)) {
      g->gc.nocdatafin = 0;
      copyTV(L, &tmp, tv);
      setnilV(tv);  /* Clear entry in finalizer table. */
      gc_call_finalizer(g, L, &tmp, o);
    }
    return;
  }
#endif
  /* Add userdata back to the main userdata list and make it white. */
  setgcrefr(o->gch.nextgc, mainthread(g)->nextgc);
  setgcref(mainthread(g)->nextgc, o);
  /* Resolve the __gc metamethod. */
  mo = lj_meta_fastg(g, tabref(gco2ud(o)->metatable), MM_gc);
  if (mo)
    gc_call_finalizer(g, L, mo, o);
}

/* Finalize all userdata objects from mmudata list. */
void lj_gc_finalize_udata(lua_State *L)
{
  global_State *g = G(L);
  while (gcref(g->gc.fin_list) != NULL)
    setgcref(g->gc.fin_list, gc_finalize_obj(L, gcref(g->gc.fin_list)));
}

#if LJ_HASFFI
/* Finalize all cdata objects from finalizer table. */
void lj_gc_finalize_cdata(lua_State *L)
{
  global_State *g = G(L);
  CTState *cts = ctype_ctsG(g);
  if (cts) {
    GCtab *t = cts->finalizer;
    Node *node = noderef(t->node);
    ptrdiff_t i;
    setgcrefnull(t->metatable);  /* Mark finalizer table as disabled. */
    for (i = (ptrdiff_t)t->hmask; i >= 0; i--)
      if (!tvisnil(&node[i].val) && tviscdata(&node[i].key)) {
	GCobj *o = gcV(&node[i].key);
	TValue tmp;
	o->gch.gcflags &= (uint8_t)~LJ_GC_CDATA_FIN;
	copyTV(L, &tmp, &node[i].val);
	setnilV(&node[i].val);
	gc_call_finalizer(g, L, &tmp, o);
      }
  }
}
#endif

/* Free all remaining GC objects. */
void lj_gc_freeall(global_State *g)
{
  GCArenaHdr *a;

  /* Free everything, except super-fixed objects (the main thread). */
  g->gc.safecolor = LJ_GC_SFIXED;
  gc_fullsweep(g, &g->gc.root);
  /* Only track malloced data from this point. */
  g->gc.total = g->gc.malloc;

  g->gc.currentsweep ^= LJ_GC_SWEEPS;

  /* Some objects may contain malloced data and may not get collected. */
  for (a = g->gc.udata; a; a = a->next) {
    GCAudata *ud = (GCAudata *)a;
    memset(ud->mark, 0, sizeof(ud->mark));
    gc_sweep_udata1(g, ud);
  }
}

/* -- Collector ----------------------------------------------------------- */

/* Atomic part of the GC cycle, transitioning from mark to sweep phase. */
static void atomic(global_State *g, lua_State *L)
{
  size_t udsize;

  setgcrefnull(g->gc.weak);
  setgcrefnull(g->gc.ephemeron);
  lj_assertG(!iswhite(g, obj2gco(mainthread(g))), "main thread turned white");
  gc_markobj(g, L);  /* Mark running thread. */
  gc_traverse_curtrace(g);  /* Traverse current trace. */
  gc_mark_gcroot(g);  /* Mark GC roots (again). */

  /* Empty the 2nd chance list. */
  propagatemark_again(g);
  /* Propagate any leftovers. Ephemeron processing clears the gray queue */
  process_ephemerons(g);

  sweep_upvals(g);

  setgcrefnull(g->gc.grayagain);

  setgcrefnull(g->gc.fin_list);
  gc_presweep_fintab(g, (GCAtab*)g->gc.fintab);
  gc_presweep_udata(g, (GCAudata *)g->gc.udata);
  udsize = gc_propagate_gray(g);

  /* All marking done, clear weak tables. */
  gc_clearweak(g, gcref(g->gc.weak));
  gc_clearweak(g, gcref(g->gc.ephemeron));

  lj_buf_shrink(L, &g->tmpbuf);  /* Shrink temp buffer. */

  /* Prepare for sweep phase. */
  /* Gray is for strings which are gray while sweeping */
  g->gc.safecolor = g->gc.currentblack | LJ_GC_GRAY | LJ_GC_SFIXED;
  if (!isminor(g)) {
    /* Need to keep the thread list around */
    setgcrefnull(g->gc.grayagain_th);
    g->gc.currentblack ^= LJ_GC_BLACKS;
    g->gc.currentblackgray ^= LJ_GC_BLACKS;
  }
  g->gc.currentsweep ^= LJ_GC_SWEEPS;
  setmref(g->gc.sweep, &g->gc.root);

  /* Expected memory consumption is everything that has been malloced +
   * everything that arena traversal found as by definition we only keep
   * things that traversal found. This can be inaccurate if object vectors
   * have been resized post-marking but that's fine, it will get corrected
   * next cycle anyway.
   * This is also why we cannot just assert that total >= malloc + accum
   * even though in practice that will almost always hold.
   *
   * String memory is not computed during mark as the objects aren't traversed
   * and uniqueness is therefore not enforced. Instead, we use the old string
   * memory as an estimate and do a fixup as we sweep.
   */
  g->gc.total = g->gc.malloc + g->gc.accum + g->gc.old_strings;
  g->gc.estimate = g->gc.total - (GCSize)udsize;  /* Initial estimate. */
  /* Strings are counted during sweep */
  g->gc.old_strings = g->gc.strings;
  g->gc.strings = 0;

  g->str.num_small = 0;
  g->str.num_dead = 0;

  /* We must clear the first arena of each type in here as the allocator
   * only checks when a new arena is acquired. Alternately a new arena
   * can be assigned. This is because new objects will not have the mark bit set
   * and would mistakenly get swept. They will also have incorrect object bits
   * but those don't matter.
   */
  gc_sweep_tab1(g, (GCAtab *)g->gc.tab);
  gc_sweep_fintab1(g, (GCAtab *)g->gc.fintab);
  gc_sweep_func1(g, (GCAfunc *)g->gc.func);
  gc_sweep_uv1(g, (GCAupval *)g->gc.uv);
  gc_sweep_udata1(g, (GCAudata *)g->gc.udata);
  gc_sweep_str_small1(g, (GCAstr*)g->gc.str_small);
  gc_sweep_str_med1(g, (GCAstr*)g->gc.str);

  lj_assertG(g->gc.bloblist_wr > 0, "no blobs?");
  g->gc.bloblist_sweep = g->gc.bloblist_wr - 2;
  if (!isminor(g))
    g->gc.bloblist_usage[g->gc.bloblist_wr - 1] = 0;
}

static void gc_sweep_hugestrings(global_State *g, uint32_t count)
{
  GCAstr **n = mref(g->gc.sweep, GCAstr*);
  GCAstr *a = *n;
  while(a) {
    GCstr *s = (GCstr *)((char *)a + offsetof(GCAstr, mark[2]));
    if (!--count) {
      setmref(g->gc.sweep, n);
      return;
    }
    if((a->free_start | a->mark[0])) {
      a->mark[0] = 0;
      g->gc.strings += a->free_h; /* This is the total size */
      n = (GCAstr**)&a->hdr.gray;
      a = (GCAstr*)a->hdr.gray;
    } else {
      GCAstr *f = a;
      *n = (GCAstr*)a->hdr.gray;
      a = (GCAstr*)a->hdr.gray;
      gc_clear_strtab(g, s->hid);

      lj_arena_freehuge(&g->gc.ctx, f, lj_huge_str_size(s->len));
    }
  }
  setmrefu(g->gc.sweep, 0);
}

static void gc_sweepblobs(global_State *g)
{
  GCAblob **list = g->gc.bloblist;
  uint32_t *usage = g->gc.bloblist_usage;
  for (int32_t i = g->gc.bloblist_sweep; i >= 0; i--) {
    lj_assertG(list[i]->id == i, "id invariant violated");
    if (!usage[i]) {
      GCAblob *a = list[i];
      list[i] = list[--g->gc.bloblist_wr];
      list[i]->id = i;
      if (a->flags & GCA_BLOB_HUGE)
        lj_arena_freehuge(&g->gc.ctx, a, a->alloc);
      else
        lj_arena_free(&g->gc.ctx, a);
    } else if (usage[i] < BLOB_REAP_THRESHOLD) {
      list[i]->flags |= GCA_BLOB_REAP;
    }
    if (!isminor(g))
      usage[i] = 0;
  }
}

static void *find_unswept(global_State *g, GCArenaHdr *a)
{
  while (a && (a->flags & LJ_GC_SWEEPS) == g->gc.currentsweep)
    a = a->next;
  return a;
}

/* GC state machine. Returns a cost estimate for each step performed. */
static size_t gc_onestep(lua_State *L)
{
  global_State *g = G(L);
  switch (g->gc.state) {
  case GCSpause:
    gc_mark_start(g);  /* Start a new GC cycle by marking all GC roots. */
    return 0;
  case GCSpropagate:
    if (gcref(g->gc.gray) != NULL)
      return propagatemark(g);  /* Propagate one gray object. */
    if (g->gc.gray_head != NULL)
      return propagatemark_arena(g, GCSTEPSIZE);
    g->gc.state = GCSatomic; /* End of mark phase. */
    return 0;
  case GCSatomic:
    if (tvref(g->jit_base))  /* Don't run atomic phase on trace. */
      return LJ_MAX_MEM;
    atomic(g, L);
    g->gc.state = GCSsweep;  /* Start of sweep phase. */
    return 0;
  case GCSsweep: {
    GCSize old = g->gc.total;
    setmref(g->gc.sweep, gc_sweep(g, mref(g->gc.sweep, GCRef), GCSWEEPMAX));
    lj_assertG(old >= g->gc.total, "sweep increased memory");
    g->gc.estimate -= old - g->gc.total;
    if (gcref(*mref(g->gc.sweep, GCRef)) == NULL) {
      g->gc.state = GCSsweep_blob;
    }
    /* TODO: make this non-atomic again */
    return 0;
    }
  case GCSsweep_blob: {
    if (~g->gc.bloblist_sweep)
      gc_sweepblobs(g);
    g->gc.state = GCSsweep_smallstring;
    setmref(g->gc.sweep, find_unswept(g, g->gc.str_small->next));
    return GCSWEEPCOST;
  }
  case GCSsweep_smallstring: {
    if (mrefu(g->gc.sweep)) {
      setmref(g->gc.sweep, gc_sweep_str_small(g, mref(g->gc.sweep, GCAstr), 10));
    } else {
      g->gc.state = GCSsweep_string;
      g->gc.strings += (GCSize)g->str.num_small << 5;
      g->str.num += g->str.num_small;
      setmref(g->gc.sweep, find_unswept(g, g->gc.str->next));
    }
    return GCSWEEPCOST;
  }
  case GCSsweep_string: {
    if (mrefu(g->gc.sweep)) {
      setmref(g->gc.sweep, gc_sweep_str_med(g, mref(g->gc.sweep, GCAstr), 10));
    } else {
      g->gc.state = GCSsweep_hugestring;
      setmref(g->gc.sweep, &g->gc.str_huge);
    }
    return GCSWEEPCOST;
  }
  case GCSsweep_hugestring: {
    if (mrefu(g->gc.sweep)) {
      gc_sweep_hugestrings(g, 20);
      return GCSWEEPCOST;
    }
    g->gc.state = GCSsweep_func;
    setmref(g->gc.sweep, find_unswept(g, g->gc.func->next));
    /* String memory is known at this point, fixup total */
    g->gc.total -= g->gc.old_strings;
    g->gc.total += g->gc.strings;
    if (g->str.num <= (g->str.mask >> 2) &&
        g->str.mask > LJ_MIN_STRTAB * 2 - 1) {
      lj_str_resize(L, g->str.mask >> 1); /* Shrink string table. */
    }
    return GCSWEEPCOST;
  }
  case GCSsweep_func:
    if (mrefu(g->gc.sweep)) {
      setmref(g->gc.sweep, gc_sweep_func(g, mref(g->gc.sweep, GCAfunc), 10));
    } else {
      g->gc.state = GCSsweep_tab;
      setmref(g->gc.sweep, find_unswept(g, g->gc.tab->next));
    }
    return GCSWEEPCOST;
  case GCSsweep_tab:
    if (mrefu(g->gc.sweep)) {
      setmref(g->gc.sweep, gc_sweep_tab(g, mref(g->gc.sweep, GCAtab), 10));
    } else {
      g->gc.state = GCSsweep_fintab;
      setmref(g->gc.sweep, find_unswept(g, g->gc.fintab->next));
    }
    return GCSWEEPCOST;
  case GCSsweep_fintab:
    if (mrefu(g->gc.sweep)) {
      setmref(g->gc.sweep, gc_sweep_fintab(g, mref(g->gc.sweep, GCAtab), 10));
    } else {
      g->gc.state = GCSsweep_uv;
      setmref(g->gc.sweep, find_unswept(g, g->gc.uv->next));
    }
    return GCSWEEPCOST;
  case GCSsweep_uv:
    if (mrefu(g->gc.sweep)) {
      setmref(g->gc.sweep, gc_sweep_uv(g, mref(g->gc.sweep, GCAupval), 10));
    } else {
      setmref(g->gc.sweep, find_unswept(g, g->gc.udata->next));
      g->gc.state = GCSsweep_udata;
    }
    return GCSWEEPCOST;
  case GCSsweep_udata:
    if (mrefu(g->gc.sweep)) {
      setmref(g->gc.sweep, gc_sweep_udata1(g, mref(g->gc.sweep, GCAudata)));
    } else {
      g->gc.state = GCSfinalize_arena;
    }
    return GCSWEEPCOST;
  case GCSfinalize_arena:
    if (gcrefu(g->gc.fin_list)) {
      if (tvref(g->jit_base)) /* Don't call finalizers on trace. */
        return LJ_MAX_MEM;
      setgcref(g->gc.fin_list, gc_finalize_obj(L, gcref(g->gc.fin_list)));
    } else {
      if (gcref(g->gc.mmudata)) { /* Need any finalizations? */
        g->gc.state = GCSfinalize;
#if LJ_HASFFI
        g->gc.nocdatafin = 1;
#endif
      } else {  /* Otherwise skip this phase to help the JIT. */
        g->gc.state = GCSpause; /* End of GC cycle. */
        g->gc.debt = 0;
      }
    }
    return GCSWEEPCOST;
  case GCSfinalize:
    if (gcref(g->gc.mmudata) != NULL) {
      GCSize old = g->gc.total;
      if (tvref(g->jit_base))  /* Don't call finalizers on trace. */
	return LJ_MAX_MEM;
      gc_finalize(L);  /* Finalize one userdata object. */
      if (old >= g->gc.total && g->gc.estimate > old - g->gc.total)
	g->gc.estimate -= old - g->gc.total;
      if (g->gc.estimate > GCFINALIZECOST)
	g->gc.estimate -= GCFINALIZECOST;
      return GCFINALIZECOST;
    }
#if LJ_HASFFI
    if (!g->gc.nocdatafin) lj_tab_rehash(L, ctype_ctsG(g)->finalizer);
#endif
    g->gc.state = GCSpause;  /* End of GC cycle. */
    g->gc.debt = 0;
    return 0;
  case GCScompact_strtab:
    return 0;
  default:
    lj_assertG(0, "bad GC state");
    return 0;
  }
}

/* Perform a limited amount of incremental GC steps. */
int LJ_FASTCALL lj_gc_step(lua_State *L)
{
  global_State *g = G(L);
  GCSize lim;
  int32_t ostate = g->vmstate;
  setvmstate(g, GC);
  lim = (GCSTEPSIZE/100) * g->gc.stepmul;
  if (lim == 0)
    lim = LJ_MAX_MEM;
  if (g->gc.total > g->gc.threshold)
    g->gc.debt += g->gc.total - g->gc.threshold;
  do {
    lim -= (GCSize)gc_onestep(L);
    if (g->gc.state == GCSpause) {
      g->gc.threshold = (g->gc.estimate/100) * g->gc.pause;
      g->vmstate = ostate;
      return 1;  /* Finished a GC cycle. */
    }
  } while (sizeof(lim) == 8 ? ((int64_t)lim > 0) : ((int32_t)lim > 0));
  if (g->gc.debt < GCSTEPSIZE) {
    g->gc.threshold = g->gc.total + GCSTEPSIZE;
    g->vmstate = ostate;
    return -1;
  } else {
    g->gc.debt -= GCSTEPSIZE;
    g->gc.threshold = g->gc.total;
    g->vmstate = ostate;
    return 0;
  }
}

/* Ditto, but fix the stack top first. */
void LJ_FASTCALL lj_gc_step_fixtop(lua_State *L)
{
  if (curr_funcisL(L)) L->top = curr_topL(L);
  lj_gc_step(L);
}

#if LJ_HASJIT
/* Perform multiple GC steps. Called from JIT-compiled code. */
int LJ_FASTCALL lj_gc_step_jit(global_State *g, MSize steps)
{
  lua_State *L = gco2th(gcref(g->cur_L));
  L->base = tvref(G(L)->jit_base);
  L->top = curr_topL(L);
  while (steps-- > 0 && lj_gc_step(L) == 0)
    ;
  /* Return 1 to force a trace exit. */
  return (G(L)->gc.state == GCSatomic || G(L)->gc.state == GCSfinalize);
}
#endif

/* Perform a full GC cycle. */
void lj_gc_fullgc(lua_State *L, int maximal)
{
  global_State *g = G(L);
  int32_t ostate = g->vmstate;
  setvmstate(g, GC);
  /* Finish any previous cycle or sweep in progress. */
  if (g->gc.state > (maximal ? GCSpause : GCSatomic)) {
    do { gc_onestep(L); } while (g->gc.state != GCSpause);
  }
  /* Now perform a full GC. */
  do { gc_onestep(L); } while (g->gc.state != GCSpause);
  g->gc.threshold = (g->gc.estimate/100) * g->gc.pause;
  g->vmstate = ostate;
}

/* -- Write barriers ------------------------------------------------------ */

/* Move the GC propagation frontier forward. */
void lj_gc_barrierf(global_State *g, GCobj *o, GCobj *v)
{
  lj_assertG(isblack(g, o) && iswhite(g, v) && !checkdead(g, v) && !checkdead(g, o),
	     "bad object states for forward barrier");
  lj_assertG(g->gc.state != GCSfinalize && g->gc.state != GCSpause,
	     "bad GC state");
  lj_assertG(o->gch.gct != ~LJ_TTAB, "barrier object is not a table");
  /* Preserve invariant during propagation. Otherwise it doesn't matter. */
  if (g->gc.state == GCSpropagate || g->gc.state == GCSatomic) {
    gc_markobj(g, v); /* Move frontier forward. */
  } else {
    makewhite(o); /* Make it white to avoid the following barrier. */
  }
}

/* Specialized barrier for closed upvalue. Pass &uv->tv. */
void LJ_FASTCALL lj_gc_barrieruv(global_State *g, TValue *tv)
{
  gc_marktv(g, tv);
}

#if LJ_HASJIT
/* Mark a trace if it's saved during the propagation phase. */
void lj_gc_barriertrace(global_State *g, uint32_t traceno)
{
  if (g->gc.state == GCSpropagate || g->gc.state == GCSatomic)
    gc_marktrace(g, traceno);
}
#endif

/* -- Allocator ----------------------------------------------------------- */

/* Call pluggable memory allocator to allocate or resize a fragment. */
void *lj_mem_realloc(lua_State *L, void *p, GCSize osz, GCSize nsz)
{
  global_State *g = G(L);
  lj_assertG((osz == 0) == (p == NULL), "realloc API violation");
  p = g->allocf(g->allocd, p, osz, nsz);
  if (p == NULL && nsz > 0)
    lj_err_mem(L);
  lj_assertG((nsz == 0) == (p == NULL), "allocf API violation");
  lj_assertG(checkptrGC(p),
	     "allocated memory address %p outside required range", p);
  g->gc.total = (g->gc.total - osz) + nsz;
  g->gc.malloc = (g->gc.malloc - osz) + nsz;
  return p;
}

/* Allocate new GC object and link it to the root set. */
void * LJ_FASTCALL lj_mem_newgco(lua_State *L, GCSize size)
{
  global_State *g = G(L);
  GCobj *o = (GCobj *)g->allocf(g->allocd, NULL, 0, size);
  if (o == NULL)
    lj_err_mem(L);
  lj_assertG(checkptrGC(o),
	     "allocated memory address %p outside required range", o);
  g->gc.total += size;
  g->gc.malloc += size;
  setgcrefr(o->gch.nextgc, g->gc.root);
  setgcref(g->gc.root, o);
  newwhite(o);
  return o;
}

/* Resize growable vector. */
void *lj_mem_grow(lua_State *L, void *p, MSize *szp, MSize lim, MSize esz)
{
  MSize sz = (*szp) << 1;
  if (sz < LJ_MIN_VECSZ)
    sz = LJ_MIN_VECSZ;
  if (sz > lim)
    sz = lim;
  p = lj_mem_realloc(L, p, (*szp)*esz, sz*esz);
  *szp = sz;
  return p;
}


int checkdead(global_State *g, GCobj *o)
{
  if (g->gc.state <= GCSatomic)
    return 0; /* Nothing can be dead before atomic finishes */
  if (is_arena_obj(o->gch.gct)) {
    /* The top 3 bits for arena types have different meanings */
    if ((g->gc.safecolor & o->gch.gcflags & ~LJ_GC_MARK_MASK))
      return 0; /* Anything marked with the safe colour is live */
    /* Anything living in a swept arena is live */
    return !(arena(o)->hdr.flags & g->gc.currentsweep);
  } else {
    if (g->gc.safecolor & o->gch.gcflags)
      return 0; /* Anything marked with the safe colour is live */
    /* Anything past sweep is live */
    return !(g->gc.state > GCSsweep);
  }
}



/* Arena allocator */


#define relink(freehead, head)                                                 \
  {                                                                            \
    GCArenaHdr *fh = freehead;                                                 \
    GCArenaHdr *fn = fh->freenext;                                             \
    if (fn)                                                                    \
      fn->freeprev = NULL;                                                     \
    freehead = fn;                                                             \
    fh->prev->next = fh->next;                                                 \
    if (fh->next)                                                              \
      fh->next->prev = fh->prev;                                               \
    head->prev = fh;                                                           \
    fh->prev = NULL;                                                           \
    fh->next = head;                                                           \
  }

/* All bitmap arenas are fundamentally the same so we can macro all of these.
 * Note that each struct has a different layout.
 * Everything other than the free bitmap can be zeroed.
 * If we are reusing an arena we need to move it to the front of the queue for
 * the type and possibly sweep it
 */
#define NEW_ARENA(fn, atype, otype, idtype, var, freevar, sweepfn, init, ...)  \
  static atype *fn(global_State *g)                                            \
  {                                                                            \
    atype *o;                                                                  \
    if (LJ_LIKELY(g->gc.freevar)) {                                            \
      o = (atype *)g->gc.freevar;                                              \
      lj_assertG(o->free_h != 0, "no free elements in freelist?");             \
      relink(g->gc.freevar, g->gc.var);                                        \
      g->gc.var = &o->hdr;                                                     \
      o->hdr.freenext = o->hdr.freeprev = NULL;                                \
      if (LJ_UNLIKELY(!(g->gc.currentsweep & o->hdr.flags))) {                 \
        if (LJ_UNLIKELY(mref(g->gc.sweep, atype) == o)) {                      \
          setmref(g->gc.sweep, o->hdr.next);                                   \
        }                                                                      \
        sweepfn##1(g, o);                                                      \
      }                                                                        \
      return o;                                                                \
    }                                                                          \
    o = (atype *)lj_arena_alloc(&g->gc.ctx);                                   \
    if (LJ_UNLIKELY(!o))                                                       \
      lj_err_mem(&gcref(g->cur_L)->th);                                        \
    init(o, g, idtype, atype, otype);                                          \
    g->gc.var->prev = &o->hdr;                                                 \
    o->hdr.next = g->gc.var;                                                   \
    g->gc.var = &o->hdr;                                                       \
    __VA_ARGS__                                                                \
   return o;                                                                   \
  }

/* All bitmap allocators are basically the same */
#define BM_ALLOC(type, arena, newfn, otype, ...)                               \
  global_State *g = G(L);                                                      \
  uint32_t i, j;                                                               \
  uint64_t f;                                                                  \
  otype *x;                                                                    \
  type *o = (type *)g->gc.arena;                                               \
  if (LJ_UNLIKELY(!o->free_h)) {                                               \
    __VA_ARGS__                                                                \
    o = newfn(g);                                                              \
  }                                                                            \
  i = tzcount64(o->free_h);                                                    \
  lj_assertG(o->free[i] != 0, "no free elemnts");                              \
  j = tzcount64(o->free[i]);                                                   \
  lj_assertG((i << 6) + j >= ELEMENTS_OCCUPIED(type, otype), "bad arena");     \
  f = reset_lowest64(o->free[i]);                                              \
  o->free[i] = f;                                                              \
  if (!f)                                                                      \
    o->free_h = reset_lowest64(o->free_h);                                     \
  x = &((otype *)o)[(i << 6) + j];                                             \
  lj_assertG((char *)x + sizeof(otype) - (char *)o <= ARENA_SIZE, "out of bounds")

NEW_ARENA(lj_arena_tab, GCAtab, GCtab, ~LJ_TTAB, tab, free_tab, gc_sweep_tab,
          do_arena_init)
NEW_ARENA(lj_arena_fintab, GCAtab, GCtab, ~LJ_TTAB, fintab, free_fintab,
          gc_sweep_fintab, do_arena_init)
NEW_ARENA(lj_arena_uv, GCAupval, GCupval, ~LJ_TUPVAL, uv, free_uv, gc_sweep_uv, do_arena_init)
NEW_ARENA(lj_arena_func, GCAfunc, GCfunc, ~LJ_TFUNC, func, free_func,
          gc_sweep_func, do_arena_init)
NEW_ARENA(lj_arena_udata, GCAudata, GCudata, ~LJ_TUDATA, udata, free_udata,
          gc_sweep_udata, do_arena_init, o->free4_h = o->free_h;)
NEW_ARENA(lj_arena_str_small, GCAstr, GCstr, ~LJ_TSTR, str_small,
          free_str_small, gc_sweep_str_small, do_smallstr_arena_init)

GCAstr *lj_arena_str_med_new(global_State *g)
{
  GCAstr *o;
  FreeBlock *b;
  o = (GCAstr *)lj_arena_alloc(&g->gc.ctx);
  if (LJ_UNLIKELY(!o))
    lj_err_mem(&gcref(g->cur_L)->th);
  /* Zero the first 16 byte slot to clear out any existing object data. */
  memset(o, 0, sizeof(GCAstr) + sizeof(FreeBlock));
  o->hdr.obj_type = ~LJ_TSTR;
  o->hdr.flags = g->gc.currentsweep;
  o->free_start = sizeof(GCAstr);
  o->mark[ELEMENTS_OCCUPIED(GCAstr, GCstr) / 64] =
      abit(ELEMENTS_OCCUPIED(GCAstr, GCstr) % 64);
  b = (FreeBlock *)(o + 1);
  b->size = (ARENA_SIZE - sizeof(GCAstr)) >> 4;
  if (LJ_LIKELY(g->gc.str))
    g->gc.str->prev = &o->hdr;
  o->hdr.next = g->gc.str;
  g->gc.str = &o->hdr;
  return o;
}

GCAstr* lj_arena_str_med(global_State *g)
{
  GCAstr *o;
  if (LJ_LIKELY(g->gc.free_str)) {
    o = (GCAstr *)g->gc.free_str;
    lj_assertG(o->hdr.flags & LJ_GC_ON_FREE_LIST, "LJ_GC_ON_FREE_LIST not set");
    relink(g->gc.free_str, g->gc.str);
    g->gc.str = &o->hdr;
    o->hdr.freenext = o->hdr.freeprev = NULL;
    if (LJ_UNLIKELY(!(g->gc.currentsweep & o->hdr.flags))) {
      if (LJ_UNLIKELY(mref(g->gc.sweep, GCAstr) == o)) {
        setmref(g->gc.sweep, o->hdr.next);
      }
      gc_sweep_str_med1(g, o);
    }
    o->hdr.flags &= ~LJ_GC_ON_FREE_LIST;
    lj_assertG(o->free_start != 0, "no free data?");
    return o;
  }
  return lj_arena_str_med_new(g);
}

StrTab* lj_mem_allocstrtab(lua_State *L, uint32_t *id)
{
  global_State *g = G(L);
  if (g->str.secondary_arena_free_head < 0) {
    /* No arenas with free space */
    if(LJ_UNLIKELY(g->str.secondary_slot_free_head < 0)) {
      /* Array is full */
      uint32_t newsz = g->str.secondary_list_capacity * 2;
      if (newsz > STRING_SECONDARY_MAXIMUM_SIZE) {
        if (g->str.secondary_list_capacity == STRING_SECONDARY_MAXIMUM_SIZE) {
          lj_err_mem(L);
        }
        newsz = STRING_SECONDARY_MAXIMUM_SIZE;
      }
      lj_mem_reallocvec(L, g->str.secondary_list, g->str.secondary_list_capacity, newsz, MRef);
      for(uint32_t i = g->str.secondary_list_capacity; i < newsz - 1; i++) {
        setmrefu(g->str.secondary_list[i], i+1);
      }
      setmrefu(g->str.secondary_list[newsz - 1], ~0ull);
      g->str.secondary_slot_free_head = (int32_t)g->str.secondary_list_capacity;
      g->str.secondary_list_capacity = newsz;
    }

    MRef *ref = &g->str.secondary_list[g->str.secondary_slot_free_head];
    int32_t next = (int32_t)mrefu(*ref);
    GCAstrtab *o = (GCAstrtab *)lj_arena_alloc(&g->gc.ctx);
    setmref(*ref, o);
    o->next = -1;
    o->prev = -1;
    o->index = (uint32_t)g->str.secondary_slot_free_head;
    o->count = 0;
#if LJ_64
    o->free_h = 0x3F;
    for(uint32_t i = 0; i < 5; i++)
      o->free[i] = ~0ull;
    o->free[5] = 0x1FFFFF;
#else
#error "Need 32-bit string table layout"
#endif
    g->str.secondary_arena_free_head = g->str.secondary_slot_free_head;
    g->str.secondary_slot_free_head = next;
  }

  GCAstrtab *st = mref(g->str.secondary_list[g->str.secondary_arena_free_head], GCAstrtab);
  uint32_t i = tzcount32(st->free_h);
  uint32_t j = tzcount64(st->free[i]);
  StrTab *ret = &st->entries[(i << 6) + j];
  *id = (g->str.secondary_arena_free_head << 13) | (i << 10) | (j << 4);
  st->free[i] = reset_lowest64(st->free[i]);
  if(!st->free[i])
    st->free_h = reset_lowest32(st->free_h);
  if(++st->count == STRTAB_ENTRIES_PER_ARENA) {
    g->str.secondary_arena_free_head = st->next;
    if (st->next != -1) {
      mref(g->str.secondary_list[st->next], GCAstrtab)->prev = -1;
    }
  }
  memset(ret, 0, sizeof(StrTab));
  return ret;
}

void lj_mem_freechainedstrtab(global_State *g, StrTab *st)
{
  /* Need to unchain this */
  StrTab *prev = get_strtab(g, st->prev_len);
  prev->next = st->next;
  if (st->next) {
    st->next->prev_len =
        (st->next->prev_len & 0xF) | (st->prev_len & 0xFFFFFFF0);
  }
  lj_mem_freestrtab(g, st);
}

void lj_mem_freestrtab(global_State *g, StrTab *st)
{
  GCAstrtab *a = gcat(st, GCAstrtab);
  int32_t index = (int32_t)(st - &a->entries[0]);

  if(!--a->count && index != g->str.secondary_arena_free_head) {
    if(a->prev >= 0) {
      GCAstrtab *p = mref(g->str.secondary_list[a->prev], GCAstrtab);
      p->next = a->next;
    } else {
      g->str.secondary_arena_free_head = a->next;
    }
    if(a->next >= 0) {
      GCAstrtab *n = mref(g->str.secondary_list[a->next], GCAstrtab);
      n->prev = a->prev;
    }

    setmrefu(g->str.secondary_list[a->index], g->str.secondary_slot_free_head);
    g->str.secondary_slot_free_head = a->index;
    lj_arena_free(&g->gc.ctx, a);
    return;
  }
  if(!a->free_h) {
    int32_t n = g->str.secondary_arena_free_head;
    if(n >= 0) {
      mref(g->str.secondary_list[n], GCAstrtab)->prev = a->index;
    }
    a->prev = -1;
    a->next = g->str.secondary_arena_free_head;
    g->str.secondary_arena_free_head = a->index;
  }
  a->free[aidxh(index)] |= abit(aidxl(index));
  a->free_h |= abit(aidxh(index));
}

static void lj_arena_newblobspace(global_State *g)
{
  if (LJ_UNLIKELY(g->gc.bloblist_wr == g->gc.bloblist_alloc)) {
    uint32_t old = g->gc.bloblist_alloc;
    g->gc.bloblist_alloc *= 2;
    g->gc.bloblist =
        (GCAblob **)g->allocf(g->allocd, g->gc.bloblist, old * sizeof(void *),
                              g->gc.bloblist_alloc * sizeof(void *));
    g->gc.bloblist_usage = (uint32_t *)g->allocf(
        g->allocd, g->gc.bloblist_usage, old * sizeof(uint32_t),
        g->gc.bloblist_alloc * sizeof(uint32_t));
  }
}

static GCAblob *lj_arena_blob(global_State *g)
{
  uint32_t id;
  GCAblob *o = (GCAblob *)lj_arena_alloc(&g->gc.ctx);
  o->alloc = sizeof(GCAblob);
  o->flags = 0;
  g->gc.blob_generic = o;
  id = g->gc.bloblist_wr++;
  o->id = id;
  g->gc.bloblist[id] = o;
  g->gc.bloblist_usage[id] = 0;
  return o;
}

GCtab *lj_mem_alloctab(lua_State *L, uint32_t asize)
{
  global_State *g = G(L);
  uint32_t i, j;
  uint64_t f;
  GCtab *x;
  GCAtab *o = (GCAtab *)g->gc.tab;
  void *blob = NULL;
  uint8_t newf = 0;
  uint32_t n = (asize * sizeof(TValue) + sizeof(GCtab) - 1) / sizeof(GCtab);
  if (LJ_UNLIKELY(!o->free_h)) {
    o = lj_arena_tab(g);
  }
  i = tzcount64(o->free_h);
  lj_assertG(o->free[i] != 0, "no free elemnts");
  j = tzcount64(o->free[i]);
  f = reset_lowest64(o->free[i]);
  if (n > 0 && n <= 3) {
    uint64_t k = o->free[i];
    /* Shift 1 if n is 1 or 2, 2 if n is 3*/
    k &= k >> ((n >> 1) + (n & 1));
    /* Shift 1 if n is 2 or 3 */
    k &= k >> (n >> 1);
    if (k) {
      j = tzcount64(k);
      f = o->free[i] ^ (((1ull << (n + 1)) - 1) << j);
      newf = size2flags(n + 1);
      blob = &((GCtab *)o)[(i << 6) + j + 1];
    }
  }

  lj_assertG((i << 6) + j >= ELEMENTS_OCCUPIED(GCAtab, GCtab), "bad arena");
  o->free[i] = f;
  if (!f)
    o->free_h = reset_lowest64(o->free_h);
  x = &((GCtab *)o)[(i << 6) + j];
  lj_assertG((char *)x + sizeof(GCtab) - (char *)o <= ARENA_SIZE,
             "out of bounds");

  x->gcflags = newf;
  x->gct = ~LJ_TTAB;
  x->nomm = (uint8_t)~0;
  x->colo = blob ? (int8_t)asize : 0;
  x->asize = asize;
  x->hmask = 0;
  setgcrefnull(x->metatable);
  if (!blob && asize > 0) {
    if (asize > LJ_MAX_ASIZE)
      lj_err_msg(L, LJ_ERR_TABOV);
    blob = lj_mem_newv(L, asize, TValue);
  }
  setmref(x->array, blob);
  g->gc.total += sizeof(GCtab) + sizeof(TValue) * asize;
  return x;
}

GCtab *lj_mem_alloctabempty_gc(lua_State *L)
{
  BM_ALLOC(GCAtab, fintab, lj_arena_fintab, GCtab);

  x->gcflags = 0;
  x->gct = ~LJ_TTAB;
  x->nomm = (uint8_t)~0;
  x->colo = 0;
  setmref(x->array, NULL);
  setgcrefnull(x->metatable);
  x->asize = 0;
  g->gc.total += sizeof(GCtab);
  return x;
}

GCstr* lj_mem_allocstr_huge(lua_State *L, MSize len)
{
  /* mark[0] contains our bit.
   * mark[1] is not 16-byte aligned.
   * mark[2] is the first legal address
   * Special logic prevents fixed from being accessed in fixstring()
   */
  global_State *g = G(L);
  size_t size = lj_huge_str_size(len);
  GCAstr *a = (GCAstr*)lj_arena_allochuge(&g->gc.ctx, size);
  a->hdr.gray = g->gc.str_huge;
  g->gc.str_huge = &a->hdr;
  a->mark[0] = 0;
  a->free_h = size;
  a->free_start = 0;
  g->gc.total += size;
  g->gc.strings += size;
  return (GCstr *)((char *)a + offsetof(GCAstr, mark[2]));
}

LJ_STATIC_ASSERT(ELEMENTS_OCCUPIED(GCAstr, GCstr) * sizeof(GCstr) == sizeof(GCAstr));

GCstr* lj_mem_allocstr_med(lua_State *L, MSize len)
{
  /* # of blocks required for the payload */
  uint32_t n = (len >> 4) + 2;
  global_State *g = G(L);
  GCAstr *a = (GCAstr*)g->gc.str;
  char *at = (char*)a + a->free_start;
  FreeBlock *prev = NULL;
  /* # of free arenas to try before getting a new one.
   * Maybe vary this by size?
   */
  uint32_t count = 3;

  if (!a->free_start) {
    a = lj_arena_str_med(g);
    at = (char *)a + a->free_start;
  }

  while (1) {
    FreeBlock *f = (FreeBlock*)at;
    uint32_t idx;
    if (f->size >= n) {
      a->in_use += n << 4;
      g->gc.total += n << 4;
      g->gc.strings += n << 4;
      if (f->size != n) {
        f->size -= n;
        at += f->size << 4;
        idx = (uint32_t)(at - (char*)a) >> 4;
        a->free[aidxh(idx)] |= abit(aidxl(idx));
        return (GCstr*)at;
      } else if(prev) {
        prev->next = f->next;
      } else {
        a->free_start = f->next;
      }
      idx = (uint32_t)(at - (char*)a) >> 4;
      a->mark[aidxh(idx)] ^= abit(aidxl(idx));
      a->free[aidxh(idx)] ^= abit(aidxl(idx));
      return (GCstr*)at;
    }
    if (f->next == 0) {
      if (!--count) {
        /* Give up on the freelist, we are just burning through free arenas */
        a = lj_arena_str_med_new(g);
      } else {
        a = lj_arena_str_med(g);
      }
      at = (char *)a + a->free_start;
      prev = NULL;
    } else {
      at = (char*)a + f->next;
      prev = f;
    }
  }
}

GCstr *lj_mem_allocstr(lua_State *L, MSize len)
{
  if(len > 15) {
    if(len > LJ_HUGE_STR_THRESHOLD) {
      return lj_mem_allocstr_huge(L, len);
    }
    return lj_mem_allocstr_med(L, len);
  }
  /* Small string. We can't use the macro because string resurrection may
   * be randomly clearing free bits and won't fixup free_h. */
  global_State *g = G(L);
  uint32_t i, j;
  uint64_t f;
  GCstr *x;
  GCAstr *o = (GCAstr *)g->gc.str_small;
  while (1) {
    if (LJ_UNLIKELY(!o->free_h)) {
      o->hdr.flags |= LJ_GC_SWEEP_DIRTY;
      o = lj_arena_str_small(g);
    }
    i = tzcount64(o->free_h);
    if (LJ_LIKELY(o->free[i]))
      break;
    o->free_h = reset_lowest64(o->free_h);
  }

  j = tzcount64(o->free[i]);
  lj_assertG((i << 6) + j >= ELEMENTS_OCCUPIED(GCAstr, GCstr), "bad arena");
  f = reset_lowest64(o->free[i]);
  o->free[i] = f;
  if (!f)
    o->free_h = reset_lowest64(o->free_h);
  x = &((GCstr *)o)[(i << 6) + j];
  lj_assertG((char *)x + sizeof(GCstr) - (char *)o <= ARENA_SIZE,
             "out of bounds");

  g->gc.total += sizeof(GCstr) * 2;
  g->gc.strings += sizeof(GCstr) * 2;
  if (o->hdr.flags & LJ_GC_SWEEP_DIRTY) {
    /* This string is already in the string table, so remove it. */
    gc_clear_strtab(g, x->hid);
  }
  return x;
}

GCupval *lj_mem_allocuv(lua_State *L)
{
  BM_ALLOC(GCAupval, uv, lj_arena_uv, GCupval);
  g->gc.total += sizeof(GCupval);
  return x;
}

static GCudata *lj_mem_allocudatamerged(lua_State *L, uint32_t n, GCAudata *a)
{
  GCudata *ud;
  while (1) {
    uint32_t i = tzcount64(a->free4_h);
    uint64_t q = a->free[i];
    q &= q >> 2;
    q &= q >> 1;
    if (!q) {
      a->free4_h = reset_lowest32(a->free4_h);
      if (!a->free4_h) {
        return NULL;
      }
      continue;
    }
    uint32_t j = tzcount64(q);

    a->free[i] ^= ((1ull << n) - 1) << j;
    if (!a->free[i])
      a->free_h ^= 1ull << i;

    ud = aobj(a, GCudata, (i << 6) + j);
    /* newwhite(ud); */ /* Not finalized. */
    ud->gct = ~LJ_TUDATA;
    ud->gcflags = size2flags(n);
    setmref(ud->payload, ud + 1);
    G(L)->gc.total += sizeof(GCudata);
    return ud;
  }
}

GCudata *lj_mem_allocudata(lua_State *L, MSize bytes)
{
  uint32_t n = (bytes + 2 * sizeof(GCudata) - 1) / sizeof(GCudata);
  global_State *g = G(L);
  GCAudata *o = (GCAudata *)g->gc.udata;
  GCudata *ud;
  if (!o->free_h) {
    o = lj_arena_udata(g);
  }
  if (n > 1 && n <= 4) {
    ud = lj_mem_allocudatamerged(L, n, o);
    if (ud)
      return ud;
    o = (GCAudata *)g->gc.free_udata;
    for (; o; o = (GCAudata*)o->hdr.freenext) {
      ud = lj_mem_allocudatamerged(L, n, o);
      if (ud) {
        if (!o->free_h) {
          /* If we allocate the last free slots in an arena
           * we have to remove it from the freelist */
          if (&o->hdr == g->gc.free_udata) {
            g->gc.free_udata = o->hdr.freenext;
            if (o->hdr.freenext)
              o->hdr.freeprev = NULL;
          } else {
            o->hdr.freeprev->freenext = o->hdr.freenext;
            if (o->hdr.freenext)
              o->hdr.freenext->freeprev = o->hdr.freeprev;
          }
        }
        return ud;
      }
    }
    o = lj_arena_udata(g);
    ud = lj_mem_allocudatamerged(L, n, o);
    return ud;
  }
  uint32_t i = tzcount64(o->free_h);
  uint32_t j = tzcount64(o->free[i]);
  uint64_t x = reset_lowest64(o->free[i]);
  o->free[i] = x;
  if (!x) {
    o->free_h &= ~abit(i);
    o->free4_h &= ~abit(i);
  }
  ud = aobj(o, GCudata, (i << 6) + j);
  g->gc.malloc += bytes;
  setmref(ud->payload, (bytes > 0) ? g->allocf(g->allocd, NULL, 0, bytes) : NULL);
  newwhite(ud); /* Not finalized. */
  ud->gct = ~LJ_TUDATA;
  return ud;
}

GCfunc *lj_mem_allocfunc(lua_State *L, MSize bytes)
{
  global_State *g = G(L);
  uint32_t i, j;
  uint64_t f;
  GCfunc *x;
  GCAfunc *o = (GCAfunc *)g->gc.func;
  void *blob = NULL;
  uint8_t newf = 0;
  uint32_t n = (bytes + sizeof(GCfunc) - 1) / sizeof(GCfunc);
  if (LJ_UNLIKELY(!o->free_h)) {
    o = lj_arena_func(g);
  }
  i = tzcount64(o->free_h);
  lj_assertG(o->free[i] != 0, "no free elemnts");
  j = tzcount64(o->free[i]);
  f = reset_lowest64(o->free[i]);
  if (n > 0 && n <= 3) {
    uint64_t k = o->free[i];
    /* Shift 1 if n is 1 or 2, 2 if n is 3*/
    k &= k >> ((n >> 1) + (n & 1));
    /* Shift 1 if n is 2 or 3 */
    k &= k >> (n >> 1);
    if (k) {
      j = tzcount64(k);
      f = o->free[i] ^ (((1ull << (n + 1)) - 1) << j);
      newf = size2flags(n + 1);
      blob = &((GCfunc *)o)[(i << 6) + j + 1];
    }
  }

  lj_assertG((i << 6) + j >= ELEMENTS_OCCUPIED(GCAfunc, GCfunc), "bad arena");
  o->free[i] = f;
  if (!f)
    o->free_h = reset_lowest64(o->free_h);
  x = &((GCfunc *)o)[(i << 6) + j];
  lj_assertG((char *)x + sizeof(GCfunc) - (char *)o <= ARENA_SIZE,
             "out of bounds");

  setmref(x->gen.data, blob ? blob : lj_mem_newblob_g(G(L), bytes));
  x->gen.gcflags = newf;
  x->gen.gct = ~LJ_TFUNC;
  g->gc.total += bytes + sizeof(GCfunc);
  return x;
}

static void *lj_mem_newblob_g(global_State *g, MSize sz)
{
  GCAblob *a = g->gc.blob_generic;
  void *ret;
  sz = (sz + 15) & ~15u;
  if (LJ_UNLIKELY(sz > ARENA_HUGE_THRESHOLD)) {
    uint32_t id;
    lj_arena_newblobspace(g);
    id = g->gc.bloblist_wr++;
    a = (GCAblob *)lj_arena_allochuge(&g->gc.ctx, sz + sizeof(GCAblob));
    a->alloc = sizeof(GCAblob);
    a->flags = GCA_BLOB_HUGE;
    /* The current blob must always be the last one so we have to shift it */
    a->id = id - 1;
    g->gc.bloblist[id - 1]->id = id;
    g->gc.bloblist[id] = g->gc.bloblist[id - 1];
    g->gc.bloblist[id - 1] = a;
    g->gc.bloblist_usage[id] = g->gc.bloblist_usage[id - 1];
    g->gc.bloblist_usage[id - 1] = 0;
  } else if (a->alloc + sz > ARENA_SIZE) {
    lj_arena_newblobspace(g);
    a = lj_arena_blob(g);
  }

  ret = (char *)a + a->alloc;
  a->alloc += sz;
  return ret;
}

void *lj_mem_newblob(lua_State *L, MSize sz)
{
  G(L)->gc.total += sz;
  return lj_mem_newblob_g(G(L), sz);
}

void *lj_mem_reallocblob(lua_State *L, void *p, MSize osz, MSize nsz)
{
  global_State *g = G(L);
  GCAblob *a;
  g->gc.total = (g->gc.total - osz) + nsz;
  if (!osz)
    return lj_mem_newblob_g(g, nsz);
  osz = (osz + 15) & ~15u;
  nsz = (nsz + 15) & ~15u;
  if (nsz <= osz) {
    if (!nsz)
      return NULL;
    return p;
  }
  if (((char *)g->gc.blob_generic + g->gc.blob_generic->alloc - osz) == p) {
    /* We *can* resize if no more allocations have occurred */
    MSize d = nsz - osz;
    if (g->gc.blob_generic->alloc + d <= ARENA_SIZE) {
      g->gc.blob_generic->alloc += (uint32_t)d;
      return p;
    }
  }

  a = gcablob(p);
  if (a->flags & GCA_BLOB_HUGE) {
    GCAblob *newp = (GCAblob *)lj_arena_reallochuge(
        &g->gc.ctx, a, osz + sizeof(GCAblob), nsz + sizeof(GCAblob));
    if (!newp)
      lj_err_mem(L);

    g->gc.bloblist[newp->id] = newp;
    newp->alloc = sizeof(GCAblob) + nsz;
    return newp + 1;
  }

  void *r = lj_mem_newblob_g(g, nsz);
  if (!r)
    lj_err_mem(L);
  memcpy(r, p, osz);
  return r;
}

void lj_mem_registergc_udata(lua_State *L, GCudata *ud)
{
  GCAudata *a = gcat(ud, GCAudata);
  uint32_t idx = aidx(ud);
  a->fin_req[aidxh(idx)] |= abit(aidxl(idx));
}

void *lj_mem_newpages(global_State *g, size_t sz)
{
  return g->gc.ctx.rawalloc(g->gc.ctx.pageud, NULL, 0, sz);
}

void lj_mem_freepages(global_State *g, void *ptr, size_t sz)
{
  g->gc.ctx.rawalloc(g->gc.ctx.pageud, ptr, sz, 0);
}
