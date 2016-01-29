/*
** Machine code management.
** Copyright (C) 2005-2015 Mike Pall. See Copyright Notice in luajit.h
*/

#define lj_mcode_c
#define LUA_CORE

#include "lj_obj.h"
#if LJ_HASJIT
#include "lj_gc.h"
#include "lj_err.h"
#include "lj_jit.h"
#include "lj_mcode.h"
#include "lj_trace.h"
#include "lj_dispatch.h"
#endif
#if LJ_HASJIT || LJ_HASFFI
#include "lj_vm.h"
#endif

/* -- OS-specific functions ----------------------------------------------- */

#if LJ_HASJIT || LJ_HASFFI

/* Define this if you want to run LuaJIT with Valgrind. */
#ifdef LUAJIT_USE_VALGRIND
#include <valgrind/valgrind.h>
#endif

#if LJ_TARGET_IOS
void sys_icache_invalidate(void *start, size_t len);
#endif

/* Synchronize data/instruction cache. */
void lj_mcode_sync(void *start, void *end)
{
#ifdef LUAJIT_USE_VALGRIND
  VALGRIND_DISCARD_TRANSLATIONS(start, (char *)end-(char *)start);
#endif
#if LJ_TARGET_X86ORX64
  UNUSED(start); UNUSED(end);
#elif LJ_TARGET_IOS
  sys_icache_invalidate(start, (char *)end-(char *)start);
#elif LJ_TARGET_PPC
  lj_vm_cachesync(start, end);
#elif defined(__GNUC__)
  __clear_cache(start, end);
#else
#error "Missing builtin to flush instruction cache"
#endif
}

#endif

#if LJ_HASJIT

#if LJ_TARGET_WINDOWS

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

#define MCPROT_RW	PAGE_READWRITE
#define MCPROT_RX	PAGE_EXECUTE_READ
#define MCPROT_RWX	PAGE_EXECUTE_READWRITE

static void *mcode_alloc_at(jit_State *J, uintptr_t hint, size_t sz, DWORD prot)
{
  void *p = VirtualAlloc((void *)hint, sz,
			 MEM_RESERVE|MEM_COMMIT|MEM_TOP_DOWN, prot);
  if (!p && !hint)
    lj_trace_err(J, LJ_TRERR_MCODEAL);
  return p;
}

static void mcode_free(jit_State *J, void *p, size_t sz)
{
  UNUSED(J); UNUSED(sz);
  VirtualFree(p, 0, MEM_RELEASE);
}

static int mcode_setprot(void *p, size_t sz, DWORD prot)
{
  DWORD oprot;
  return !VirtualProtect(p, sz, prot, &oprot);
}

#elif LJ_TARGET_POSIX

#include <sys/mman.h>

#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS	MAP_ANON
#endif

#define MCPROT_RW	(PROT_READ|PROT_WRITE)
#define MCPROT_RX	(PROT_READ|PROT_EXEC)
#define MCPROT_RWX	(PROT_READ|PROT_WRITE|PROT_EXEC)

static void *mcode_alloc_at(jit_State *J, uintptr_t hint, size_t sz, int prot)
{
  void *p = mmap((void *)hint, sz, prot, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
  if (p == MAP_FAILED) {
    if (!hint) lj_trace_err(J, LJ_TRERR_MCODEAL);
    p = NULL;
  }
  return p;
}

static void mcode_free(jit_State *J, void *p, size_t sz)
{
  UNUSED(J);
  munmap(p, sz);
}

static int mcode_setprot(void *p, size_t sz, int prot)
{
  return mprotect(p, sz, prot);
}

#elif LJ_64

#error "Missing OS support for explicit placement of executable memory"

#else

/* Fallback allocator. This will fail if memory is not executable by default. */
#define LUAJIT_UNPROTECT_MCODE
#define MCPROT_RW	0
#define MCPROT_RX	0
#define MCPROT_RWX	0

static void *mcode_alloc_at(jit_State *J, uintptr_t hint, size_t sz, int prot)
{
  UNUSED(hint); UNUSED(prot);
  return lj_mem_new(J->L, sz);
}

static void mcode_free(jit_State *J, void *p, size_t sz)
{
  lj_mem_free(J2G(J), p, sz);
}

#endif

/* -- MCode area protection ----------------------------------------------- */

/* Define this ONLY if page protection twiddling becomes a bottleneck. */
#ifdef LUAJIT_UNPROTECT_MCODE

/* It's generally considered to be a potential security risk to have
** pages with simultaneous write *and* execute access in a process.
**
** Do not even think about using this mode for server processes or
** apps handling untrusted external data (such as a browser).
**
** The security risk is not in LuaJIT itself -- but if an adversary finds
** any *other* flaw in your C application logic, then any RWX memory page
** simplifies writing an exploit considerably.
*/
#define MCPROT_GEN	MCPROT_RWX
#define MCPROT_RUN	MCPROT_RWX

static void mcode_protect(jit_State *J, int prot)
{
  UNUSED(J); UNUSED(prot);
}

#else

/* This is the default behaviour and much safer:
**
** Most of the time the memory pages holding machine code are executable,
** but NONE of them is writable.
**
** The current memory area is marked read-write (but NOT executable) only
** during the short time window while the assembler generates machine code.
*/
#define MCPROT_GEN	MCPROT_RW
#define MCPROT_RUN	MCPROT_RX

/* Protection twiddling failed. Probably due to kernel security. */
static LJ_NOINLINE void mcode_protfail(jit_State *J)
{
  lua_CFunction panic = J2G(J)->panic;
  if (panic) {
    lua_State *L = J->L;
    setstrV(L, L->top++, lj_err_str(L, LJ_ERR_JITPROT));
    panic(L);
  }
}

/* Change protection of MCode area. */
static void mcode_protect(jit_State *J, int prot)
{
  if (J->mcprot != prot) {
    if (LJ_UNLIKELY(mcode_setprot(J->mcarea, J->szmcarea, prot)))
      mcode_protfail(J);
    J->mcprot = prot;
  }
}

#endif

/* -- MCode area allocation ----------------------------------------------- */

#if LJ_TARGET_X64
#define mcode_validptr(p)	((p) && (uintptr_t)(p) < (uintptr_t)1<<47)
#else
#define mcode_validptr(p)	((p) && (uintptr_t)(p) < 0xffff0000)
#endif

#ifdef LJ_TARGET_JUMPRANGE

/* Get memory within relative jump distance of our code in 64 bit mode. */
static void *mcode_alloc(jit_State *J, size_t sz)
{
  /* Target an address in the static assembler code (64K aligned).
  ** Try addresses within a distance of target-range/2+1MB..target+range/2-1MB.
  ** Use half the jump range so every address in the range can reach any other.
  */
#if LJ_TARGET_MIPS
  /* Use the middle of the 256MB-aligned region. */
  uintptr_t target = ((uintptr_t)(void *)lj_vm_exit_handler & 0xf0000000u) +
		     0x08000000u;
#else
  uintptr_t target = (uintptr_t)(void *)lj_vm_exit_handler & ~(uintptr_t)0xffff;
#endif
  const uintptr_t range = (1u << (LJ_TARGET_JUMPRANGE-1)) - (1u << 21);
  /* First try a contiguous area below the last one. */
  uintptr_t hint = J->mcarea ? (uintptr_t)J->mcarea - sz : 0;
  int i;
  for (i = 0; i < 32; i++) {  /* 32 attempts ought to be enough ... */
    if (mcode_validptr(hint)) {
      void *p = mcode_alloc_at(J, hint, sz, MCPROT_GEN);

      if (mcode_validptr(p) &&
	  ((uintptr_t)p + sz - target < range || target - (uintptr_t)p < range))
	return p;
      if (p) mcode_free(J, p, sz);  /* Free badly placed area. */
    }
    /* Next try probing pseudo-random addresses. */
    do {
      hint = (0x78fb ^ LJ_PRNG_BITS(J, 15)) << 16;  /* 64K aligned. */
    } while (!(hint + sz < range));
    hint = target + hint - (range>>1);
  }
  lj_trace_err(J, LJ_TRERR_MCODEAL);  /* Give up. OS probably ignores hints? */
  return NULL;
}

#else

/* All memory addresses are reachable by relative jumps. */
static void *mcode_alloc(jit_State *J, size_t sz)
{
#ifdef __OpenBSD__
  /* Allow better executable memory allocation for OpenBSD W^X mode. */
  void *p = mcode_alloc_at(J, 0, sz, MCPROT_RUN);
  if (p && mcode_setprot(p, sz, MCPROT_GEN)) {
    mcode_free(J, p, sz);
    return NULL;
  }
  return p;
#else
  return mcode_alloc_at(J, 0, sz, MCPROT_GEN);
#endif
}

#endif

/* -- MCode area management ----------------------------------------------- */

/* Linked list of MCode areas. */
typedef struct MCLink {
  MCode *next;		/* Next area. */
  size_t size;		/* Size of current area. */
#if LJ_ABI_WIN && LJ_TARGET_X64
  char ehandler[6];	/* Stub which jumps to exception handler. */
  uint16_t numunwind;	/* Length of MCUnwind chain. */
  struct MCUnwind *unwind; /* Head of MCUnwind chain, one per trace. */
#endif
} MCLink;

#if LJ_ABI_WIN && LJ_TARGET_X64

typedef struct MCUnwind {
  RUNTIME_FUNCTION rf;	  /* Specifies range of code and pointer to xdata */
  uint16_t xdata[4];	  /* Unwind data for this code range */
  RUNTIME_FUNCTION chain; /* Pointer to remainder of xdata (UnwindInfoAddress)
                             and to next MCUnwind in chain (EndAddress) */
} MCUnwind;

#if LUA_BUILD_AS_DLL

/* Taken from CoreCLR's clrnt.h: */
typedef struct UndocumentedDynamicFunctionTable {
  LIST_ENTRY Links;
  PRUNTIME_FUNCTION FunctionTable;
  LARGE_INTEGER Timestamp;
  ULONG64 MinimumAddress;
  ULONG64 MaximumAddress;
  ULONG64 BaseAddress;
  PGET_RUNTIME_FUNCTION_CALLBACK Callback;
  PVOID Context;
} UndocumentedDynamicFunctionTable;

/* Used by out-of-process debuggers to get unwind data for mcode regions.
   Registered via RtlInstallFunctionTableCallback.
   Can require a KnownFunctionTableDlls registry entry in order to be called
   (see src/dlls/mscordac/mscordac.vrg in CoreCLR) */
LUA_API DWORD OutOfProcessFunctionTableCallback(HANDLE process,
  UndocumentedDynamicFunctionTable *dftable, PDWORD outnumfuncs,
  PRUNTIME_FUNCTION *outfuncs)
{
  char *mc;
  MCLink link;

  if (!outnumfuncs) return (DWORD)0xC00000F1L;
  if (!outfuncs) return (DWORD)0xC00000F2L;
  *outnumfuncs = 0;
  *outfuncs = NULL;

#define read(src, dst) \
  if (!ReadProcessMemory(process, (src), &(dst), sizeof((dst)), NULL)) \
    return (DWORD)0xC0000001L

  read(&dftable->Context, mc);
  read(mc, link);

  if (link.numunwind) {
    uint32_t numunwind = link.numunwind;
    uint32_t i = 0;
    MCUnwind unwind;
    PRUNTIME_FUNCTION funcs = (PRUNTIME_FUNCTION)HeapAlloc(GetProcessHeap(),
				     0, sizeof(RUNTIME_FUNCTION) * numunwind);
    if (!funcs) return (DWORD)0xC0000017L;
    *outfuncs = funcs;
    read(link.unwind, unwind);
    for (;;) {
      funcs[i] = unwind.rf;
      if (++i == numunwind) {
	break;
      }
      read(mc + unwind.chain.EndAddress, unwind);
    }
    *outnumfuncs = numunwind;
  }

#undef read
  return 0;
}

static const wchar_t* mcode_our_dll_name()
{
  extern IMAGE_DOS_HEADER __ImageBase;
  static const wchar_t* result;
  if (!result) {
    static wchar_t buf[MAX_PATH];
    DWORD n = GetModuleFileNameW((HMODULE)&__ImageBase, buf, MAX_PATH);
    if (n == 0 || n == MAX_PATH) {
      result = L"";
    } else {
      result = buf;
    }
  }
  return result;
}

#else

#define mcode_our_dll_name() NULL

#endif

static PRUNTIME_FUNCTION mcode_find_win64_unwind_data(DWORD64 pc, PVOID mc)
{
  MCLink *link = (MCLink *)mc;
  MCUnwind *unwind = link->unwind;
  uint32_t numunwind = link->numunwind;
  uint32_t i;
  DWORD off = (DWORD)(pc - (DWORD64)mc);
  for (i = 0; i < numunwind; ++i) {
    if (unwind->rf.BeginAddress <= off && off < unwind->rf.EndAddress) {
      return &unwind->rf;
    }
    unwind = (MCUnwind *)((char *)mc + unwind->chain.EndAddress);
  }
  return NULL;
}

/* Common tail of xdata for traces. */
static const uint16_t mcode_win64tracexdata[] = {
  0x01|0x08|0x10,  /* Ver. 1, [eu]handler, prolog size 0. */
  0x0023,  /* Number of unwind codes, no frame pointer. */
  0xF800,  2,  /* Save xmm15 */
  0xE800,  3,  /* Save xmm14 */
  0xD800,  4,  /* Save xmm13 */
  0xC800,  5,  /* Save xmm12 */
  0xB800,  6,  /* Save xmm11 */
  0xA800,  7,  /* Save xmm10 */
  0x9800,  8,  /* Save xmm9 */
  0x8800,  9,  /* Save xmm8 */
  0x7800, 10,  /* Save xmm7 */
  0x6800, 11,  /* Save xmm6 */
  0x0100, 22,  /* Sub rsp, 9*16+4*8 */
  0xE400,  2,  /* Mov CSAVE_3, r15 */
  0xE400,  3,  /* Mov CSAVE_4, r14 */
  0xD400,  4,  /* Mov TMPa, r13 */
  0xC400, 10,  /* Mov TMPQ, r12 */
  0x4200,  /* Stack offset 4*8+8 = aword*5. */
  0x3000,  /* Push rbx. */
  0x6000,  /* Push rsi. */
  0x7000,  /* Push rdi. */
  0x5000,  /* Push rbp. */
  0,  /* Alignment. */
  offsetof(MCLink, ehandler), 0 /* Handler. */
};
#endif

/* Allocate a new MCode area. */
static void mcode_allocarea(jit_State *J)
{
  MCode *oldarea = J->mcarea;
  size_t sz = (size_t)J->param[JIT_P_sizemcode] << 10;
  MCLink *link;
  sz = (sz + LJ_PAGESIZE-1) & ~(size_t)(LJ_PAGESIZE - 1);
  J->mcarea = (MCode *)mcode_alloc(J, sz);
  J->szmcarea = sz;
  J->mcprot = MCPROT_GEN;
  J->mctop = (MCode *)((char *)J->mcarea + J->szmcarea);
  J->mcbot = (MCode *)((char *)J->mcarea + sizeof(MCLink));
  link = (MCLink *)J->mcarea;
  link->next = oldarea;
  link->size = sz;
  J->szallmcarea += sz;
#if LJ_ABI_WIN && LJ_TARGET_X64
  if (J->mcarea > J->win64tracexdata) {
    /* The xdata tail must be within [0, 4G) of J->mcarea, else it cannot be
       referred to. As all mcode areas are within [-2G, +2G] of each other, it
       sufficies to ensure that the mcode area with the highest address
       contains a copy of the xdata tail. */ 
    J->mctop -= sizeof(mcode_win64tracexdata);
    memcpy(J->mctop, mcode_win64tracexdata, sizeof(mcode_win64tracexdata));
    J->win64tracexdata = J->mctop;
  }
  /* Stub which jumps to lj_err_unwind_trace_win64 (the offset to the landing
     pad code is relative to the base of the mcode area, and is specified in
     the xdata tail - so it has to be the same offset for every mcode area) */
  link->ehandler[0] = 0xE9;
  *(int32_t*)(link->ehandler + 1) = (int32_t)
    ((char*)&lj_err_unwind_trace_win64 - link->ehandler - 5);
  link->ehandler[5] = 0xCC;
  
  link->numunwind = 0;
  link->unwind = (MCUnwind *)J->mctop;
  RtlInstallFunctionTableCallback(3|(DWORD64)link, (DWORD64)link, (DWORD)sz,
				  mcode_find_win64_unwind_data, link,
				  mcode_our_dll_name());
#endif
}

/* Free all MCode areas. */
void lj_mcode_free(jit_State *J)
{
  MCode *mc = J->mcarea;
  J->mcarea = NULL;
  J->szallmcarea = 0;
  while (mc) {
    MCode *next = ((MCLink *)mc)->next;
#if LJ_ABI_WIN && LJ_TARGET_X64
    RtlDeleteFunctionTable((PRUNTIME_FUNCTION)(3|(DWORD64)mc));
#endif
    mcode_free(J, mc, ((MCLink *)mc)->size);
    mc = next;
  }
}

/* -- MCode transactions -------------------------------------------------- */

/* Reserve the remainder of the current MCode area. */
MCode *lj_mcode_reserve(jit_State *J, MCode **lim)
{
  if (!J->mcarea)
    mcode_allocarea(J);
  else
    mcode_protect(J, MCPROT_GEN);
  *lim = J->mcbot;
#if LJ_ABI_WIN && LJ_TARGET_X64
  return J->mctop - sizeof(MCUnwind);
#else
  return J->mctop;
#endif
}

/* Commit the top part of the current MCode area. */
void lj_mcode_commit(jit_State *J, MCode *top)
{
#if LJ_ABI_WIN && LJ_TARGET_X64
  MCLink *link = (MCLink *)J->mcarea;
  MCUnwind *unwind = (MCUnwind *)(J->mctop - sizeof(MCUnwind));
  uint16_t spadj = J->cur.spadjust;
  unwind->rf.BeginAddress = (DWORD)(top - J->mcarea);
  unwind->rf.EndAddress = (DWORD)(J->mctop - J->mcarea);
  if (spadj) {
    /* Stack is adjusted - encode adjustment in local xdata, and then chain
       onto the common xdata tail. The chain.BeginAddress and chain.EndAddress
       fields will end up specifying a superset of the range specified in
       rf.BeginAddress and rf.EndAddress, which seems to suffice. */
    unwind->rf.UnwindInfoAddress = (DWORD)((MCode *)&unwind->xdata - J->mcarea);
    unwind->xdata[0] = 0x01|0x20;  /* Ver. 1, chained, prolog size 0. */
    unwind->xdata[1] = 0x0002;  /* Number of unwind codes, no frame pointer. */
    unwind->xdata[2] = 0x0100;  /* Sub rsp, xdata[3] * 8 */
    unwind->xdata[3] = spadj / 8; /* NB: Used by lj_err_unwind_trace_win64 */
    unwind->chain.BeginAddress = 0;
    unwind->chain.UnwindInfoAddress = (DWORD)(J->win64tracexdata - J->mcarea);
  } else {
    /* No stack adjustment - don't use chained unwind data (the in-process
       unwinder is fine with chained unwind data, but the VS out-of-process
       unwinder doesn't seem to like it, so we avoid it when possible) */
    unwind->rf.UnwindInfoAddress = (DWORD)(J->win64tracexdata - J->mcarea);
  }
  unwind->chain.EndAddress = (DWORD)((MCode *)link->unwind - J->mcarea);
  link->unwind = unwind;
  ++link->numunwind;
  top -= (uintptr_t)top & 3;
#endif
  J->mctop = top;
  mcode_protect(J, MCPROT_RUN);
}

/* Abort the reservation. */
void lj_mcode_abort(jit_State *J)
{
  if (J->mcarea)
    mcode_protect(J, MCPROT_RUN);
}

/* Set/reset protection to allow patching of MCode areas. */
MCode *lj_mcode_patch(jit_State *J, MCode *ptr, int finish)
{
#ifdef LUAJIT_UNPROTECT_MCODE
  UNUSED(J); UNUSED(ptr); UNUSED(finish);
  return NULL;
#else
  if (finish) {
    if (J->mcarea == ptr)
      mcode_protect(J, MCPROT_RUN);
    else if (LJ_UNLIKELY(mcode_setprot(ptr, ((MCLink *)ptr)->size, MCPROT_RUN)))
      mcode_protfail(J);
    return NULL;
  } else {
    MCode *mc = J->mcarea;
    /* Try current area first to use the protection cache. */
    if (ptr >= mc && ptr < (MCode *)((char *)mc + J->szmcarea)) {
      mcode_protect(J, MCPROT_GEN);
      return mc;
    }
    /* Otherwise search through the list of MCode areas. */
    for (;;) {
      mc = ((MCLink *)mc)->next;
      lua_assert(mc != NULL);
      if (ptr >= mc && ptr < (MCode *)((char *)mc + ((MCLink *)mc)->size)) {
	if (LJ_UNLIKELY(mcode_setprot(mc, ((MCLink *)mc)->size, MCPROT_GEN)))
	  mcode_protfail(J);
	return mc;
      }
    }
  }
#endif
}

/* Limit of MCode reservation reached. */
void lj_mcode_limiterr(jit_State *J, size_t need)
{
  size_t sizemcode, maxmcode;
  lj_mcode_abort(J);
  sizemcode = (size_t)J->param[JIT_P_sizemcode] << 10;
  sizemcode = (sizemcode + LJ_PAGESIZE-1) & ~(size_t)(LJ_PAGESIZE - 1);
  maxmcode = (size_t)J->param[JIT_P_maxmcode] << 10;
  if ((size_t)need > sizemcode)
    lj_trace_err(J, LJ_TRERR_MCODEOV);  /* Too long for any area. */
  if (J->szallmcarea + sizemcode > maxmcode)
    lj_trace_err(J, LJ_TRERR_MCODEAL);
  mcode_allocarea(J);
  lj_trace_err(J, LJ_TRERR_MCODELM);  /* Retry with new area. */
}

#endif
