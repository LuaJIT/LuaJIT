/*
** Error handling and debugging API.
** Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
**
** Portions taken verbatim or adapted from the Lua interpreter.
** Copyright (C) 1994-2008 Lua.org, PUC-Rio. See Copyright Notice in lua.h
*/

#define lj_err_c
#define LUA_CORE

#include "lj_obj.h"
#include "lj_err.h"
#include "lj_str.h"
#include "lj_tab.h"
#include "lj_func.h"
#include "lj_state.h"
#include "lj_frame.h"
#include "lj_bc.h"
#include "lj_ff.h"
#include "lj_trace.h"
#include "lj_vm.h"

/*
** LuaJIT can either use internal or external frame unwinding:
**
** - Internal frame unwinding (INT) is free-standing and doesn't require
**   any OS or library support.
**
** - External frame unwinding (EXT) uses the system-provided unwind handler.
**
** Pros and Cons:
**
** - EXT requires unwind tables for *all* functions on the C stack between
**   the pcall/catch and the error/throw. This is the default on x64,
**   but needs to be manually enabled on x86/PPC for non-C++ code.
**
** - INT is faster when actually throwing errors (but this happens rarely).
**   Setting up error handlers is zero-cost in any case.
**
** - EXT provides full interoperability with C++ exceptions. You can throw
**   Lua errors or C++ exceptions through a mix of Lua frames and C++ frames.
**   C++ destructors are called as needed. C++ exceptions caught by pcall
**   are converted to the string "C++ exception". Lua errors can be caught
**   with catch (...) in C++.
**
** - INT has only limited support for automatically catching C++ exceptions
**   on POSIX systems using DWARF2 stack unwinding. Other systems may use
**   the wrapper function feature. Lua errors thrown through C++ frames
**   cannot be caught by C++ code and C++ destructors are not run.
**
** EXT is the default on x64 systems, INT is the default on all other systems.
**
** EXT can be manually enabled on POSIX systems using GCC and DWARF2 stack
** unwinding with -DLUAJIT_UNWIND_EXTERNAL. *All* C code must be compiled
** with -funwind-tables (or -fexceptions). This includes LuaJIT itself (set
** TARGET_CFLAGS), all of your C/Lua binding code, all loadable C modules
** and all C libraries that have callbacks which may be used to call back
** into Lua. C++ code must *not* be compiled with -fno-exceptions.
**
** EXT cannot be enabled on WIN32 since system exceptions use code-driven SEH.
** EXT is mandatory on WIN64 since the calling convention has an abundance
** of callee-saved registers (rbx, rbp, rsi, rdi, r12-r15, xmm6-xmm15).
** EXT is mandatory on POSIX/x64 since the interpreter doesn't save r12/r13.
*/

#if defined(__GNUC__) && (LJ_TARGET_X64 || defined(LUAJIT_UNWIND_EXTERNAL))
#define LJ_UNWIND_EXT	1
#elif LJ_TARGET_X64 && LJ_TARGET_WINDOWS
#define LJ_UNWIND_EXT	1
#endif

/* -- Error messages ------------------------------------------------------ */

/* Error message strings. */
LJ_DATADEF const char *lj_err_allmsg =
#define ERRDEF(name, msg)	msg "\0"
#include "lj_errmsg.h"
;

/* -- Frame and function introspection ------------------------------------ */

static BCPos currentpc(lua_State *L, GCfunc *fn, cTValue *nextframe)
{
  const BCIns *ins;
  lua_assert(fn->c.gct == ~LJ_TFUNC || fn->c.gct == ~LJ_TTHREAD);
  if (!isluafunc(fn)) {  /* Cannot derive a PC for non-Lua functions. */
    return ~(BCPos)0;
  } else if (nextframe == NULL) {  /* Lua function on top. */
    void *cf = cframe_raw(L->cframe);
    if (cf == NULL || (char *)cframe_pc(cf) == (char *)cframe_L(cf))
      return ~(BCPos)0;
    ins = cframe_pc(cf);  /* Only happens during error/hook handling. */
  } else {
    if (frame_islua(nextframe)) {
      ins = frame_pc(nextframe);
    } else if (frame_iscont(nextframe)) {
      ins = frame_contpc(nextframe);
    } else {
      /* Lua function below errfunc/gc/hook: find cframe to get the PC. */
      void *cf = cframe_raw(L->cframe);
      TValue *f = L->base-1;
      if (cf == NULL)
	return ~(BCPos)0;
      while (f > nextframe) {
	if (frame_islua(f)) {
	  f = frame_prevl(f);
	} else {
	  if (frame_isc(f))
	    cf = cframe_raw(cframe_prev(cf));
	  f = frame_prevd(f);
	}
      }
      if (cframe_prev(cf))
	cf = cframe_raw(cframe_prev(cf));
      ins = cframe_pc(cf);
    }
  }
  return proto_bcpos(funcproto(fn), ins) - 1;
}

static BCLine currentline(lua_State *L, GCfunc *fn, cTValue *nextframe)
{
  BCPos pc = currentpc(L, fn, nextframe);
  if (pc != ~(BCPos)0) {
    GCproto *pt = funcproto(fn);
    lua_assert(pc < pt->sizebc);
    return proto_line(pt, pc);
  } else {
    return -1;
  }
}

static const char *getvarname(const GCproto *pt, BCPos pc, BCReg slot)
{
  MSize i;
  VarInfo *vi = proto_varinfo(pt);
  for (i = 0; i < pt->sizevarinfo && vi[i].startpc <= pc; i++)
    if (pc < vi[i].endpc && slot-- == 0)
      return strdata(gco2str(gcref(vi[i].name)));
  return NULL;
}

static const char *getobjname(GCproto *pt, const BCIns *ip, BCReg slot,
			      const char **name)
{
  const char *lname;
restart:
  lname = getvarname(pt, proto_bcpos(pt, ip), slot);
  if (lname != NULL) { *name = lname; return "local"; }
  while (--ip > proto_bc(pt)) {
    BCIns ins = *ip;
    BCOp op = bc_op(ins);
    BCReg ra = bc_a(ins);
    if (bcmode_a(op) == BCMbase) {
      if (slot >= ra && (op != BC_KNIL || slot <= bc_d(ins)))
	return NULL;
    } else if (bcmode_a(op) == BCMdst && ra == slot) {
      switch (bc_op(ins)) {
      case BC_MOV:
	if (ra == slot) { slot = bc_d(ins); goto restart; }
	break;
      case BC_GGET:
	*name = strdata(gco2str(proto_kgc(pt, ~(ptrdiff_t)bc_d(ins))));
	return "global";
      case BC_TGETS:
	*name = strdata(gco2str(proto_kgc(pt, ~(ptrdiff_t)bc_c(ins))));
	if (ip > proto_bc(pt)) {
	  BCIns insp = ip[-1];
	  if (bc_op(insp) == BC_MOV && bc_a(insp) == ra+1 &&
	      bc_d(insp) == bc_b(ins))
	    return "method";
	}
	return "field";
      case BC_UGET:
	*name = strdata(proto_uvname(pt, bc_d(ins)));
	return "upvalue";
      default:
	return NULL;
      }
    }
  }
  return NULL;
}

static const char *getfuncname(lua_State *L, TValue *frame, const char **name)
{
  MMS mm;
  const BCIns *ip;
  TValue *pframe;
  GCfunc *fn;
  BCPos pc;
  if (frame_isvarg(frame))
    frame = frame_prevd(frame);
  pframe = frame_prev(frame);
  fn = frame_func(pframe);
  pc = currentpc(L, fn, frame);
  if (pc == ~(BCPos)0)
    return NULL;
  lua_assert(pc < funcproto(fn)->sizebc);
  ip = &proto_bc(funcproto(fn))[pc];
  mm = bcmode_mm(bc_op(*ip));
  if (mm == MM_call) {
    BCReg slot = bc_a(*ip);
    if (bc_op(*ip) == BC_ITERC) slot -= 3;
    return getobjname(funcproto(fn), ip, slot, name);
  } else if (mm != MM_MAX) {
    *name = strdata(mmname_str(G(L), mm));
    return "metamethod";
  } else {
    return NULL;
  }
}

void lj_err_pushloc(lua_State *L, GCproto *pt, BCPos pc)
{
  GCstr *name = proto_chunkname(pt);
  if (name) {
    const char *s = strdata(name);
    MSize i, len = name->len;
    BCLine line = pc < pt->sizebc ? proto_line(pt, pc) : 0;
    if (*s == '@') {
      s++; len--;
      for (i = len; i > 0; i--)
	if (s[i] == '/' || s[i] == '\\') {
	  s += i+1;
	  break;
	}
      lj_str_pushf(L, "%s:%d", s, line);
    } else if (len > 40) {
      lj_str_pushf(L, "%p:%d", pt, line);
    } else if (*s == '=') {
      lj_str_pushf(L, "%s:%d", s+1, line);
    } else {
      lj_str_pushf(L, "\"%s\":%d", s, line);
    }
  } else {
    lj_str_pushf(L, "%p:%u", pt, pc);
  }
}

static void err_chunkid(char *out, const char *src)
{
  if (*src == '=') {
    strncpy(out, src+1, LUA_IDSIZE);  /* remove first char */
    out[LUA_IDSIZE-1] = '\0';  /* ensures null termination */
  } else if (*src == '@') { /* out = "source", or "...source" */
    size_t l = strlen(++src);  /* skip the `@' */
    if (l >= LUA_IDSIZE) {
      src += l-(LUA_IDSIZE-4);  /* get last part of file name */
      strcpy(out, "...");
      out += 3;
    }
    strcpy(out, src);
  } else {  /* out = [string "string"] */
    size_t len; /* Length, up to first control char. */
    for (len = 0; len < LUA_IDSIZE-12; len++)
      if (((const unsigned char *)src)[len] < ' ') break;
    strcpy(out, "[string \""); out += 9;
    if (src[len] != '\0') {  /* must truncate? */
      if (len > LUA_IDSIZE-15) len = LUA_IDSIZE-15;
      strncpy(out, src, len); out += len;
      strcpy(out, "..."); out += 3;
    } else {
      strcpy(out, src); out += len;
    }
    strcpy(out, "\"]");
  }
}

/* -- Public debug API ---------------------------------------------------- */

static TValue *findlocal(lua_State *L, const lua_Debug *ar,
			 const char **name, BCReg slot)
{
  uint32_t offset = (uint32_t)ar->i_ci & 0xffff;
  uint32_t size = (uint32_t)ar->i_ci >> 16;
  TValue *frame = tvref(L->stack) + offset;
  TValue *nextframe = size ? frame + size : NULL;
  GCfunc *fn = frame_func(frame);
  BCPos pc = currentpc(L, fn, nextframe);
  if (pc != ~(BCPos)0 &&
      (*name = getvarname(funcproto(fn), pc, slot-1)) != NULL)
    ;
  else if (slot > 0 && frame + slot < (nextframe ? nextframe : L->top))
    *name = "(*temporary)";
  else
    *name = NULL;
  return frame+slot;
}

LUA_API const char *lua_getlocal(lua_State *L, const lua_Debug *ar, int n)
{
  const char *name;
  TValue *o = findlocal(L, ar, &name, (BCReg)n);
  if (name) {
    copyTV(L, L->top, o);
    incr_top(L);
  }
  return name;
}


LUA_API const char *lua_setlocal(lua_State *L, const lua_Debug *ar, int n)
{
  const char *name;
  TValue *o = findlocal(L, ar, &name, (BCReg)n);
  if (name)
    copyTV(L, o, L->top-1);
  L->top--;
  return name;
}

LUA_API int lua_getinfo(lua_State *L, const char *what, lua_Debug *ar)
{
  int status = 1;
  TValue *frame = NULL;
  TValue *nextframe = NULL;
  GCfunc *fn;
  if (*what == '>') {
    TValue *func = L->top - 1;
    api_check(L, tvisfunc(func));
    fn = funcV(func);
    L->top--;
    what++;
  } else {
    uint32_t offset = (uint32_t)ar->i_ci & 0xffff;
    uint32_t size = (uint32_t)ar->i_ci >> 16;
    lua_assert(offset != 0);
    frame = tvref(L->stack) + offset;
    if (size) nextframe = frame + size;
    lua_assert(frame <= tvref(L->maxstack) &&
	       (!nextframe || nextframe <= tvref(L->maxstack)));
    fn = frame_func(frame);
    lua_assert(fn->c.gct == ~LJ_TFUNC);
  }
  for (; *what; what++) {
    switch (*what) {
    case 'S':
      if (isluafunc(fn)) {
	GCproto *pt = funcproto(fn);
	ar->source = strdata(proto_chunkname(pt));
	ar->linedefined = (int)proto_line(pt, 0);
	ar->lastlinedefined = (int)pt->lastlinedefined;
	ar->what = (ar->linedefined == 0) ? "main" : "Lua";
      } else {
	ar->source = "=[C]";
	ar->linedefined = -1;
	ar->lastlinedefined = -1;
	ar->what = "C";
      }
      err_chunkid(ar->short_src, ar->source);
      break;
    case 'l':
      ar->currentline = frame ? currentline(L, fn, nextframe) : -1;
      break;
    case 'u':
      ar->nups = fn->c.nupvalues;
      break;
    case 'n':
      ar->namewhat = frame ? getfuncname(L, frame, &ar->name) : NULL;
      if (ar->namewhat == NULL) {
	ar->namewhat = "";
	ar->name = NULL;
      }
      break;
    case 'f':
      setfuncV(L, L->top, fn);
      incr_top(L);
      break;
    case 'L':
      if (isluafunc(fn)) {
	GCtab *t = lj_tab_new(L, 0, 0);
	GCproto *pt = funcproto(fn);
	BCLine *lineinfo = proto_lineinfo(pt);
	MSize i, szl = pt->sizebc;
	for (i = 1; i < szl; i++)
	  setboolV(lj_tab_setint(L, t, lineinfo[i]), 1);
	settabV(L, L->top, t);
      } else {
	setnilV(L->top);
      }
      incr_top(L);
      break;
    default:
      status = 0;  /* Bad option. */
      break;
    }
  }
  return status;
}

cTValue *lj_err_getframe(lua_State *L, int level, int *size)
{
  cTValue *frame, *nextframe, *bot = tvref(L->stack);
  /* Traverse frames backwards. */
  for (nextframe = frame = L->base-1; frame > bot; ) {
    if (frame_gc(frame) == obj2gco(L))
      level++;  /* Skip dummy frames. See lj_meta_call(). */
    if (level-- == 0) {
      *size = (int)(nextframe - frame);
      return frame;  /* Level found. */
    }
    nextframe = frame;
    if (frame_islua(frame)) {
      frame = frame_prevl(frame);
    } else {
      if (frame_isvarg(frame))
	level++;  /* Skip vararg pseudo-frame. */
      frame = frame_prevd(frame);
    }
  }
  *size = level;
  return NULL;  /* Level not found. */
}

LUA_API int lua_getstack(lua_State *L, int level, lua_Debug *ar)
{
  int size;
  cTValue *frame = lj_err_getframe(L, level, &size);
  if (frame) {
    ar->i_ci = (size << 16) + (int)(frame - tvref(L->stack));
    return 1;
  } else {
    ar->i_ci = level - size;
    return 0;
  }
}

/* -- Internal frame unwinding -------------------------------------------- */

/* Unwind Lua stack and move error message to new top. */
LJ_NOINLINE static void unwindstack(lua_State *L, TValue *top)
{
  lj_func_closeuv(L, top);
  if (top < L->top-1) {
    copyTV(L, top, L->top-1);
    L->top = top+1;
  }
  lj_state_relimitstack(L);
}

/* Unwind until stop frame. Optionally cleanup frames. */
static void *err_unwind(lua_State *L, void *stopcf, int errcode)
{
  TValue *frame = L->base-1;
  void *cf = L->cframe;
  while (cf) {
    int32_t nres = cframe_nres(cframe_raw(cf));
    if (nres < 0) {  /* C frame without Lua frame? */
      TValue *top = restorestack(L, -nres);
      if (frame < top) {  /* Frame reached? */
	if (errcode) {
	  L->cframe = cframe_prev(cf);
	  L->base = frame+1;
	  unwindstack(L, top);
	}
	return cf;
      }
    }
    if (frame <= tvref(L->stack))
      break;
    switch (frame_typep(frame)) {
    case FRAME_LUA:  /* Lua frame. */
    case FRAME_LUAP:
      frame = frame_prevl(frame);
      break;
    case FRAME_C:  /* C frame. */
#if LJ_UNWIND_EXT
      if (errcode) {
	L->cframe = cframe_prev(cf);
	L->base = frame_prevd(frame) + 1;
	unwindstack(L, frame);
      } else if (cf != stopcf) {
	cf = cframe_prev(cf);
	frame = frame_prevd(frame);
	break;
      }
      return NULL;  /* Continue unwinding. */
#else
      UNUSED(stopcf);
      cf = cframe_prev(cf);
      frame = frame_prevd(frame);
      break;
#endif
    case FRAME_CP:  /* Protected C frame. */
      if (cframe_canyield(cf)) {  /* Resume? */
	if (errcode) {
	  L->cframe = NULL;
	  L->status = (uint8_t)errcode;
	}
	return cf;
      }
      if (errcode) {
	L->cframe = cframe_prev(cf);
	L->base = frame_prevd(frame) + 1;
	unwindstack(L, frame);
      }
      return cf;
    case FRAME_CONT:  /* Continuation frame. */
    case FRAME_VARG:  /* Vararg frame. */
      frame = frame_prevd(frame);
      break;
    case FRAME_PCALL:  /* FF pcall() frame. */
    case FRAME_PCALLH:  /* FF pcall() frame inside hook. */
      if (errcode) {
	if (errcode == LUA_YIELD) {
	  frame = frame_prevd(frame);
	  break;
	}
	if (frame_typep(frame) == FRAME_PCALL)
	  hook_leave(G(L));
	L->cframe = cf;
	L->base = frame_prevd(frame) + 1;
	unwindstack(L, L->base);
      }
      return (void *)((intptr_t)cf | CFRAME_UNWIND_FF);
    }
  }
  /* No C frame. */
  if (errcode) {
    L->cframe = NULL;
    L->base = tvref(L->stack)+1;
    unwindstack(L, L->base);
    if (G(L)->panic)
      G(L)->panic(L);
    exit(EXIT_FAILURE);
  }
  return L;  /* Anything non-NULL will do. */
}

/* -- External frame unwinding -------------------------------------------- */

#if defined(__GNUC__) && !LJ_TARGET_ARM

#ifdef __clang__
/* http://llvm.org/bugs/show_bug.cgi?id=8703 */
#define __unwind_word__ word
#endif

#include <unwind.h>

#define LJ_UEXCLASS		0x4c55414a49543200ULL	/* LUAJIT2\0 */
#define LJ_UEXCLASS_MAKE(c)	(LJ_UEXCLASS | (_Unwind_Exception_Class)(c))
#define LJ_UEXCLASS_CHECK(cl)	(((cl) ^ LJ_UEXCLASS) <= 0xff)
#define LJ_UEXCLASS_ERRCODE(cl)	((int)((cl) & 0xff))

/* DWARF2 personality handler referenced from interpreter .eh_frame. */
LJ_FUNCA int lj_err_unwind_dwarf(int version, _Unwind_Action actions,
  _Unwind_Exception_Class uexclass, struct _Unwind_Exception *uex,
  struct _Unwind_Context *ctx)
{
  void *cf;
  lua_State *L;
  if (version != 1)
    return _URC_FATAL_PHASE1_ERROR;
  UNUSED(uexclass);
  cf = (void *)_Unwind_GetCFA(ctx);
  L = cframe_L(cf);
  if ((actions & _UA_SEARCH_PHASE)) {
#if LJ_UNWIND_EXT
    if (err_unwind(L, cf, 0) == NULL)
      return _URC_CONTINUE_UNWIND;
#endif
    if (!LJ_UEXCLASS_CHECK(uexclass)) {
      setstrV(L, L->top++, lj_err_str(L, LJ_ERR_ERRCPP));
    }
    return _URC_HANDLER_FOUND;
  }
  if ((actions & _UA_CLEANUP_PHASE)) {
    int errcode;
    if (LJ_UEXCLASS_CHECK(uexclass)) {
      errcode = LJ_UEXCLASS_ERRCODE(uexclass);
    } else {
      if ((actions & _UA_HANDLER_FRAME))
	_Unwind_DeleteException(uex);
      errcode = LUA_ERRRUN;
    }
#if LJ_UNWIND_EXT
    cf = err_unwind(L, cf, errcode);
    if (cf) {
      _Unwind_SetGR(ctx, LJ_TARGET_EHRETREG, errcode);
      _Unwind_SetIP(ctx, (_Unwind_Ptr)(cframe_unwind_ff(cf) ?
				       lj_vm_unwind_ff_eh :
				       lj_vm_unwind_c_eh));
      return _URC_INSTALL_CONTEXT;
    }
#if LJ_TARGET_X86ORX64
    else if ((actions & _UA_HANDLER_FRAME)) {
      /* Workaround for ancient libgcc bug. Still present in RHEL 5.5. :-/
      ** Real fix: http://gcc.gnu.org/viewcvs/trunk/gcc/unwind-dw2.c?r1=121165&r2=124837&pathrev=153877&diff_format=h
      */
      _Unwind_SetGR(ctx, LJ_TARGET_EHRETREG, errcode);
      _Unwind_SetIP(ctx, (_Unwind_Ptr)lj_vm_unwind_rethrow);
      return _URC_INSTALL_CONTEXT;
    }
#endif
#else
    /* This is not the proper way to escape from the unwinder. We get away with
    ** it on x86/PPC because the interpreter restores all callee-saved regs.
    */
    lj_err_throw(L, errcode);
#endif
  }
  return _URC_CONTINUE_UNWIND;
}

#if LJ_UNWIND_EXT
/* NYI: this is not thread-safe. */
static struct _Unwind_Exception static_uex;

/* Raise DWARF2 exception. */
static void err_raise_ext(int errcode)
{
  static_uex.exception_class = LJ_UEXCLASS_MAKE(errcode);
  static_uex.exception_cleanup = NULL;
  _Unwind_RaiseException(&static_uex);
}
#endif

#elif LJ_TARGET_X64 && LJ_TARGET_WINDOWS

/*
** Someone in Redmond owes me several days of my life. A lot of this is
** undocumented or just plain wrong on MSDN. Some of it can be gathered
** from 3rd party docs or must be found by trial-and-error. They really
** don't want you to write your own language-specific exception handler
** or to interact gracefully with MSVC. :-(
**
** Apparently MSVC doesn't call C++ destructors for foreign exceptions
** unless you compile your C++ code with /EHa. Unfortunately this means
** catch (...) also catches things like access violations. The use of
** _set_se_translator doesn't really help, because it requires /EHa, too.
*/

#define WIN32_LEAN_AND_MEAN
#include <windows.h>

/* Taken from: http://www.nynaeve.net/?p=99 */
typedef struct UndocumentedDispatcherContext {
  ULONG64 ControlPc;
  ULONG64 ImageBase;
  PRUNTIME_FUNCTION FunctionEntry;
  ULONG64 EstablisherFrame;
  ULONG64 TargetIp;
  PCONTEXT ContextRecord;
  PEXCEPTION_ROUTINE LanguageHandler;
  PVOID HandlerData;
  PUNWIND_HISTORY_TABLE HistoryTable;
  ULONG ScopeIndex;
  ULONG Fill0;
} UndocumentedDispatcherContext;

#ifdef _MSC_VER
/* Another wild guess. */
extern __DestructExceptionObject(EXCEPTION_RECORD *rec, int nothrow);
#endif

#define LJ_MSVC_EXCODE		((DWORD)0xe06d7363)

#define LJ_EXCODE		((DWORD)0xe24c4a00)
#define LJ_EXCODE_MAKE(c)	(LJ_EXCODE | (DWORD)(c))
#define LJ_EXCODE_CHECK(cl)	(((cl) ^ LJ_EXCODE) <= 0xff)
#define LJ_EXCODE_ERRCODE(cl)	((int)((cl) & 0xff))

/* Win64 exception handler for interpreter frame. */
LJ_FUNCA EXCEPTION_DISPOSITION lj_err_unwind_win64(EXCEPTION_RECORD *rec,
  void *cf, CONTEXT *ctx, UndocumentedDispatcherContext *dispatch)
{
  lua_State *L = cframe_L(cf);
  int errcode = LJ_EXCODE_CHECK(rec->ExceptionCode) ?
		LJ_EXCODE_ERRCODE(rec->ExceptionCode) : LUA_ERRRUN;
  if ((rec->ExceptionFlags & 6)) {  /* EH_UNWINDING|EH_EXIT_UNWIND */
    /* Unwind internal frames. */
    err_unwind(L, cf, errcode);
  } else {
    void *cf2 = err_unwind(L, cf, 0);
    if (cf2) {  /* We catch it, so start unwinding the upper frames. */
      if (rec->ExceptionCode == LJ_MSVC_EXCODE) {
#ifdef _MSC_VER
	__DestructExceptionObject(rec, 1);
#endif
	setstrV(L, L->top++, lj_err_str(L, LJ_ERR_ERRCPP));
      } else if (!LJ_EXCODE_CHECK(rec->ExceptionCode)) {
	/* Don't catch access violations etc. */
	return ExceptionContinueSearch;
      }
      /* Unwind the stack and call all handlers for all lower C frames
      ** (including ourselves) again with EH_UNWINDING set. Then set
      ** rsp = cf, rax = errcode and jump to the specified target.
      */
      RtlUnwindEx(cf, (void *)((cframe_unwind_ff(cf2) && errcode != LUA_YIELD) ?
			       lj_vm_unwind_ff_eh :
			       lj_vm_unwind_c_eh),
		  rec, (void *)errcode, ctx, dispatch->HistoryTable);
      /* RtlUnwindEx should never return. */
    }
  }
  return ExceptionContinueSearch;
}

/* Raise Windows exception. */
static void err_raise_ext(int errcode)
{
  RaiseException(LJ_EXCODE_MAKE(errcode), 1 /* EH_NONCONTINUABLE */, 0, NULL);
}

#endif

/* -- Error handling ------------------------------------------------------ */

/* Throw error. Find catch frame, unwind stack and continue. */
LJ_NOINLINE void LJ_FASTCALL lj_err_throw(lua_State *L, int errcode)
{
  global_State *g = G(L);
  lj_trace_abort(g);
  setgcrefnull(g->jit_L);
  L->status = 0;
#if LJ_UNWIND_EXT
  err_raise_ext(errcode);
  /*
  ** A return from this function signals a corrupt C stack that cannot be
  ** unwound. We have no choice but to call the panic function and exit.
  **
  ** Usually this is caused by a C function without unwind information.
  ** This should never happen on x64, but may happen if you've manually
  ** enabled LUAJIT_UNWIND_EXTERNAL and forgot to recompile *every*
  ** non-C++ file with -funwind-tables.
  */
  if (G(L)->panic)
    G(L)->panic(L);
#else
  {
    void *cf = err_unwind(L, NULL, errcode);
    if (cframe_unwind_ff(cf))
      lj_vm_unwind_ff(cframe_raw(cf));
    else
      lj_vm_unwind_c(cframe_raw(cf), errcode);
  }
#endif
  exit(EXIT_FAILURE);
}

/* Return string object for error message. */
LJ_NOINLINE GCstr *lj_err_str(lua_State *L, ErrMsg em)
{
  return lj_str_newz(L, err2msg(em));
}

/* Out-of-memory error. */
LJ_NOINLINE void lj_err_mem(lua_State *L)
{
  if (L->status == LUA_ERRERR+1)  /* Don't touch the stack during lua_open. */
    lj_vm_unwind_c(L->cframe, LUA_ERRMEM);
  L->top = L->base;
  setstrV(L, L->top++, lj_err_str(L, LJ_ERR_ERRMEM));
  lj_err_throw(L, LUA_ERRMEM);
}

/* Find error function for runtime errors. Requires an extra stack traversal. */
static ptrdiff_t finderrfunc(lua_State *L)
{
  cTValue *frame = L->base-1, *bot = tvref(L->stack);
  void *cf = L->cframe;
  while (frame > bot) {
    lua_assert(cf != NULL);
    while (cframe_nres(cframe_raw(cf)) < 0) {  /* cframe without frame? */
      if (frame >= restorestack(L, -cframe_nres(cf)))
	break;
      if (cframe_errfunc(cf) >= 0)  /* Error handler not inherited (-1)? */
	return cframe_errfunc(cf);
      cf = cframe_prev(cf);  /* Else unwind cframe and continue searching. */
      if (cf == NULL)
	return 0;
    }
    switch (frame_typep(frame)) {
    case FRAME_LUA:
    case FRAME_LUAP:
      frame = frame_prevl(frame);
      break;
    case FRAME_C:
      cf = cframe_prev(cf);
      /* fallthrough */
    case FRAME_CONT:
    case FRAME_VARG:
      frame = frame_prevd(frame);
      break;
    case FRAME_CP:
      if (cframe_canyield(cf)) return 0;
      if (cframe_errfunc(cf) >= 0)
	return cframe_errfunc(cf);
      frame = frame_prevd(frame);
      break;
    case FRAME_PCALL:
    case FRAME_PCALLH:
      if (frame_ftsz(frame) >= (ptrdiff_t)(2*sizeof(TValue)))  /* xpcall? */
	return savestack(L, frame-1);  /* Point to xpcall's errorfunc. */
      return 0;
    default:
      lua_assert(0);
      return 0;
    }
  }
  return 0;
}

/* Runtime error. */
LJ_NOINLINE void lj_err_run(lua_State *L)
{
  ptrdiff_t ef = finderrfunc(L);
  if (ef) {
    TValue *errfunc = restorestack(L, ef);
    TValue *top = L->top;
    lj_trace_abort(G(L));
    if (!tvisfunc(errfunc) || L->status == LUA_ERRERR) {
      setstrV(L, top-1, lj_err_str(L, LJ_ERR_ERRERR));
      lj_err_throw(L, LUA_ERRERR);
    }
    L->status = LUA_ERRERR;
    copyTV(L, top, top-1);
    copyTV(L, top-1, errfunc);
    L->top = top+1;
    lj_vm_call(L, top, 1+1);  /* Stack: |errfunc|msg| -> |msg| */
  }
  lj_err_throw(L, LUA_ERRRUN);
}

/* Add location to error message. */
LJ_NOINLINE static void err_loc(lua_State *L, const char *msg,
				cTValue *frame, cTValue *nextframe)
{
  if (frame) {
    GCfunc *fn = frame_func(frame);
    if (isluafunc(fn)) {
      char buff[LUA_IDSIZE];
      BCLine line = currentline(L, fn, nextframe);
      if (line >= 0) {
	err_chunkid(buff, strdata(proto_chunkname(funcproto(fn))));
	lj_str_pushf(L, "%s:%d: %s", buff, line, msg);
	return;
      }
    }
  }
  lj_str_pushf(L, "%s", msg);
}

/* Formatted runtime error message. */
LJ_NORET LJ_NOINLINE static void err_msgv(lua_State *L, ErrMsg em, ...)
{
  const char *msg;
  va_list argp;
  va_start(argp, em);
  if (curr_funcisL(L)) L->top = curr_topL(L);
  msg = lj_str_pushvf(L, err2msg(em), argp);
  va_end(argp);
  err_loc(L, msg, L->base-1, NULL);
  lj_err_run(L);
}

/* Non-vararg variant for better calling conventions. */
LJ_NOINLINE void lj_err_msg(lua_State *L, ErrMsg em)
{
  err_msgv(L, em);
}

/* Lexer error. */
LJ_NOINLINE void lj_err_lex(lua_State *L, const char *src, const char *tok,
			    BCLine line, ErrMsg em, va_list argp)
{
  char buff[LUA_IDSIZE];
  const char *msg;
  err_chunkid(buff, src);
  msg = lj_str_pushvf(L, err2msg(em), argp);
  msg = lj_str_pushf(L, "%s:%d: %s", buff, line, msg);
  if (tok)
    lj_str_pushf(L, err2msg(LJ_ERR_XNEAR), msg, tok);
  lj_err_throw(L, LUA_ERRSYNTAX);
}

/* Typecheck error for operands. */
LJ_NOINLINE void lj_err_optype(lua_State *L, cTValue *o, ErrMsg opm)
{
  const char *tname = typename(o);
  const char *oname = NULL;
  const char *opname = err2msg(opm);
  if (curr_funcisL(L)) {
    GCproto *pt = curr_proto(L);
    const BCIns *pc = cframe_Lpc(L) - 1;
    const char *kind = getobjname(pt, pc, (BCReg)(o - L->base), &oname);
    if (kind)
      err_msgv(L, LJ_ERR_BADOPRT, opname, kind, oname, tname);
  }
  err_msgv(L, LJ_ERR_BADOPRV, opname, tname);
}

/* Typecheck error for ordered comparisons. */
LJ_NOINLINE void lj_err_comp(lua_State *L, cTValue *o1, cTValue *o2)
{
  const char *t1 = typename(o1);
  const char *t2 = typename(o2);
  err_msgv(L, t1 == t2 ? LJ_ERR_BADCMPV : LJ_ERR_BADCMPT, t1, t2);
  /* This assumes the two "boolean" entries are commoned by the C compiler. */
}

/* Typecheck error for __call. */
LJ_NOINLINE void lj_err_optype_call(lua_State *L, TValue *o)
{
  /* Gross hack if lua_[p]call or pcall/xpcall fail for a non-callable object:
  ** L->base still points to the caller. So add a dummy frame with L instead
  ** of a function. See lua_getstack().
  */
  const BCIns *pc = cframe_Lpc(L);
  if (((ptrdiff_t)pc & FRAME_TYPE) != FRAME_LUA) {
    const char *tname = typename(o);
    setframe_pc(o, pc);
    setframe_gc(o, obj2gco(L));
    L->top = L->base = o+1;
    err_msgv(L, LJ_ERR_BADCALL, tname);
  }
  lj_err_optype(L, o, LJ_ERR_OPCALL);
}

/* Error in context of caller. */
LJ_NOINLINE void lj_err_callermsg(lua_State *L, const char *msg)
{
  TValue *frame = L->base-1;
  TValue *pframe = NULL;
  if (frame_islua(frame)) {
    pframe = frame_prevl(frame);
  } else if (frame_iscont(frame)) {
    pframe = frame_prevd(frame);
#if LJ_HASFFI
    /* Remove frame for FFI metamethods. */
    if (frame_func(frame)->c.ffid >= FF_ffi_meta___index &&
	frame_func(frame)->c.ffid <= FF_ffi_meta___tostring) {
      L->base = pframe+1;
      L->top = frame;
    }
#endif
  }
  err_loc(L, msg, pframe, frame);
  lj_err_run(L);
}

/* Formatted error in context of caller. */
LJ_NOINLINE void lj_err_callerv(lua_State *L, ErrMsg em, ...)
{
  const char *msg;
  va_list argp;
  va_start(argp, em);
  msg = lj_str_pushvf(L, err2msg(em), argp);
  va_end(argp);
  lj_err_callermsg(L, msg);
}

/* Error in context of caller. */
LJ_NOINLINE void lj_err_caller(lua_State *L, ErrMsg em)
{
  lj_err_callermsg(L, err2msg(em));
}

/* Argument error message. */
LJ_NORET LJ_NOINLINE static void err_argmsg(lua_State *L, int narg,
					    const char *msg)
{
  const char *fname = "?";
  const char *ftype = getfuncname(L, L->base - 1, &fname);
  if (narg < 0 && narg > LUA_REGISTRYINDEX)
    narg = (int)(L->top - L->base) + narg + 1;
  if (ftype && ftype[3] == 'h' && --narg == 0)  /* Check for "method". */
    msg = lj_str_pushf(L, err2msg(LJ_ERR_BADSELF), fname, msg);
  else
    msg = lj_str_pushf(L, err2msg(LJ_ERR_BADARG), narg, fname, msg);
  lj_err_callermsg(L, msg);
}

/* Formatted argument error. */
LJ_NOINLINE void lj_err_argv(lua_State *L, int narg, ErrMsg em, ...)
{
  const char *msg;
  va_list argp;
  va_start(argp, em);
  msg = lj_str_pushvf(L, err2msg(em), argp);
  va_end(argp);
  err_argmsg(L, narg, msg);
}

/* Argument error. */
LJ_NOINLINE void lj_err_arg(lua_State *L, int narg, ErrMsg em)
{
  err_argmsg(L, narg, err2msg(em));
}

/* Typecheck error for arguments. */
LJ_NOINLINE void lj_err_argtype(lua_State *L, int narg, const char *xname)
{
  TValue *o = L->base + narg-1;
  const char *tname = o < L->top ? typename(o) : lj_obj_typename[0];
  const char *msg = lj_str_pushf(L, err2msg(LJ_ERR_BADTYPE), xname, tname);
  err_argmsg(L, narg, msg);
}

/* Typecheck error for arguments. */
LJ_NOINLINE void lj_err_argt(lua_State *L, int narg, int tt)
{
  lj_err_argtype(L, narg, lj_obj_typename[tt+1]);
}

/* -- Public error handling API ------------------------------------------- */

LUA_API lua_CFunction lua_atpanic(lua_State *L, lua_CFunction panicf)
{
  lua_CFunction old = G(L)->panic;
  G(L)->panic = panicf;
  return old;
}

/* Forwarders for the public API (C calling convention and no LJ_NORET). */
LUA_API int lua_error(lua_State *L)
{
  lj_err_run(L);
  return 0;  /* unreachable */
}

LUALIB_API int luaL_argerror(lua_State *L, int narg, const char *msg)
{
  err_argmsg(L, narg, msg);
  return 0;  /* unreachable */
}

LUALIB_API int luaL_typerror(lua_State *L, int narg, const char *xname)
{
  lj_err_argtype(L, narg, xname);
  return 0;  /* unreachable */
}

LUALIB_API void luaL_where(lua_State *L, int level)
{
  int size;
  cTValue *frame = lj_err_getframe(L, level, &size);
  err_loc(L, "", frame, size ? frame+size : NULL);
}

LUALIB_API int luaL_error(lua_State *L, const char *fmt, ...)
{
  const char *msg;
  va_list argp;
  va_start(argp, fmt);
  msg = lj_str_pushvf(L, fmt, argp);
  va_end(argp);
  lj_err_callermsg(L, msg);
  return 0;  /* unreachable */
}

