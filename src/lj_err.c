/*
** Error handling and debugging API.
** Copyright (C) 2005-2009 Mike Pall. See Copyright Notice in luajit.h
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
#include "lj_trace.h"
#include "lj_vm.h"

/* -- Error messages ------------------------------------------------------ */

/* Error message strings. */
static const char *lj_err_allmsg =
#define ERRDEF(name, msg)	msg "\0"
#include "lj_errmsg.h"
;

#define err2msg(em)	(lj_err_allmsg+(int)(em))

/* -- Frame and function introspection ------------------------------------ */

static BCPos currentpc(lua_State *L, GCfunc *fn, cTValue *nextframe)
{
  const BCIns *ins;
  lua_assert(fn->c.gct == ~LJ_TFUNC || fn->c.gct == ~LJ_TTHREAD);
  if (!isluafunc(fn)) {  /* Cannot derive a PC for non-Lua functions. */
    return ~(BCPos)0;
  } else if (nextframe == NULL) {  /* Lua function on top. */
    ins = cframe_Lpc(L);  /* Only happens during error/hook handling. */
  } else {
    if (frame_islua(nextframe)) {
      ins = frame_pc(nextframe);
    } else if (frame_iscont(nextframe)) {
      ins = frame_contpc(nextframe);
    } else {
      /* Lua function below errfunc/gc/hook: find cframe to get the PC. */
      void *cf = cframe_raw(L->cframe);
      TValue *f = L->base-1;
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
  return (BCPos)((ins - funcproto(fn)->bc) - 1);
}

static BCLine currentline(lua_State *L, GCfunc *fn, cTValue *nextframe)
{
  BCPos pc = currentpc(L, fn, nextframe);
  if (pc != ~(BCPos)0) {
    GCproto *pt = funcproto(fn);
    lua_assert(pc < pt->sizebc);
    return pt->lineinfo ? pt->lineinfo[pc] : 0;
  } else {
    return -1;
  }
}

static const char *getvarname(const GCproto *pt, BCPos pc, BCReg slot)
{
  MSize i;
  for (i = 0; i < pt->sizevarinfo && pt->varinfo[i].startpc <= pc; i++)
    if (pc < pt->varinfo[i].endpc && slot-- == 0)
      return strdata(pt->varinfo[i].name);
  return NULL;
}

static const char *getobjname(GCproto *pt, const BCIns *ip, BCReg slot,
			      const char **name)
{
  const char *lname;
restart:
  lname = getvarname(pt, (BCPos)(ip - pt->bc), slot);
  if (lname != NULL) { *name = lname; return "local"; }
  while (--ip >= pt->bc) {
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
	*name = strdata(gco2str(gcref(pt->k.gc[~bc_d(ins)])));
	return "global";
      case BC_TGETS:
	*name = strdata(gco2str(gcref(pt->k.gc[~bc_c(ins)])));
	if (ip > pt->bc) {
	  BCIns insp = ip[-1];
	  if (bc_op(insp) == BC_MOV && bc_a(insp) == ra+1 &&
	      bc_d(insp) == bc_b(ins))
	    return "method";
	}
	return "field";
      case BC_UGET:
	*name = pt->uvname ? strdata(pt->uvname[bc_d(ins)]) : "?";
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
  ip = &funcproto(fn)->bc[pc];
  mm = bcmode_mm(bc_op(*ip));
  if (mm == MM_call) {
    BCReg slot = bc_a(*ip);
    if (bc_op(*ip) == BC_ITERC) slot -= 3;
    return getobjname(funcproto(fn), ip, slot, name);
  } else if (mm != MM_MAX) {
    *name = strdata(strref(G(L)->mmname[mm]));
    return "metamethod";
  } else {
    return NULL;
  }
}

void lj_err_pushloc(lua_State *L, GCproto *pt, BCPos pc)
{
  GCstr *name = pt->chunkname;
  if (name) {
    const char *s = strdata(name);
    MSize i, len = name->len;
    BCLine line;
    if (pc)
      line = pt->lineinfo ? pt->lineinfo[pc-1] : 0;
    else
      line = pt->linedefined;
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
  TValue *frame = L->stack + offset;
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
    frame = L->stack + offset;
    if (size) nextframe = frame + size;
    lua_assert(frame<=L->maxstack && (!nextframe || nextframe<=L->maxstack));
    fn = frame_func(frame);
    lua_assert(fn->c.gct == ~LJ_TFUNC);
  }
  for (; *what; what++) {
    switch (*what) {
    case 'S':
      if (isluafunc(fn)) {
	ar->source = strdata(funcproto(fn)->chunkname);
	ar->linedefined = cast_int(funcproto(fn)->linedefined);
	ar->lastlinedefined = cast_int(funcproto(fn)->lastlinedefined);
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
	BCLine *lineinfo = funcproto(fn)->lineinfo;
	uint32_t i, szl = funcproto(fn)->sizelineinfo;
	for (i = 0; i < szl; i++)
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
  cTValue *frame, *nextframe;
  /* Traverse frames backwards. */
  for (nextframe = frame = L->base-1; frame > L->stack; ) {
    if (frame_gc(frame) == obj2gco(L))
      level++;  /* Skip dummy frames. See lj_meta_call(). */
    if (level-- == 0) {
      *size = cast_int(nextframe - frame);
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
    ar->i_ci = (size << 16) + cast_int(frame - L->stack);
    return 1;
  } else {
    ar->i_ci = level - size;
    return 0;
  }
}

/* -- Error handling ------------------------------------------------------ */

/* Return string object for error message. */
LJ_NOINLINE GCstr *lj_err_str(lua_State *L, ErrMsg em)
{
  return lj_str_newz(L, err2msg(em));
}

/* Unwind Lua stack and add error message on top. */
LJ_NOINLINE static void unwindstack(lua_State *L, TValue *top, int errcode)
{
  lj_func_closeuv(L, top);
  switch (errcode) {
  case LUA_ERRMEM:
    setstrV(L, top, lj_err_str(L, LJ_ERR_ERRMEM));
    break;
  case LUA_ERRERR:
    setstrV(L, top, lj_err_str(L, LJ_ERR_ERRERR));
    break;
  case LUA_ERRSYNTAX:
  case LUA_ERRRUN:
    copyTV(L, top, L->top - 1);
    break;
  default:
    lua_assert(0);
    break;
  }
  L->top = top+1;
  lj_state_relimitstack(L);
}

/* Throw error. Find catch frame, unwind stack and continue. */
LJ_NOINLINE void lj_err_throw(lua_State *L, int errcode)
{
  TValue *frame = L->base-1;
  void *cf = L->cframe;
  global_State *g = G(L);
  if (L->status == LUA_ERRERR+1) {  /* Don't touch the stack during lua_open. */
    lj_vm_unwind_c(cf, errcode);
    goto uncaught;  /* unreachable */
  }
  lj_trace_abort(g);
  setgcrefnull(g->jit_L);
  L->status = 0;
  while (cf) {
    if (cframe_nres(cframe_raw(cf)) < 0) {  /* cframe without frame? */
      TValue *top = restorestack(L, -cframe_nres(cf));
      if (frame < top) {
	L->cframe = cframe_prev(cf);
	L->base = frame+1;
	unwindstack(L, top, errcode);
	lj_vm_unwind_c(cf, errcode);
	goto uncaught;  /* unreachable */
      }
    }
    if (frame <= L->stack)
      break;
    switch (frame_typep(frame)) {
    case FRAME_LUA:
    case FRAME_LUAP:
      frame = frame_prevl(frame);
      break;
    case FRAME_C:
      if (cframe_canyield(cf)) goto uncaught;
      cf = cframe_prev(cf);
      /* fallthrough */
    case FRAME_CONT:
    case FRAME_VARG:
      frame = frame_prevd(frame);
      break;
    case FRAME_CP:
      L->cframe = cframe_prev(cf);
      L->base = frame_prevd(frame) + 1;
      unwindstack(L, frame, errcode);
      lj_vm_unwind_c(cf, errcode);
      goto uncaught;  /* unreachable */
    case FRAME_PCALL:
      hook_leave(g);
      /* fallthrough */
    case FRAME_PCALLH:
      L->cframe = cf;
      L->base = frame_prevd(frame) + 1;
      unwindstack(L, L->base, errcode);
      lj_vm_unwind_ff(cf);
      goto uncaught;  /* unreachable */
    default:
      lua_assert(0);
      goto uncaught;
    }
  }
  /* No catch frame found. Must be a resume or an unprotected error. */
uncaught:
  L->status = cast_byte(errcode);
  L->cframe = NULL;
  if (cframe_canyield(cf)) {  /* Resume? */
    unwindstack(L, L->top, errcode);
    lj_vm_unwind_c(cf, errcode);
  }
  /* Better rethrow on main thread than panic. */
  {
    if (L != mainthread(g))
      lj_err_throw(mainthread(g), errcode);
    if (g->panic) {
      L->base = L->stack+1;
      unwindstack(L, L->base, errcode);
      g->panic(L);
    }
  }
  exit(EXIT_FAILURE);
}

/* Find error function for runtime errors. Requires an extra stack traversal. */
static ptrdiff_t finderrfunc(lua_State *L)
{
  TValue *frame = L->base-1;
  void *cf = L->cframe;
  while (frame > L->stack) {
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
      if (cframe_canyield(cf)) return 0;
      cf = cframe_prev(cf);
      /* fallthrough */
    case FRAME_CONT:
    case FRAME_VARG:
      frame = frame_prevd(frame);
      break;
    case FRAME_CP:
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
    if (!tvisfunc(errfunc) || L->status == LUA_ERRERR)
      lj_err_throw(L, LUA_ERRERR);
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
      err_chunkid(buff, strdata(funcproto(fn)->chunkname));
      lj_str_pushf(L, "%s:%d: %s", buff, line, msg);
      return;
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
  cTValue *frame = L->base-1;
  cTValue *pframe = frame_islua(frame) ? frame_prevl(frame) : NULL;
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
    narg = (L->top - L->base) + narg + 1;
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

/* -- C++ exception support ----------------------------------------------- */

#if defined(__ELF__) || defined(__MACH__)
typedef enum
{
  _URC_NO_REASON,
  _URC_FOREIGN_EXCEPTION_CAUGHT,
  _URC_FATAL_PHASE2_ERROR,
  _URC_FATAL_PHASE1_ERROR,
  _URC_NORMAL_STOP,
  _URC_END_OF_STACK,
  _URC_HANDLER_FOUND,
  _URC_INSTALL_CONTEXT,
  _URC_CONTINUE_UNWIND
} _Unwind_Reason_Code;

#define _UA_SEARCH_PHASE	1
#define _UA_CLEANUP_PHASE	2
#define _UA_HANDLER_FRAME	4
#define _UA_FORCE_UNWIND	8
#define _UA_END_OF_STACK	16

extern void *_Unwind_GetCFA(void *ctx);
extern void _Unwind_DeleteException(void *uex);

/* DWARF2 personality handler referenced from .eh_frame. */
LJ_FUNCA int lj_err_unwind_dwarf(int version, int actions, uint64_t uexclass,
				 void *uex, void *ctx)
{
  if (version != 1)
    return _URC_FATAL_PHASE1_ERROR;
  UNUSED(uexclass);
  if ((actions & _UA_SEARCH_PHASE))
    return _URC_HANDLER_FOUND;
  if ((actions & _UA_HANDLER_FRAME)) {
    void *cf = _Unwind_GetCFA(ctx);
    lua_State *L = cframe_L(cf);
    _Unwind_DeleteException(uex);
    lj_err_msg(L, LJ_ERR_ERRCPP);
  }
  return _URC_CONTINUE_UNWIND;
}
#endif

