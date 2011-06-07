/*
** Debugging and introspection.
** Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
*/

#define lj_debug_c
#define LUA_CORE

#include "lj_obj.h"
#include "lj_err.h"
#include "lj_str.h"
#include "lj_tab.h"
#include "lj_state.h"
#include "lj_frame.h"
#include "lj_bc.h"

/* -- Frames -------------------------------------------------------------- */

/* Get frame corresponding to a level. */
cTValue *lj_debug_frame(lua_State *L, int level, int *size)
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

/* Invalid bytecode position. */
#define NO_BCPOS	(~(BCPos)0)

/* Return bytecode position for function/frame or NO_BCPOS. */
static BCPos debug_framepc(lua_State *L, GCfunc *fn, cTValue *nextframe)
{
  const BCIns *ins;
  lua_assert(fn->c.gct == ~LJ_TFUNC || fn->c.gct == ~LJ_TTHREAD);
  if (!isluafunc(fn)) {  /* Cannot derive a PC for non-Lua functions. */
    return NO_BCPOS;
  } else if (nextframe == NULL) {  /* Lua function on top. */
    void *cf = cframe_raw(L->cframe);
    if (cf == NULL || (char *)cframe_pc(cf) == (char *)cframe_L(cf))
      return NO_BCPOS;
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
	return NO_BCPOS;
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

/* -- Line numbers -------------------------------------------------------- */

/* Get line number for a bytecode position. */
BCLine LJ_FASTCALL lj_debug_line(GCproto *pt, BCPos pc)
{
  BCLine *lineinfo = proto_lineinfo(pt);
  if (pc < pt->sizebc && lineinfo)
    return lineinfo[pc];
  return 0;
}

/* Get line number for function/frame. */
static BCLine debug_frameline(lua_State *L, GCfunc *fn, cTValue *nextframe)
{
  BCPos pc = debug_framepc(L, fn, nextframe);
  if (pc != NO_BCPOS) {
    GCproto *pt = funcproto(fn);
    lua_assert(pc < pt->sizebc);
    return lj_debug_line(pt, pc);
  }
  return -1;
}

/* -- Variable names ------------------------------------------------------ */

/* Get name of a local variable from slot number and PC. */
static const char *debug_varname(const GCproto *pt, BCPos pc, BCReg slot)
{
  MSize i;
  VarInfo *vi = proto_varinfo(pt);
  for (i = 0; i < pt->sizevarinfo && vi[i].startpc <= pc; i++)
    if (pc < vi[i].endpc && slot-- == 0)
      return strdata(gco2str(gcref(vi[i].name)));
  return NULL;
}

/* Get name of local variable from 1-based slot number and function/frame. */
static TValue *debug_localname(lua_State *L, const lua_Debug *ar,
			       const char **name, BCReg slot1)
{
  uint32_t offset = (uint32_t)ar->i_ci & 0xffff;
  uint32_t size = (uint32_t)ar->i_ci >> 16;
  TValue *frame = tvref(L->stack) + offset;
  TValue *nextframe = size ? frame + size : NULL;
  GCfunc *fn = frame_func(frame);
  BCPos pc = debug_framepc(L, fn, nextframe);
  if (pc != NO_BCPOS &&
      (*name = debug_varname(funcproto(fn), pc, slot1-1)) != NULL)
    ;
  else if (slot1 > 0 && frame + slot1 < (nextframe ? nextframe : L->top))
    *name = "(*temporary)";
  else
    *name = NULL;
  return frame+slot1;
}

/* Get name of upvalue. */
const char *lj_debug_uvname(cTValue *o, uint32_t idx, TValue **tvp)
{
  if (tvisfunc(o)) {
    GCfunc *fn = funcV(o);
    if (isluafunc(fn)) {
      GCproto *pt = funcproto(fn);
      if (idx < pt->sizeuv) {
	*tvp = uvval(&gcref(fn->l.uvptr[idx])->uv);
	return strdata(proto_uvname(pt, idx));
      }
    } else {
      if (idx < fn->c.nupvalues) {
	*tvp = &fn->c.upvalue[idx];
	return "";
      }
    }
  }
  return NULL;
}

/* Deduce name of an object from slot number and PC. */
const char *lj_debug_slotname(GCproto *pt, const BCIns *ip, BCReg slot,
			      const char **name)
{
  const char *lname;
restart:
  lname = debug_varname(pt, proto_bcpos(pt, ip), slot);
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

/* Deduce function name from caller of a frame. */
const char *lj_debug_funcname(lua_State *L, TValue *frame, const char **name)
{
  TValue *pframe;
  GCfunc *fn;
  BCPos pc;
  if (frame_isvarg(frame))
    frame = frame_prevd(frame);
  pframe = frame_prev(frame);
  fn = frame_func(pframe);
  pc = debug_framepc(L, fn, frame);
  if (pc != NO_BCPOS) {
    GCproto *pt = funcproto(fn);
    const BCIns *ip = &proto_bc(pt)[check_exp(pc < pt->sizebc, pc)];
    MMS mm = bcmode_mm(bc_op(*ip));
    if (mm == MM_call) {
      BCReg slot = bc_a(*ip);
      if (bc_op(*ip) == BC_ITERC) slot -= 3;
      return lj_debug_slotname(pt, ip, slot, name);
    } else if (mm != MM__MAX) {
      *name = strdata(mmname_str(G(L), mm));
      return "metamethod";
    }
  }
  return NULL;
}

/* -- Source code locations ----------------------------------------------- */

/* Generate shortened source name. */
void lj_debug_shortname(char *out, const char *src)
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

/* Add current location of a frame to error message. */
void lj_debug_addloc(lua_State *L, const char *msg,
		     cTValue *frame, cTValue *nextframe)
{
  if (frame) {
    GCfunc *fn = frame_func(frame);
    if (isluafunc(fn)) {
      BCLine line = debug_frameline(L, fn, nextframe);
      if (line >= 0) {
	char buf[LUA_IDSIZE];
	lj_debug_shortname(buf, strdata(proto_chunkname(funcproto(fn))));
	lj_str_pushf(L, "%s:%d: %s", buf, line, msg);
	return;
      }
    }
  }
  lj_str_pushf(L, "%s", msg);
}

/* Push location string for a bytecode position to Lua stack. */
void lj_debug_pushloc(lua_State *L, GCproto *pt, BCPos pc)
{
  GCstr *name = proto_chunkname(pt);
  if (name) {
    const char *s = strdata(name);
    MSize i, len = name->len;
    BCLine line = lj_debug_line(pt, pc);
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

/* -- Public debug API ---------------------------------------------------- */

/* lua_getupvalue() and lua_setupvalue() are in lj_api.c. */

LUA_API const char *lua_getlocal(lua_State *L, const lua_Debug *ar, int n)
{
  const char *name;
  TValue *o = debug_localname(L, ar, &name, (BCReg)n);
  if (name) {
    copyTV(L, L->top, o);
    incr_top(L);
  }
  return name;
}

LUA_API const char *lua_setlocal(lua_State *L, const lua_Debug *ar, int n)
{
  const char *name;
  TValue *o = debug_localname(L, ar, &name, (BCReg)n);
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
	ar->what = ar->linedefined ? "Lua" : "main";
      } else {
	ar->source = "=[C]";
	ar->linedefined = -1;
	ar->lastlinedefined = -1;
	ar->what = "C";
      }
      lj_debug_shortname(ar->short_src, ar->source);
      break;
    case 'l':
      ar->currentline = frame ? debug_frameline(L, fn, nextframe) : -1;
      break;
    case 'u':
      ar->nups = fn->c.nupvalues;
      break;
    case 'n':
      ar->namewhat = frame ? lj_debug_funcname(L, frame, &ar->name) : NULL;
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

LUA_API int lua_getstack(lua_State *L, int level, lua_Debug *ar)
{
  int size;
  cTValue *frame = lj_debug_frame(L, level, &size);
  if (frame) {
    ar->i_ci = (size << 16) + (int)(frame - tvref(L->stack));
    return 1;
  } else {
    ar->i_ci = level - size;
    return 0;
  }
}

