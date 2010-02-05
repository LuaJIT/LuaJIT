/*
** Instruction dispatch handling.
** Copyright (C) 2005-2010 Mike Pall. See Copyright Notice in luajit.h
*/

#define lj_dispatch_c
#define LUA_CORE

#include "lj_obj.h"
#include "lj_err.h"
#include "lj_state.h"
#include "lj_frame.h"
#include "lj_bc.h"
#if LJ_HASJIT
#include "lj_jit.h"
#endif
#include "lj_trace.h"
#include "lj_dispatch.h"
#include "lj_vm.h"
#include "luajit.h"

/* -- Dispatch table management ------------------------------------------- */

/* Initialize instruction dispatch table and hot counters. */
void lj_dispatch_init(GG_State *GG)
{
  uint32_t i;
  ASMFunction *disp = GG->dispatch;
  for (i = 0; i < BC__MAX; i++)
    disp[GG_DISP_STATIC+i] = disp[i] = makeasmfunc(lj_bc_ofs[i]);
  /* The JIT engine is off by default. luaopen_jit() turns it on. */
  disp[BC_FORL] = disp[BC_IFORL];
  disp[BC_ITERL] = disp[BC_IITERL];
  disp[BC_LOOP] = disp[BC_ILOOP];
}

#if LJ_HASJIT
/* Initialize hotcount table. */
void lj_dispatch_init_hotcount(global_State *g)
{
  HotCount start = (HotCount)G2J(g)->param[JIT_P_hotloop];
  HotCount *hotcount = G2GG(g)->hotcount;
  uint32_t i;
  for (i = 0; i < HOTCOUNT_SIZE; i++)
    hotcount[i] = start;
}
#endif

/* Update dispatch table depending on various flags. */
void lj_dispatch_update(global_State *g)
{
  uint8_t oldmode = g->dispatchmode;
  uint8_t mode = 0;
#if LJ_HASJIT
  mode |= (G2J(g)->flags & JIT_F_ON) ? 1 : 0;
  mode |= G2J(g)->state != LJ_TRACE_IDLE ? 6 : 0;
#endif
  mode |= (g->hookmask & HOOK_EVENTMASK) ? 2 : 0;
  if (oldmode != mode) {  /* Mode changed? */
    ASMFunction *disp = G2GG(g)->dispatch;
    ASMFunction f_forl, f_iterl, f_loop;
    g->dispatchmode = mode;
    if ((mode & 5) == 1) {  /* Hotcount if JIT is on, but not when recording. */
      f_forl = makeasmfunc(lj_bc_ofs[BC_FORL]);
      f_iterl = makeasmfunc(lj_bc_ofs[BC_ITERL]);
      f_loop = makeasmfunc(lj_bc_ofs[BC_LOOP]);
    } else {  /* Otherwise use the non-hotcounting instructions. */
      f_forl = disp[GG_DISP_STATIC+BC_IFORL];
      f_iterl = disp[GG_DISP_STATIC+BC_IITERL];
      f_loop = disp[GG_DISP_STATIC+BC_ILOOP];
    }
    /* Set static loop ins first (may be copied below). */
    disp[GG_DISP_STATIC+BC_FORL] = f_forl;
    disp[GG_DISP_STATIC+BC_ITERL] = f_iterl;
    disp[GG_DISP_STATIC+BC_LOOP] = f_loop;
    if ((oldmode & 6) != (mode & 6)) {  /* Need to change whole table? */
      if ((mode & 6) == 0) {  /* No hooks and no recording? */
	/* Copy static dispatch table to dynamic dispatch table. */
	memcpy(&disp[0], &disp[GG_DISP_STATIC], sizeof(ASMFunction)*BC__MAX);
      } else {
	/* The recording dispatch also checks for hooks. */
	ASMFunction f = (mode & 6) == 6 ? lj_vm_record : lj_vm_hook;
	uint32_t i;
	for (i = 0; i < BC__MAX; i++)
	  disp[i] = f;
      }
    } else if ((mode & 6) == 0) {  /* Fix dynamic loop ins unless overriden. */
      disp[BC_FORL] = f_forl;
      disp[BC_ITERL] = f_iterl;
      disp[BC_LOOP] = f_loop;
    }
#if LJ_HASJIT
    if ((mode & 1) && !(oldmode & 1))  /* JIT off to on transition. */
      lj_dispatch_init_hotcount(g);
#endif
  }
}

/* -- JIT mode setting ---------------------------------------------------- */

#if LJ_HASJIT
/* Set JIT mode for a single prototype. */
static void setptmode(global_State *g, GCproto *pt, int mode)
{
  if ((mode & LUAJIT_MODE_ON)) {  /* (Re-)enable JIT compilation. */
    pt->flags &= ~PROTO_NO_JIT;
    lj_trace_reenableproto(pt);  /* Unpatch all ILOOP etc. bytecodes. */
  } else {  /* Flush and/or disable JIT compilation. */
    if (!(mode & LUAJIT_MODE_FLUSH))
      pt->flags |= PROTO_NO_JIT;
    lj_trace_flushproto(g, pt);  /* Flush all traces of prototype. */
  }
}

/* Recursively set the JIT mode for all children of a prototype. */
static void setptmode_all(global_State *g, GCproto *pt, int mode)
{
  ptrdiff_t i;
  for (i = -(ptrdiff_t)pt->sizekgc; i < 0; i++) {
    GCobj *o = proto_kgc(pt, i);
    if (o->gch.gct == ~LJ_TPROTO) {
      setptmode(g, gco2pt(o), mode);
      setptmode_all(g, gco2pt(o), mode);
    }
  }
}
#endif

/* Public API function: control the JIT engine. */
int luaJIT_setmode(lua_State *L, int idx, int mode)
{
  global_State *g = G(L);
  int mm = mode & LUAJIT_MODE_MASK;
  lj_trace_abort(g);  /* Abort recording on any state change. */
  /* Avoid pulling the rug from under our own feet. */
  if ((g->hookmask & HOOK_GC))
    lj_err_caller(L, LJ_ERR_NOGCMM);
  switch (mm) {
#if LJ_HASJIT
  case LUAJIT_MODE_ENGINE:
    if ((mode & LUAJIT_MODE_FLUSH)) {
      lj_trace_flushall(L);
    } else {
      if ((mode & LUAJIT_MODE_ON))
	G2J(g)->flags |= (uint32_t)JIT_F_ON;
      else
	G2J(g)->flags &= ~(uint32_t)JIT_F_ON;
      lj_dispatch_update(g);
    }
    break;
  case LUAJIT_MODE_FUNC:
  case LUAJIT_MODE_ALLFUNC:
  case LUAJIT_MODE_ALLSUBFUNC: {
    cTValue *tv = idx == 0 ? frame_prev(L->base-1) :
		  idx > 0 ? L->base + (idx-1) : L->top + idx;
    GCproto *pt;
    if ((idx == 0 || tvisfunc(tv)) && isluafunc(&gcval(tv)->fn))
      pt = funcproto(&gcval(tv)->fn);  /* Cannot use funcV() for frame slot. */
    else if (tvisproto(tv))
      pt = protoV(tv);
    else
      return 0;  /* Failed. */
    if (mm != LUAJIT_MODE_ALLSUBFUNC)
      setptmode(g, pt, mode);
    if (mm != LUAJIT_MODE_FUNC)
      setptmode_all(g, pt, mode);
    break;
    }
  case LUAJIT_MODE_TRACE:
    if (!(mode & LUAJIT_MODE_FLUSH))
      return 0;  /* Failed. */
    return lj_trace_flush(G2J(g), idx);
#else
  case LUAJIT_MODE_ENGINE:
  case LUAJIT_MODE_FUNC:
  case LUAJIT_MODE_ALLFUNC:
  case LUAJIT_MODE_ALLSUBFUNC:
    UNUSED(idx);
    if ((mode & LUAJIT_MODE_ON))
      return 0;  /* Failed. */
    break;
#endif
  case LUAJIT_MODE_WRAPCFUNC:
    if ((mode & LUAJIT_MODE_ON)) {
      if (idx != 0) {
	cTValue *tv = idx > 0 ? L->base + (idx-1) : L->top + idx;
	if (tvislightud(tv) && lightudV(tv) != NULL)
	  g->wrapf = (lua_CFunction)lightudV(tv);
	else
	  return 0;  /* Failed. */
      }
      g->wrapmode = 1;
    } else {
      g->wrapmode = 0;
    }
    break;
  default:
    return 0;  /* Failed. */
  }
  return 1;  /* OK. */
}

/* Enforce (dynamic) linker error for version mismatches. See luajit.c. */
LUA_API void LUAJIT_VERSION_SYM(void)
{
}

/* -- Hooks --------------------------------------------------------------- */

/* This function can be called asynchronously (e.g. during a signal). */
LUA_API int lua_sethook(lua_State *L, lua_Hook func, int mask, int count)
{
  global_State *g = G(L);
  mask &= HOOK_EVENTMASK;
  if (func == NULL || mask == 0) { mask = 0; func = NULL; }  /* Consistency. */
  g->hookf = func;
  g->hookcount = g->hookcstart = (int32_t)count;
  g->hookmask = (uint8_t)((g->hookmask & ~HOOK_EVENTMASK) | mask);
  lj_trace_abort(g);  /* Abort recording on any hook change. */
  lj_dispatch_update(g);
  return 1;
}

LUA_API lua_Hook lua_gethook(lua_State *L)
{
  return G(L)->hookf;
}

LUA_API int lua_gethookmask(lua_State *L)
{
  return G(L)->hookmask & HOOK_EVENTMASK;
}

LUA_API int lua_gethookcount(lua_State *L)
{
  return (int)G(L)->hookcstart;
}

/* Call a hook. */
static void callhook(lua_State *L, int event, BCLine line)
{
  global_State *g = G(L);
  lua_Hook hookf = g->hookf;
  if (hookf && !hook_active(g)) {
    lua_Debug ar;
    lj_trace_abort(g);  /* Abort recording on any hook call. */
    ar.event = event;
    ar.currentline = line;
    ar.i_ci = cast_int((L->base-1) - L->stack); /* Top frame, nextframe=NULL. */
    lj_state_checkstack(L, 1+LUA_MINSTACK);
    hook_enter(g);
    hookf(L, &ar);
    lua_assert(hook_active(g));
    hook_leave(g);
  }
}

/* -- Instruction dispatch callbacks -------------------------------------- */

/* Calculate number of used stack slots in the current frame. */
static BCReg cur_topslot(GCproto *pt, const BCIns *pc, uint32_t nres)
{
  BCIns ins = pc[-1];
  for (;;) {
    switch (bc_op(ins)) {
    case BC_UCLO: ins = pc[bc_j(ins)]; break;
    case BC_CALLM:
    case BC_CALLMT: return bc_a(ins) + bc_c(ins) + nres-1+1;
    case BC_RETM: return bc_a(ins) + bc_d(ins) + nres-1;
    case BC_TSETM: return bc_a(ins) + nres-1;
    default: return pt->framesize;
    }
  }
}

/* Instruction dispatch callback for instr/line hooks or when recording. */
void LJ_FASTCALL lj_dispatch_ins(lua_State *L, const BCIns *pc)
{
  GCfunc *fn = curr_func(L);
  GCproto *pt = funcproto(fn);
  void *cf = cframe_raw(L->cframe);
  const BCIns *oldpc = cframe_pc(cf);
  global_State *g = G(L);
  BCReg slots;
  setcframe_pc(cf, pc);
  slots = cur_topslot(pt, pc, cframe_multres(cf));
  L->top = L->base + slots;  /* Fix top. */
#if LJ_HASJIT
  {
    jit_State *J = G2J(g);
    if (J->state != LJ_TRACE_IDLE) {
      J->L = L;
      J->pc = pc-1;
      J->fn = fn;
      J->pt = pt;
      lj_trace_ins(J);
    }
  }
#endif
  if ((g->hookmask & LUA_MASKCOUNT) && g->hookcount == 0) {
    g->hookcount = g->hookcstart;
    callhook(L, LUA_HOOKCOUNT, -1);
  }
  if ((g->hookmask & LUA_MASKLINE) && proto_lineinfo(pt)) {
    BCPos npc = proto_bcpos(pt, pc) - 1;
    BCPos opc = proto_bcpos(pt, oldpc) - 1;
    BCLine line = proto_line(pt, npc);
    if (npc == 0 || pc <= oldpc ||
	opc >= pt->sizebc || line != proto_line(pt, opc)) {
      L->top = L->base + slots;  /* Fix top again after instruction hook. */
      callhook(L, LUA_HOOKLINE, line);
    }
  }
}

