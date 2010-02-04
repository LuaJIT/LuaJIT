/*
** Instruction dispatch handling.
** Copyright (C) 2005-2010 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_DISPATCH_H
#define _LJ_DISPATCH_H

#include "lj_obj.h"
#include "lj_bc.h"
#if LJ_HASJIT
#include "lj_jit.h"
#endif

/* Type of hot counter. Must match the code in the assembler VM. */
/* 16 bits are sufficient. Only 0.0015% overhead with maximum slot penalty. */
typedef uint16_t HotCount;

/* Number of hot counter hash table entries (must be a power of two). */
#define HOTCOUNT_SIZE		64
#define HOTCOUNT_PCMASK		((HOTCOUNT_SIZE-1)*sizeof(HotCount))
#define HOTCOUNT_MIN_PENALTY	103
#define HOTCOUNT_MAX_PENALTY	60000

/* Global state, main thread and extra fields are allocated together. */
typedef struct GG_State {
  lua_State L;				/* Main thread. */
  global_State g;			/* Global state. */
#if LJ_HASJIT
  jit_State J;				/* JIT state. */
  HotCount hotcount[HOTCOUNT_SIZE];	/* Hot counters. */
#endif
  ASMFunction dispatch[2*BC__MAX];	/* Instruction dispatch tables. */
} GG_State;

#define GG_DISP_STATIC	BC__MAX

#define GG_OFS(field)	((int)offsetof(GG_State, field))
#define G2GG(gl) \
  ((GG_State *)(((char *)(gl))-((char *)(&((GG_State *)0)->g))))
#define J2GG(j) \
  ((GG_State *)(((char *)(j))-((char *)(&((GG_State *)0)->J))))
#define L2GG(L)		G2GG(G(L))
#define J2G(J)		(&J2GG(J)->g)
#define G2J(gl)		(&G2GG(gl)->J)
#define L2J(L)		(&L2GG(L)->J)
#define GG_G2DISP	(GG_OFS(dispatch) - GG_OFS(g))
#define GG_DISP2G	(GG_OFS(g) - GG_OFS(dispatch))
#define GG_DISP2J	(GG_OFS(J) - GG_OFS(dispatch))
#define GG_DISP2HOT	(GG_OFS(hotcount) - GG_OFS(dispatch))

#define hotcount_get(gg, pc) \
  (gg)->hotcount[(u32ptr(pc)>>2) & (HOTCOUNT_SIZE-1)]
#define hotcount_set(gg, pc, val) \
  (hotcount_get((gg), (pc)) = (HotCount)(val))

/* Dispatch table management. */
LJ_FUNC void lj_dispatch_init(GG_State *GG);
LJ_FUNC void lj_dispatch_init_hotcount(global_State *g);
LJ_FUNC void lj_dispatch_update(global_State *g);

/* Instruction dispatch callback for instr/line hooks or when recording. */
LJ_FUNCA void LJ_FASTCALL lj_dispatch_ins(lua_State *L, const BCIns *pc);

#endif
