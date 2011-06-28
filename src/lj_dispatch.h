/*
** Instruction dispatch handling.
** Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
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

/* Hotcount decrements. */
#define HOTCOUNT_LOOP		2
#define HOTCOUNT_CALL		1

/* This solves a circular dependency problem -- bump as needed. Sigh. */
#define GG_NUM_ASMFF	62

#define GG_LEN_DDISP	(BC__MAX + GG_NUM_ASMFF)
#define GG_LEN_SDISP	BC_FUNCF
#define GG_LEN_DISP	(GG_LEN_DDISP + GG_LEN_SDISP)

/* Global state, main thread and extra fields are allocated together. */
typedef struct GG_State {
  lua_State L;				/* Main thread. */
  global_State g;			/* Global state. */
#if LJ_HASJIT
  jit_State J;				/* JIT state. */
  HotCount hotcount[HOTCOUNT_SIZE];	/* Hot counters. */
#endif
  ASMFunction dispatch[GG_LEN_DISP];	/* Instruction dispatch tables. */
  BCIns bcff[GG_NUM_ASMFF];		/* Bytecode for ASM fast functions. */
} GG_State;

#define GG_OFS(field)	((int)offsetof(GG_State, field))
#define G2GG(gl)	((GG_State *)((char *)(gl) - GG_OFS(g)))
#define J2GG(j)		((GG_State *)((char *)(j) - GG_OFS(J)))
#define L2GG(L)		(G2GG(G(L)))
#define J2G(J)		(&J2GG(J)->g)
#define G2J(gl)		(&G2GG(gl)->J)
#define L2J(L)		(&L2GG(L)->J)
#define GG_G2DISP	(GG_OFS(dispatch) - GG_OFS(g))
#define GG_DISP2G	(GG_OFS(g) - GG_OFS(dispatch))
#define GG_DISP2J	(GG_OFS(J) - GG_OFS(dispatch))
#define GG_DISP2HOT	(GG_OFS(hotcount) - GG_OFS(dispatch))
#define GG_DISP2STATIC	(GG_LEN_DDISP*(int)sizeof(ASMFunction))

#define hotcount_get(gg, pc) \
  (gg)->hotcount[(u32ptr(pc)>>2) & (HOTCOUNT_SIZE-1)]
#define hotcount_set(gg, pc, val) \
  (hotcount_get((gg), (pc)) = (HotCount)(val))

/* Dispatch table management. */
LJ_FUNC void lj_dispatch_init(GG_State *GG);
#if LJ_HASJIT
LJ_FUNC void lj_dispatch_init_hotcount(global_State *g);
#endif
LJ_FUNC void lj_dispatch_update(global_State *g);

/* Instruction dispatch callback for hooks or when recording. */
LJ_FUNCA void LJ_FASTCALL lj_dispatch_ins(lua_State *L, const BCIns *pc);
LJ_FUNCA ASMFunction LJ_FASTCALL lj_dispatch_call(lua_State *L, const BCIns*pc);
LJ_FUNCA void LJ_FASTCALL lj_dispatch_return(lua_State *L, const BCIns *pc);

#if LJ_HASFFI && !defined(_BUILDVM_H)
/* Save/restore errno and GetLastError() around hooks, exits and recording. */
#include <errno.h>
#if LJ_TARGET_WINDOWS
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#define ERRNO_SAVE	int olderr = errno; DWORD oldwerr = GetLastError();
#define ERRNO_RESTORE	errno = olderr; SetLastError(oldwerr);
#else
#define ERRNO_SAVE	int olderr = errno;
#define ERRNO_RESTORE	errno = olderr;
#endif
#else
#define ERRNO_SAVE
#define ERRNO_RESTORE
#endif

#endif
