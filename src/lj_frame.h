/*
** Stack frames.
** Copyright (C) 2005-2009 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_FRAME_H
#define _LJ_FRAME_H

#include "lj_obj.h"
#include "lj_bc.h"

/* -- Lua stack frame ----------------------------------------------------- */

/* Frame type markers in callee function slot (callee base-1). */
enum {
  FRAME_LUA, FRAME_C, FRAME_CONT, FRAME_VARG,
  FRAME_LUAP, FRAME_CP, FRAME_PCALL, FRAME_PCALLH
};
#define FRAME_TYPE		3
#define FRAME_P			4
#define FRAME_TYPEP		(FRAME_TYPE|FRAME_P)

/* Macros to access and modify Lua frames. */
#define frame_gc(f)		(gcref((f)->fr.func))
#define frame_func(f)		(&frame_gc(f)->fn)
#define frame_ftsz(f)		((f)->fr.tp.ftsz)

#define frame_type(f)		(frame_ftsz(f) & FRAME_TYPE)
#define frame_typep(f)		(frame_ftsz(f) & FRAME_TYPEP)
#define frame_islua(f)		(frame_type(f) == FRAME_LUA)
#define frame_isc(f)		(frame_type(f) == FRAME_C)
#define frame_iscont(f)		(frame_typep(f) == FRAME_CONT)
#define frame_isvarg(f)		(frame_typep(f) == FRAME_VARG)
#define frame_ispcall(f)	((frame_ftsz(f) & 6) == FRAME_PCALL)

#define frame_pc(f)		(mref((f)->fr.tp.pcr, const BCIns))
#define frame_contpc(f)		(frame_pc((f)-1))
#if LJ_64
#define frame_contf(f) \
  ((ASMFunction)(void *)((intptr_t)lj_vm_asm_begin+(((f)-1)->u64 & 0xffffffff)))
#else
#define frame_contf(f)		((ASMFunction)gcrefp(((f)-1)->gcr, void))
#endif
#define frame_delta(f)		(frame_ftsz(f) >> 3)
#define frame_sized(f)		(frame_ftsz(f) & ~FRAME_TYPEP)

#define frame_prevl(f)		((f) - (1+bc_a(frame_pc(f)[-1])))
#define frame_prevd(f)		((TValue *)((char *)(f) - frame_sized(f)))
#define frame_prev(f)		(frame_islua(f)?frame_prevl(f):frame_prevd(f))
/* Note: this macro does not skip over FRAME_VARG. */

#define setframe_pc(f, pc)	(setmref((f)->fr.tp.pcr, (pc)))
#define setframe_gc(f, p)	(setgcref((f)->fr.func, (p)))

/* -- C stack frame ------------------------------------------------------- */

/* Macros to access and modify the C stack frame chain. */

/* These definitions must match with the arch-specific *.dasc files. */
#if LJ_TARGET_X86
#define CFRAME_OFS_ERRF		(15*sizeof(void *))
#define CFRAME_OFS_NRES		(14*sizeof(void *))
#define CFRAME_OFS_PREV		(13*sizeof(void *))
#define CFRAME_OFS_L		(12*sizeof(void *))
#define CFRAME_OFS_PC		(6*sizeof(void *))
#define CFRAME_SIZE		(12*sizeof(void *))
#else
#error "Missing CFRAME_* definitions for this architecture"
#endif

#define CFRAME_RESUME		1
#define CFRAME_CANYIELD		((intptr_t)(CFRAME_RESUME))
#define CFRAME_RAWMASK		(~CFRAME_CANYIELD)

#define cframe_errfunc(cf)	(*(ptrdiff_t *)(((char *)cf)+CFRAME_OFS_ERRF))
#define cframe_nres(cf)		(*(ptrdiff_t *)(((char *)cf)+CFRAME_OFS_NRES))
#define cframe_prev(cf)		(*(void **)(((char *)cf)+CFRAME_OFS_PREV))
#define cframe_L(cf)		(*(lua_State **)(((char *)cf)+CFRAME_OFS_L))
#define cframe_pc(cf)		(*(const BCIns **)(((char *)cf)+CFRAME_OFS_PC))
#define cframe_canyield(cf)	((intptr_t)(cf) & CFRAME_CANYIELD)
#define cframe_raw(cf)		((void *)((intptr_t)(cf) & CFRAME_RAWMASK))
#define cframe_Lpc(L)		cframe_pc(cframe_raw(L->cframe))

#endif
