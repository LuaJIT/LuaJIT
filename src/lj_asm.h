/*
** IR assembler (SSA IR -> machine code).
** Copyright (C) 2005-2010 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_ASM_H
#define _LJ_ASM_H

#include "lj_jit.h"

#if LJ_HASJIT
LJ_FUNC void lj_asm_trace(jit_State *J, Trace *T);
LJ_FUNC void lj_asm_patchexit(jit_State *J, Trace *T, ExitNo exitno,
			      MCode *target);
#endif

#endif
