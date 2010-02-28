/*
** Target architecture selection.
** Copyright (C) 2005-2010 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_ARCH_H
#define _LJ_ARCH_H

#include "lua.h"


/* Target endianess. */
#define LUAJIT_LE	0
#define LUAJIT_BE	1

/* Target architectures. */
#define LUAJIT_ARCH_X86		1
#define LUAJIT_ARCH_x86		1
#define LUAJIT_ARCH_X64		2
#define LUAJIT_ARCH_x64		2


/* Select native target if no target defined. */
#ifndef LUAJIT_TARGET

#if defined(__i386) || defined(__i386__) || defined(_M_IX86)
#define LUAJIT_TARGET	LUAJIT_ARCH_X86
#elif defined(__x86_64__) || defined(__x86_64) || defined(_M_X64) || defined(_M_AMD64)
#define LUAJIT_TARGET	LUAJIT_ARCH_X64
#else
#error "No support for this architecture (yet)"
#endif

#endif

/* Set target properties. */
#if LUAJIT_TARGET == LUAJIT_ARCH_X86
#define LJ_ARCH_NAME		"x86"
#define LJ_ARCH_BITS		32
#define LJ_ARCH_ENDIAN		LUAJIT_LE
#define LJ_TARGET_X86		1
#define LJ_TARGET_X86ORX64	1
#define LJ_PAGESIZE		4096
#elif LUAJIT_TARGET == LUAJIT_ARCH_X64
#define LJ_ARCH_NAME		"x64"
#define LJ_ARCH_BITS		64
#define LJ_ARCH_ENDIAN		LUAJIT_LE
#define LJ_TARGET_X64		1
#define LJ_TARGET_X86ORX64	1
#define LJ_PAGESIZE		4096
#else
#error "No target architecture defined"
#endif

/* Disable or enable the JIT compiler. */
#if defined(LUAJIT_DISABLE_JIT) || defined(LJ_ARCH_NOJIT)
#define LJ_HASJIT		0
#else
#define LJ_HASJIT		1
#endif

#if LJ_ARCH_ENDIAN == LUAJIT_BE
#define LJ_ENDIAN_SELECT(le, be)	be
#define LJ_ENDIAN_LOHI(lo, hi)		hi lo
#else
#define LJ_ENDIAN_SELECT(le, be)	le
#define LJ_ENDIAN_LOHI(lo, hi)		lo hi
#endif

#if LJ_ARCH_BITS == 32
#define LJ_32			1
#define LJ_64			0
#elif LJ_ARCH_BITS == 64
#define LJ_32			0
#define LJ_64			1
#else
#error "Bad LJ_ARCH_BITS setting"
#endif

/* Whether target CPU masks the shift count by the operand length or not. */
#if LJ_TARGET_X86ORX64
#define LJ_TARGET_MASKEDSHIFT	1
#else
#define LJ_TARGET_MASKEDSHIFT	0
#endif

#endif
