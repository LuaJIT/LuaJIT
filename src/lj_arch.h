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
#define LUAJIT_ARCH_PPC		3
#define LUAJIT_ARCH_ppc		3
#define LUAJIT_ARCH_PPCSPE	4
#define LUAJIT_ARCH_ppcspe	4


/* Select native target if no target defined. */
#ifndef LUAJIT_TARGET

#if defined(__i386) || defined(__i386__) || defined(_M_IX86)
#define LUAJIT_TARGET	LUAJIT_ARCH_X86
#elif defined(__x86_64__) || defined(__x86_64) || defined(_M_X64) || defined(_M_AMD64)
#define LUAJIT_TARGET	LUAJIT_ARCH_X64
#elif defined(__ppc__) || defined(__ppc) || defined(__PPC__) || defined(__PPC) || defined(__powerpc__) || defined(__powerpc) || defined(__POWERPC__) || defined(__POWERPC) || defined(_M_PPC)
#ifdef __NO_FPRS__
#define LUAJIT_TARGET	LUAJIT_ARCH_PPCSPE
#else
#define LUAJIT_TARGET	LUAJIT_ARCH_PPC
#endif
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
#define LJ_TARGET_MASKSHIFT	1
#define LJ_TARGET_MASKROT	1

#elif LUAJIT_TARGET == LUAJIT_ARCH_X64

#define LJ_ARCH_NAME		"x64"
#define LJ_ARCH_BITS		64
#define LJ_ARCH_ENDIAN		LUAJIT_LE
#define LJ_TARGET_X64		1
#define LJ_TARGET_X86ORX64	1
#define LJ_PAGESIZE		4096
#define LJ_TARGET_MASKSHIFT	1
#define LJ_TARGET_MASKROT	1

#elif LUAJIT_TARGET == LUAJIT_ARCH_PPC

#error "No support for plain PowerPC CPUs (yet)"

#elif LUAJIT_TARGET == LUAJIT_ARCH_PPCSPE

#define LJ_ARCH_NAME		"ppcspe"
#define LJ_ARCH_BITS		32
#define LJ_ARCH_ENDIAN		LUAJIT_BE
#define LJ_TARGET_PPC		1
#define LJ_TARGET_PPCSPE	1
#define LJ_PAGESIZE		4096
#define LJ_TARGET_MASKSHIFT	0
#define LJ_TARGET_MASKROT	1
#define LJ_ARCH_NOJIT		1

#else
#error "No target architecture defined"
#endif

/* Check target-specific constraints. */
#ifndef _BUILDVM_H
#if LJ_TARGET_PPC
#if defined(_SOFT_FLOAT) || defined(_SOFT_DOUBLE)
#error "No support for PowerPC CPUs without double-precision FPU"
#endif
#if defined(_LITTLE_ENDIAN)
#error "No support for little-endian PowerPC"
#endif
#if defined(_LP64)
#error "No support for PowerPC 64 bit mode"
#endif
#endif
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
#else
#define LJ_32			0
#define LJ_64			1
#endif

#endif
