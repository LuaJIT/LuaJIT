/*
** IR CALL* instruction definitions.
** Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_IRCALL_H
#define _LJ_IRCALL_H

#include "lj_obj.h"
#include "lj_ir.h"
#include "lj_jit.h"

/* C call info for CALL* instructions. */
typedef struct CCallInfo {
  ASMFunction func;		/* Function pointer. */
  uint32_t flags;		/* Number of arguments and flags. */
} CCallInfo;

#define CCI_NARGS(ci)		((ci)->flags & 0xff)	/* Extract # of args. */
#define CCI_NARGS_MAX		32			/* Max. # of args. */

#define CCI_OTSHIFT		16
#define CCI_OPTYPE(ci)		((ci)->flags >> CCI_OTSHIFT)  /* Get op/type. */
#define CCI_OPSHIFT		24
#define CCI_OP(ci)		((ci)->flags >> CCI_OPSHIFT)  /* Get op. */

#define CCI_CALL_N		(IR_CALLN << CCI_OPSHIFT)
#define CCI_CALL_L		(IR_CALLL << CCI_OPSHIFT)
#define CCI_CALL_S		(IR_CALLS << CCI_OPSHIFT)
#define CCI_CALL_FN		(CCI_CALL_N|CCI_FASTCALL)
#define CCI_CALL_FL		(CCI_CALL_L|CCI_FASTCALL)
#define CCI_CALL_FS		(CCI_CALL_S|CCI_FASTCALL)

/* C call info flags. */
#define CCI_L			0x0100	/* Implicit L arg. */
#define CCI_CASTU64		0x0200	/* Cast u64 result to number. */
#define CCI_NOFPRCLOBBER	0x0400	/* Does not clobber any FPRs. */
#define CCI_FASTCALL		0x0800	/* Fastcall convention. */

/* Function definitions for CALL* instructions. */
#if LJ_HASFFI
#if LJ_32
#define ARG2_64		4	/* Treat as 4 32 bit arguments. */
#define IRCALLDEF_FFI32(_) \
  _(lj_carith_mul64,	ARG2_64,   N, I64, CCI_NOFPRCLOBBER)
#else
#define ARG2_64		2
#define IRCALLDEF_FFI32(_)
#endif
#define IRCALLDEF_FFI(_) \
  IRCALLDEF_FFI32(_) \
  _(lj_carith_divi64,	ARG2_64,   N, I64, CCI_NOFPRCLOBBER) \
  _(lj_carith_divu64,	ARG2_64,   N, U64, CCI_NOFPRCLOBBER) \
  _(lj_carith_modi64,	ARG2_64,   N, I64, CCI_NOFPRCLOBBER) \
  _(lj_carith_modu64,	ARG2_64,   N, U64, CCI_NOFPRCLOBBER) \
  _(lj_carith_powi64,	ARG2_64,   N, I64, CCI_NOFPRCLOBBER) \
  _(lj_carith_powu64,	ARG2_64,   N, U64, CCI_NOFPRCLOBBER) \
  _(lj_cdata_setfin,	2,        FN, P32, CCI_L) \
  _(strlen,		1,         N, INTP, 0) \
  _(memcpy,		3,         S, PTR, 0) \
  _(memset,		3,         S, PTR, 0)
#else
#define IRCALLDEF_FFI(_)
#endif
#define IRCALLDEF(_) \
  _(lj_str_cmp,		2,  FN, INT, CCI_NOFPRCLOBBER) \
  _(lj_str_new,		3,   S, STR, CCI_L) \
  _(lj_str_tonum,	2,  FN, INT, 0) \
  _(lj_str_fromint,	2,  FN, STR, CCI_L) \
  _(lj_str_fromnum,	2,  FN, STR, CCI_L) \
  _(lj_tab_new1,	2,  FS, TAB, CCI_L) \
  _(lj_tab_dup,		2,  FS, TAB, CCI_L) \
  _(lj_tab_newkey,	3,   S, P32, CCI_L) \
  _(lj_tab_len,		1,  FL, INT, 0) \
  _(lj_gc_step_jit,	2,  FS, NIL, CCI_L) \
  _(lj_gc_barrieruv,	2,  FS, NIL, 0) \
  _(lj_mem_newgco,	2,  FS, P32, CCI_L) \
  _(lj_math_random_step, 1, FS, NUM, CCI_CASTU64|CCI_NOFPRCLOBBER) \
  IRCALLDEF_FFI(_) \
  _(sinh,		1,  N, NUM, 0) \
  _(cosh,		1,  N, NUM, 0) \
  _(tanh,		1,  N, NUM, 0) \
  _(fputc,		2,  S, INT, 0) \
  _(fwrite,		4,  S, INT, 0) \
  _(fflush,		1,  S, INT, 0) \
  \
  /* End of list. */

typedef enum {
#define IRCALLENUM(name, nargs, kind, type, flags)	IRCALL_##name,
IRCALLDEF(IRCALLENUM)
#undef IRCALLENUM
  IRCALL__MAX
} IRCallID;

LJ_FUNC TRef lj_ir_call(jit_State *J, IRCallID id, ...);

LJ_DATA const CCallInfo lj_ir_callinfo[IRCALL__MAX+1];

#endif
