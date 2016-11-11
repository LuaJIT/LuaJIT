/*
** Definitions for S390 CPUs.
** Copyright (C) 2005-2016 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_TARGET_S390_H
#define _LJ_TARGET_S390_H

/* -- Registers IDs ------------------------------------------------------- */

#define GPRDEF(_) \
  _(R0) _(R1) _(R2) _(R3) _(R4) _(R5) _(R6) _(R7) \
  _(R8) _(R9) _(R10) _(R11) _(R12) _(R13) _(R14) _R(15) \
#if LJ_SOFTFP
#define FPRDEF(_)
#else
#define FPRDEF(_) \
  _(F0) _(F1) _(F2) _(F3) \
  _(F4) _(F5) _(F6) _(F7) \
  _(F8) _(F9) _(F10) _(F11) \
  _(F12) _(F13) _(F14) _(F15) 
#endif
#define VRIDDEF(_)

#define RIDENUM(name)	RID_##name,

enum {
  GPRDEF(RIDENUM)		/* General-purpose registers (GPRs). */
  FPRDEF(RIDENUM)		/* Floating-point registers (FPRs). */
  RID_MAX,
  RID_TMP = RID_LR,

  /* Calling conventions. */
  RID_RET = RID_R0,
  RID_RETLO = RID_R0,
  RID_RETHI = RID_R1,
#if LJ_SOFTFP
  RID_FPRET = RID_R0,
#else
  RID_FPRET = RID_D0,
#endif

  /* These definitions must match with the *.dasc file(s): */
  RID_BASE = RID_R9,		/* Interpreter BASE. */
  RID_LPC = RID_R6,		/* Interpreter PC. */
  RID_DISPATCH = RID_R7,	/* Interpreter DISPATCH table. */
  RID_LREG = RID_R8,		/* Interpreter L. */

  /* Register ranges [min, max) and number of registers. */
  RID_MIN_GPR = RID_R0,
  RID_MAX_GPR = RID_PC+1,
  RID_MIN_FPR = RID_MAX_GPR,
#if LJ_SOFTFP
  RID_MAX_FPR = RID_MIN_FPR,
#else
  RID_MAX_FPR = RID_D15+1,
#endif
  RID_NUM_GPR = RID_MAX_GPR - RID_MIN_GPR,
  RID_NUM_FPR = RID_MAX_FPR - RID_MIN_FPR
};

#define RID_NUM_KREF		RID_NUM_GPR
#define RID_MIN_KREF		RID_R0

/* -- Register sets ------------------------------------------------------- */

/* Make use of all registers, except sp, lr and pc. */
#define RSET_GPR		(RSET_RANGE(RID_MIN_GPR, RID_R12+1))
#define RSET_GPREVEN \
  (RID2RSET(RID_R0)|RID2RSET(RID_R2)|RID2RSET(RID_R4)|RID2RSET(RID_R6)| \
   RID2RSET(RID_R8)|RID2RSET(RID_R10))
#define RSET_GPRODD \
  (RID2RSET(RID_R1)|RID2RSET(RID_R3)|RID2RSET(RID_R5)|RID2RSET(RID_R7)| \
   RID2RSET(RID_R9)|RID2RSET(RID_R11))
#if LJ_SOFTFP
#define RSET_FPR		0
#else
#define RSET_FPR		(RSET_RANGE(RID_MIN_FPR, RID_MAX_FPR))
#endif
#define RSET_ALL		(RSET_GPR|RSET_FPR)
#define RSET_INIT		RSET_ALL

/* ABI-specific register sets. lr is an implicit scratch register. */
#define RSET_SCRATCH_GPR_	(RSET_RANGE(RID_R0, RID_R3+1)|RID2RSET(RID_R12))
#ifdef __APPLE__
#define RSET_SCRATCH_GPR	(RSET_SCRATCH_GPR_|RID2RSET(RID_R9))
#else
#define RSET_SCRATCH_GPR	RSET_SCRATCH_GPR_
#endif
#if LJ_SOFTFP
#define RSET_SCRATCH_FPR	0
#else
#define RSET_SCRATCH_FPR	(RSET_RANGE(RID_D0, RID_D7+1))
#endif
#define RSET_SCRATCH		(RSET_SCRATCH_GPR|RSET_SCRATCH_FPR)
#define REGARG_FIRSTGPR		RID_R0
#define REGARG_LASTGPR		RID_R3
#define REGARG_NUMGPR		4
#if LJ_ABI_SOFTFP
#define REGARG_FIRSTFPR		0
#define REGARG_LASTFPR		0
#define REGARG_NUMFPR		0
#else
#define REGARG_FIRSTFPR		RID_D0
#define REGARG_LASTFPR		RID_D7
#define REGARG_NUMFPR		8
#endif

/* -- Spill slots --------------------------------------------------------- */

/* Spill slots are 32 bit wide. An even/odd pair is used for FPRs.
**
** SPS_FIXED: Available fixed spill slots in interpreter frame.
** This definition must match with the *.dasc file(s).
**
** SPS_FIRST: First spill slot for general use. Reserve min. two 32 bit slots.
*/
#define SPS_FIXED	2
#define SPS_FIRST	2

#define SPOFS_TMP	0

#define sps_scale(slot)		(4 * (int32_t)(slot))
#define sps_align(slot)		(((slot) - SPS_FIXED + 1) & ~1)

/* -- Exit state ---------------------------------------------------------- */

/* This definition must match with the *.dasc file(s). */
typedef struct {
#if !LJ_SOFTFP
  lua_Number fpr[RID_NUM_FPR];	/* Floating-point registers. */
#endif
  int32_t gpr[RID_NUM_GPR];	/* General-purpose registers. */
  int32_t spill[256];		/* Spill slots. */
} ExitState;

/* PC after instruction that caused an exit. Used to find the trace number. */
#define EXITSTATE_PCREG		RID_PC
/* Highest exit + 1 indicates stack check. */
#define EXITSTATE_CHECKEXIT	1

#define EXITSTUB_SPACING        4
#define EXITSTUBS_PER_GROUP     32

/* -- Instructions -------------------------------------------------------- */

/* Instruction fields. */
#define ARMF_CC(ai, cc)	(((ai) ^ ARMI_CCAL) | ((cc) << 28))
#define ARMF_N(r)	((r) << 16)
#define ARMF_D(r)	((r) << 12)
#define ARMF_S(r)	((r) << 8)
#define ARMF_M(r)	(r)
#define ARMF_SH(sh, n)	(((sh) << 5) | ((n) << 7))
#define ARMF_RSH(sh, r)	(0x10 | ((sh) << 5) | ARMF_S(r))

typedef enum S390xIns {
  S390I_SR = 0x1B000000,
  S390I_AR = 0x1A000000,
  S390I_NR = 0x14000000,
  S390I_XR = 0x17000000,
  S390I_MR = 0x1C000000,
  S390I_LR = 0x18000000,
  S390I_C = 0x59000000,
  S390I_LH = 0x48000000,
  S390I_BASR = 0x0D000000,
  S390I_MVCL = 0x0e000000,
  S390I_ST = 0x50000000,
  S390I_TM = 0x91000000,
  S390I_MP = 0xbd000090,
  S390I_CLR = 0x15000000,
} S390xIns;

typedef enum S390xShift {
  S390SH_SLL, S390SH_SRL, S390SH_SRA
} S390xShift;

/* ARM condition codes. */
typedef enum S390xCC {
  
} S390xCC;

#endif
