/*
** Definitions for MIPS CPUs.
** Copyright (C) 2005-2012 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_TARGET_MIPS_H
#define _LJ_TARGET_MIPS_H

/* -- Registers IDs ------------------------------------------------------- */

#define GPRDEF(_) \
  _(R0) _(R1) _(R2) _(R3) _(R4) _(R5) _(R6) _(R7) \
  _(R8) _(R9) _(R10) _(R11) _(R12) _(R13) _(R14) _(R15) \
  _(R16) _(R17) _(R18) _(R19) _(R20) _(R21) _(R22) _(R23) \
  _(R24) _(R25) _(SYS1) _(SYS2) _(GP) _(SP) _(R30) _(RA)
#define FPRDEF(_) \
  _(F0) _(F1) _(F2) _(F3) _(F4) _(F5) _(F6) _(F7) \
  _(F8) _(F9) _(F10) _(F11) _(F12) _(F13) _(F14) _(F15) \
  _(F16) _(F17) _(F18) _(F19) _(F20) _(F21) _(F22) _(F23) \
  _(F24) _(F25) _(F26) _(F27) _(F28) _(F29) _(F30) _(F31)
#define VRIDDEF(_)

#define RIDENUM(name)	RID_##name,

enum {
  GPRDEF(RIDENUM)		/* General-purpose registers (GPRs). */
  FPRDEF(RIDENUM)		/* Floating-point registers (FPRs). */
  RID_MAX,
  RID_TMP = RID_RA,

  /* Calling conventions. */
  RID_RET = RID_R2,
#if LJ_LE
  RID_RETHI = RID_R3,
  RID_RETLO = RID_R2,
#else
  RID_RETHI = RID_R2,
  RID_RETLO = RID_R3,
#endif
  RID_FPRET = RID_F0,

  /* These definitions must match with the *.dasc file(s): */
  RID_BASE = RID_R16,		/* Interpreter BASE. */
  RID_LPC = RID_R18,		/* Interpreter PC. */
  RID_DISPATCH = RID_R19,	/* Interpreter DISPATCH table. */
  RID_LREG = RID_R20,		/* Interpreter L. */
  RID_JGL = RID_R30,		/* On-trace: global_State + 32768. */

  /* Register ranges [min, max) and number of registers. */
  RID_MIN_GPR = RID_R0,
  RID_MAX_GPR = RID_RA+1,
  RID_MIN_FPR = RID_F0,
  RID_MAX_FPR = RID_F31+1,
  RID_NUM_GPR = RID_MAX_GPR - RID_MIN_GPR,
  RID_NUM_FPR = (RID_MAX_FPR - RID_MIN_FPR)/2
};

#define RID_NUM_KREF		RID_NUM_GPR
#define RID_MIN_KREF		RID_R0

/* -- Register sets ------------------------------------------------------- */

/* Make use of all registers, except TMP, SP, SYS1, SYS2 and JGL. */
#define RSET_FIXED \
  (RID2RSET(RID_TMP)|RID2RSET(RID_SP)|RID2RSET(RID_SYS1)|\
   RID2RSET(RID_SYS2)|RID2RSET(RID_JGL))
#define RSET_GPR	(RSET_RANGE(RID_MIN_GPR, RID_MAX_GPR) - RSET_FIXED)
#define RSET_FPR \
  (RID2RSET(RID_F0)|RID2RSET(RID_F2)|RID2RSET(RID_F4)|RID2RSET(RID_F6)|\
   RID2RSET(RID_F8)|RID2RSET(RID_F10)|RID2RSET(RID_F12)|RID2RSET(RID_F14)|\
   RID2RSET(RID_F16)|RID2RSET(RID_F18)|RID2RSET(RID_F20)|RID2RSET(RID_F22)|\
   RID2RSET(RID_F24)|RID2RSET(RID_F26)|RID2RSET(RID_F28)|RID2RSET(RID_F30))
#define RSET_ALL	(RSET_GPR|RSET_FPR)
#define RSET_INIT	RSET_ALL

#define RSET_SCRATCH_GPR \
  (RSET_RANGE(RID_R1, RID_R15+1)|\
   RID2RSET(RID_R24)|RID2RSET(RID_R25)|RID2RSET(RID_GP)|RID2RSET(RID_RA))
#define RSET_SCRATCH_FPR \
  (RID2RSET(RID_F0)|RID2RSET(RID_F2)|RID2RSET(RID_F4)|RID2RSET(RID_F6)|\
   RID2RSET(RID_F8)|RID2RSET(RID_F10)|RID2RSET(RID_F12)|RID2RSET(RID_F14)|\
   RID2RSET(RID_F16)|RID2RSET(RID_F18))
#define RSET_SCRATCH		(RSET_SCRATCH_GPR|RSET_SCRATCH_FPR)
#define REGARG_FIRSTGPR		RID_R4
#define REGARG_LASTGPR		RID_R7
#define REGARG_NUMGPR		4
#define REGARG_FIRSTFPR		RID_F12
#define REGARG_LASTFPR		RID_F14
#define REGARG_NUMFPR		2

/* -- Spill slots --------------------------------------------------------- */

/* Spill slots are 32 bit wide. An even/odd pair is used for FPRs.
**
** SPS_FIXED: Available fixed spill slots in interpreter frame.
** This definition must match with the *.dasc file(s).
**
** SPS_FIRST: First spill slot for general use.
*/
#define SPS_FIXED	5
#define SPS_FIRST	4

#define sps_scale(slot)		(4 * (int32_t)(slot))
#define sps_align(slot)		(((slot) - SPS_FIXED + 1) & ~1)

/* -- Exit state ---------------------------------------------------------- */

/* This definition must match with the *.dasc file(s). */
typedef struct {
  lua_Number fpr[RID_NUM_FPR];	/* Floating-point registers. */
  int32_t gpr[RID_NUM_GPR];	/* General-purpose registers. */
  int32_t spill[256];		/* Spill slots. */
} ExitState;

/* Highest exit + 1 indicates stack check. */
#define EXITSTATE_CHECKEXIT	1

#define EXITSTUB_SPACING	8
#define EXITSTUBS_PER_GROUP	16

/* -- Instructions -------------------------------------------------------- */

/* Instruction fields. */
#define MIPSF_S(r)	((r) << 21)
#define MIPSF_T(r)	((r) << 16)
#define MIPSF_D(r)	((r) << 11)
#define MIPSF_R(r)	((r) << 21)
#define MIPSF_H(r)	((r) << 16)
#define MIPSF_G(r)	((r) << 11)
#define MIPSF_F(r)	((r) << 6)
#define MIPSF_A(n)	((n) << 6)
#define MIPSF_M(n)	((n) << 11)

typedef enum MIPSIns {
  /* Integer instructions. */
  MIPSI_MOVE = 0x00000021,
  MIPSI_NOP = 0x00000000,

  MIPSI_LI = 0x24000000,
  MIPSI_LU = 0x34000000,
  MIPSI_LUI = 0x3c000000,

  MIPSI_ORI = 0x34000000,

  MIPSI_B = 0x10000000,
  MIPSI_JR = 0x00000008,

  /* Load/store instructions. */
  MIPSI_LW = 0x8c000000,
  MIPSI_SW = 0xac000000,
} MIPSIns;

#endif
