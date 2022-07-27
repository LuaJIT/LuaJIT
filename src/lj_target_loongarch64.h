/*
** Definitions for LoongArch CPUs.
** Copyright (C) 2005-2022 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_TARGET_LOONGARCH_H
#define _LJ_TARGET_LOONGARCH_H

/* -- Registers IDs ------------------------------------------------------- */

#define GPRDEF(_) \
  _(R0) _(RA) _(R2) _(SP) _(R4) _(R5) _(R6) _(R7) \
  _(R8) _(R9) _(R10) _(R11) _(R12) _(R13) _(R14) _(R15) \
  _(R16) _(R17) _(R18) _(R19) _(R20) _(X) _(R22) _(R23) \
  _(R24) _(R25) _(R26) _(R27) _(R28) _(R29) _(R30) _(R31)
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
  RID_ZERO = RID_R0,
  RID_TMP = RID_RA,

  /* Calling conventions. */
  RID_RET = RID_R4,

  RID_RETHI = RID_R5,
  RID_RETLO = RID_R4,

  RID_FPRET = RID_F0,

  /* These definitions must match with the *.dasc file(s): */
  RID_BASE = RID_R23,		/* Interpreter BASE. */
  RID_LPC = RID_R25,		/* Interpreter PC. */
  RID_DISPATCH = RID_R26,	/* Interpreter DISPATCH table. */
  RID_LREG = RID_R27,		/* Interpreter L. */
  RID_JGL = RID_R22,		/* On-trace: global_State + 32768. */

  /* Register ranges [min, max) and number of registers. */
  RID_MIN_GPR = RID_R0,
  RID_MAX_GPR = RID_R31+1,
  RID_MIN_FPR = RID_MAX_GPR,
  RID_MAX_FPR = RID_F31+1,
  RID_NUM_GPR = RID_MAX_GPR - RID_MIN_GPR,
  RID_NUM_FPR = RID_MAX_FPR - RID_MIN_FPR
};

#define RID_NUM_KREF		RID_NUM_GPR
#define RID_MIN_KREF		RID_R0

/* -- Register sets ------------------------------------------------------- */

/* Make use of all registers, except ZERO, TMP, R2, SP, JGL, R20 and X. */
#define RSET_FIXED \
  (RID2RSET(RID_ZERO)|RID2RSET(RID_TMP)|RID2RSET(RID_R2)|\
   RID2RSET(RID_SP)|RID2RSET(RID_JGL)|RID2RSET(RID_R20)|\
   RID2RSET(RID_X))
#define RSET_GPR	(RSET_RANGE(RID_MIN_GPR, RID_MAX_GPR) - RSET_FIXED)
#define RSET_FPR	RSET_RANGE(RID_MIN_FPR, RID_MAX_FPR)
#define RSET_ALL	(RSET_GPR|RSET_FPR)
#define RSET_INIT	RSET_ALL

/* scratch register. */
#define RSET_SCRATCH_GPR	RSET_RANGE(RID_R4, RID_R19+1)
#define RSET_SCRATCH_FPR	RSET_RANGE(RID_F0, RID_F23+1)
#define RSET_SCRATCH		(RSET_SCRATCH_GPR|RSET_SCRATCH_FPR)
#define REGARG_FIRSTGPR		RID_R4
#define REGARG_LASTGPR		RID_R11
#define REGARG_NUMGPR		8
#define REGARG_FIRSTFPR		RID_F0
#define REGARG_LASTFPR		RID_F7
#define REGARG_NUMFPR		8

/* -- Spill slots --------------------------------------------------------- */

/* Spill slots are 32 bit wide. An even/odd pair is used for FPRs.
**
** SPS_FIXED: Available fixed spill slots in interpreter frame.
** This definition must match with the *.dasc file(s).
**
** SPS_FIRST: First spill slot for general use.
*/
#define SPS_FIXED	4
#define SPS_FIRST	4

#define SPOFS_TMP	0

#define sps_scale(slot)		(4 * (int32_t)(slot))
#define sps_align(slot)		(((slot) - SPS_FIXED + 3) & ~3)

/* -- Exit state ---------------------------------------------------------- */

/* This definition must match with the *.dasc file(s). */
typedef struct {
  lua_Number fpr[RID_NUM_FPR];	/* Floating-point registers. */
  intptr_t gpr[RID_NUM_GPR];	/* General-purpose registers. */
  int32_t spill[256];		/* Spill slots. */
} ExitState;

/* Highest exit + 1 indicates stack check. */
#define EXITSTATE_CHECKEXIT	1

/* Return the address of a per-trace exit stub. */
static LJ_AINLINE uint32_t *exitstub_trace_addr_(uint32_t *p)
{
  while (*p == 0x03400000) p++;		/* Skip LOONGI_NOP. */
  return p;
}
/* Avoid dependence on lj_jit.h if only including lj_target.h. */
#define exitstub_trace_addr(T, exitno) \
  exitstub_trace_addr_((MCode *)((char *)(T)->mcode + (T)->szmcode))

/* -- Instructions -------------------------------------------------------- */

/* Instruction fields. */
#define LOONGF_D(r)	(r)
#define LOONGF_J(r)	((r) << 5)
#define LOONGF_K(r)	((r) << 10)
#define LOONGF_A(r)	((r) << 15)
#define LOONGF_I(n)	((n) << 10)
#define LOONGF_I20(n)	((n) << 5)
#define LOONGF_M(n)	((n) << 16)

/* Check for valid field range. */
#define LOONGF_S_OK(x, b) ((((x) + (1 << (b-1))) >> (b)) == 0)

typedef enum LOONGIns {
/* Integer instructions. */
  LOONGI_MOVE = 0x00150000,
  LOONGI_NOP = 0x03400000,

  LOONGI_AND = 0x00148000,
  LOONGI_ANDI = 0x03400000,
  LOONGI_OR = 0x00150000,
  LOONGI_ORI = 0x03800000,
  LOONGI_XOR = 0x00158000,
  LOONGI_XORI = 0x03c00000,
  LOONGI_NOR = 0x00140000,

  LOONGI_SLT = 0x00120000,
  LOONGI_SLTU = 0x00128000,
  LOONGI_SLTI = 0x02000000,
  LOONGI_SLTUI = 0x02400000,

  LOONGI_ADD_W = 0x00100000,
  LOONGI_ADDI_W = 0x02800000,
  LOONGI_SUB_W = 0x00110000,
  LOONGI_MUL_W = 0x001c0000,
  LOONGI_MULH_W = 0x001c8000,
  LOONGI_DIV_W = 0x00200000,
  LOONGI_DIV_WU = 0x00210000,

  LOONGI_SLLI_W = 0x00408000,
  LOONGI_SRLI_W = 0x00448000,
  LOONGI_SRAI_W = 0x00488000,
  LOONGI_ROTRI_W = 0x004c8000,
  LOONGI_ROTRI_D = 0x004d0000,
  LOONGI_SLL_W = 0x00170000,
  LOONGI_SRL_W = 0x00178000,
  LOONGI_SRA_W = 0x00180000,
  LOONGI_ROTR_W = 0x001b0000,
  LOONGI_ROTR_D = 0x001b8000,

  LOONGI_EXT_W_B = 0x00005c00,
  LOONGI_EXT_W_H = 0x00005800,
  LOONGI_REVB_2H = 0x00003000,
  LOONGI_REVB_4H = 0x00003400,

  LOONGI_ALSL_W = 0x00040000,
  LOONGI_ALSL_D = 0x002c0000,

  LOONGI_B = 0x50000000,
  LOONGI_BL = 0x54000000,
  LOONGI_JIRL = 0x4c000000,

  LOONGI_BEQ = 0x58000000,
  LOONGI_BNE = 0x5c000000,
  LOONGI_BLT = 0x60000000,
  LOONGI_BGE = 0x64000000,
  LOONGI_BGEU = 0x6c000000,
  LOONGI_BLTU = 0x68000000,
  LOONGI_BCEQZ = 0x48000000,
  LOONGI_BCNEZ = 0x48000100,

  /* Load/store instructions. */
  LOONGI_LD_W = 0x28800000,
  LOONGI_LD_D = 0x28c00000,
  LOONGI_ST_W = 0x29800000,
  LOONGI_ST_D = 0x29c00000,
  LOONGI_LD_B = 0x28000000,
  LOONGI_ST_B = 0x29000000,
  LOONGI_LD_H = 0x28400000,
  LOONGI_ST_H = 0x29400000,
  LOONGI_LD_BU = 0x2a000000,
  LOONGI_LD_HU = 0x2a400000,
  LOONGI_LDX_B = 0x38000000,
  LOONGI_LDX_BU = 0x38200000,
  LOONGI_LDX_H = 0x38040000,
  LOONGI_LDX_HU = 0x38240000,
  LOONGI_LDX_D = 0x380c0000,
  LOONGI_STX_D = 0x381c0000,
  LOONGI_LDX_W = 0x38080000,
  LOONGI_STX_W = 0x38180000,
  LOONGI_STX_B = 0x38100000, 
  LOONGI_STX_H = 0x38140000,
  LOONGI_FLD_S = 0x2b000000,
  LOONGI_FST_S = 0x2b400000,
  LOONGI_FLD_D = 0x2b800000,
  LOONGI_FST_D = 0x2bc00000,
  LOONGI_FLDX_D = 0x38340000,
  LOONGI_FLDX_S = 0x38300000,
  LOONGI_FSTX_D = 0x383c0000,
  LOONGI_FSTX_S = 0x38380000,

  LOONGI_ADD_D = 0x00108000,
  LOONGI_ADDI_D = 0x02c00000,
  LOONGI_ADDU16I_D = 0x10000000,
  LOONGI_LU12I_W = 0x14000000,
  LOONGI_LU32I_D = 0x16000000,
  LOONGI_LU52I_D = 0x3000000,
  LOONGI_SUB_D = 0x00118000,
  LOONGI_DIV_D = 0x00220000,
  LOONGI_DIV_DU = 0x00230000,
  LOONGI_MUL_D = 0x001d8000,

  LOONGI_SLLI_D = 0x00410000,
  LOONGI_SRLI_D = 0x00450000,
  LOONGI_SLL_D = 0x00188000,
  LOONGI_SRL_D = 0x00190000,
  LOONGI_SRAI_D = 0x00490000,
  LOONGI_SRA_D = 0x00198000,
  LOONGI_REVH_D = 0x00004400,

  /* Extract/insert instructions. */
  LOONGI_BSTRPICK_D = 0x00c00000,
  LOONGI_BSTRINS_D = 0x00800000,

  LOONGI_MASKEQZ = 0x00130000,
  LOONGI_MASKNEZ = 0x00138000,

  /* FP instructions. */
  LOONGI_FRINT_S = 0x011e4400,
  LOONGI_FRINT_D = 0x011e4800,
  LOONGI_FTINTRM_L_D = 0x011a2800,
  LOONGI_FTINTRP_L_D = 0x011a6800,
  LOONGI_FTINTRNE_L_D = 0x011ae800,

  LOONGI_FMOV_S = 0x01149400,
  LOONGI_FMOV_D = 0x01149800,

  LOONGI_FABS_D = 0x01140800,
  LOONGI_FNEG_D = 0x01141800,

  LOONGI_FADD_D = 0x01010000,
  LOONGI_FSUB_D = 0x01030000,
  LOONGI_FMUL_D = 0x01050000,
  LOONGI_FDIV_D = 0x01070000,
  LOONGI_FSQRT_D = 0x01144800,

  LOONGI_FMIN_D = 0x010b0000,
  LOONGI_FMAX_D = 0x01090000,

  LOONGI_FADD_S = 0x01008000,
  LOONGI_FSUB_S = 0x01028000,

  LOONGI_FMADD_S = 0x08100000,
  LOONGI_FMADD_D = 0x08200000,
  LOONGI_FNMADD_D = 0x08a00000,
  LOONGI_FMSUB_S = 0x08500000,
  LOONGI_FMSUB_D = 0x08600000,
  LOONGI_FNMSUB_D = 0x08e00000,

  LOONGI_FCVT_D_S = 0x01192400,
  LOONGI_FTINT_W_S = 0x011b0400,
  LOONGI_FCVT_S_D = 0x01191800,
  LOONGI_FTINT_W_D = 0x011b0800,
  LOONGI_FFINT_S_W = 0x011d1000,
  LOONGI_FFINT_D_W = 0x011d2000,
  LOONGI_FFINT_S_L = 0x011d1800,
  LOONGI_FFINT_D_L = 0x011d2800,

  LOONGI_FTINTRZ_W_S = 0x011a8400,
  LOONGI_FTINTRZ_W_D = 0x011a8800,
  LOONGI_FTINTRZ_L_S = 0x011aa400,
  LOONGI_FTINTRZ_L_D = 0x011aa800,
  LOONGI_FTINTRM_W_S = 0x011a0400,
  LOONGI_FTINTRM_W_D = 0x011a0800,

  LOONGI_MOVFR2GR_S = 0x0114b400,
  LOONGI_MOVGR2FR_W = 0x0114a400,
  LOONGI_MOVGR2FR_D = 0x0114a800,
  LOONGI_MOVFR2GR_D = 0x0114b800,

  LOONGI_FCMP_CEQ_D = 0x0c220000,
  LOONGI_FCMP_CLT_S = 0x0c110000,
  LOONGI_FCMP_CLT_D = 0x0c210000,
  LOONGI_FCMP_CLE_D = 0x0c230000,
  LOONGI_FCMP_CULE_D = 0x0c270000,
  LOONGI_FCMP_CULT_D = 0x0c250000,
  LOONGI_FCMP_CNE_D = 0x0c280000,
  LOONGI_FSEL = 0x0d000000,
} LOONGIns;

#endif

