/*
** This file has been pre-processed with DynASM.
** http://luajit.org/dynasm.html
** DynASM version 1.2.1, DynASM ppc version 1.2.1
** DO NOT EDIT! The original file is in "buildvm_ppc.dasc".
*/

#if DASM_VERSION != 10201
#error "Version mismatch between DynASM and included encoding engine"
#endif

#define DASM_SECTION_CODE_OP	0
#define DASM_SECTION_CODE_SUB	1
#define DASM_MAXSECTION		2
static const unsigned int build_actionlist[462] = {
0x00010001,
0x00060014,
0x7c810808,
0x00060015,
0x7c810808,
0x00060016,
0x7c810808,
0x00060017,
0x7c810808,
0x00060018,
0x7c810808,
0x00060019,
0x7c810808,
0x0006001a,
0x7c810808,
0x0006001b,
0x7c810808,
0x0006001c,
0x7c810808,
0x0006001d,
0x7c810808,
0x0006001e,
0x7c810808,
0x0006001f,
0x7c810808,
0x00060020,
0x7c810808,
0x00060021,
0x7c810808,
0x00060022,
0x7c810808,
0x00060023,
0x7c810808,
0x00060024,
0x7c810808,
0x00060025,
0x7c810808,
0x00060026,
0x7c810808,
0x00060027,
0x7c810808,
0x00060028,
0x7c810808,
0x00060029,
0x00000000,
0x7c810808,
0x0006002a,
0x7c810808,
0x0006002b,
0x7c810808,
0x0006002c,
0x7c810808,
0x0006002d,
0x7c810808,
0x0006002e,
0x7c810808,
0x0006002f,
0x7c810808,
0x00060030,
0x7c810808,
0x00060031,
0x7c810808,
0x00060032,
0x7c810808,
0x00060033,
0x7c810808,
0x00060034,
0x7c810808,
0x00060035,
0x7c810808,
0x00060036,
0x7c810808,
0x00060037,
0x7c810808,
0x00060038,
0x7c810808,
0x00060039,
0x7c810808,
0x0006003a,
0x7c810808,
0x0006003b,
0x7c810808,
0x0006003c,
0x7c810808,
0x0006003d,
0x7c810808,
0x0006003e,
0x7c810808,
0x7c810808,
0x0006003f,
0x00000000,
0x7c810808,
0x7c810808,
0x00060040,
0x7c810808,
0x7c810808,
0x00060041,
0x7c810808,
0x00060042,
0x7c810808,
0x7c810808,
0x00060043,
0x7c810808,
0x7c810808,
0x00060044,
0x7c810808,
0x00060045,
0x7c810808,
0x7c810808,
0x00060046,
0x7c810808,
0x7c810808,
0x00060047,
0x7c810808,
0x00060048,
0x7c810808,
0x7c810808,
0x00060049,
0x7c810808,
0x7c810808,
0x0006004a,
0x7c810808,
0x7c810808,
0x0006004b,
0x7c810808,
0x7c810808,
0x0006004c,
0x7c810808,
0x0006004d,
0x7c810808,
0x0006004e,
0x7c810808,
0x7c810808,
0x7c810808,
0x0006004f,
0x7c810808,
0x00060050,
0x7c810808,
0x00060051,
0x7c810808,
0x00060052,
0x7c810808,
0x00060053,
0x7c810808,
0x00060054,
0x7c810808,
0x00060055,
0x00000000,
0x7c810808,
0x00060056,
0x7c810808,
0x00060057,
0x7c810808,
0x00060058,
0x7c810808,
0x00060059,
0x7c810808,
0x0006005a,
0x7c810808,
0x0006005b,
0x7c810808,
0x0006005c,
0x7c810808,
0x0006005d,
0x7c810808,
0x0006005e,
0x7c810808,
0x0006005f,
0x7c810808,
0x00060060,
0x7c810808,
0x00060061,
0x7c810808,
0x00060062,
0x00060063,
0x7c810808,
0x7c810808,
0x00060064,
0x7c810808,
0x7c810808,
0x7c810808,
0x00060065,
0x7c810808,
0x7c810808,
0x7c810808,
0x00060066,
0x7c810808,
0x7c810808,
0x7c810808,
0x00060067,
0x7c810808,
0x7c810808,
0x7c810808,
0x00060068,
0x7c810808,
0x7c810808,
0x00060069,
0x7c810808,
0x7c810808,
0x0006006a,
0x7c810808,
0x7c810808,
0x0006006b,
0x00000000,
0x7c810808,
0x0006006c,
0x7c810808,
0x0006006d,
0x7c810808,
0x0006006e,
0x7c810808,
0x0006006f,
0x7c810808,
0x00060070,
0x7c810808,
0x7c810808,
0x00060071,
0x7c810808,
0x7c810808,
0x00060072,
0x7c810808,
0x7c810808,
0x00060073,
0x7c810808,
0x7c810808,
0x00060074,
0x7c810808,
0x7c810808,
0x00060075,
0x7c810808,
0x7c810808,
0x7c810808,
0x00060076,
0x7c810808,
0x7c810808,
0x7c810808,
0x7c810808,
0x00060077,
0x7c810808,
0x7c810808,
0x7c810808,
0x7c810808,
0x00060078,
0x7c810808,
0x7c810808,
0x7c810808,
0x7c810808,
0x00060079,
0x7c810808,
0x7c810808,
0x7c810808,
0x7c810808,
0x0006007a,
0x7c810808,
0x7c810808,
0x7c810808,
0x7c810808,
0x0006007b,
0x7c810808,
0x0006007c,
0x7c810808,
0x0006007d,
0x7c810808,
0x7c810808,
0x7c810808,
0x0006007e,
0x7c810808,
0x7c810808,
0x7c810808,
0x0006007f,
0x7c810808,
0x7c810808,
0x7c810808,
0x00060080,
0x7c810808,
0x7c810808,
0x7c810808,
0x00060081,
0x00000000,
0x7c810808,
0x7c810808,
0x7c810808,
0x00060082,
0x7c810808,
0x00060083,
0x7c810808,
0x00060084,
0x00000000,
0x7c810808,
0x00000000,
0x00060085,
0x7c810808,
0x00060086,
0x7c810808,
0x00060087,
0x7c810808,
0x00060088,
0x00000000,
0x7c810808,
0x00000000,
0x00060089,
0x7c810808,
0x0006008a,
0x00000000,
0x7c810808,
0x00000000,
0x0006008b,
0x00000000,
0x7c810808,
0x00000000,
0x0006008c,
0x00000000,
0x7c810808,
0x00000000,
0x0006008d,
0x7c810808,
0x0006008e,
0x7c810808,
0x0006008f,
0x7c810808,
0x00060090,
0x7c810808,
0x00060091,
0x7c810808,
0x00060092,
0x7c810808,
0x00060093,
0x7c810808,
0x00060094,
0x7c810808,
0x00060095,
0x7c810808,
0x00060096,
0x7c810808,
0x00000000,
0x00080000,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x7c810808,
0x00000000,
0x00010000
};

enum {
  GLOB_vm_returnp,
  GLOB_vm_returnc,
  GLOB_vm_return,
  GLOB_vm_leave_cp,
  GLOB_vm_leave_unw,
  GLOB_vm_unwind_c,
  GLOB_vm_unwind_c_eh,
  GLOB_vm_unwind_ff,
  GLOB_vm_unwind_ff_eh,
  GLOB_vm_growstack_c,
  GLOB_vm_growstack_l,
  GLOB_vm_resume,
  GLOB_vm_pcall,
  GLOB_vm_call,
  GLOB_vm_call_dispatch,
  GLOB_vm_call_dispatch_f,
  GLOB_vm_cpcall,
  GLOB_cont_dispatch,
  GLOB_cont_cat,
  GLOB_vmeta_tgets,
  GLOB_vmeta_tgetb,
  GLOB_vmeta_tgetv,
  GLOB_vmeta_tsets,
  GLOB_vmeta_tsetb,
  GLOB_vmeta_tsetv,
  GLOB_vmeta_comp,
  GLOB_cont_nop,
  GLOB_cont_ra,
  GLOB_cont_condt,
  GLOB_cont_condf,
  GLOB_vmeta_equal,
  GLOB_vmeta_arith_vn,
  GLOB_vmeta_arith_nv,
  GLOB_vmeta_unm,
  GLOB_vmeta_arith_vv,
  GLOB_vmeta_binop,
  GLOB_vmeta_len,
  GLOB_vmeta_call_ra,
  GLOB_vmeta_call,
  GLOB_vmeta_for,
  GLOB_ff_assert,
  GLOB_ff_type,
  GLOB_ff_getmetatable,
  GLOB_ff_setmetatable,
  GLOB_ff_rawget,
  GLOB_ff_tonumber,
  GLOB_ff_tostring,
  GLOB_ff_next,
  GLOB_fff_res2,
  GLOB_ff_pairs,
  GLOB_ff_ipairs_aux,
  GLOB_fff_res0,
  GLOB_ff_ipairs,
  GLOB_ff_pcall,
  GLOB_ff_xpcall,
  GLOB_ff_coroutine_resume,
  GLOB_ff_coroutine_wrap_aux,
  GLOB_ff_coroutine_yield,
  GLOB_ff_math_abs,
  GLOB_fff_res1,
  GLOB_fff_res,
  GLOB_ff_math_floor,
  GLOB_ff_math_ceil,
  GLOB_ff_math_sqrt,
  GLOB_ff_math_log,
  GLOB_ff_math_log10,
  GLOB_ff_math_exp,
  GLOB_ff_math_sin,
  GLOB_ff_math_cos,
  GLOB_ff_math_tan,
  GLOB_ff_math_asin,
  GLOB_ff_math_acos,
  GLOB_ff_math_atan,
  GLOB_ff_math_sinh,
  GLOB_ff_math_cosh,
  GLOB_ff_math_tanh,
  GLOB_ff_math_atan2,
  GLOB_ff_math_fmod,
  GLOB_ff_math_deg,
  GLOB_ff_math_rad,
  GLOB_ff_math_ldexp,
  GLOB_ff_math_frexp,
  GLOB_ff_math_modf,
  GLOB_ff_math_pow,
  GLOB_ff_math_min,
  GLOB_ff_math_max,
  GLOB_ff_string_len,
  GLOB_ff_string_byte,
  GLOB_ff_string_char,
  GLOB_fff_newstr,
  GLOB_ff_string_sub,
  GLOB_fff_emptystr,
  GLOB_ff_string_rep,
  GLOB_ff_string_reverse,
  GLOB_ff_string_lower,
  GLOB_ff_string_upper,
  GLOB_ff_table_getn,
  GLOB_ff_bit_tobit,
  GLOB_ff_bit_band,
  GLOB_ff_bit_bor,
  GLOB_ff_bit_bxor,
  GLOB_ff_bit_bswap,
  GLOB_ff_bit_bnot,
  GLOB_fff_resbit,
  GLOB_fff_fallback_bit_op,
  GLOB_ff_bit_lshift,
  GLOB_ff_bit_rshift,
  GLOB_ff_bit_arshift,
  GLOB_ff_bit_rol,
  GLOB_ff_bit_ror,
  GLOB_fff_fallback,
  GLOB_fff_gcstep,
  GLOB_vm_record,
  GLOB_vm_rethook,
  GLOB_vm_inshook,
  GLOB_cont_hook,
  GLOB_vm_hotloop,
  GLOB_vm_callhook,
  GLOB_vm_hotcall,
  GLOB_vm_exit_handler,
  GLOB_vm_exit_interp,
  GLOB_vm_floor,
  GLOB_vm_ceil,
  GLOB_vm_trunc,
  GLOB_vm_mod,
  GLOB_vm_exp,
  GLOB_vm_exp2,
  GLOB_vm_pow,
  GLOB_vm_powi_sse,
  GLOB_vm_foldfpm,
  GLOB_vm_foldarith,
  GLOB__MAX
};
static const char *const globnames[] = {
  "vm_returnp",
  "vm_returnc",
  "vm_return",
  "vm_leave_cp",
  "vm_leave_unw",
  "vm_unwind_c",
  "vm_unwind_c_eh",
  "vm_unwind_ff",
  "vm_unwind_ff_eh",
  "vm_growstack_c",
  "vm_growstack_l",
  "vm_resume",
  "vm_pcall",
  "vm_call",
  "vm_call_dispatch",
  "vm_call_dispatch_f",
  "vm_cpcall",
  "cont_dispatch",
  "cont_cat",
  "vmeta_tgets",
  "vmeta_tgetb",
  "vmeta_tgetv",
  "vmeta_tsets",
  "vmeta_tsetb",
  "vmeta_tsetv",
  "vmeta_comp",
  "cont_nop",
  "cont_ra",
  "cont_condt",
  "cont_condf",
  "vmeta_equal",
  "vmeta_arith_vn",
  "vmeta_arith_nv",
  "vmeta_unm",
  "vmeta_arith_vv",
  "vmeta_binop",
  "vmeta_len",
  "vmeta_call_ra",
  "vmeta_call",
  "vmeta_for",
  "ff_assert",
  "ff_type",
  "ff_getmetatable",
  "ff_setmetatable",
  "ff_rawget",
  "ff_tonumber",
  "ff_tostring",
  "ff_next",
  "fff_res2",
  "ff_pairs",
  "ff_ipairs_aux",
  "fff_res0",
  "ff_ipairs",
  "ff_pcall",
  "ff_xpcall",
  "ff_coroutine_resume",
  "ff_coroutine_wrap_aux",
  "ff_coroutine_yield",
  "ff_math_abs",
  "fff_res1",
  "fff_res",
  "ff_math_floor",
  "ff_math_ceil",
  "ff_math_sqrt",
  "ff_math_log",
  "ff_math_log10",
  "ff_math_exp",
  "ff_math_sin",
  "ff_math_cos",
  "ff_math_tan",
  "ff_math_asin",
  "ff_math_acos",
  "ff_math_atan",
  "ff_math_sinh",
  "ff_math_cosh",
  "ff_math_tanh",
  "ff_math_atan2",
  "ff_math_fmod",
  "ff_math_deg",
  "ff_math_rad",
  "ff_math_ldexp",
  "ff_math_frexp",
  "ff_math_modf",
  "ff_math_pow",
  "ff_math_min",
  "ff_math_max",
  "ff_string_len",
  "ff_string_byte",
  "ff_string_char",
  "fff_newstr",
  "ff_string_sub",
  "fff_emptystr",
  "ff_string_rep",
  "ff_string_reverse",
  "ff_string_lower",
  "ff_string_upper",
  "ff_table_getn",
  "ff_bit_tobit",
  "ff_bit_band",
  "ff_bit_bor",
  "ff_bit_bxor",
  "ff_bit_bswap",
  "ff_bit_bnot",
  "fff_resbit",
  "fff_fallback_bit_op",
  "ff_bit_lshift",
  "ff_bit_rshift",
  "ff_bit_arshift",
  "ff_bit_rol",
  "ff_bit_ror",
  "fff_fallback",
  "fff_gcstep",
  "vm_record",
  "vm_rethook",
  "vm_inshook",
  "cont_hook",
  "vm_hotloop",
  "vm_callhook",
  "vm_hotcall",
  "vm_exit_handler",
  "vm_exit_interp",
  "vm_floor",
  "vm_ceil",
  "vm_trunc",
  "vm_mod",
  "vm_exp",
  "vm_exp2",
  "vm_pow",
  "vm_powi_sse",
  "vm_foldfpm",
  "vm_foldarith",
  (const char *)0
};
static const char *const extnames[] = {
  (const char *)0
};
#define DISPATCH_GL(field)	(GG_DISP2G + (int)offsetof(global_State, field))
#define DISPATCH_J(field)	(GG_DISP2J + (int)offsetof(jit_State, field))
#define PC2PROTO(field)  ((int)offsetof(GCproto, field)-(int)sizeof(GCproto))

/* Generate subroutines used by opcodes and other parts of the VM. */
/* The .code_sub section should be last to help static branch prediction. */
static void build_subroutines(BuildCtx *ctx)
{
  dasm_put(Dst, 0);
  dasm_put(Dst, 1);
  dasm_put(Dst, 45);
  dasm_put(Dst, 91);
  dasm_put(Dst, 148);
  dasm_put(Dst, 204);
  dasm_put(Dst, 279);
#if LJ_HASJIT
  dasm_put(Dst, 288);
#endif
  dasm_put(Dst, 290);
#if LJ_HASJIT
  dasm_put(Dst, 298);
#endif
  dasm_put(Dst, 300);
#if LJ_HASJIT
  dasm_put(Dst, 304);
#endif
  dasm_put(Dst, 306);
#if LJ_HASJIT
  dasm_put(Dst, 308);
#endif
  dasm_put(Dst, 310);
#if LJ_HASJIT
  dasm_put(Dst, 312);
#endif
  dasm_put(Dst, 314);
}

/* Generate the code for a single instruction. */
static void build_ins(BuildCtx *ctx, BCOp op, int defop)
{
  dasm_put(Dst, 335, defop);

  switch (op) {

  /* -- Comparison ops ---------------------------------------------------- */

  /* Remember: all ops branch for a true comparison, fall through otherwise. */

  case BC_ISLT: case BC_ISGE: case BC_ISLE: case BC_ISGT:
    dasm_put(Dst, 337);
    break;

  case BC_ISEQV: case BC_ISNEV:
    dasm_put(Dst, 339);
    break;

  case BC_ISEQS: case BC_ISNES:
    dasm_put(Dst, 341);
    break;

  case BC_ISEQN: case BC_ISNEN:
    dasm_put(Dst, 343);
    break;

  case BC_ISEQP: case BC_ISNEP:
    dasm_put(Dst, 345);
    break;

  /* -- Unary test and copy ops ------------------------------------------- */

  case BC_ISTC: case BC_ISFC: case BC_IST: case BC_ISF:
    dasm_put(Dst, 347);
    break;

  /* -- Unary ops --------------------------------------------------------- */

  case BC_MOV:
    dasm_put(Dst, 349);
    break;
  case BC_NOT:
    dasm_put(Dst, 351);
    break;
  case BC_UNM:
    dasm_put(Dst, 353);
    break;
  case BC_LEN:
    dasm_put(Dst, 355);
    break;

  /* -- Binary ops -------------------------------------------------------- */

  case BC_ADDVN: case BC_ADDNV: case BC_ADDVV:
    dasm_put(Dst, 357);
    break;
  case BC_SUBVN: case BC_SUBNV: case BC_SUBVV:
    dasm_put(Dst, 359);
    break;
  case BC_MULVN: case BC_MULNV: case BC_MULVV:
    dasm_put(Dst, 361);
    break;
  case BC_DIVVN: case BC_DIVNV: case BC_DIVVV:
    dasm_put(Dst, 363);
    break;
  case BC_MODVN:
    dasm_put(Dst, 365);
    break;
  case BC_MODNV: case BC_MODVV:
    dasm_put(Dst, 367);
    break;
  case BC_POW:
    dasm_put(Dst, 369);
    break;

  case BC_CAT:
    dasm_put(Dst, 371);
    break;

  /* -- Constant ops ------------------------------------------------------ */

  case BC_KSTR:
    dasm_put(Dst, 373);
    break;
  case BC_KSHORT:
    dasm_put(Dst, 375);
    break;
  case BC_KNUM:
    dasm_put(Dst, 377);
    break;
  case BC_KPRI:
    dasm_put(Dst, 379);
    break;
  case BC_KNIL:
    dasm_put(Dst, 381);
    break;

  /* -- Upvalue and function ops ------------------------------------------ */

  case BC_UGET:
    dasm_put(Dst, 383);
    break;
  case BC_USETV:
    dasm_put(Dst, 385);
    break;
  case BC_USETS:
    dasm_put(Dst, 387);
    break;
  case BC_USETN:
    dasm_put(Dst, 389);
    break;
  case BC_USETP:
    dasm_put(Dst, 391);
    break;
  case BC_UCLO:
    dasm_put(Dst, 393);
    break;

  case BC_FNEW:
    dasm_put(Dst, 395);
    break;

  /* -- Table ops --------------------------------------------------------- */

  case BC_TNEW:
    dasm_put(Dst, 397);
    break;
  case BC_TDUP:
    dasm_put(Dst, 399);
    break;

  case BC_GGET:
  case BC_GSET:
    dasm_put(Dst, 401);
    break;

  case BC_TGETV:
    dasm_put(Dst, 403);
    break;
  case BC_TGETS:
    dasm_put(Dst, 405);
    break;
  case BC_TGETB:
    dasm_put(Dst, 407);
    break;

  case BC_TSETV:
    dasm_put(Dst, 409);
    break;
  case BC_TSETS:
    dasm_put(Dst, 411);
    break;
  case BC_TSETB:
    dasm_put(Dst, 413);
    break;

  case BC_TSETM:
    dasm_put(Dst, 415);
    break;

  /* -- Calls and vararg handling ----------------------------------------- */

  case BC_CALLM:
    dasm_put(Dst, 417);
    break;
  case BC_CALL:
    dasm_put(Dst, 419);
    break;

  case BC_CALLMT:
    dasm_put(Dst, 421);
    break;
  case BC_CALLT:
    dasm_put(Dst, 423);
    break;

  case BC_ITERC:
    dasm_put(Dst, 425);
    break;

  case BC_VARG:
    dasm_put(Dst, 427);
    break;

  /* -- Returns ----------------------------------------------------------- */

  case BC_RETM:
    dasm_put(Dst, 429);
    break;

  case BC_RET:
    dasm_put(Dst, 431);
    break;

  case BC_RET0: case BC_RET1:
    dasm_put(Dst, 433);
    break;

  /* -- Loops and branches ------------------------------------------------ */

  case BC_FORL:
#if LJ_HASJIT
    dasm_put(Dst, 435);
#endif
    break;

  case BC_JFORI:
  case BC_JFORL:
#if !LJ_HASJIT
    break;
#endif
  case BC_FORI:
  case BC_IFORL:
    dasm_put(Dst, 437);
    break;

  case BC_ITERL:
#if LJ_HASJIT
    dasm_put(Dst, 439);
#endif
    break;

  case BC_JITERL:
#if !LJ_HASJIT
    break;
#endif
  case BC_IITERL:
    dasm_put(Dst, 441);
    break;

  case BC_LOOP:
    dasm_put(Dst, 443);
#if LJ_HASJIT
#endif
    break;

  case BC_ILOOP:
    dasm_put(Dst, 445);
    break;

  case BC_JLOOP:
    dasm_put(Dst, 447);
    break;

  case BC_JMP:
    dasm_put(Dst, 449);
    break;

  /* -- Function headers -------------------------------------------------- */

  case BC_FUNCF:
#if LJ_HASJIT
    dasm_put(Dst, 451);
#endif
  case BC_FUNCV:  /* NYI: compiled vararg functions. */
    break;

  case BC_JFUNCF:
#if !LJ_HASJIT
    break;
#endif
  case BC_IFUNCF:
    dasm_put(Dst, 453);
    break;

  case BC_JFUNCV:
#if !LJ_HASJIT
    break;
#endif
    dasm_put(Dst, 455);
    break;  /* NYI: compiled vararg functions. */

  case BC_IFUNCV:
    dasm_put(Dst, 457);
    break;

  case BC_FUNCC:
  case BC_FUNCCW:
    dasm_put(Dst, 459);
    break;

  /* ---------------------------------------------------------------------- */

  default:
    fprintf(stderr, "Error: undefined opcode BC_%s\n", bc_names[op]);
    exit(2);
    break;
  }
}

static int build_backend(BuildCtx *ctx)
{
  int op;

  dasm_growpc(Dst, BC__MAX);

  build_subroutines(ctx);

  dasm_put(Dst, 461);
  for (op = 0; op < BC__MAX; op++)
    build_ins(ctx, (BCOp)op, op);

  return BC__MAX;
}

/* Emit pseudo frame-info for all assembler functions. */
static void emit_asm_debug(BuildCtx *ctx)
{
  /* NYI */
  UNUSED(ctx);
}

