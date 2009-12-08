/*
** Math library.
** Copyright (C) 2005-2009 Mike Pall. See Copyright Notice in luajit.h
*/

#include <math.h>

#define lib_math_c
#define LUA_LIB

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "lj_obj.h"
#include "lj_lib.h"

/* ------------------------------------------------------------------------ */

#define LJLIB_MODULE_math

LJLIB_ASM(math_abs)		LJLIB_REC(.)
{
  lj_lib_checknum(L, 1);
  return FFH_RETRY;
}
LJLIB_ASM_(math_floor)		LJLIB_REC(math_round IRFPM_FLOOR)
LJLIB_ASM_(math_ceil)		LJLIB_REC(math_round IRFPM_CEIL)
LJLIB_ASM_(math_sqrt)		LJLIB_REC(math_unary IRFPM_SQRT)
LJLIB_ASM_(math_log)		LJLIB_REC(math_unary IRFPM_LOG)
LJLIB_ASM_(math_log10)		LJLIB_REC(math_unary IRFPM_LOG10)
LJLIB_ASM_(math_exp)		LJLIB_REC(math_unary IRFPM_EXP)
LJLIB_ASM_(math_sin)		LJLIB_REC(math_unary IRFPM_SIN)
LJLIB_ASM_(math_cos)		LJLIB_REC(math_unary IRFPM_COS)
LJLIB_ASM_(math_tan)		LJLIB_REC(math_unary IRFPM_TAN)
LJLIB_ASM_(math_asin)		LJLIB_REC(math_atrig FF_math_asin)
LJLIB_ASM_(math_acos)		LJLIB_REC(math_atrig FF_math_acos)
LJLIB_ASM_(math_atan)		LJLIB_REC(math_atrig FF_math_atan)
LJLIB_ASM_(math_sinh)
LJLIB_ASM_(math_cosh)
LJLIB_ASM_(math_tanh)
LJLIB_ASM_(math_frexp)
LJLIB_ASM_(math_modf)		LJLIB_REC(.)

LJLIB_PUSH(57.29577951308232)
LJLIB_ASM_(math_deg)		LJLIB_REC(math_degrad)

LJLIB_PUSH(0.017453292519943295)
LJLIB_ASM_(math_rad)		LJLIB_REC(math_degrad)

LJLIB_ASM(math_atan2)		LJLIB_REC(math_binary IR_ATAN2)
{
  lj_lib_checknum(L, 1);
  lj_lib_checknum(L, 2);
  return FFH_RETRY;
}
LJLIB_ASM_(math_ldexp)		LJLIB_REC(math_binary IR_LDEXP)
LJLIB_ASM_(math_pow)		LJLIB_REC(.)
LJLIB_ASM_(math_fmod)

LJLIB_ASM(math_min)		LJLIB_REC(math_minmax IR_MIN)
{
  int i = 0;
  do { lj_lib_checknum(L, ++i); } while (L->base+i < L->top);
  return FFH_RETRY;
}
LJLIB_ASM_(math_max)		LJLIB_REC(math_minmax IR_MAX)

LJLIB_PUSH(3.14159265358979323846) LJLIB_SET(pi)
LJLIB_PUSH(1e310) LJLIB_SET(huge)

LJ_FUNCA double lj_wrapper_sinh(double x) { return sinh(x); }
LJ_FUNCA double lj_wrapper_cosh(double x) { return cosh(x); }
LJ_FUNCA double lj_wrapper_tanh(double x) { return tanh(x); }

/* ------------------------------------------------------------------------ */

/* This implements a Tausworthe PRNG with period 2^223. Based on:
**   Tables of maximally-equidistributed combined LFSR generators,
**   Pierre L'Ecuyer, 1991, table 3, 1st entry.
** Full-period ME-CF generator with L=64, J=4, k=223, N1=49.
*/

/* PRNG state. */
typedef struct TW223State {
  uint64_t gen[4];	/* State of the 4 LFSR generators. */
  int valid;		/* State is valid. */
} TW223State;

/* Union needed for bit-pattern conversion between uint64_t and double. */
typedef union { uint64_t u64; double d; } U64double;

/* Update generator i and compute a running xor of all states. */
#define TW223_GEN(i, k, q, s) \
  z = tw->gen[i]; \
  z = (((z<<q)^z) >> (k-s)) ^ ((z&((uint64_t)(int64_t)-1 << (64-k)))<<s); \
  r ^= z; tw->gen[i] = z;

/* PRNG step function. Returns a double in the range 1.0 <= d < 2.0. */
static LJ_NOINLINE double tw223_step(TW223State *tw)
{
  uint64_t z, r = 0;
  U64double u;
  TW223_GEN(0, 63, 31, 18)
  TW223_GEN(1, 58, 19, 28)
  TW223_GEN(2, 55, 24,  7)
  TW223_GEN(3, 47, 21,  8)
  u.u64 = (r & (((uint64_t)1 << 52)-1)) | ((uint64_t)0x3ff << 52);
  return u.d;
}

/* PRNG initialization function. */
static void tw223_init(TW223State *tw, double d)
{
  uint32_t r = 0x11090601;  /* 64-k[i] as four 8 bit constants. */
  int i;
  for (i = 0; i < 4; i++) {
    U64double u;
    uint32_t m = 1u << (r&255);
    r >>= 8;
    u.d = d = d * 3.14159265358979323846 + 2.7182818284590452354;
    if (u.u64 < m) u.u64 += m;  /* Ensure k[i] MSB of gen[i] are non-zero. */
    tw->gen[i] = u.u64;
  }
  tw->valid = 1;
  for (i = 0; i < 10; i++)
    tw223_step(tw);
}

/* PRNG extract function. */
LJLIB_PUSH(top-2)  /* Upvalue holds userdata with TW223State. */
LJLIB_CF(math_random)
{
  int n = cast_int(L->top - L->base);
  TW223State *tw = (TW223State *)(uddata(udataV(lj_lib_upvalue(L, 1))));
  double d;
  if (LJ_UNLIKELY(!tw->valid)) tw223_init(tw, 0.0);
  d = tw223_step(tw) - 1.0;
  if (n > 0) {
    double r1 = lj_lib_checknum(L, 1);
    if (n == 1) {
      d = floor(d*r1) + 1.0;  /* d is an int in range [1, r1] */
    } else {
      double r2 = lj_lib_checknum(L, 2);
      d = floor(d*(r2-r1+1.0)) + r1;  /* d is an int in range [r1, r2] */
    }
  }  /* else: d is a double in range [0, 1] */
  setnumV(L->top++, d);
  return 1;
}

/* PRNG seed function. */
LJLIB_PUSH(top-2)  /* Upvalue holds userdata with TW223State. */
LJLIB_CF(math_randomseed)
{
  TW223State *tw = (TW223State *)(uddata(udataV(lj_lib_upvalue(L, 1))));
  tw223_init(tw, lj_lib_checknum(L, 1));
  return 0;
}

/* ------------------------------------------------------------------------ */

#include "lj_libdef.h"

LUALIB_API int luaopen_math(lua_State *L)
{
  TW223State *tw;
  tw = (TW223State *)lua_newuserdata(L, sizeof(TW223State));
  tw->valid = 0;  /* Use lazy initialization to save some time on startup. */
  LJ_LIB_REG(L, math);
#if defined(LUA_COMPAT_MOD)
  lua_getfield(L, -1, "fmod");
  lua_setfield(L, -2, "mod");
#endif
  return 1;
}

