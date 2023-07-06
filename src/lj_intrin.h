#ifndef _LJ_INTRIN_H_
#define _LJ_INTRIN_H_

#include "lj_arch.h"

#if LUAJIT_TARGET == LUAJIT_ARCH_X64
#if LJ_TARGET_WINDOWS
#include <immintrin.h>
#else
#include <x86intrin.h>
#endif

/* # of contiguous low 0 bits */
#define tzcount32(x) (unsigned)_tzcnt_u32(x)
#define tzcount64(x) (unsigned)_tzcnt_u64(x)

/* x & (x - 1) */
#define reset_lowest32(x) _blsr_u32(x)
#define reset_lowest64(x) _blsr_u64(x)

/* 256 bit SIMD */
#define LJ_SIMD_256 1
#define I256 __m256i
#define I256_ZERO(o) o = _mm256_setzero_si256()
/* vpxor a, a, a; vpcmpeqq a, a, a sets all bits to 1 */
#define I256_ONES(o) o = _mm256_cmpeq_epi64(_mm256_setzero_si256(), _mm256_setzero_si256())
#define I256_EQ_64_MASK(x, y) (uint64_t)_mm256_movemask_pd(_mm256_castsi256_pd(_mm256_cmpeq_epi64(x, y)))
#define I256_AND(o, x, y) o = _mm256_and_si256(x, y)
#define I256_XOR(o, x, y) o = _mm256_xor_si256(x, y)
#define I256_OR(o, x, y) o = _mm256_or_si256(x, y)
#define I256_LOADA(o, ptr) o = _mm256_load_si256((__m256i *)ptr)
#define I256_STOREA(ptr, v) _mm256_store_si256((__m256i *)ptr, v)

#else
#error "No intrinsics defined for arch"
#endif

#endif
