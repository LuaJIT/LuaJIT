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
#define reset_lowest32(x) (uint32_t)_blsr_u32(x)
#define reset_lowest64(x) (uint64_t)_blsr_u64(x)

/* x ^ (x - 1) */
#define mask_lowest32(x) (uint32_t)_blsmsk_u32(x)
#define mask_lowest64(x) (uint64_t)_blsmsk_u64(x)

/* x & ~y */
#define and_not32(x, y) (uint32_t)_andn_u32(y, x)
#define and_not64(x, y) (uint64_t)_andn_u64(y, x)

#define popcount64(x) (unsigned)_mm_popcnt_u64(x)

/* 256 bit SIMD */
#define LJ_SIMD_256 1
#define I256 __m256i
#define I256_ZERO(o) o = _mm256_setzero_si256()
/* vpxor a, a, a; vpcmpeqq a, a, a sets all bits to 1 */
#define I256_ONES(o) o = _mm256_cmpeq_epi64(_mm256_setzero_si256(), _mm256_setzero_si256())
#define I256_BCAST_8(o, v) o = _mm256_set1_epi8((char)v)
#define I256_BCAST_32(o, v) o = _mm256_set1_epi32((int)v)
#define I256_NEQ_64_MASK(x, y) ((uint64_t)_mm256_movemask_pd(_mm256_castsi256_pd(_mm256_cmpeq_epi64(x, y))) ^ 0xF)
#define I256_EQ_64_MASK(x, y) (uint64_t)_mm256_movemask_pd(_mm256_castsi256_pd(_mm256_cmpeq_epi64(x, y)))
#define I256_EQ_32_MASK(x, y) (uint64_t)_mm256_movemask_ps(_mm256_castsi256_ps(_mm256_cmpeq_epi32(x, y)))
#define I256_AND(o, x, y) o = _mm256_and_si256(x, y)
#define I256_XOR(o, x, y) o = _mm256_xor_si256(x, y)
#define I256_OR(o, x, y) o = _mm256_or_si256(x, y)
#define I256_ANDNOT(o, x, y) o = _mm256_andnot_si256(y, x) /* x & ~y */
#define I256_SHL_64(o, x, n) o = _mm256_slli_epi64(x, n)
#define I256_SHUFFLE_64(o, x, mask) \
  o = _mm256_castpd_si256(_mm256_permute_pd(_mm256_castsi256_pd(x), mask))
#define I256_LOADA(o, ptr) o = _mm256_load_si256((__m256i *)(ptr))
#define I256_STOREA(ptr, v) _mm256_store_si256((__m256i *)(ptr), v)
#define I256_EXTRACT(x, n) (uint64_t)_mm256_extract_epi64(x, n)

#else
#error "No intrinsics defined for arch"
#endif

#endif
