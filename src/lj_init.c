#include <stdint.h>
#include "lj_arch.h"
#include "lj_jit.h"
#include "lj_vm.h"
#include "lj_str.h"

uint32_t LJ_CPU_FLAGS = 0;

#if LJ_TARGET_ARM && LJ_TARGET_LINUX
#include <sys/utsname.h>
#endif

#ifdef _MSC_VER
/*
** Append a function pointer to the static constructor table executed by
** the C runtime.
** Based on https://stackoverflow.com/questions/1113409/attribute-constructor-equivalent-in-vc
** see also https://docs.microsoft.com/en-us/cpp/c-runtime-library/crt-initialization.
*/
#pragma section(".CRT$XCU",read)
#define LJ_INITIALIZER2_(f,p) \
        static void f(void); \
        __declspec(allocate(".CRT$XCU")) void (*f##_)(void) = f; \
        __pragma(comment(linker,"/include:" p #f "_")) \
        static void f(void)
#ifdef _WIN64
#define LJ_INITIALIZER(f) LJ_INITIALIZER2_(f,"")
#else
#define LJ_INITIALIZER(f) LJ_INITIALIZER2_(f,"_")
#endif

#else
#define LJ_INITIALIZER(f) static void __attribute__((constructor)) f(void)
#endif

/* Arch-dependent CPU detection. */
LJ_INITIALIZER(lj_cpudetect)
{
  uint32_t flags = 0;
#if LJ_TARGET_X86ORX64
  uint32_t vendor[4];
  uint32_t features[4];
  if (lj_vm_cpuid(0, vendor) && lj_vm_cpuid(1, features)) {
#if !LJ_HASJIT
#define JIT_F_SSE2	2
#endif
    flags |= ((features[3] >> 26)&1) * JIT_F_SSE2;
#if LJ_HASJIT
    flags |= ((features[2] >> 0)&1) * JIT_F_SSE3;
    flags |= ((features[2] >> 19)&1) * JIT_F_SSE4_1;
    flags |= ((features[2] >> 20)&1) * JIT_F_SSE4_2;
    if (vendor[2] == 0x6c65746e) {  /* Intel. */
      if ((features[0] & 0x0fff0ff0) == 0x000106c0)  /* Atom. */
	flags |= JIT_F_LEA_AGU;
    } else if (vendor[2] == 0x444d4163) {  /* AMD. */
      uint32_t fam = (features[0] & 0x0ff00f00);
      if (fam >= 0x00000f00)  /* K8, K10. */
	flags |= JIT_F_PREFER_IMUL;
    }
    if (vendor[0] >= 7) {
      uint32_t xfeatures[4];
      lj_vm_cpuid(7, xfeatures);
      flags |= ((xfeatures[1] >> 8)&1) * JIT_F_BMI2;
    }
#endif
  }
#elif LJ_TARGET_ARM
#if LJ_HASJIT
  int ver = LJ_ARCH_VERSION;  /* Compile-time ARM CPU detection. */
#if LJ_TARGET_LINUX
  if (ver < 70) {  /* Runtime ARM CPU detection. */
    struct utsname ut;
    uname(&ut);
    if (strncmp(ut.machine, "armv", 4) == 0) {
      if (ut.machine[4] >= '7')
	ver = 70;
      else if (ut.machine[4] == '6')
	ver = 60;
    }
  }
#endif
  flags |= ver >= 70 ? JIT_F_ARMV7 :
	   ver >= 61 ? JIT_F_ARMV6T2_ :
	   ver >= 60 ? JIT_F_ARMV6_ : 0;
  flags |= LJ_ARCH_HASFPU == 0 ? 0 : ver >= 70 ? JIT_F_VFPV3 : JIT_F_VFPV2;
#endif
#elif LJ_TARGET_ARM64
  /* No optional CPU features to detect (for now). */
#elif LJ_TARGET_PPC
#if LJ_HASJIT
#if LJ_ARCH_SQRT
  flags |= JIT_F_SQRT;
#endif
#if LJ_ARCH_ROUND
  flags |= JIT_F_ROUND;
#endif
#endif
#elif LJ_TARGET_MIPS
#if LJ_HASJIT
  /* Compile-time MIPS CPU detection. */
#if LJ_ARCH_VERSION >= 20
  flags |= JIT_F_MIPSXXR2;
#endif
  /* Runtime MIPS CPU detection. */
#if defined(__GNUC__)
  if (!(flags & JIT_F_MIPSXXR2)) {
    int x;
#ifdef __mips16
    x = 0;  /* Runtime detection is difficult. Ensure optimal -march flags. */
#else
    /* On MIPS32R1 rotr is treated as srl. rotr r2,r2,1 -> srl r2,r2,1. */
    __asm__("li $2, 1\n\t.long 0x00221042\n\tmove %0, $2" : "=r"(x) : : "$2");
#endif
    if (x) flags |= JIT_F_MIPSXXR2;  /* Either 0x80000000 (R2) or 0 (R1). */
  }
#endif
#endif
#elif LJ_TARGET_S390X
  /* No optional CPU features to detect (for now). */
#else
#error "Missing CPU detection for this architecture"
#endif
  LJ_CPU_FLAGS = flags;
}
