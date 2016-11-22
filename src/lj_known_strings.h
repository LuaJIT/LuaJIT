#ifndef LJ_KNOWN_STRINGS_H
#define LJ_KNOWN_STRINGS_H
/* small hash function for lj_cparse.c and lib_ffi.c */
static LJ_AINLINE uint32_t lj_ks(GCstr* s)
{
  uint32_t h = 0;
  const uint8_t* v = (const uint8_t*)strdata(s);
  /* do not use s->len for loop cause clang produces bad code then */
  for (; *v; v++) {
    h = h*9 + *v;
    h = lj_rol(h, 7);
  }
  return h;
}
#define LJ_KS_32bit	0x01555882
#define LJ_KS_64bit	0x21558175
#define LJ_KS___aligned__	0x569f93c0
#define LJ_KS___cdecl__	0xd9fd5be2
#define LJ_KS___fastcall__	0xa9df7119
#define LJ_KS___mode__	0x9fcb48c2
#define LJ_KS___packed__	0xd3c4c96f
#define LJ_KS___regparm__	0x5f530d2e
#define LJ_KS___sseregparm__	0x7f8cef82
#define LJ_KS___stdcall__	0x1b3e9db8
#define LJ_KS___thiscall__	0xb007f754
#define LJ_KS___vector_size__	0x45505b8d
#define LJ_KS_align	0xe845f6c4
#define LJ_KS_aligned	0xc5e4d088
#define LJ_KS_be	0x00dcb280
#define LJ_KS_cdecl	0x3fbc4f68
#define LJ_KS_eabi	0xa6fcc27d
#define LJ_KS_fastcall	0xee79726d
#define LJ_KS_fpu	0x09bc3a84
#define LJ_KS_gc64	0xdad9ac58
#define LJ_KS_hardfp	0x825f5c57
#define LJ_KS_le	0x00f33280
#define LJ_KS_line	0xe817c4bc
#define LJ_KS_mode	0xb4c144ea
#define LJ_KS_pack	0xd6ff07f2
#define LJ_KS_packed	0x61c931ac
#define LJ_KS_pop	0x6ef9f804
#define LJ_KS_pragma	0x196f8b59
#define LJ_KS_push	0xa1a30673
#define LJ_KS_regparm	0xdbe7ffae
#define LJ_KS_softfp	0x6db65aa5
#define LJ_KS_sseregparm	0xdd0742dd
#define LJ_KS_stdcall	0xefb972d6
#define LJ_KS_thiscall	0xa729bb14
#define LJ_KS_vector_size	0x639def61
#define LJ_KS_win	0xb5cc7704
#endif
