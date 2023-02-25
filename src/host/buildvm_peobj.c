/*
** LuaJIT VM builder: PE object emitter.
** Copyright (C) 2005-2022 Mike Pall. See Copyright Notice in luajit.h
**
** Only used for building on Windows, since we cannot assume the presence
** of a suitable assembler. The host and target byte order must match.
*/

#include "buildvm.h"
#include "lj_bc.h"

#if LJ_TARGET_WINDOWS

/* Context for PE object emitter. */
static char *strtab;
static size_t strtabofs;

/* -- PE object definitions ----------------------------------------------- */

/* PE header. */
typedef struct PEheader {
  uint16_t arch;
  uint16_t nsects;
  uint32_t time;
  uint32_t symtabofs;
  uint32_t nsyms;
  uint16_t opthdrsz;
  uint16_t flags;
} PEheader;

/* PE section. */
typedef struct PEsection {
  char name[8];
  uint32_t vsize;
  uint32_t vaddr;
  uint32_t size;
  uint32_t ofs;
  uint32_t relocofs;
  uint32_t lineofs;
  uint16_t nreloc;
  uint16_t nline;
  uint32_t flags;
} PEsection;

/* PE relocation. */
typedef struct PEreloc {
  uint32_t vaddr;
  uint32_t symidx;
  uint16_t type;
} PEreloc;

/* Cannot use sizeof, because it pads up to the max. alignment. */
#define PEOBJ_RELOC_SIZE	(4+4+2)

/* PE symbol table entry. */
typedef struct PEsym {
  union {
    char name[8];
    uint32_t nameref[2];
  } n;
  uint32_t value;
  int16_t sect;
  uint16_t type;
  uint8_t scl;
  uint8_t naux;
} PEsym;

/* PE symbol table auxiliary entry for a section. */
typedef struct PEsymaux {
  uint32_t size;
  uint16_t nreloc;
  uint16_t nline;
  uint32_t cksum;
  uint16_t assoc;
  uint8_t comdatsel;
  uint8_t unused[3];
} PEsymaux;

#if LJ_TARGET_ARM64
#pragma pack(push, 1)
typedef struct UnwindInfo {
  uint32_t FunctionLength : 18;
  uint32_t Version : 2;
  uint32_t X : 1;
  uint32_t E : 1;
  uint32_t EpilogCount : 5;
  uint32_t CodeWords : 5;
} UnwindInfo;
#pragma pack(pop)
#endif

/* Cannot use sizeof, because it pads up to the max. alignment. */
#define PEOBJ_SYM_SIZE	(8+4+2+2+1+1)

/* PE object CPU specific defines. */
#if LJ_TARGET_X86
#define PEOBJ_ARCH_TARGET	0x014c
#define PEOBJ_RELOC_REL32	0x14  /* MS: REL32, GNU: DISP32. */
#define PEOBJ_RELOC_DIR32	0x06
#define PEOBJ_RELOC_OFS		0
#define PEOBJ_TEXT_FLAGS	0x60500020  /* 60=r+x, 50=align16, 20=code. */
#elif LJ_TARGET_X64
#define PEOBJ_ARCH_TARGET	0x8664
#define PEOBJ_RELOC_REL32	0x04  /* MS: REL32, GNU: DISP32. */
#define PEOBJ_RELOC_DIR32	0x02
#define PEOBJ_RELOC_ADDR32NB	0x03
#define PEOBJ_RELOC_OFS		0
#define PEOBJ_TEXT_FLAGS	0x60500020  /* 60=r+x, 50=align16, 20=code. */
#elif LJ_TARGET_ARM64
#define PEOBJ_ARCH_TARGET	0xaa64
#define PEOBJ_RELOC_REL32	0x03  /* MS: BRANCH26 */
#define PEOBJ_RELOC_DIR32	0x01
#define PEOBJ_RELOC_ADDR32NB	0x02
#define PEOBJ_RELOC_OFS		0
#define PEOBJ_TEXT_FLAGS	0x60500020  /* 60=r+x, 50=align16, 20=code. */
#endif

/* Section numbers (0-based). */
enum {
  PEOBJ_SECT_ABS = -2,
  PEOBJ_SECT_UNDEF = -1,
  PEOBJ_SECT_TEXT,
#if LJ_TARGET_X64 || LJ_TARGET_ARM64
  PEOBJ_SECT_PDATA,
  PEOBJ_SECT_XDATA,
#elif LJ_TARGET_X86
  PEOBJ_SECT_SXDATA,
#endif
  PEOBJ_SECT_RDATA_Z,
  PEOBJ_NSECTIONS
};

/* Symbol types. */
#define PEOBJ_TYPE_NULL		0
#define PEOBJ_TYPE_FUNC		0x20

/* Symbol storage class. */
#define PEOBJ_SCL_EXTERN	2
#define PEOBJ_SCL_STATIC	3

/* -- PE object emitter --------------------------------------------------- */

/* Emit PE object symbol. */
static void emit_peobj_sym(BuildCtx *ctx, const char *name, uint32_t value,
			   int sect, int type, int scl)
{
  PEsym sym;
  size_t len = strlen(name);
  if (!strtab) {  /* Pass 1: only calculate string table length. */
    if (len > 8) strtabofs += len+1;
    return;
  }
  if (len <= 8) {
    memcpy(sym.n.name, name, len);
    memset(sym.n.name+len, 0, 8-len);
  } else {
    sym.n.nameref[0] = 0;
    sym.n.nameref[1] = (uint32_t)strtabofs;
    memcpy(strtab + strtabofs, name, len);
    strtab[strtabofs+len] = 0;
    strtabofs += len+1;
  }
  sym.value = value;
  sym.sect = (int16_t)(sect+1);  /* 1-based section number. */
  sym.type = (uint16_t)type;
  sym.scl = (uint8_t)scl;
  sym.naux = 0;
  owrite(ctx, &sym, PEOBJ_SYM_SIZE);
}

/* Emit PE object section symbol. */
static void emit_peobj_sym_sect(BuildCtx *ctx, PEsection *pesect, int sect)
{
  PEsym sym;
  PEsymaux aux;
  if (!strtab) return;  /* Pass 1: no output. */
  memcpy(sym.n.name, pesect[sect].name, 8);
  sym.value = 0;
  sym.sect = (int16_t)(sect+1);  /* 1-based section number. */
  sym.type = PEOBJ_TYPE_NULL;
  sym.scl = PEOBJ_SCL_STATIC;
  sym.naux = 1;
  owrite(ctx, &sym, PEOBJ_SYM_SIZE);
  memset(&aux, 0, sizeof(PEsymaux));
  aux.size = pesect[sect].size;
  aux.nreloc = pesect[sect].nreloc;
  owrite(ctx, &aux, PEOBJ_SYM_SIZE);
}

/* Emit Windows PE object file. */
void emit_peobj(BuildCtx *ctx)
{
  PEheader pehdr;
  PEsection pesect[PEOBJ_NSECTIONS];
  uint32_t sofs;
  int i, nrsym;
  union { uint8_t b; uint32_t u; } host_endian;

  sofs = sizeof(PEheader) + PEOBJ_NSECTIONS*sizeof(PEsection);

  /* Fill in PE sections. */
  memset(&pesect, 0, PEOBJ_NSECTIONS*sizeof(PEsection));
  memcpy(pesect[PEOBJ_SECT_TEXT].name, ".text", sizeof(".text")-1);
  pesect[PEOBJ_SECT_TEXT].ofs = sofs;
  sofs += (pesect[PEOBJ_SECT_TEXT].size = (uint32_t)ctx->codesz);
  pesect[PEOBJ_SECT_TEXT].relocofs = sofs;
  sofs += (pesect[PEOBJ_SECT_TEXT].nreloc = (uint16_t)ctx->nreloc) * PEOBJ_RELOC_SIZE;
  /* Flags: 60 = read+execute, 50 = align16, 20 = code. */
  pesect[PEOBJ_SECT_TEXT].flags = PEOBJ_TEXT_FLAGS;

#if LJ_TARGET_X64 || LJ_TARGET_ARM64
  memcpy(pesect[PEOBJ_SECT_PDATA].name, ".pdata", sizeof(".pdata")-1);
  pesect[PEOBJ_SECT_PDATA].ofs = sofs;
  #if LJ_TARGET_X64
  sofs += (pesect[PEOBJ_SECT_PDATA].size = 6*4);
  pesect[PEOBJ_SECT_PDATA].relocofs = sofs;
  sofs += (pesect[PEOBJ_SECT_PDATA].nreloc = 6) * PEOBJ_RELOC_SIZE;
  #else
  sofs += (pesect[PEOBJ_SECT_PDATA].size = 16);
  pesect[PEOBJ_SECT_PDATA].relocofs = sofs;
  sofs += (pesect[PEOBJ_SECT_PDATA].nreloc = 4) * PEOBJ_RELOC_SIZE;
  #endif
  /* Flags: 40 = read, 30 = align4, 40 = initialized data. */
  pesect[PEOBJ_SECT_PDATA].flags = 0x40300040;

  memcpy(pesect[PEOBJ_SECT_XDATA].name, ".xdata", sizeof(".xdata")-1);
  pesect[PEOBJ_SECT_XDATA].ofs = sofs;
#if LJ_TARGET_X64
  sofs += (pesect[PEOBJ_SECT_XDATA].size = 8*2+4+6*2);  /* See below. */
#else
  sofs += (pesect[PEOBJ_SECT_XDATA].size = sizeof(UnwindInfo)+44+4+4+/**/+sizeof(UnwindInfo)+8);  /* See below. */
#endif
  pesect[PEOBJ_SECT_XDATA].relocofs = sofs;
  sofs += (pesect[PEOBJ_SECT_XDATA].nreloc = 1) * PEOBJ_RELOC_SIZE;
  /* Flags: 40 = read, 30 = align4, 40 = initialized data. */
  pesect[PEOBJ_SECT_XDATA].flags = 0x40300040;
#elif LJ_TARGET_X86
  memcpy(pesect[PEOBJ_SECT_SXDATA].name, ".sxdata", sizeof(".sxdata")-1);
  pesect[PEOBJ_SECT_SXDATA].ofs = sofs;
  sofs += (pesect[PEOBJ_SECT_SXDATA].size = 4);
  pesect[PEOBJ_SECT_SXDATA].relocofs = sofs;
  /* Flags: 40 = read, 30 = align4, 02 = lnk_info, 40 = initialized data. */
  pesect[PEOBJ_SECT_SXDATA].flags = 0x40300240;
#endif

  memcpy(pesect[PEOBJ_SECT_RDATA_Z].name, ".rdata$Z", sizeof(".rdata$Z")-1);
  pesect[PEOBJ_SECT_RDATA_Z].ofs = sofs;
  sofs += (pesect[PEOBJ_SECT_RDATA_Z].size = (uint32_t)strlen(ctx->dasm_ident)+1);
  /* Flags: 40 = read, 30 = align4, 40 = initialized data. */
  pesect[PEOBJ_SECT_RDATA_Z].flags = 0x40300040;

  /* Fill in PE header. */
  pehdr.arch = PEOBJ_ARCH_TARGET;
  pehdr.nsects = PEOBJ_NSECTIONS;
  pehdr.time = 0;  /* Timestamp is optional. */
  pehdr.symtabofs = sofs;
  pehdr.opthdrsz = 0;
  pehdr.flags = 0;

  /* Compute the size of the symbol table:
  ** @feat.00 + nsections*2
  ** + asm_start + nsym
  ** + nrsym
  */
  nrsym = ctx->nrelocsym;
  pehdr.nsyms = 1+PEOBJ_NSECTIONS*2 + 1+ctx->nsym + nrsym;
#if LJ_TARGET_X64 || LJ_TARGET_ARM64
  pehdr.nsyms += 1;  /* Symbol for lj_err_unwind_win. */
#endif

  /* Write PE object header and all sections. */
  owrite(ctx, &pehdr, sizeof(PEheader));
  owrite(ctx, &pesect, sizeof(PEsection)*PEOBJ_NSECTIONS);

  /* Write .text section. */
  host_endian.u = 1;
  if (host_endian.b != LJ_ENDIAN_SELECT(1, 0)) {
    fprintf(stderr, "Error: different byte order for host and target\n");
    exit(1);
  }
  owrite(ctx, ctx->code, ctx->codesz);
  for (i = 0; i < ctx->nreloc; i++) {
    PEreloc reloc;
    reloc.vaddr = (uint32_t)ctx->reloc[i].ofs + PEOBJ_RELOC_OFS;
    reloc.symidx = 1+2+ctx->reloc[i].sym;  /* Reloc syms are after .text sym. */
    reloc.type = ctx->reloc[i].type ? PEOBJ_RELOC_REL32 : PEOBJ_RELOC_DIR32;
    owrite(ctx, &reloc, PEOBJ_RELOC_SIZE);
  }

#if LJ_TARGET_X64 || LJ_TARGET_ARM64
  { /* Write .pdata section. */
    PEreloc reloc;
    uint32_t fcofs = (uint32_t)ctx->sym[ctx->nsym-1].ofs;
    #if LJ_TARGET_X64
    uint32_t pdata[3];  /* Start of .text, end of .text and .xdata. */
    PEreloc reloc;
    pdata[0] = 0; pdata[1] = fcofs; pdata[2] = 0;
    owrite(ctx, &pdata, sizeof(pdata));
    pdata[0] = fcofs; pdata[1] = (uint32_t)ctx->codesz; pdata[2] = 20;
    owrite(ctx, &pdata, sizeof(pdata));
    reloc.vaddr = 0; reloc.symidx = 1+2+nrsym+2+2+1;
    reloc.type = PEOBJ_RELOC_ADDR32NB;
    owrite(ctx, &reloc, PEOBJ_RELOC_SIZE);
    reloc.vaddr = 4; reloc.symidx = 1+2+nrsym+2+2+1;
    reloc.type = PEOBJ_RELOC_ADDR32NB;
    owrite(ctx, &reloc, PEOBJ_RELOC_SIZE);
    reloc.vaddr = 8; reloc.symidx = 1+2+nrsym+2;
    reloc.type = PEOBJ_RELOC_ADDR32NB;
    owrite(ctx, &reloc, PEOBJ_RELOC_SIZE);
    reloc.vaddr = 12; reloc.symidx = 1+2+nrsym+2+2+1;
    reloc.type = PEOBJ_RELOC_ADDR32NB;
    owrite(ctx, &reloc, PEOBJ_RELOC_SIZE);
    reloc.vaddr = 16; reloc.symidx = 1+2+nrsym+2+2+1;
    reloc.type = PEOBJ_RELOC_ADDR32NB;
    owrite(ctx, &reloc, PEOBJ_RELOC_SIZE);
    reloc.vaddr = 20; reloc.symidx = 1+2+nrsym+2;
    reloc.type = PEOBJ_RELOC_ADDR32NB;
    owrite(ctx, &reloc, PEOBJ_RELOC_SIZE);
    #else
    uint32_t pdata[4];
    pdata[0] = 0;
    pdata[1] = 0;
    pdata[2] = fcofs;
    pdata[3] = sizeof(UnwindInfo)+44+4+4;
    owrite(ctx, &pdata, sizeof(pdata));

    /* Mark the .text */
    reloc.vaddr = 0; reloc.symidx = 1+2+nrsym+2+2+1;
    reloc.type = PEOBJ_RELOC_ADDR32NB;
    owrite(ctx, &reloc, PEOBJ_RELOC_SIZE);
    /* Mark the exception VA address as in .xdata section */
    reloc.vaddr = 4; reloc.symidx = 1+2+nrsym+2;
    reloc.type = PEOBJ_RELOC_ADDR32NB;
    owrite(ctx, &reloc, PEOBJ_RELOC_SIZE);

    /* Mark the vm_ffi_call function which requires special unwinding */
    reloc.vaddr = 8; reloc.symidx = 1+2+nrsym+2+2+1;
    reloc.type = PEOBJ_RELOC_ADDR32NB;
    owrite(ctx, &reloc, PEOBJ_RELOC_SIZE);
    /* Mark the exception VA address as in .xdata section */
    reloc.vaddr = 12; reloc.symidx = 1+2+nrsym+2;
    reloc.type = PEOBJ_RELOC_ADDR32NB;
    owrite(ctx, &reloc, PEOBJ_RELOC_SIZE);
    #endif
  }
#if LJ_TARGET_X64
  { /* Write .xdata section. */
    uint16_t xdata[8+2+6];
    PEreloc reloc;
    xdata[0] = 0x01|0x08|0x10;  /* Ver. 1, uhandler/ehandler, prolog size 0. */
    xdata[1] = 0x0005;  /* Number of unwind codes, no frame pointer. */
    xdata[2] = 0x4200;  /* Stack offset 4*8+8 = aword*5. */
    xdata[3] = 0x3000;  /* Push rbx. */
    xdata[4] = 0x6000;  /* Push rsi. */
    xdata[5] = 0x7000;  /* Push rdi. */
    xdata[6] = 0x5000;  /* Push rbp. */
    xdata[7] = 0;  /* Alignment. */
    xdata[8] = xdata[9] = 0;  /* Relocated address of exception handler. */
    xdata[10] = 0x01;  /* Ver. 1, no handler, prolog size 0. */
    xdata[11] = 0x1504;  /* Number of unwind codes, fp = rbp, fpofs = 16. */
    xdata[12] = 0x0300;  /* set_fpreg. */
    xdata[13] = 0x0200;  /* stack offset 0*8+8 = aword*1. */
    xdata[14] = 0x3000;  /* Push rbx. */
    xdata[15] = 0x5000;  /* Push rbp. */
    owrite(ctx, &xdata, sizeof(xdata));
    reloc.vaddr = 2*8; reloc.symidx = 1+2+nrsym+2+2;
    reloc.type = PEOBJ_RELOC_ADDR32NB;
    owrite(ctx, &reloc, PEOBJ_RELOC_SIZE);
  }
#else
  { /* Write .xdata section. */
    UnwindInfo ui = {0};
    uint32_t handler[2];
    uint8_t codes[64];
    uint8_t *p;
    PEreloc reloc;
    uint32_t fcofs = (uint32_t)ctx->sym[ctx->nsym-1].ofs;

    /* https://learn.microsoft.com/en-us/cpp/build/arm64-exception-handling?view=msvc-170#unwind-codes */

    /* 000xxxxx: allocate small stack with size < 512 (2^5 * 16). */
    #define ALLOC_S(size) do { \
      *p++ = ((size) / 16) & 0x1f; \
    } while(0)
    /* 01zzzzzz: save <x29,lr> pair at [sp+#Z*8], offset <= 504. */
    #define SAVE_FPLR(offs) do { \
      *p++ = 0x40 | (((offs) / 8) & 0x1f); \
    } while(0)
    /* 110100xx'xxzzzzzz: save reg x(19+#X) at [sp+#Z*8], offset <= 504 */
    #define SAVE_REG(reg, offs) do { \
      uint16_t code = 0xd000 | (((reg) - 19) << 6) | ((offs) / 8); \
      WRITE_CODE16(code); \
    } while (0)
    /* 1101110x'xxzzzzzz: save reg d(8+#X) at [sp+#Z*8], offset <= 504 */
    #define SAVE_FREG(reg, offs) do { \
      uint16_t code = 0xdc00 | (((reg) - 8) << 6) | ((offs) / 8); \
      WRITE_CODE16(code); \
    } while (0)
    /* 1101010x'xxxzzzzz: save reg x(19+#X) at [sp-(#Z+1)*8]!, pre-indexed offset >= -256 */
    #define SAVE_REG_X(reg, offs) do { \
      uint16_t code = 0xd400 | (((reg) - 19) << 5) | (((offs * -1) / 8) - 1); \
      WRITE_CODE16(code); \
    } while (0)
    /* 11100010'xxxxxxxx: set up x29 with add x29,sp,#x*8 */
    #define ADD_FP(size) do { \
      uint16_t code = 0xe200 | (((size) / 8) & 0xff); \
      WRITE_CODE16(code); \
    } while(0)
    #define WRITE_CODE16(code) do { \
      /* the 2-byte code is saved in big endian */ \
      *p++ = ((code) & 0xff00) >> 8; \
      *p++ = ((code) & 0xff); \
    } while (0)

    /* fill the codes with NOP (e3) */
    memset(codes, 0xe3, sizeof(codes));
    p = codes;
    ALLOC_S(208);
    SAVE_FPLR(192);
    ADD_FP(192);
    SAVE_REG(19, 184);
    SAVE_REG(20, 176);
    SAVE_REG(21, 168);
    SAVE_REG(22, 160);
    SAVE_REG(23, 152);
    SAVE_REG(24, 144);
    SAVE_REG(25, 136);
    SAVE_REG(26, 128);
    SAVE_REG(27, 120);
    SAVE_REG(28, 112);
    SAVE_FREG(8, 104);
    SAVE_FREG(9, 96);
    SAVE_FREG(10, 88);
    SAVE_FREG(11, 80);
    SAVE_FREG(12, 72);
    SAVE_FREG(13, 64);
    SAVE_FREG(14, 56);
    SAVE_FREG(15, 48);
    /* end of unwinding codes */
    *p++ = 0xe4;
    /* codes must be aligned on 4-byte word */
    const size_t unwindsize1 = ((p - codes) + 3) & ~3;

    ui.X = 1;
    ui.CodeWords = unwindsize1 / 4;
    ui.FunctionLength = fcofs / 4;
    owrite(ctx, &ui, sizeof(ui));
    owrite(ctx, &codes, unwindsize1);
    handler[0] = 0;
    handler[1] = 0xffffffff;
    owrite(ctx, &handler, sizeof(handler));

    memset(&ui, 0, sizeof(ui));
    memset(codes, 0xe3, sizeof(codes));

    p = codes;
    SAVE_FPLR(16);
    ADD_FP(16);
    SAVE_REG_X(19, -24);
    SAVE_REG_X(20, -32);
    *p++ = 0xe4;
    const size_t unwindsize2 = ((p - codes) + 3) & ~3;

    ui.CodeWords = unwindsize2 / 4;
    ui.FunctionLength = (uint32_t)(ctx->codesz - fcofs) / 4;
    owrite(ctx, &ui, sizeof(ui));
    owrite(ctx, &codes, unwindsize2);

    reloc.vaddr = sizeof(ui)+unwindsize1; reloc.symidx = 1+2+nrsym+2+2;
    reloc.type = PEOBJ_RELOC_ADDR32NB;
    owrite(ctx, &reloc, PEOBJ_RELOC_SIZE);
  }
#endif
#elif LJ_TARGET_X86
  /* Write .sxdata section. */
  for (i = 0; i < nrsym; i++) {
    if (!strcmp(ctx->relocsym[i], "_lj_err_unwind_win")) {
      uint32_t symidx = 1+2+i;
      owrite(ctx, &symidx, 4);
      break;
    }
  }
  if (i == nrsym) {
    fprintf(stderr, "Error: extern lj_err_unwind_win not used\n");
    exit(1);
  }
#endif

  /* Write .rdata$Z section. */
  owrite(ctx, ctx->dasm_ident, strlen(ctx->dasm_ident)+1);

  /* Write symbol table. */
  strtab = NULL;  /* 1st pass: collect string sizes. */
  for (;;) {
    strtabofs = 4;
    /* Mark as SafeSEH compliant. */
    emit_peobj_sym(ctx, "@feat.00", 1,
		   PEOBJ_SECT_ABS, PEOBJ_TYPE_NULL, PEOBJ_SCL_STATIC);

    emit_peobj_sym_sect(ctx, pesect, PEOBJ_SECT_TEXT);
    for (i = 0; i < nrsym; i++)
      emit_peobj_sym(ctx, ctx->relocsym[i], 0,
		     PEOBJ_SECT_UNDEF, PEOBJ_TYPE_FUNC, PEOBJ_SCL_EXTERN);

#if LJ_TARGET_X64 || LJ_TARGET_ARM64
    emit_peobj_sym_sect(ctx, pesect, PEOBJ_SECT_PDATA);
    emit_peobj_sym_sect(ctx, pesect, PEOBJ_SECT_XDATA);
    emit_peobj_sym(ctx, "lj_err_unwind_win", 0,
		   PEOBJ_SECT_UNDEF, PEOBJ_TYPE_FUNC, PEOBJ_SCL_EXTERN);
#elif LJ_TARGET_X86
    emit_peobj_sym_sect(ctx, pesect, PEOBJ_SECT_SXDATA);
#endif

    emit_peobj_sym(ctx, ctx->beginsym, 0,
		   PEOBJ_SECT_TEXT, PEOBJ_TYPE_NULL, PEOBJ_SCL_EXTERN);
    for (i = 0; i < ctx->nsym; i++)
      emit_peobj_sym(ctx, ctx->sym[i].name, (uint32_t)ctx->sym[i].ofs,
		     PEOBJ_SECT_TEXT, PEOBJ_TYPE_FUNC, PEOBJ_SCL_EXTERN);

    emit_peobj_sym_sect(ctx, pesect, PEOBJ_SECT_RDATA_Z);

    if (strtab)
      break;
    /* 2nd pass: alloc strtab, write syms and copy strings. */
    strtab = (char *)malloc(strtabofs);
    *(uint32_t *)strtab = (uint32_t)strtabofs;
  }

  /* Write string table. */
  owrite(ctx, strtab, strtabofs);
}

#else

void emit_peobj(BuildCtx *ctx)
{
  UNUSED(ctx);
  fprintf(stderr, "Error: no PE object support for this target\n");
  exit(1);
}

#endif
