/*
** LuaJIT VM builder: Assembler source code emitter.
** Copyright (C) 2005-2010 Mike Pall. See Copyright Notice in luajit.h
*/

#include "buildvm.h"
#include "lj_bc.h"

/* ------------------------------------------------------------------------ */

/* Emit bytes piecewise as assembler text. */
static void emit_asm_bytes(BuildCtx *ctx, uint8_t *p, int n)
{
  int i;
  for (i = 0; i < n; i++) {
    if ((i & 15) == 0)
      fprintf(ctx->fp, "\t.byte %d", p[i]);
    else
      fprintf(ctx->fp, ",%d", p[i]);
    if ((i & 15) == 15) putc('\n', ctx->fp);
  }
  if ((n & 15) != 0) putc('\n', ctx->fp);
}

/* Emit relocation */
static void emit_asm_reloc(BuildCtx *ctx, int type, const char *sym)
{
  switch (ctx->mode) {
  case BUILD_elfasm:
    if (type)
      fprintf(ctx->fp, "\t.long %s-.-4\n", sym);
    else
      fprintf(ctx->fp, "\t.long %s\n", sym);
    break;
  case BUILD_coffasm:
    fprintf(ctx->fp, "\t.def %s; .scl 3; .type 32; .endef\n", sym);
    if (type)
      fprintf(ctx->fp, "\t.long %s-.-4\n", sym);
    else
      fprintf(ctx->fp, "\t.long %s\n", sym);
    break;
  default:  /* BUILD_machasm for relative relocations handled below. */
    fprintf(ctx->fp, "\t.long %s\n", sym);
    break;
  }
}

static const char *const jccnames[] = {
  "jo", "jno", "jb", "jnb", "jz", "jnz", "jbe", "ja",
  "js", "jns", "jpe", "jpo", "jl", "jge", "jle", "jg"
};

/* Emit relocation for the incredibly stupid OSX assembler. */
static void emit_asm_reloc_mach(BuildCtx *ctx, uint8_t *cp, int n,
				const char *sym)
{
  const char *opname = NULL;
  if (--n < 0) goto err;
  if (cp[n] == 0xe8) {
    opname = "call";
  } else if (cp[n] == 0xe9) {
    opname = "jmp";
  } else if (cp[n] >= 0x80 && cp[n] <= 0x8f && n > 0 && cp[n-1] == 0x0f) {
    opname = jccnames[cp[n]-0x80];
    n--;
  } else {
err:
    fprintf(stderr, "Error: unsupported opcode for %s symbol relocation.\n",
	    sym);
    exit(1);
  }
  emit_asm_bytes(ctx, cp, n);
  fprintf(ctx->fp, "\t%s %s\n", opname, sym);
}

/* Emit an assembler label. */
static void emit_asm_label(BuildCtx *ctx, const char *name, int size, int isfunc)
{
  switch (ctx->mode) {
  case BUILD_elfasm:
    fprintf(ctx->fp,
      "\n\t.globl %s\n"
      "\t.hidden %s\n"
      "\t.type %s, @%s\n"
      "\t.size %s, %d\n"
      "%s:\n",
      name, name, name, isfunc ? "function" : "object", name, size, name);
    break;
  case BUILD_coffasm:
    fprintf(ctx->fp, "\n\t.globl %s\n", name);
    if (isfunc)
      fprintf(ctx->fp, "\t.def %s; .scl 3; .type 32; .endef\n", name);
    fprintf(ctx->fp, "%s:\n", name);
    break;
  case BUILD_machasm:
    fprintf(ctx->fp,
      "\n\t.private_extern %s\n"
      "%s:\n", name, name);
    break;
  default:
    break;
  }
}

/* Emit alignment. */
static void emit_asm_align(BuildCtx *ctx, int bits)
{
  switch (ctx->mode) {
  case BUILD_elfasm:
  case BUILD_coffasm:
    fprintf(ctx->fp, "\t.p2align %d\n", bits);
    break;
  case BUILD_machasm:
    fprintf(ctx->fp, "\t.align %d\n", bits);
    break;
  default:
    break;
  }
}

/* ------------------------------------------------------------------------ */

/* Emit assembler source code. */
void emit_asm(BuildCtx *ctx)
{
  char name[80];
  int32_t prev;
  int i, pi, rel;
#if LJ_64
  const char *symprefix = ctx->mode == BUILD_machasm ? "_" : "";
  int keepfc = 0;
#else
  const char *symprefix = ctx->mode != BUILD_elfasm ? "_" : "";
  /* Keep fastcall suffix for COFF on WIN32. */
  int keepfc = (ctx->mode == BUILD_coffasm);
#endif

  fprintf(ctx->fp, "\t.file \"buildvm_%s.dasc\"\n", ctx->dasm_arch);
  fprintf(ctx->fp, "\t.text\n");
  emit_asm_align(ctx, 4);

  sprintf(name, "%s" LABEL_ASM_BEGIN, symprefix);
  emit_asm_label(ctx, name, 0, 0);
  if (ctx->mode != BUILD_machasm)
    fprintf(ctx->fp, ".Lbegin:\n");

  i = 0;
  do {
    pi = ctx->perm[i++];
    prev = ctx->sym_ofs[pi];
  } while (prev < 0);  /* Skip the _Z symbols. */

  for (rel = 0; i <= ctx->nsym; i++) {
    int ni = ctx->perm[i];
    int32_t next = ctx->sym_ofs[ni];
    int size = (int)(next - prev);
    int32_t stop = next;
    if (pi >= ctx->npc) {
      char *p;
      sprintf(name, "%s" LABEL_PREFIX "%s", symprefix,
	      ctx->globnames[pi-ctx->npc]);
      p = strchr(name, '@');
      if (p) { if (keepfc) name[0] = '@'; else *p = '\0'; }
      emit_asm_label(ctx, name, size, 1);
#if LJ_HASJIT
    } else {
#else
    } else if (!(pi == BC_JFORI || pi == BC_JFORL || pi == BC_JITERL ||
		 pi == BC_JLOOP || pi == BC_IFORL || pi == BC_IITERL ||
		 pi == BC_ILOOP)) {
#endif
      sprintf(name, "%s" LABEL_PREFIX_BC "%s", symprefix, bc_names[pi]);
      emit_asm_label(ctx, name, size, 1);
    }
    while (rel < ctx->nreloc && ctx->reloc[rel].ofs < stop) {
      BuildReloc *r = &ctx->reloc[rel];
      int n = r->ofs - prev;
      char *p;
      sprintf(name, "%s%s", symprefix, ctx->extnames[r->sym]);
      p = strchr(name, '@');
      if (p) { if (keepfc) name[0] = '@'; else *p = '\0'; }
      if (ctx->mode == BUILD_machasm && r->type != 0) {
	emit_asm_reloc_mach(ctx, ctx->code+prev, n, name);
      } else {
	emit_asm_bytes(ctx, ctx->code+prev, n);
	emit_asm_reloc(ctx, r->type, name);
      }
      prev += n+4;
      rel++;
    }
    emit_asm_bytes(ctx, ctx->code+prev, stop-prev);
    prev = next;
    pi = ni;
  }

  fprintf(ctx->fp, "\n");
  switch (ctx->mode) {
  case BUILD_elfasm:
    fprintf(ctx->fp, "\t.section .note.GNU-stack,\"\",@progbits\n");
    /* fallthrough */
  case BUILD_coffasm:
    fprintf(ctx->fp, "\t.ident \"%s\"\n", ctx->dasm_ident);
    break;
  case BUILD_machasm:
    fprintf(ctx->fp,
      "\t.cstring\n"
      "\t.ascii \"%s\\0\"\n", ctx->dasm_ident);
    break;
  default:
    break;
  }
  fprintf(ctx->fp, "\n");
}

