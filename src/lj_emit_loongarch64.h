/*
** LoongArch instruction emitter.
** Copyright (C) 2005-2022 Mike Pall. See Copyright Notice in luajit.h
*/

static intptr_t get_k64val(ASMState *as, IRRef ref)
{
  IRIns *ir = IR(ref);
  if (ir->o == IR_KINT64) {
    return (intptr_t)ir_kint64(ir)->u64;
  } else if (ir->o == IR_KGC) {
    return (intptr_t)ir_kgc(ir);
  } else if (ir->o == IR_KPTR || ir->o == IR_KKPTR) {
    return (intptr_t)ir_kptr(ir);
  } else {
    lj_assertA(ir->o == IR_KINT || ir->o == IR_KNULL,
               "bad 64 bit const IR op %d", ir->o);
    return ir->i;  /* Sign-extended. */
  }
}

#define get_kval(as, ref)       get_k64val(as, ref)

/* -- Emit basic instructions --------------------------------------------- */

static void emit_djk(ASMState *as, LOONGIns loongi, Reg rd, Reg rj, Reg rk)
{
  *--as->mcp = loongi | LOONGF_D(rd & 0x1f) | LOONGF_J(rj & 0x1f) | LOONGF_K(rk & 0x1f);
}

#define emit_dj(as, loongi, rd, rj)         emit_djk(as, loongi, rd, rj, 0)

static void emit_di(ASMState *as, LOONGIns loongi, Reg rd, int32_t i)
{
  *--as->mcp = loongi | LOONGF_D(rd & 0x1f) | LOONGF_I20(i & 0xfffff);
}

static void emit_dji(ASMState *as, LOONGIns loongi, Reg rd, Reg rj, int32_t i)
{
  *--as->mcp = loongi | LOONGF_D(rd & 0x1f) | LOONGF_J(rj & 0x1f) | LOONGF_I(i);
}

static void emit_dju(ASMState *as, LOONGIns loongi, Reg rd, Reg rj, uint32_t u)
{
  *--as->mcp = loongi | LOONGF_D(rd & 0x1f) | LOONGF_J(rj & 0x1f) | LOONGF_I(u);
}

#define checki12(x)	LOONGF_S_OK(x, 12)
#define checku12(x)	((x) == ((x) & 0xfff))

static Reg ra_allock(ASMState *as, intptr_t k, RegSet allow);
static void ra_allockreg(ASMState *as, intptr_t k, Reg r);
static Reg ra_scratch(ASMState *as, RegSet allow);

static void emit_dj32i(ASMState *as, Reg rd, Reg rj, int32_t i)
{
  if (checki12(i)) {
    *--as->mcp = LOONGI_ADDI_D | LOONGF_D(rd) | LOONGF_J(rj) | LOONGF_I(i&0xfff);
  } else {
    emit_djk(as, LOONGI_ADD_D, rd, RID_R20, rj);
    emit_dju(as, LOONGI_ORI, RID_R20, RID_R20, i&0xfff);
    emit_di(as, LOONGI_LU12I_W, RID_R20, (i>>12)&0xfffff);
  }
}

static void emit_d16i(ASMState *as, Reg rd, int32_t i)
{
  emit_dji(as, LOONGI_SRAI_D, rd, rd, 16);
  emit_dji(as, LOONGI_ADDU16I_D, rd, RID_ZERO, (i&0xffff));
}

static void emit_djml(ASMState *as, LOONGIns loongi, Reg rd, Reg rj, uint32_t m, uint32_t l)
{
  *--as->mcp = loongi | LOONGF_D(rd & 0x1f) | LOONGF_J(rj & 0x1f) | LOONGF_I(l & 0x3f) | LOONGF_M(m & 0x3f);
}

static void emit_djka(ASMState *as, LOONGIns loongi, Reg rd, Reg rj, Reg rk, Reg ra)
{
  *--as->mcp = loongi | LOONGF_D(rd & 0x1f) | LOONGF_J(rj & 0x1f) | LOONGF_K(rk & 0x1f) | LOONGF_A(ra & 0x1f);
}

static void emit_b_bl(ASMState *as, LOONGIns loongi, uint32_t i)
{
  *--as->mcp = loongi | LOONGF_I(i & 0xffff) | ((i >> 16) & 0x3ff);
}


/* -- Emit loads/stores --------------------------------------------------- */

/* Prefer rematerialization of BASE/L from global_State over spills. */
#define emit_canremat(ref)	((ref) <= REF_BASE)


/* Load a 32 bit constant into a GPR. */
static void emit_loadi(ASMState *as, Reg r, int32_t i)
{
  emit_dj32i(as, r, RID_ZERO, i);
}

/* Load a 64 bit constant into a GPR. */
static void emit_loadu64(ASMState *as, Reg r, uint64_t u64)
{
  if (checki32((int64_t)u64)) {
    emit_dj32i(as, r, RID_ZERO, (int32_t)u64);
  } else {
      *--as->mcp = LOONGI_LU52I_D | LOONGF_D(r) | LOONGF_J(r) | LOONGF_I((u64>>52)&0xfff);
      *--as->mcp = LOONGI_LU32I_D | LOONGF_D(r) | LOONGF_I20((u64>>32)&0xfffff);
      *--as->mcp = LOONGI_ORI | LOONGF_D(r) | LOONGF_J(r) | LOONGF_I(u64&0xfff);
      *--as->mcp = LOONGI_LU12I_W | LOONGF_D(r) | LOONGF_I20((u64>>12)&0xfffff);
  }
}

#define emit_loada(as, r, addr)         emit_loadu64(as, (r), u64ptr((addr)))

/* Get/set from constant pointer. */
static void emit_lsptr(ASMState *as, LOONGIns loongi, Reg r, void *p, RegSet allow)
{
  intptr_t jgl = (intptr_t)(J2G(as->J));
  intptr_t i = (intptr_t)(p);
  Reg base;
  if ((uint32_t)(i-jgl) < 65536) {
    i = i-jgl-32768;
    base = RID_JGL;
  } else {
    base = ra_allock(as, i-(int16_t)i, allow);
  }
  if (checki12(i)) {
    emit_dji(as, loongi, r, base, i&0xfff);
  }
  else {
    /* ld.d->ldx.d, fld.d->fldx.d, ld.s->fldx.s */
    if (loongi == LOONGI_LD_D)
      loongi = LOONGI_LDX_D;
    else if (loongi == LOONGI_FLD_D)
      loongi = LOONGI_FLDX_D;
    else if (loongi == LOONGI_FLD_S)
      loongi = LOONGI_FLDX_S;
    emit_djk(as, loongi, r, base, RID_R20);

    /* move i to a GPR */
    emit_d16i(as, RID_R20, i);	// i&0xffff
  }
}

/* Load 64 bit IR constant into register. */
static void emit_loadk64(ASMState *as, Reg r, IRIns *ir)
{
  const uint64_t *k = &ir_k64(ir)->u64;
  Reg r64 = r;
  if (rset_test(RSET_FPR, r)) {
    r64 = RID_TMP;
    emit_dj(as, LOONGI_MOVGR2FR_D, r, r64);
  }
  if ((uint32_t)((intptr_t)k-(intptr_t)J2G(as->J)) < 65536)
    emit_lsptr(as, LOONGI_LD_D, r64, (void *)k, 0);	/*To copy a doubleword from a GPR to an FPR*/
  else
    emit_loadu64(as, r64, *k);
}

/* Get/set global_State fields. */
static void emit_lsglptr2(ASMState *as, LOONGIns loongi, Reg r, int32_t ofs)
{
  emit_djk(as, loongi, r, RID_JGL, RID_R20);
  emit_loadi(as, RID_R20, (ofs-32768));
}

#define emit_getgl(as, r, field) \
  emit_lsglptr2(as, LOONGI_LDX_D, (r), (int32_t)offsetof(global_State, field))
#define emit_setgl(as, r, field) \
  emit_lsglptr2(as, LOONGI_STX_D, (r), (int32_t)offsetof(global_State, field))

/* Trace number is determined from per-trace exit stubs. */
#define emit_setvmstate(as, i)		UNUSED(i)

/* -- Emit control-flow instructions -------------------------------------- */

/* Label for internal jumps. */
typedef MCode *MCLabel;

/* Return label pointing to current PC. */
#define emit_label(as)		((as)->mcp)

static void emit_branch(ASMState *as, LOONGIns loongi, Reg rj, Reg rd, MCode *target)
{
  MCode *p = as->mcp;
  ptrdiff_t delta = target - (p - 1);
  lj_assertA(((delta + 0x8000) >> 16) == 0, "branch target out of range");
  /*BEQ BNE BGE BLZ*/
  *--p = loongi | LOONGF_D(rd) | LOONGF_J(rj) | LOONGF_I(((uint32_t)delta & 0xffffu));
  as->mcp = p;
}

static void emit_branch21(ASMState *as, LOONGIns loongi, Reg rj, MCode *target)
{
  MCode *p = as->mcp;
  ptrdiff_t delta = target - (p - 1);
  lj_assertA(((delta + 0x100000) >> 21) == 0, "branch target out of range");
  *--p = loongi | LOONGF_J(rj) | LOONGF_I(((uint32_t)delta & 0xffffu))
         | (((uint32_t)delta & 0x1f0000u)>>16);		/*BEQZ BNEZ BCEQZ BCNEZ*/
  as->mcp = p;
}

static void emit_jmp(ASMState *as, MCode *target)
{
  MCode *p = as->mcp;
  ptrdiff_t delta = target - (p - 1);
  emit_b_bl(as, LOONGI_B, (delta&0x3ffffff));	/*offs 26*/
}

#define emit_move(as, dst, src) \
  emit_djk(as, LOONGI_OR, (dst), (src), RID_ZERO)

static void emit_call(ASMState *as, void *target)
{
  MCode *p = --as->mcp;
  ptrdiff_t delta = (char *)target - (char *)p;
  if (LOONGF_S_OK(delta>>2, 26)) {
    *p = LOONGI_BL | LOONGF_I((delta>>2) & 0xffff) | (((delta>>2) >> 16) & 0x3ff);
  } else {  /* Target out of range: need indirect call. */
    Reg r = ra_allock(as, (intptr_t)target, RSET_RANGE(RID_R12, RID_R19+1));
    *p = LOONGI_JIRL | LOONGF_D(RID_RA) | LOONGF_J(r) | LOONGF_I(0);
  }
}

/* -- Emit generic operations --------------------------------------------- */

/* Generic move between two regs. */
static void emit_movrr(ASMState *as, IRIns *ir, Reg dst, Reg src)
{
  if (dst < RID_MAX_GPR && src >= RID_MIN_FPR)
    emit_dj(as, irt_isnum(ir->t) ? LOONGI_MOVFR2GR_D : LOONGI_MOVFR2GR_S, dst, src);
  else if (dst < RID_MAX_GPR)
    emit_move(as, dst, src);
  else
    emit_dj(as, irt_isnum(ir->t) ? LOONGI_FMOV_D : LOONGI_FMOV_S, dst, src);
}

/* Emit an arithmetic operation with a constant operand. */
static void emit_addk(ASMState *as, Reg dest, Reg src, int32_t i, RegSet allow)
{
  if (checki12(i)) {
    emit_dji(as, LOONGI_ADDI_D, dest, src, i&0xfff);
  } else {
    Reg src2 = ra_allock(as, i, allow);
    emit_djk(as, LOONGI_ADD_D, dest, src, src2);
  }
}

static void emit_lso(ASMState *as, LOONGIns loongi, Reg dest, Reg src, int64_t i, RegSet allow)
{
  if (checki12(i)) {
    emit_dji(as, loongi, dest, src, i&0xfff);
  } else {
    LOONGIns loongk = LOONGI_NOP;
    switch (loongi) {
      case LOONGI_LD_D: loongk = LOONGI_LDX_D; break;
      case LOONGI_LD_W: loongk = LOONGI_LDX_W; break;
      case LOONGI_ST_D: loongk = LOONGI_STX_D; break;
      case LOONGI_FLD_D: loongk = LOONGI_FLDX_D; break;
      case LOONGI_FST_D: loongk = LOONGI_FSTX_D; break;
      case LOONGI_LD_B: loongk = LOONGI_LDX_B; break;
      case LOONGI_LD_BU: loongk = LOONGI_LDX_BU; break;
      case LOONGI_LD_H: loongk = LOONGI_LDX_H; break;
      case LOONGI_LD_HU: loongk = LOONGI_LDX_HU; break;
      case LOONGI_FLD_S: loongk = LOONGI_FLDX_S; break;
      default: break;
    }
    //Reg src2 = ra_allock(as, i, allow);
    Reg src2 = ra_scratch(as, allow);
    emit_djk(as, loongk, dest, src, src2);
    emit_d16i(as, src2, i);
  }
}

/* Generic load of register with base and (small) offset address. */
static void emit_loadofs(ASMState *as, IRIns *ir, Reg r, Reg base, int32_t ofs)
{
  if (r < RID_MAX_GPR) {
    emit_djk(as, irt_is64(ir->t) ? LOONGI_LDX_D : LOONGI_LDX_W, r, base, RID_R20);
  } else {
    emit_djk(as, irt_isnum(ir->t) ? LOONGI_FLDX_D : LOONGI_FLDX_S, r, base, RID_R20);
  }
  emit_d16i(as, RID_R20, ofs);
}

/* Generic store of register with base and (small) offset address. */
static void emit_storeofs(ASMState *as, IRIns *ir, Reg r, Reg base, int32_t ofs)
{
  if (r < RID_MAX_GPR) {
    emit_djk(as, irt_is64(ir->t) ? LOONGI_STX_D : LOONGI_STX_W, r, base, RID_R20);
  } else {
    emit_djk(as, irt_isnum(ir->t) ? LOONGI_FSTX_D : LOONGI_FSTX_S, (r&31), base, RID_R20);
  }
  emit_d16i(as, RID_R20, ofs);
}

/* Add offset to pointer. */
static void emit_addptr(ASMState *as, Reg r, int32_t ofs)
{
  if (ofs) {
    emit_addk(as, r, r, ofs, rset_exclude(RSET_GPR, r));
  }
}


#define emit_spsub(as, ofs)	emit_addptr(as, RID_SP, -(ofs))
