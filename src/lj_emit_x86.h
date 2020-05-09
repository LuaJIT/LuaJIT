/*
** x86/x64 instruction emitter.
** Copyright (C) 2005-2020 Mike Pall. See Copyright Notice in luajit.h
*/

/* -- Emit basic instructions --------------------------------------------- */

#define MODRM(mode, r1, r2)	((MCode)((mode)+(((r1)&7)<<3)+((r2)&7)))

#if LJ_64
#define REXRB(p, rr, rb) \
    { MCode rex = 0x40 + (((rr)>>1)&4) + (((rb)>>3)&1); \
      if (rex != 0x40) *--(p) = rex; }
#define FORCE_REX		0x200
#define REX_64			(FORCE_REX|0x080000)
#define VEX_64			0x800000
#else
#define REXRB(p, rr, rb)	((void)0)
#define FORCE_REX		0
#define REX_64			0
#define VEX_64			0
#endif
#if LJ_GC64
#define REX_GC64		REX_64
#else
#define REX_GC64		0
#endif

#define OP4B 0x4000

#define VEX_256 0x40000

/* msb is also set to c5 so we can spot a vex op in op_emit */
#define VEX2 0xc5c5

#define VEX_OP2(o, pp)	((uint32_t)(0xf8c5c5 | ((pp<<16) + (o<<24))))
#define VEX_OP3(o, pp, mode) ((uint32_t)(0x78e0c4 | (mode << 8) | ((pp<<16) + (o<<24))))

/* vvvv bits in the opcode are assumed to be set */
#define VEXOP_SETVVVV(o, rid) ((o) ^ (((rid < RID_MIN_FPR ? \
                                        rid : (rid)-RID_MIN_FPR)) << 19))

/* extract and merge the opcode,vvv,L,pp, W and set VEXMAP_0F */
#define VEX2TO3(op) ((op & 0xff7f0000) | 0xe1c4 | ((op & 0x800000) >> 8))

static int vexpp(uint32_t byte)
{
  switch (byte) {
  case 0x66:
    return VEXPP_66;
  case 0xf3:
    return VEXPP_f3;
  case 0xf2:
    return VEXPP_f2;
  default:
    return VEXPP_0f;
  }
}

static int vexmap(uint32_t byte)
{
  switch (byte & 0xffff) {
  case 0x380F:
    return VEXMAP_0F38;
  case 0x3a0f:
    return VEXMAP_0F3A;
  default:
    lua_assert((byte & 0xff) == 0x0f);
    return VEXMAP_0F;
  }
}

uint32_t sse2vex(uint32_t op, uint32_t len, uint32_t vex_w)
{
  x86Op vo = op >> 24;
  int32_t pp = vexpp((op >> ((4-len) * 8)) & 0xff);
  uint32_t mode = vexmap(op >> (len == 4 ? 8 : 16));

  if (!vex_w && (len == 2 || (len == 3 && mode == VEXMAP_0F))) {
    vo = VEX_OP2(vo, pp);
  } else {
    vo = VEX_OP3(vo, pp, mode);
    if(vex_w) 
      vo |= VEX_64;
  }
  return vo;
}

#define emit_i8(as, i)		(*--as->mcp = (MCode)(i))
#define emit_i32(as, i)		(*(int32_t *)(as->mcp-4) = (i), as->mcp -= 4)
#define emit_u32(as, u)		(*(uint32_t *)(as->mcp-4) = (u), as->mcp -= 4)

#define emit_x87op(as, xo) \
  (*(uint16_t *)(as->mcp-2) = (uint16_t)(xo), as->mcp -= 2)

/* VEX encoded op */
static MCode *emit_vop(x86Op xo, Reg rr, Reg rb, Reg rx,
                        MCode *p, int delta)
{
  int n = ((int8_t)xo)+55;
#if LJ_64
  if ((uint8_t)xo == 0xc5 && !((rb|rx) & 8)) {
    xo ^= (rr & 8) << 20;
  } else {
    if ((uint8_t)xo == 0xc5) {
      xo = VEX2TO3(xo);
      n--;
    }
    xo ^= (((rr>>1)&4)+((rx>>2)&2)+((rb>>3)&1))<<13;
  }
#else
  UNUSED(rb); UNUSED(rx);
#endif
  xo |= rr & VEX_256;
  *(uint32_t *)(p+delta-5) = (uint32_t)xo;
  p += n + delta;
  return p;
}

/* op */
static LJ_AINLINE MCode *emit_op(x86Op xo, Reg rr, Reg rb, Reg rx,
				 MCode *p, int delta)
{
  int n = (int8_t)xo;
  if ((n + 58) <= 0) { /* VEX-encoded instruction */
    return emit_vop(xo, rr, rb, rx, p, delta);
  } else if (rr & OP4B) {
    n = -5;
  }
#if defined(__GNUC__)
  if (__builtin_constant_p(xo) && n == -2)
    p[delta-2] = (MCode)(xo >> 24);
  else if (__builtin_constant_p(xo) && n == -3)
    *(uint16_t *)(p+delta-3) = (uint16_t)(xo >> 16);
  else
#endif
    *(uint32_t *)(p+delta-5) = (uint32_t)xo;
  p += n + delta;
#if LJ_64
  {
    uint32_t rex = 0x40 + ((rr>>1)&(4+(FORCE_REX>>1)))+((rx>>2)&2)+((rb>>3)&1);
    if (rex != 0x40) {
      rex |= (rr >> 16);
      if (n == -4) { *p = (MCode)rex; rex = (MCode)(xo >> 8); }
      else if (n == -5) { *p = (MCode)rex; rex = (MCode)(xo); }
      else if ((xo & 0xffffff) == 0x6600fd) { *p = (MCode)rex; rex = 0x66; }
      *--p = (MCode)rex;
    }
  }
#else
  UNUSED(rr); UNUSED(rb); UNUSED(rx);
#endif
  return p;
}

/* op + modrm */
#define emit_opm(xo, mode, rr, rb, p, delta) \
  (p[(delta)-1] = MODRM((mode), (rr), (rb)), \
   emit_op((xo), (rr), (rb), 0, (p), (delta)))

/* op + modrm + sib */
#define emit_opmx(xo, mode, scale, rr, rb, rx, p) \
  (p[-1] = MODRM((scale), (rx), (rb)), \
   p[-2] = MODRM((mode), (rr), RID_ESP), \
   emit_op((xo), (rr), (rb), (rx), (p), -1))

/* op r1, r2 */
static void emit_rr(ASMState *as, x86Op xo, Reg r1, Reg r2)
{
  MCode *p = as->mcp;
  as->mcp = emit_opm(xo, XM_REG, r1, r2, p, 0);
}

#if LJ_64 && defined(LUA_USE_ASSERT)
/* [addr] is sign-extended in x64 and must be in lower 2G (not 4G). */
static int32_t ptr2addr(const void *p)
{
  lua_assert((uintptr_t)p < (uintptr_t)0x80000000);
  return i32ptr(p);
}
#else
#define ptr2addr(p)	(i32ptr((p)))
#endif

/* op r, [base+ofs] */
static void emit_rmro(ASMState *as, x86Op xo, Reg rr, Reg rb, int32_t ofs)
{
  MCode *p = as->mcp;
  x86Mode mode;
  if (ra_hasreg(rb)) {
    if (LJ_GC64 && rb == RID_RIP) {
      mode = XM_OFS0;
      p -= 4;
      *(int32_t *)p = ofs;
    } else if (ofs == 0 && (rb&7) != RID_EBP) {
      mode = XM_OFS0;
    } else if (checki8(ofs)) {
      *--p = (MCode)ofs;
      mode = XM_OFS8;
    } else {
      p -= 4;
      *(int32_t *)p = ofs;
      mode = XM_OFS32;
    }
    if ((rb&7) == RID_ESP)
      *--p = MODRM(XM_SCALE1, RID_ESP, RID_ESP);
  } else {
    *(int32_t *)(p-4) = ofs;
#if LJ_64
    p[-5] = MODRM(XM_SCALE1, RID_ESP, RID_EBP);
    p -= 5;
    rb = RID_ESP;
#else
    p -= 4;
    rb = RID_EBP;
#endif
    mode = XM_OFS0;
  }
  as->mcp = emit_opm(xo, mode, rr, rb, p, 0);
}

/* op r, [base+idx*scale+ofs] */
static void emit_rmrxo(ASMState *as, x86Op xo, Reg rr, Reg rb, Reg rx,
		       x86Mode scale, int32_t ofs)
{
  MCode *p = as->mcp;
  x86Mode mode;
  if (ofs == 0 && (rb&7) != RID_EBP) {
    mode = XM_OFS0;
  } else if (checki8(ofs)) {
    mode = XM_OFS8;
    *--p = (MCode)ofs;
  } else {
    mode = XM_OFS32;
    p -= 4;
    *(int32_t *)p = ofs;
  }
  as->mcp = emit_opmx(xo, mode, scale, rr, rb, rx, p);
}

/* op r, i */
static void emit_gri(ASMState *as, x86Group xg, Reg rb, int32_t i)
{
  MCode *p = as->mcp;
  x86Op xo;
  if (checki8(i)) {
    *--p = (MCode)i;
    xo = XG_TOXOi8(xg);
  } else {
    p -= 4;
    *(int32_t *)p = i;
    xo = XG_TOXOi(xg);
  }
  as->mcp = emit_opm(xo, XM_REG, (Reg)(xg & 7) | (rb & REX_64), rb, p, 0);
}

/* op [base+ofs], i */
static void emit_gmroi(ASMState *as, x86Group xg, Reg rb, int32_t ofs,
		       int32_t i)
{
  x86Op xo;
  if (checki8(i)) {
    emit_i8(as, i);
    xo = XG_TOXOi8(xg);
  } else {
    emit_i32(as, i);
    xo = XG_TOXOi(xg);
  }
  emit_rmro(as, xo, (Reg)(xg & 7), rb, ofs);
}

#define emit_shifti(as, xg, r, i) \
  (emit_i8(as, (i)), emit_rr(as, XO_SHIFTi, (Reg)(xg), (r)))

/* op r, rm/mrm */
static void emit_mrm(ASMState *as, x86Op xo, Reg rr, Reg rb)
{
  MCode *p = as->mcp;
  x86Mode mode = XM_REG;
  if (rb == RID_MRM) {
    rb = as->mrm.base;
    if (rb == RID_NONE) {
      rb = RID_EBP;
      mode = XM_OFS0;
      p -= 4;
      *(int32_t *)p = as->mrm.ofs;
      if (as->mrm.idx != RID_NONE)
	goto mrmidx;
#if LJ_64
      *--p = MODRM(XM_SCALE1, RID_ESP, RID_EBP);
      rb = RID_ESP;
#endif
    } else if (LJ_GC64 && rb == RID_RIP) {
      lua_assert(as->mrm.idx == RID_NONE);
      mode = XM_OFS0;
      p -= 4;
      *(int32_t *)p = as->mrm.ofs;
    } else {
      if (as->mrm.ofs == 0 && (rb&7) != RID_EBP) {
	mode = XM_OFS0;
      } else if (checki8(as->mrm.ofs)) {
	*--p = (MCode)as->mrm.ofs;
	mode = XM_OFS8;
      } else {
	p -= 4;
	*(int32_t *)p = as->mrm.ofs;
	mode = XM_OFS32;
      }
      if (as->mrm.idx != RID_NONE) {
      mrmidx:
	as->mcp = emit_opmx(xo, mode, as->mrm.scale, rr, rb, as->mrm.idx, p);
	return;
      }
      if ((rb&7) == RID_ESP)
	*--p = MODRM(XM_SCALE1, RID_ESP, RID_ESP);
    }
  }
  as->mcp = emit_opm(xo, mode, rr, rb, p, 0);
}

/* op rm/mrm, i */
static void emit_gmrmi(ASMState *as, x86Group xg, Reg rb, int32_t i)
{
  x86Op xo;
  if (checki8(i)) {
    emit_i8(as, i);
    xo = XG_TOXOi8(xg);
  } else {
    emit_i32(as, i);
    xo = XG_TOXOi(xg);
  }
  emit_mrm(as, xo, (Reg)(xg & 7) | (rb & REX_64), (rb & ~REX_64));
}

/* -- Emit loads/stores --------------------------------------------------- */

/* mov [base+ofs], i */
static void emit_movmroi(ASMState *as, Reg base, int32_t ofs, int32_t i)
{
  emit_i32(as, i);
  emit_rmro(as, XO_MOVmi, 0, base, ofs);
}

/* mov [base+ofs], r */
#define emit_movtomro(as, r, base, ofs) \
  emit_rmro(as, XO_MOVto, (r), (base), (ofs))

/* Get/set global_State fields. */
#define emit_opgl(as, xo, r, field) \
  emit_rma(as, (xo), (r), (void *)&J2G(as->J)->field)
#define emit_getgl(as, r, field) emit_opgl(as, XO_MOV, (r)|REX_GC64, field)
#define emit_setgl(as, r, field) emit_opgl(as, XO_MOVto, (r)|REX_GC64, field)

#define emit_setvmstate(as, i) \
  (emit_i32(as, i), emit_opgl(as, XO_MOVmi, 0, vmstate))

/* mov r, i / xor r, r */
static void emit_loadi(ASMState *as, Reg r, int32_t i)
{
  /* XOR r,r is shorter, but modifies the flags. This is bad for HIOP/jcc. */
  if (i == 0 && !(LJ_32 && (IR(as->curins)->o == IR_HIOP ||
			    (as->curins+1 < as->T->nins &&
			     IR(as->curins+1)->o == IR_HIOP))) &&
		!((*as->mcp == 0x0f && (as->mcp[1] & 0xf0) == XI_JCCn) ||
		  (*as->mcp & 0xf0) == XI_JCCs)) {
    emit_rr(as, XO_ARITH(XOg_XOR), r, r);
  } else {
    MCode *p = as->mcp;
    *(int32_t *)(p-4) = i;
    p[-5] = (MCode)(XI_MOVri+(r&7));
    p -= 5;
    REXRB(p, 0, r);
    as->mcp = p;
  }
}

#if LJ_GC64
#define dispofs(as, k) \
  ((intptr_t)((uintptr_t)(k) - (uintptr_t)J2GG(as->J)->dispatch))
#define mcpofs(as, k) \
  ((intptr_t)((uintptr_t)(k) - (uintptr_t)as->mcp))
#define mctopofs(as, k) \
  ((intptr_t)((uintptr_t)(k) - (uintptr_t)as->mctop))
/* mov r, addr */
#define emit_loada(as, r, addr) \
  emit_loadu64(as, (r), (uintptr_t)(addr))
#else
/* mov r, addr */
#define emit_loada(as, r, addr) \
  emit_loadi(as, (r), ptr2addr((addr)))
#endif

#if LJ_64
/* mov r, imm64 or shorter 32 bit extended load. */
static void emit_loadu64(ASMState *as, Reg r, uint64_t u64)
{
  if (checku32(u64)) {  /* 32 bit load clears upper 32 bits. */
    emit_loadi(as, r, (int32_t)u64);
  } else if (checki32((int64_t)u64)) {  /* Sign-extended 32 bit load. */
    MCode *p = as->mcp;
    *(int32_t *)(p-4) = (int32_t)u64;
    as->mcp = emit_opm(XO_MOVmi, XM_REG, REX_64, r, p, -4);
#if LJ_GC64
  } else if (checki32(dispofs(as, u64))) {
    emit_rmro(as, XO_LEA, r|REX_64, RID_DISPATCH, (int32_t)dispofs(as, u64));
  } else if (checki32(mcpofs(as, u64)) && checki32(mctopofs(as, u64))) {
    /* Since as->realign assumes the code size doesn't change, check
    ** RIP-relative addressing reachability for both as->mcp and as->mctop.
    */
    emit_rmro(as, XO_LEA, r|REX_64, RID_RIP, (int32_t)mcpofs(as, u64));
#endif
  } else {  /* Full-size 64 bit load. */
    MCode *p = as->mcp;
    *(uint64_t *)(p-8) = u64;
    p[-9] = (MCode)(XI_MOVri+(r&7));
    p[-10] = 0x48 + ((r>>3)&1);
    p -= 10;
    as->mcp = p;
  }
}
#endif

static void emit_loadisp(ASMState *as)
{
  uint64_t dispaddr = (uintptr_t)J2GG(as->J)->dispatch;
  Reg ra = RID_DISPATCH;
  if (checku32(dispaddr)) {
    emit_loadi(as, ra, (int32_t)dispaddr);
  } else {  /* Full-size 64 bit load. */
    MCode *p = as->mcp;
    *(uint64_t *)(p-8) = dispaddr;
    p[-9] = (MCode)(XI_MOVri+(ra&7));
    p[-10] = 0x48 + ((ra>>3)&1);
    p -= 10;
    as->mcp = p;
  }
}

/* op r, [addr] */
static void emit_rma(ASMState *as, x86Op xo, Reg rr, const void *addr)
{
#if LJ_GC64
  if (checki32(dispofs(as, addr))) {
    emit_rmro(as, xo, rr, RID_DISPATCH, (int32_t)dispofs(as, addr));
  } else if (checki32(mcpofs(as, addr)) && checki32(mctopofs(as, addr))) {
    emit_rmro(as, xo, rr, RID_RIP, (int32_t)mcpofs(as, addr));
  } else if (!checki32((intptr_t)addr)) {
    Reg ra = (rr & 15);
    if (xo != XO_MOV) {
      /* We can't allocate a register here. Use and restore DISPATCH. Ugly. */
      uint64_t dispaddr = (uintptr_t)J2GG(as->J)->dispatch;
      uint8_t i8 = xo == XO_GROUP3b ? *as->mcp++ : 0;
      ra = RID_DISPATCH;
      if (checku32(dispaddr)) {
	emit_loadi(as, ra, (int32_t)dispaddr);
      } else {  /* Full-size 64 bit load. */
	MCode *p = as->mcp;
	*(uint64_t *)(p-8) = dispaddr;
	p[-9] = (MCode)(XI_MOVri+(ra&7));
	p[-10] = 0x48 + ((ra>>3)&1);
	p -= 10;
	as->mcp = p;
      }
      if (xo == XO_GROUP3b) emit_i8(as, i8);
    }
    emit_rmro(as, xo, rr, ra, 0);
    emit_loadu64(as, ra, (uintptr_t)addr);
  } else
#endif
  {
    MCode *p = as->mcp;
    *(int32_t *)(p-4) = ptr2addr(addr);
#if LJ_64
    p[-5] = MODRM(XM_SCALE1, RID_ESP, RID_EBP);
    as->mcp = emit_opm(xo, XM_OFS0, rr, RID_ESP, p, -5);
#else
    as->mcp = emit_opm(xo, XM_OFS0, rr, RID_EBP, p, -4);
#endif
  }
}

/* Load 64 bit IR constant into register. */
static void emit_loadk64(ASMState *as, Reg r, IRIns *ir)
{
  Reg r64;
  x86Op xo;
  const uint64_t *k = &ir_k64(ir)->u64;
  if (rset_test(RSET_FPR, r)) {
    r64 = r;
    xo = XO_MOVSD;
  } else {
    r64 = r | REX_64;
    xo = XO_MOV;
  }
  if (*k == 0) {
    emit_rr(as, rset_test(RSET_FPR, r) ? XO_XORPS : XO_ARITH(XOg_XOR), r, r);
#if LJ_GC64
  } else if (checki32((intptr_t)k) || checki32(dispofs(as, k)) ||
	     (checki32(mcpofs(as, k)) && checki32(mctopofs(as, k)))) {
    emit_rma(as, xo, r64, k);
  } else {
    if (ir->i) {
      lua_assert(*k == *(uint64_t*)(as->mctop - ir->i));
    } else if (as->curins <= as->stopins && rset_test(RSET_GPR, r)) {
      emit_loadu64(as, r, *k);
      return;
    } else {
      /* If all else fails, add the FP constant at the MCode area bottom. */
      while ((uintptr_t)as->mcbot & 7) *as->mcbot++ = XI_INT3;
      *(uint64_t *)as->mcbot = *k;
      ir->i = (int32_t)(as->mctop - as->mcbot);
      as->mcbot += 8;
      as->mclim = as->mcbot + MCLIM_REDZONE;
      lj_mcode_commitbot(as->J, as->mcbot);
    }
    emit_rmro(as, xo, r64, RID_RIP, (int32_t)mcpofs(as, as->mctop - ir->i));
#else
  } else {
    emit_rma(as, xo, r64, k);
#endif
  }
}

static void emit_push(ASMState *as, Reg r)
{
  if (r < 8) {
    *--as->mcp = XI_PUSH + r;
  } else {
    *--as->mcp = XI_PUSH + r;
    *--as->mcp = 0x41;
  }
}

static void emit_pop(ASMState *as, Reg r)
{
  if (r < 8) {
    *--as->mcp = XI_POP + r;
  } else {
    *--as->mcp = XI_POP + r;
    *--as->mcp = 0x41;
  }
}

/* -- Emit control-flow instructions -------------------------------------- */

/* Label for short jumps. */
typedef MCode *MCLabel;

#if LJ_32 && LJ_HASFFI
/* jmp short target */
static void emit_sjmp(ASMState *as, MCLabel target)
{
  MCode *p = as->mcp;
  ptrdiff_t delta = target - p;
  lua_assert(delta == (int8_t)delta);
  p[-1] = (MCode)(int8_t)delta;
  p[-2] = XI_JMPs;
  as->mcp = p - 2;
}
#endif

/* jcc short target */
static void emit_sjcc(ASMState *as, int cc, MCLabel target)
{
  MCode *p = as->mcp;
  ptrdiff_t delta = target - p;
  lua_assert(delta == (int8_t)delta);
  p[-1] = (MCode)(int8_t)delta;
  p[-2] = (MCode)(XI_JCCs+(cc&15));
  as->mcp = p - 2;
}

/* jcc short (pending target) */
static MCLabel emit_sjcc_label(ASMState *as, int cc)
{
  MCode *p = as->mcp;
  p[-1] = 0;
  p[-2] = (MCode)(XI_JCCs+(cc&15));
  as->mcp = p - 2;
  return p;
}

/* Fixup jcc short target. */
static void emit_sfixup(ASMState *as, MCLabel source)
{
  source[-1] = (MCode)(as->mcp-source);
}

/* Return label pointing to current PC. */
#define emit_label(as)		((as)->mcp)

/* Compute relative 32 bit offset for jump and call instructions. */
static LJ_AINLINE int32_t jmprel(MCode *p, MCode *target)
{
  ptrdiff_t delta = target - p;
  lua_assert(delta == (int32_t)delta);
  return (int32_t)delta;
}

/* jcc target */
static void emit_jcc(ASMState *as, int cc, MCode *target)
{
  MCode *p = as->mcp;
  *(int32_t *)(p-4) = jmprel(p, target);
  p[-5] = (MCode)(XI_JCCn+(cc&15));
  p[-6] = 0x0f;
  as->mcp = p - 6;
}

/* jmp target */
static void emit_jmp(ASMState *as, MCode *target)
{
  MCode *p = as->mcp;
  *(int32_t *)(p-4) = jmprel(p, target);
  p[-5] = XI_JMP;
  as->mcp = p - 5;
}

/* call target */
static void emit_call_(ASMState *as, MCode *target, Reg temp)
{
  MCode *p = as->mcp;
#if LJ_64
  if (target-p != (int32_t)(target-p)) {
    /* Assumes RID_RET is never an argument to calls and always clobbered. */
    if (temp == RID_NONE) temp = RID_RET;
    emit_rr(as, XO_GROUP5, XOg_CALL, temp);
    emit_loadu64(as, temp, (uint64_t)target);
    return;
  }
#endif
  *(int32_t *)(p-4) = jmprel(p, target);
  p[-5] = XI_CALL;
  as->mcp = p - 5;
}

#define emit_call(as, f)	emit_call_(as, (MCode *)(void *)(f), RID_NONE)

/* -- Emit generic operations --------------------------------------------- */

/* Use 64 bit operations to handle 64 bit IR types. */
#if LJ_64
#define REX_64IR(ir, r)		((r) + (irt_is64((ir)->t) ? REX_64 : 0))
#define VEX_64IR(ir, r)		((r) + (irt_is64((ir)->t) ? VEX_64 : 0))
#else
#define REX_64IR(ir, r)		(r)
#define VEX_64IR(ir, r)		(r)
#endif

/* Generic move between two regs. */
static void emit_movrr(ASMState *as, IRIns *ir, Reg dst, Reg src)
{
  UNUSED(ir);
  if (dst < RID_MAX_GPR)
    emit_rr(as, XO_MOV, REX_64IR(ir, dst), src);
  else
    emit_rr(as, XO_MOVAPS, dst, src);
}

#define emit_loadofs(as, ir, r, base, ofs) \
    emit_loadofsirt(as, irt_type(ir->t), r, base, ofs)

/* Generic load of register with base and (small) offset address. */
static void emit_loadofsirt(ASMState *as, IRType irt, Reg r, Reg base, int32_t ofs)
{
  if (r < RID_MAX_GPR) {
    emit_rmro(as, XO_MOV, r | ((LJ_64 && ((IRT_IS64 >> irt) & 1)) ? REX_64 : 0),
              base, ofs);
  } else {
    emit_rmro(as, irt == IRT_NUM ? XO_MOVSD : XO_MOVSS, r, base, ofs);
  }
}

#define emit_storeofs(as, ir, r, base, ofs) \
    emit_storeofsirt(as, irt_type(ir->t), r, base, ofs)

/* Generic store of register with base and (small) offset address. */
static void emit_storeofsirt(ASMState *as, IRType irt, Reg r, Reg base, int32_t ofs)
{
  if (r < RID_MAX_GPR) {
    emit_rmro(as, XO_MOVto, r | ((LJ_64 && ((IRT_IS64 >> irt) & 1)) ? REX_64 : 0),
              base, ofs);
  } else {
    emit_rmro(as, irt == IRT_NUM ? XO_MOVSDto : XO_MOVSSto, r, base, ofs);
  }
}

/* Add offset to pointer. */
static void emit_addptr(ASMState *as, Reg r, int32_t ofs)
{
  if (ofs) {
    if ((as->flags & JIT_F_LEA_AGU))
      emit_rmro(as, XO_LEA, r|REX_GC64, r, ofs);
    else
      emit_gri(as, XG_ARITHi(XOg_ADD), r|REX_GC64, ofs);
  }
}

#define emit_spsub(as, ofs)	emit_addptr(as, RID_ESP|REX_64, -(ofs))

/* Prefer rematerialization of BASE/L from global_State over spills. */
#define emit_canremat(ref)	((ref) <= REF_BASE)

#if LJ_HASINTRINSICS

#if LJ_64
#define NEEDSFP 0
#else
#define NEEDSFP 1
#endif

#define SPILLSTART (2*sizeof(intptr_t))
#define TEMPSPILL (1*sizeof(intptr_t))
#define CONTEXTSPILL (0)


static MCode* emit_intrins(ASMState *as, CIntrinsic *intrins, Reg r1, 
                           uintptr_t r2, Reg r3)
{
  uint32_t regmode = intrin_regmode(intrins);
  if (regmode) {
    if (regmode == DYNREG_OPEXT) {
      r2 = intrin_getopextb(intrins);
    }

    /* force 64 bit operands */
    if (intrins->flags & INTRINSFLAG_REXW) {
      r2 |= REX_64;
    }

    if (intrins->flags & INTRINSFLAG_IMMB) {
      *--as->mcp = intrins->immb;
    }
    /* Tell emit_op the opcode is 4 bytes long */
    if (intrins->flags & INTRINSFLAG_LARGEOP) {
      r2 |= OP4B;
    }

    if (intrins->flags & INTRINSFLAG_VEX) {
      x86Op op = intrins->opcode;
      if (r3 != RID_NONE) {
        op = VEXOP_SETVVVV(op, r3);
      } 
      emit_mrm(as, op, (Reg)r2, r1);
    } else {
      emit_mrm(as, intrins->opcode, (Reg)r2, r1);
    }

    if (intrins->flags & INTRINSFLAG_PREFIX) {
      *--as->mcp = intrins->prefix;
    }

    checkmclim(as);
  } else if (intrins->flags & INTRINSFLAG_CALLED) {
    lua_assert(r2);
    emit_call_(as, (MCode*)r2, r1);
    return NULL;
  } else {
    AsmHeader *hdr = ((AsmHeader*)r2)-1;
    lua_assert((hdr->asmofs != 0 || hdr->asmofs < hdr->totalzs));

    /* Directly copy the unmodified machine code of the intrinsic in */
    asm_mcode(as, ((char*)r2)+hdr->asmofs, hdr->asmsz);
  }
  return as->mcp;
}

static int lj_popcnt(uint32_t i)
{
  i = i - ((i >> 1) & 0x55555555);
  i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
  return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

#define align16(n) ((n + 16) & ~(16 - 1))

static int32_t alignsp(int32_t spadj, RegSet savereg) {
  int32_t gprsave = lj_popcnt(savereg & RSET_GPR_DISPATCH) * sizeof(intptr_t);

  if (NEEDSFP && rset_test(savereg, RID_EBP)) {
    gprsave -= sizeof(intptr_t);
  }
/* TODO: use shadow space/red zone on x64 to skip setting stack frame */
  spadj += gprsave;

#if LJ_64
#if LJ_ABI_WIN
  if (savereg & RSET_FPR) {
    spadj += lj_popcnt(savereg & RSET_FPR) * 16;
    /* Add some slack in case the starting fpr save offset needs rounding up */
    spadj += 8;
  }
#endif
  if (spadj == 0)
    return 0;

  spadj = align16(spadj);
  /* No ebp pushed so the stack starts aligned to 8 bytes */
  if (!NEEDSFP)spadj += 8;
#endif

  return spadj;
}

static void emit_prologue(ASMState *as, int spadj, RegSet modregs)
{
  int32_t offset, i;
  RegSet savereg = modregs & ~RSET_SCRATCH;

  /* Save volatile registers after requested stack space */
  offset = spadj;

  /* save non scratch registers */
  for (i = RID_MIN_GPR; i < RID_MAX_GPR; i++) {
    if (rset_test(savereg, i) && (i != RID_EBP || !NEEDSFP)) {
      emit_rmro(as, XO_MOVto, i|REX_64, RID_SP, offset);
      checkmclim(as);
      offset += sizeof(intptr_t);
    }
  }

#if LJ_ABI_WIN && LJ_64
  offset = align16(offset);
  for (i = RID_MIN_FPR; i < RID_MAX_FPR; i++) {
    if (rset_test(savereg, i)) {
      emit_rmro(as, XO_MOVAPSto, i|REX_64, RID_SP, offset);
      checkmclim(as);
      offset += 16;
    }
  }
#endif
  spadj = alignsp(spadj, savereg);

  if (spadj) {
    emit_spsub(as, spadj);
  }

  if (NEEDSFP) {
    emit_rr(as, XO_MOV, RID_EBP|REX_64, RID_ESP);
    emit_push(as, RID_EBP);
  }
}

static void emit_epilogue(ASMState *as, int spadj, RegSet modregs, int32_t ret)
{
  int32_t offset, i;
  RegSet savereg = modregs & ~RSET_SCRATCH;
  checkmclim(as);

  *--as->mcp = XI_RET;
  if (NEEDSFP)
    emit_pop(as, RID_EBP);
  /* Save volatile registers after requested stack space */
  offset = spadj;
  
  spadj = alignsp(spadj, savereg);

  if (spadj != 0) {
    emit_spsub(as, -spadj);
  }
 
  as->mcp -= 4;
  *(int32_t *)as->mcp = ret;
  *--as->mcp = XI_MOVri + RID_RET;

  if (savereg == RSET_EMPTY) {
    return;
  }

  /* Restore non scratch registers */
  for (i = RID_MIN_GPR; i < RID_MAX_GPR; i++) {
    if (rset_test(savereg, i) && (i != RID_EBP || !NEEDSFP)) {
      emit_rmro(as, XO_MOV, i|REX_64, RID_SP, offset);
      checkmclim(as);
      offset += sizeof(intptr_t);
    }
  }

#if LJ_ABI_WIN && LJ_64
  offset = align16(offset);
  for (i = RID_MIN_FPR; i < RID_MAX_FPR; i++) {
    if (rset_test(savereg, i)) {
      emit_rmro(as, XO_MOVAPS, i|REX_64, RID_SP, offset);
      checkmclim(as);
      offset += 16;
    }
  }
#endif
}

/* Trys to pick free register from the scratch or modified set first 
 * before resorting to register that will need tobe saved. 
 */
static Reg intrinsic_scratch(ASMState *as, RegSet allow)
{
  RegSet pick = (as->freeset & allow) & (as->modset|RSET_SCRATCH);
  Reg r;

  if (!pick) {
    pick = as->freeset & allow;
    
    if (pick == 0) {
      /* No free registers */
      lj_trace_err(as->J, LJ_TRERR_BADRA);
    }
    
    r = rset_pickbot(pick);
    as->modset |= RID2RSET(r);
  } else {
    r = rset_pickbot(pick);
  }

  /* start from the bottom where most of the non spilled registers are */
  return r;
}

static void emit_tvload_gco(ASMState *as, Reg base, Reg temp, int32_t ofs)
{
#if LJ_GC64
  emit_shifti(as, XOg_SHR|REX_64, temp, 17);

  as->mrm.base = base;
  as->mrm.idx = RID_NONE;
  as->mrm.ofs = ofs;
  if ((as->flags & JIT_F_BMI2))
  {
    emit_i8(as, 47);
    emit_mrm(as, XV_RORX|VEX_64, temp, RID_MRM);
  }
  else
  {
    emit_shifti(as, XOg_ROR|REX_64, temp, 47);
    emit_mrm(as, XO_MOV, temp|REX_64, RID_MRM);
  }
#else
  emit_rmro(as, XO_MOV, temp, base, ofs);
#endif
}

static void emit_savegpr(ASMState *as, Reg reg, Reg base, int ofs)
{
  Reg temp, r = reg_rid(reg);
  uint32_t kind = reg_kind(reg);
  lua_assert(r < RID_NUM_GPR);

  if (kind == REGKIND_GPRI32) {
#if LJ_DUALNUM
    emit_i32(as, LJ_TISNUM);
    emit_rmro(as, XO_MOVmi, 0, base, ofs+4);
    emit_rmro(as, XO_MOVto, r, base, ofs);
#else
    temp = intrinsic_scratch(as, RSET_FPR);
    emit_rmro(as, XO_MOVSDto, temp, base, ofs);
    emit_mrm(as, XO_CVTSI2SD, temp, r);
#endif
    return;
  }

  if (kind == REGKIND_GPR64) {
    r |= REX_64;
  }

  temp = intrinsic_scratch(as, RSET_GPR);
  /* Save the register into a cdata who's pointer is inside a TValue on the Lua stack */
  emit_rmro(as, XO_MOVto, r, temp, sizeof(GCcdata));
  emit_tvload_gco(as, base, temp, ofs);
}

static void emit_loadfpr(ASMState *as, uint32_t reg, Reg base, int ofs)
{
  x86Op op = XO_MOVSD;
  Reg r = reg_rid(reg)-RID_MIN_FPR;
  uint32_t kind = reg_kind(reg);
  lua_assert(r < RID_NUM_FPR);

  switch (kind) {
  case REGKIND_FPR64:
    op = XO_MOVSD;
    break;
  case REGKIND_FPR32:
    op = XO_MOVSS;
    break;
  case REGKIND_V128:
    op = XO_MOVUPS;
    break;
  case REGKIND_V256:
    op = XV_MOVUPS;
    r |= VEX_256;
    break;
  }

  if (!rk_isvec(kind)) {
    emit_rmro(as, op, r, base, ofs);
  } else {
    Reg temp = intrinsic_scratch(as, RSET_GPR);
    emit_rmro(as, op, r, temp, 0);

    /* Load a pointer to the vector out of the input context */
    emit_rmro(as, XO_MOV, temp|REX_64, base, ofs);
  }
}

static void emit_savefpr(ASMState *as, Reg reg, Reg base, int ofs)
{
  x86Op op;
  Reg r = reg_rid(reg)-RID_MIN_FPR;
  uint32_t kind = reg_kind(reg) & 3;
  lua_assert(r < RID_NUM_FPR);

  switch (kind) {
  case REGKIND_FPR64:
    op = XO_MOVSDto;
    break;
  case REGKIND_FPR32:
    op = XO_MOVSDto;
    break;
  case REGKIND_V128:
    op = XO_MOVUPSto;
    break;
  case REGKIND_V256:
    op = XV_MOVUPSto;
    r |= VEX_256;
    break;
  }

  if (!rk_isvec(kind)) {
    emit_rmro(as, op, r, base, ofs);
    if (kind == REGKIND_FPR32) {
      emit_mrm(as, XO_CVTSS2SD, r, r);
    }
  } else {
    Reg temp = intrinsic_scratch(as, RSET_GPR);

    /* Save the register into a cdata who's pointer is inside a TValue on the Lua stack */
    emit_rmro(as, op, r, temp, sizeof(GCcdata));
    emit_tvload_gco(as, base, temp, ofs);
  }
}

#endif

