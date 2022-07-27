/*
** LoongArch IR assembler (SSA IR -> machine code).
** Copyright (C) 2005-2022 Mike Pall. See Copyright Notice in luajit.h
*/

/* -- Register allocator extensions --------------------------------------- */

/* Allocate a register with a hint. */
static Reg ra_hintalloc(ASMState *as, IRRef ref, Reg hint, RegSet allow)
{
  Reg r = IR(ref)->r;
  if (ra_noreg(r)) {
    if (!ra_hashint(r) && !iscrossref(as, ref))
      ra_sethint(IR(ref)->r, hint);  /* Propagate register hint. */
    r = ra_allocref(as, ref, allow);
  }
  ra_noweak(as, r);
  return r;
}

/* Allocate two source registers for three-operand instructions. */
static Reg ra_alloc2(ASMState *as, IRIns *ir, RegSet allow)
{
  IRIns *irl = IR(ir->op1), *irr = IR(ir->op2);
  Reg left = irl->r, right = irr->r;
  if (ra_hasreg(left)) {
    ra_noweak(as, left);
    if (ra_noreg(right))
      right = ra_allocref(as, ir->op2, rset_exclude(allow, left));
    else
      ra_noweak(as, right);
  } else if (ra_hasreg(right)) {
    ra_noweak(as, right);
    left = ra_allocref(as, ir->op1, rset_exclude(allow, right));
  } else if (ra_hashint(right)) {
    right = ra_allocref(as, ir->op2, allow);
    left = ra_alloc1(as, ir->op1, rset_exclude(allow, right));
  } else {
    left = ra_allocref(as, ir->op1, allow);
    right = ra_alloc1(as, ir->op2, rset_exclude(allow, left));
  }
  return left | (right << 8);
}

/* -- Guard handling ------------------------------------------------------ */

/* Setup exit stub after the end of each trace. */
static void asm_exitstub_setup(ASMState *as)
{
  MCode *mxp = as->mctop;
  if (as->mcp == mxp)
    --as->mcp;
  /* st.w TMP, sp, 0; li TMP, traceno; jirl ->vm_exit_handler;*/
  *--mxp = LOONGI_JIRL | RID_R0 | LOONGF_J(RID_R20) | 0<<10;
  emit_dj32i(as, RID_TMP, RID_ZERO, as->T->traceno);
  *--mxp = *as->mcp;
  *--mxp = LOONGI_LU52I_D | RID_R20 | LOONGF_J(RID_R20)
            | LOONGF_I((((uintptr_t)(void *)lj_vm_exit_handler)>>52)&0xfff);
  *--mxp = LOONGI_LU32I_D | RID_R20
            | LOONGF_I20((((uintptr_t)(void *)lj_vm_exit_handler)>>32)&0xfffff);
  *--mxp = LOONGI_ORI | RID_R20 | LOONGF_J(RID_R20)
            | LOONGF_I(((uintptr_t)(void *)lj_vm_exit_handler)&0xfff);
  *--mxp = LOONGI_LU12I_W | RID_R20
            | LOONGF_I20((((uintptr_t)(void *)lj_vm_exit_handler)&0xfffff000)>>12);
  *--mxp = LOONGI_ST_W | LOONGF_D(RID_TMP) | LOONGF_J(RID_SP);
  as->mctop = mxp;
}

/* Keep this in-sync with exitstub_trace_addr(). */
#define asm_exitstub_addr(as)	((as)->mctop)

/* Emit conditional branch to exit for guard. */
static void asm_guard(ASMState *as, LOONGIns loongi, Reg rj, Reg rd)
{
  MCode *target = asm_exitstub_addr(as);
  MCode *p = as->mcp;
  if (LJ_UNLIKELY(p == as->invmcp)) {
    as->invmcp = NULL;
    as->loopinv = 1;
    as->mcp = p;
    loongi = loongi ^ ((loongi>>28) == 4 ? 0x00000100u : 0x04000000u);  /* Invert cond. BEQ BNE BGE BLZ*/
    target = p - 1;  /* Patch target later in asm_loop_fixup. */
  }
    emit_branch(as, loongi, rj, rd, target);
    emit_dji(as, LOONGI_ADDI_D, RID_TMP, RID_ZERO, as->snapno);
}

static void asm_guard21(ASMState *as, LOONGIns loongi, Reg rj)
{
  MCode *target = asm_exitstub_addr(as);
  MCode *p = as->mcp;
  if (LJ_UNLIKELY(p == as->invmcp)) {
    as->invmcp = NULL;
    as->loopinv = 1;
    as->mcp = p;
    loongi = loongi ^ ((loongi>>28) == 4 ? 0x00000100u : 0x04000000u);  /* Invert cond. BCEQZ BCNEZ*/
    target = p - 1;  /* Patch target later in asm_loop_fixup. */
  }
    emit_branch21(as, loongi, rj, target);
    emit_dji(as, LOONGI_ADDI_D, RID_TMP, RID_ZERO, as->snapno);
}

/* -- Operand fusion ------------------------------------------------------ */

/* Limit linear search to this distance. Avoids O(n^2) behavior. */
#define CONFLICT_SEARCH_LIM	31

/* Check if there's no conflicting instruction between curins and ref. */
static int noconflict(ASMState *as, IRRef ref, IROp conflict)
{
  IRIns *ir = as->ir;
  IRRef i = as->curins;
  if (i > ref + CONFLICT_SEARCH_LIM)
    return 0;  /* Give up, ref is too far away. */
  while (--i > ref)
    if (ir[i].o == conflict)
      return 0;  /* Conflict found. */
  return 1;  /* Ok, no conflict. */
}

/* Fuse the array base of colocated arrays. */
static int32_t asm_fuseabase(ASMState *as, IRRef ref)
{
  IRIns *ir = IR(ref);
  if (ir->o == IR_TNEW && ir->op1 <= LJ_MAX_COLOSIZE &&
      !neverfuse(as) && noconflict(as, ref, IR_NEWREF))
    return (int32_t)sizeof(GCtab);
  return 0;
}

/* Fuse array/hash/upvalue reference into register+offset operand. */
static Reg asm_fuseahuref(ASMState *as, IRRef ref, int32_t *ofsp, RegSet allow)
{
  IRIns *ir = IR(ref);
  if (ra_noreg(ir->r)) {
    if (ir->o == IR_AREF) {
      if (mayfuse(as, ref)) {
	if (irref_isk(ir->op2)) {
	  IRRef tab = IR(ir->op1)->op1;
	  int32_t ofs = asm_fuseabase(as, tab);
	  IRRef refa = ofs ? tab : ir->op1;
	  ofs += 8*IR(ir->op2)->i;
	  if (checki16(ofs)) {
	    *ofsp = ofs;
	    return ra_alloc1(as, refa, allow);
	  }
	}
      }
    } else if (ir->o == IR_HREFK) {
      if (mayfuse(as, ref)) {
	int32_t ofs = (int32_t)(IR(ir->op2)->op2 * sizeof(Node));
	if (checki16(ofs)) {
	  *ofsp = ofs;
	  return ra_alloc1(as, ir->op1, allow);
	}
      }
    } else if (ir->o == IR_UREFC) {
      if (irref_isk(ir->op1)) {
	GCfunc *fn = ir_kfunc(IR(ir->op1));
	intptr_t ofs = (intptr_t)&gcref(fn->l.uvptr[(ir->op2 >> 8)])->uv.tv;
	intptr_t jgl = (intptr_t)J2G(as->J);
	if ((uintptr_t)(ofs-jgl) < 65536) {
	  *ofsp = ofs-jgl-32768;
	  return RID_JGL;
	} else {
	  *ofsp = (int16_t)ofs;
	  return ra_allock(as, ofs-(int16_t)ofs, allow);
	}
      }
    } else if (ir->o == IR_TMPREF) {
      *ofsp = (int32_t)(offsetof(global_State, tmptv)-32768);
      return RID_JGL;
    }
  }
  *ofsp = 0;
  return ra_alloc1(as, ref, allow);
}

/* Fuse XLOAD/XSTORE reference into load/store operand. */
static void asm_fusexref(ASMState *as, LOONGIns loongi, Reg rd, IRRef ref,
			 RegSet allow, int32_t ofs)
{
  IRIns *ir = IR(ref);
  Reg base;
  if (ra_noreg(ir->r) && canfuse(as, ir)) {
    intptr_t ofs2;
    if (ir->o == IR_ADD) {
      if (irref_isk(ir->op2) && (ofs2 = ofs + get_kval(as, ir->op2),
				 checki12(ofs2))) {
	ref = ir->op1;
	ofs = (int32_t)ofs2;
      }
    } else if (ir->o == IR_STRREF) {
      ofs2 = 4096;
      lj_assertA(ofs == 0, "bad usage");
      ofs = (int32_t)sizeof(GCstr);
      if (irref_isk(ir->op2)) {
	ofs2 = ofs + get_kval(as, ir->op2);
	ref = ir->op1;
      } else if (irref_isk(ir->op1)) {
	ofs2 = ofs + get_kval(as, ir->op1);
	ref = ir->op2;
      }
      if (!checki12(ofs2)) {
        /* NYI: Fuse ADD with constant. */
        Reg right, left = ra_alloc2(as, ir, allow);
        right = (left >> 8); left &= 255;
        emit_dji(as, loongi, rd, RID_TMP, ofs&0xfff);
        emit_djk(as, LOONGI_ADD_D, RID_TMP, left, right);
        return;
      }
      ofs = ofs2;
    }
  }
  base = ra_alloc1(as, ref, allow);
  emit_dji(as, loongi, rd, base, ofs&0xfff);
}

/* Fuse FP multiply-add/sub. */

static int asm_fusemadd(ASMState *as, IRIns *ir, LOONGIns loongi, LOONGIns loongir)
{
  IRRef lref = ir->op1, rref = ir->op2;
  IRIns *irm;
  if (lref != rref &&
      ((mayfuse(as, lref) && (irm = IR(lref), irm->o == IR_MUL) &&
       ra_noreg(irm->r)) ||
       (mayfuse(as, rref) && (irm = IR(rref), irm->o == IR_MUL) &&
       (rref = lref, loongi = loongir, ra_noreg(irm->r))))) {
    Reg dest = ra_dest(as, ir, RSET_FPR);
    Reg add = ra_hintalloc(as, rref, dest, RSET_FPR);
    Reg left = ra_alloc2(as, irm, rset_exclude(rset_exclude(RSET_FPR, dest), add));
    Reg right = (left >> 8); left &= 255;
    emit_djka(as, loongi, (dest & 0x1f), (left & 0x1f), (right & 0x1f), (add & 0x1f));
    return 1;
  }
  return 0;
}
/* -- Calls --------------------------------------------------------------- */

/* Generate a call to a C function. */
static void asm_gencall(ASMState *as, const CCallInfo *ci, IRRef *args)
{
  uint32_t n, nargs = CCI_XNARGS(ci);
  int32_t ofs = 0;
  Reg gpr, fpr = REGARG_FIRSTFPR;
  if ((void *)ci->func)
    emit_call(as, (void *)ci->func);
  for (gpr = REGARG_FIRSTGPR; gpr <= REGARG_LASTGPR; gpr++)
    as->cost[gpr] = REGCOST(~0u, ASMREF_L);
  gpr = REGARG_FIRSTGPR;
  for (n = 0; n < nargs; n++) { /* Setup args. */
    IRRef ref = args[n];
    if (ref) {
      IRIns *ir = IR(ref);
      if (irt_isfp(ir->t) && (n == 0 || !(ci->flags & CCI_VARARG))) {
        if (fpr <= REGARG_LASTFPR) {
	  lj_assertA(rset_test(as->freeset, fpr),
	             "reg %d not free", fpr);  /* Must have been evicted. */
          ra_leftov(as, fpr, ref);
	  fpr++;
	} else if (gpr <= REGARG_LASTGPR) {
	  lj_assertA(rset_test(as->freeset, gpr),
	             "reg %d not free", gpr);  /* Must have been evicted. */
          ra_leftov(as, gpr, ref);
	  gpr++;
	} else {
	  Reg r = ra_alloc1(as, ref, RSET_FPR);
	  emit_spstore(as, ir, r, ofs);
	  ofs += 8;
	}
      } else {
        if (gpr <= REGARG_LASTGPR) {
	  lj_assertA(rset_test(as->freeset, gpr),
	             "reg %d not free", gpr);  /* Must have been evicted. */
          ra_leftov(as, gpr, ref);
	  gpr++;
	} else {
	  Reg r = ra_alloc1(as, ref, RSET_GPR);
	  emit_spstore(as, ir, r, ofs);
	  ofs += 8;
	}
      }
    }
  }
}

/* Setup result reg/sp for call. Evict scratch regs. */
static void asm_setupresult(ASMState *as, IRIns *ir, const CCallInfo *ci)
{
  RegSet drop = RSET_SCRATCH;
  int hiop = ((ir+1)->o == IR_HIOP && !irt_isnil((ir+1)->t));
  if (ra_hasreg(ir->r))
    rset_clear(drop, ir->r);  /* Dest reg handled below. */
  if (hiop && ra_hasreg((ir+1)->r))
    rset_clear(drop, (ir+1)->r);  /* Dest reg handled below. */
  ra_evictset(as, drop);  /* Evictions must be performed first. */
  if (ra_used(ir)) {
    lj_assertA(!irt_ispri(ir->t), "PRI dest");
    if (irt_isfp(ir->t)) {
      if ((ci->flags & CCI_CASTU64)) {
        Reg dest = ra_dest(as, ir, RSET_FPR);
	emit_dj(as, irt_isnum(ir->t) ? LOONGI_MOVGR2FR_D : LOONGI_MOVGR2FR_W,
	        dest, RID_RET);
      } else {
	ra_destreg(as, ir, RID_FPRET);
      }
    } else if (hiop) {
      ra_destpair(as, ir);
    } else {
      ra_destreg(as, ir, RID_RET);
    }
  }
}

static void asm_callx(ASMState *as, IRIns *ir)
{
  IRRef args[CCI_NARGS_MAX*2];
  CCallInfo ci;
  IRRef func;
  IRIns *irf;
  ci.flags = asm_callx_flags(as, ir);
  asm_collectargs(as, ir, &ci, args);
  asm_setupresult(as, ir, &ci);
  func = ir->op2; irf = IR(func);
  if (irf->o == IR_CARG) { func = irf->op1; irf = IR(func); }
  if (irref_isk(func)) {  /* Call to constant address. */
    ci.func = (ASMFunction)(void *)get_kval(as, func);
  } else {  /* Need specific register for indirect calls. */
    Reg freg = ra_alloc1(as, func, RSET_RANGE(RID_R12, RID_MAX_GPR)-RSET_FIXED);
    *--as->mcp = LOONGI_JIRL | LOONGF_D(RID_RA) | LOONGF_J(freg);
    ci.func = (ASMFunction)(void *)0;
  }
  asm_gencall(as, &ci, args);
}

static void asm_callround(ASMState *as, IRIns *ir, IRCallID id)
{
  /* The modified regs must match with the *.dasc implementation. */
  RegSet drop = RID2RSET(RID_R12)|RID2RSET(RID_R13)|RID2RSET(RID_F0)|
                RID2RSET(RID_F4)|RID2RSET(RID_F9)|RID2RSET(RID_F22)
                |RID2RSET(RID_F23);
  if (ra_hasreg(ir->r)) rset_clear(drop, ir->r);
  ra_evictset(as, drop);
  ra_destreg(as, ir, RID_FPRET);
  emit_call(as, (void *)lj_ir_callinfo[id].func);
  ra_leftov(as, REGARG_FIRSTFPR, ir->op1);
}

/* -- Returns ------------------------------------------------------------- */

/* Return to lower frame. Guard that it goes to the right spot. */
static void asm_retf(ASMState *as, IRIns *ir)
{
  Reg base = ra_alloc1(as, REF_BASE, RSET_GPR);
  void *pc = ir_kptr(IR(ir->op2));
  int32_t delta = 1+LJ_FR2+bc_a(*((const BCIns *)pc - 1));
  as->topslot -= (BCReg)delta;
  if ((int32_t)as->topslot < 0) as->topslot = 0;
  irt_setmark(IR(REF_BASE)->t);  /* Children must not coalesce with BASE reg. */
  emit_setgl(as, base, jit_base);
  emit_addptr(as, base, -8*delta);
  Reg tmp = ra_scratch(as, rset_exclude(RSET_GPR, base));
  asm_guard(as, LOONGI_BNE, tmp,
	    ra_allock(as, igcptr(pc), rset_exclude(rset_exclude(RSET_GPR, base), tmp)));
  emit_dji(as, LOONGI_LD_D, tmp, base, -8&0xfff);
}

/* -- Buffer operations --------------------------------------------------- */

#if LJ_HASBUFFER
static void asm_bufhdr_write(ASMState *as, Reg sb)
{
  Reg tmp = ra_scratch(as, rset_exclude(RSET_GPR, sb));
  IRIns irgc;
  irgc.ot = IRT(0, IRT_PGC);  /* GC type. */
  emit_storeofs(as, &irgc, RID_TMP, sb, offsetof(SBuf, L));
  emit_djml(as, LOONGI_BSTRINS_D, RID_TMP, tmp,
            lj_fls(SBUF_MASK_FLAG), 0);
  emit_getgl(as, RID_TMP, cur_L);
  emit_loadofs(as, &irgc, tmp, sb, offsetof(SBuf, L));
}
#endif

/* -- Type conversions ---------------------------------------------------- */

static void asm_tointg(ASMState *as, IRIns *ir, Reg left)
{
  Reg tmp = ra_scratch(as, rset_exclude(RSET_FPR, left));
  Reg dest = ra_dest(as, ir, RSET_GPR);
  asm_guard21(as, LOONGI_BCEQZ, 0);
  emit_djk(as, LOONGI_FCMP_CEQ_D, 0, tmp, left);
  emit_dj(as, LOONGI_FFINT_D_W, tmp, tmp);
  emit_dj(as, LOONGI_MOVFR2GR_S, dest, tmp);
  emit_dj(as, LOONGI_FTINT_W_D, tmp, left);
}

static void asm_tobit(ASMState *as, IRIns *ir)
{
  RegSet allow = RSET_FPR;
  Reg dest = ra_dest(as, ir, RSET_GPR);
  Reg left = ra_alloc1(as, ir->op1, allow);
  Reg right = ra_alloc1(as, ir->op2, rset_clear(allow, left));
  Reg tmp = ra_scratch(as, rset_clear(allow, right));
  emit_dj(as, LOONGI_MOVFR2GR_S, dest, tmp);
  emit_djk(as, LOONGI_FADD_D, tmp, left, right);
}

static void asm_conv(ASMState *as, IRIns *ir)
{
  IRType st = (IRType)(ir->op2 & IRCONV_SRCMASK);	// source type
  int stfp = (st == IRT_NUM || st == IRT_FLOAT);
  int st64 = (st == IRT_I64 || st == IRT_U64 || st == IRT_P64);
  IRRef lref = ir->op1;
  lj_assertA(irt_type(ir->t) != st, "inconsistent types for CONV");
  /* Use GPR to pass floating-point arguments */
  if (irt_isfp(ir->t) && ir->r >= RID_R4 && ir->r <= RID_R11) {
    Reg dest = ra_dest(as, ir, RSET_GPR);
    Reg ftmp = ra_scratch(as, RSET_FPR);
    if (stfp) {  /* FP to FP conversion. */
      emit_dj(as, st == IRT_NUM ? LOONGI_MOVFR2GR_S : LOONGI_MOVFR2GR_D, dest, ftmp);
      emit_dj(as, st == IRT_NUM ? LOONGI_FCVT_S_D : LOONGI_FCVT_D_S,
	      ftmp, ra_alloc1(as, lref, RSET_FPR));
    } else if (st == IRT_U32) {  /* U32 to FP conversion. */
      /* y = (x ^ 0x80000000) + 2147483648.0 */
      Reg left = ra_alloc1(as, lref, RSET_GPR);
      Reg tmp = ra_scratch(as, rset_exclude(RSET_FPR, ftmp));
      if (irt_isfloat(ir->t)) {
        emit_dj(as, LOONGI_MOVFR2GR_S, dest, ftmp);
	emit_dj(as, LOONGI_FCVT_S_D, ftmp, ftmp);
      } else {
        emit_dj(as, LOONGI_MOVFR2GR_D, dest, ftmp);
      }
      /* Must perform arithmetic with doubles to keep the precision. */
      emit_djk(as, LOONGI_FADD_D, ftmp, ftmp, tmp);
      emit_dj(as, LOONGI_FFINT_D_W, ftmp, ftmp);
      emit_lsptr(as, LOONGI_FLD_D, (tmp & 0x1f),
		 (void *)&as->J->k64[LJ_K64_2P31], RSET_GPR);
      emit_dj(as, LOONGI_MOVGR2FR_W, ftmp, RID_TMP);
      emit_djk(as, LOONGI_XOR, RID_TMP, RID_TMP, left);
      emit_dji(as, LOONGI_ADDU16I_D, RID_TMP, RID_R0, 0x8000);
    } else if(st == IRT_U64) {  /* U64 to FP conversion. */
      /* if (x >= 1u<<63) y = (double)(int64_t)(x&(1u<<63)-1) + pow(2.0, 63) */
      Reg left = ra_alloc1(as, lref, RSET_GPR);
      Reg tmp = ra_scratch(as, rset_exclude(RSET_FPR, ftmp));
      MCLabel l_end = emit_label(as);
      if (irt_isfloat(ir->t)) {
        emit_dj(as, LOONGI_MOVFR2GR_S, dest, ftmp);
	emit_djk(as, LOONGI_FADD_S, ftmp, ftmp, tmp);
	emit_lsptr(as, LOONGI_FLD_S, (tmp & 0x1f), (void *)&as->J->k32[LJ_K32_2P63],
		   rset_exclude(RSET_GPR, left));
	emit_branch(as, LOONGI_BGE, left, RID_ZERO, l_end);
	emit_dj(as, LOONGI_FFINT_S_L, ftmp, ftmp);
      } else {
        emit_dj(as, LOONGI_MOVFR2GR_D, dest, ftmp);
	emit_djk(as, LOONGI_FADD_D, ftmp, ftmp, tmp);
	emit_lsptr(as, LOONGI_FLD_D, (tmp & 0x1f), (void *)&as->J->k64[LJ_K64_2P63],
		   rset_exclude(RSET_GPR, left));
	emit_branch(as, LOONGI_BGE, left, RID_ZERO, l_end);
	emit_dj(as, LOONGI_FFINT_D_L, ftmp, ftmp);
      }
      emit_dj(as, LOONGI_MOVGR2FR_D, ftmp, RID_TMP);
      emit_djml(as, LOONGI_BSTRPICK_D, RID_TMP, left, 62, 0);
    } else {  /* Integer to FP conversion. */
      Reg left = ra_alloc1(as, lref, RSET_GPR);
      LOONGIns loongi = irt_isfloat(ir->t) ?
        (st64 ? LOONGI_FFINT_S_L : LOONGI_FFINT_S_W) :
        (st64 ? LOONGI_FFINT_D_L : LOONGI_FFINT_D_W);
      emit_dj(as, st64 ? LOONGI_MOVFR2GR_D : LOONGI_MOVFR2GR_S, dest, ftmp);
      emit_dj(as, loongi, ftmp, ftmp);
      emit_dj(as, st64 ? LOONGI_MOVGR2FR_D : LOONGI_MOVGR2FR_W, ftmp, left);
    }
  } else if (irt_isfp(ir->t)) {
    Reg dest = ra_dest(as, ir, RSET_FPR);
    if (stfp) {  /* FP to FP conversion. */
      emit_dj(as, st == IRT_NUM ? LOONGI_FCVT_S_D : LOONGI_FCVT_D_S,
	      dest, ra_alloc1(as, lref, RSET_FPR));
    } else if (st == IRT_U32) {  /* U32 to FP conversion. */
      /* y = (x ^ 0x80000000) + 2147483648.0 */
      Reg left = ra_alloc1(as, lref, RSET_GPR);
      Reg tmp = ra_scratch(as, rset_exclude(RSET_FPR, dest));
      if (irt_isfloat(ir->t))
	emit_dj(as, LOONGI_FCVT_S_D, dest, dest);
      /* Must perform arithmetic with doubles to keep the precision. */
      emit_djk(as, LOONGI_FADD_D, dest, dest, tmp);
      emit_dj(as, LOONGI_FFINT_D_W, dest, dest);
      emit_lsptr(as, LOONGI_FLD_D, (tmp & 0x1f),
		 (void *)&as->J->k64[LJ_K64_2P31], RSET_GPR);
      emit_dj(as, LOONGI_MOVGR2FR_W, dest, RID_TMP);
      emit_djk(as, LOONGI_XOR, RID_TMP, RID_TMP, left);
      emit_dji(as, LOONGI_ADDU16I_D, RID_TMP, RID_R0, 0x8000);
    } else if(st == IRT_U64) {  /* U64 to FP conversion. */
      /* if (x >= 1u<<63) y = (double)(int64_t)(x&(1u<<63)-1) + pow(2.0, 63) */
      Reg left = ra_alloc1(as, lref, RSET_GPR);
      Reg tmp = ra_scratch(as, rset_exclude(RSET_FPR, dest));
      MCLabel l_end = emit_label(as);
      if (irt_isfloat(ir->t)) {
	emit_djk(as, LOONGI_FADD_S, dest, dest, tmp);
	emit_lsptr(as, LOONGI_FLD_S, (tmp & 0x1f), (void *)&as->J->k32[LJ_K32_2P63],
		   rset_exclude(RSET_GPR, left));
	emit_branch(as, LOONGI_BGE, left, RID_ZERO, l_end);
	emit_dj(as, LOONGI_FFINT_S_L, dest, dest);
      } else {
	emit_djk(as, LOONGI_FADD_D, dest, dest, tmp);
	emit_lsptr(as, LOONGI_FLD_D, (tmp & 0x1f), (void *)&as->J->k64[LJ_K64_2P63],
		   rset_exclude(RSET_GPR, left));
	emit_branch(as, LOONGI_BGE, left, RID_ZERO, l_end);
	emit_dj(as, LOONGI_FFINT_D_L, dest, dest);
      }
      emit_dj(as, LOONGI_MOVGR2FR_D, dest, RID_TMP);
      emit_djml(as, LOONGI_BSTRPICK_D, RID_TMP, left, 62, 0);
    } else {  /* Integer to FP conversion. */
      Reg left = ra_alloc1(as, lref, RSET_GPR);
      LOONGIns loongi = irt_isfloat(ir->t) ?
	(st64 ? LOONGI_FFINT_S_L : LOONGI_FFINT_S_W) :
	(st64 ? LOONGI_FFINT_D_L : LOONGI_FFINT_D_W);
      emit_dj(as, loongi, dest, dest);
      emit_dj(as, st64 ? LOONGI_MOVGR2FR_D : LOONGI_MOVGR2FR_W, dest, left);
    }
  } else if (stfp) {  /* FP to integer conversion. */
    if (irt_isguard(ir->t)) {
      /* Checked conversions are only supported from number to int. */
      lj_assertA(irt_isint(ir->t) && st == IRT_NUM,
		 "bad type for checked CONV");
      asm_tointg(as, ir, ra_alloc1(as, lref, RSET_FPR));
    } else {
      Reg dest = ra_dest(as, ir, RSET_GPR);
      Reg left = ra_alloc1(as, lref, RSET_FPR);
      Reg tmp = ra_scratch(as, rset_exclude(RSET_FPR, left));
      if (irt_isu32(ir->t)) {  /* FP to U32 conversion. */
	/* y = (int)floor(x - 2147483648.0) ^ 0x80000000 */
	emit_djk(as, LOONGI_XOR, dest, dest, RID_TMP);
	emit_dji(as, LOONGI_ADDU16I_D, RID_TMP, RID_R0, 0x8000);
	emit_dj(as, LOONGI_MOVFR2GR_S, dest, tmp);
	emit_dj(as, st == IRT_FLOAT ? LOONGI_FTINTRM_W_S : LOONGI_FTINTRM_W_D,
		tmp, tmp);
	emit_djk(as, st == IRT_FLOAT ? LOONGI_FSUB_S : LOONGI_FSUB_D,
		 tmp, left, tmp);
	if (st == IRT_FLOAT)
	  emit_lsptr(as, LOONGI_FLD_S, (tmp & 0x1f),
		     (void *)&as->J->k32[LJ_K32_2P31], RSET_GPR);
	else
	  emit_lsptr(as, LOONGI_FLD_D, (tmp & 0x1f),
		     (void *)&as->J->k64[LJ_K64_2P31], RSET_GPR);
      } else if (irt_isu64(ir->t)) {  /* FP to U64 conversion. */
	MCLabel l_end;
	emit_dj(as, LOONGI_MOVFR2GR_D, dest, tmp);
	l_end = emit_label(as);
	/* For inputs >= 2^63 add -2^64 and convert again. */
	if (st == IRT_NUM) {
	  emit_dj(as, LOONGI_FTINTRZ_L_D, tmp, tmp);
	  emit_djk(as, LOONGI_FADD_D, tmp, left, tmp);
	  emit_lsptr(as, LOONGI_FLD_D, (tmp & 0x1f),
		     (void *)&as->J->k64[LJ_K64_M2P64],
		     rset_exclude(RSET_GPR, dest));
	  emit_branch21(as, LOONGI_BCNEZ, 0, l_end);
	  emit_dj(as, LOONGI_FTINTRZ_L_D, tmp, left);
	  emit_djk(as, LOONGI_FCMP_CLT_D, 0, left, tmp);
	  emit_lsptr(as, LOONGI_FLD_D, (tmp & 0x1f),
		     (void *)&as->J->k64[LJ_K64_2P63],
		     rset_exclude(RSET_GPR, dest));
	} else {
	  emit_dj(as, LOONGI_FTINTRZ_L_S, tmp, tmp);
	  emit_djk(as, LOONGI_FADD_S, tmp, left, tmp);
	  emit_lsptr(as, LOONGI_FLD_S, (tmp & 0x1f),
		     (void *)&as->J->k32[LJ_K32_M2P64],
		     rset_exclude(RSET_GPR, dest));
	  emit_branch21(as, LOONGI_BCNEZ, 0, l_end);
	  emit_dj(as, LOONGI_FTINTRZ_L_S, tmp, left);
	  emit_djk(as, LOONGI_FCMP_CLT_S, 0, left, tmp);
	  emit_lsptr(as, LOONGI_FLD_S, (tmp & 0x1f),
		     (void *)&as->J->k32[LJ_K32_2P63],
		     rset_exclude(RSET_GPR, dest));
	}
      } else {
	LOONGIns loongi = irt_is64(ir->t) ?
	  (st == IRT_NUM ? LOONGI_FTINTRZ_L_D : LOONGI_FTINTRZ_L_S) :
	  (st == IRT_NUM ? LOONGI_FTINTRZ_W_D : LOONGI_FTINTRZ_W_S);
	emit_dj(as, irt_is64(ir->t) ? LOONGI_MOVFR2GR_D : LOONGI_MOVFR2GR_S, dest, left);
	emit_dj(as, loongi, left, left);
      }
    }
  } else {
    Reg dest = ra_dest(as, ir, RSET_GPR);
    if (st >= IRT_I8 && st <= IRT_U16) {  /* Extend to 32 bit integer. */
      Reg left = ra_alloc1(as, ir->op1, RSET_GPR);
      lj_assertA(irt_isint(ir->t) || irt_isu32(ir->t), "bad type for CONV EXT");
      if ((ir->op2 & IRCONV_SEXT)) {	// sign-extend
	emit_dj(as, st == IRT_I8 ? LOONGI_EXT_W_B : LOONGI_EXT_W_H, dest, left);
      } else {	// zero-extend
        int msbd = st == IRT_U8 ? 7 : 15;
        emit_djml(as, LOONGI_BSTRPICK_D, dest, left, msbd, 0);
      }
    } else {  /* 32/64 bit integer conversions. */
      if (irt_is64(ir->t)) {
	if (st64) {
	  /* 64/64 bit no-op (cast)*/
	  ra_leftov(as, dest, lref);	/* Do nothing, but may need to move regs. */
	} else {
	  Reg left = ra_alloc1(as, lref, RSET_GPR);
	  if ((ir->op2 & IRCONV_SEXT)) {  /* 32 to 64 bit sign extension. */
	    emit_dju(as, LOONGI_SLLI_W, dest, left, 0);
	  } else {  /* 32 to 64 bit zero extension. */
	    emit_djml(as, LOONGI_BSTRPICK_D, dest, left, 31, 0);
	  }
	}
      } else {
	if (st64 && !(ir->op2 & IRCONV_NONE)) {
	  /* This is either a 32 bit reg/reg mov which zeroes the hiword
	  ** or a load of the loword from a 64 bit address.
	  */
	  Reg left = ra_alloc1(as, lref, RSET_GPR);
	  emit_djml(as, LOONGI_BSTRPICK_D, dest, left, 31, 0);
	} else {  /* 32/32 bit no-op (cast). */
	  ra_leftov(as, dest, lref);	/* Do nothing, but may need to move regs. */
	}
      }
    }
  }
}

static void asm_strto(ASMState *as, IRIns *ir)
{
  const CCallInfo *ci = &lj_ir_callinfo[IRCALL_lj_strscan_num];
  IRRef args[2];
  int32_t ofs = SPOFS_TMP;
  RegSet drop = RSET_SCRATCH;
  if (ra_hasreg(ir->r)) rset_set(drop, ir->r);  /* Spill dest reg (if any). */
  ra_evictset(as, drop);
  if (ir->s) ofs = sps_scale(ir->s);
  asm_guard(as, LOONGI_BEQ, RID_RET, RID_ZERO);  /* Test return status. */
  args[0] = ir->op1;      /* GCstr *str */
  args[1] = ASMREF_TMP1;  /* TValue *n  */
  asm_gencall(as, ci, args);
  /* Store the result to the spill slot or temp slots. */
  Reg tmp = ra_releasetmp(as, ASMREF_TMP1);
  emit_addk(as, tmp, RID_SP, ofs, RSET_GPR);
}

/* -- Memory references --------------------------------------------------- */

/* Store tagged value for ref at base+ofs. */
static void asm_tvstore64(ASMState *as, Reg base, int32_t ofs, IRRef ref)
{
  RegSet allow = rset_exclude(RSET_GPR, base);
  IRIns *ir = IR(ref);
  lj_assertA(irt_ispri(ir->t) || irt_isaddr(ir->t) || irt_isinteger(ir->t),
	     "store of IR type %d", irt_type(ir->t));
  if (irref_isk(ref)) {
    TValue k;
    lj_ir_kvalue(as->J->L, &k, ir);
    Reg ku64 = ra_allock(as, (int64_t)k.u64, allow);
    rset_clear(allow, ku64);
    if (checki12(ofs)) {
      emit_dji(as, LOONGI_ST_D, ku64, base, ofs&0xfff);
    } else {
      emit_djk(as, LOONGI_STX_D, ku64, base, ra_allock(as, ofs, allow));
    }
  } else {
    Reg src = ra_alloc1(as, ref, allow);
    rset_clear(allow, src);
    Reg type = ra_allock(as, (int64_t)irt_toitype(ir->t) << 47, allow);
    emit_dji(as, LOONGI_ST_D, RID_TMP, base, ofs&0xfff);
    if (irt_isinteger(ir->t)) {
      emit_djk(as, LOONGI_ADD_D, RID_TMP, RID_TMP, type);
      emit_djml(as, LOONGI_BSTRPICK_D, RID_TMP, src, 31, 0);
    } else {
      emit_djk(as, LOONGI_ADD_D, RID_TMP, src, type);
    }
  }
}

/* Get pointer to TValue. */
static void asm_tvptr(ASMState *as, Reg dest, IRRef ref, MSize mode)	// todo-new
{
  int32_t tmpofs = (int32_t)(offsetof(global_State, tmptv)-32768);
  RegSet allow = RSET_GPR;
  if ((mode & IRTMPREF_IN1)) {
    IRIns *ir = IR(ref);
    if (irt_isnum(ir->t)) {
      if ((mode & IRTMPREF_OUT1)) {
        Reg src = ra_alloc1(as, ref, RSET_FPR);
	emit_addk(as, dest, RID_JGL, tmpofs, allow);
        emit_lso(as, LOONGI_ST_D, src, RID_JGL, tmpofs, allow);
      } else if (irref_isk(ref)) {
        /* Use the number constant itself as a TValue. */
        ra_allockreg(as, igcptr(ir_knum(ir)), dest);
      } else {
        emit_dji(as, LOONGI_ADDI_D, dest, RID_SP, ra_spill(as, ir)&0xfff);
      }
    } else {
      /* Otherwise use g->tmptv to hold the TValue. */
      asm_tvstore64(as, dest, 0, ref);
      emit_addk(as, dest, RID_JGL, tmpofs, RSET_GPR);
    }
  } else {
    emit_addk(as, dest, RID_JGL, tmpofs, RSET_GPR);
  }
}

static void asm_aref(ASMState *as, IRIns *ir)
{
  Reg dest = ra_dest(as, ir, RSET_GPR);
  Reg idx, base;
  if (irref_isk(ir->op2)) {
    IRRef tab = IR(ir->op1)->op1;
    int32_t ofs = asm_fuseabase(as, tab);
    IRRef refa = ofs ? tab : ir->op1;
    ofs += 8*IR(ir->op2)->i;
    if (checki12(ofs)) {
      base = ra_alloc1(as, refa, RSET_GPR);
      emit_dji(as, LOONGI_ADDI_D, dest, base, ofs&0xfff);
      return;
    }
  }
  base = ra_alloc1(as, ir->op1, RSET_GPR);
  idx = ra_alloc1(as, ir->op2, rset_exclude(RSET_GPR, base));
  emit_djk(as, LOONGI_ADD_D, dest, RID_TMP, base);
  emit_dju(as, LOONGI_SLLI_D, RID_TMP, idx, 3);
}

/* Inlined hash lookup. Specialized for key type and for const keys.
** The equivalent C code is:
**   Node *n = hashkey(t, key);
**   do {
**     if (lj_obj_equal(&n->key, key)) return &n->val;
**   } while ((n = nextnode(n)));
**   return niltv(L);
*/
static void asm_href(ASMState *as, IRIns *ir, IROp merge)
{
  RegSet allow = RSET_GPR;
  int destused = ra_used(ir);
  Reg dest = ra_dest(as, ir, allow);
  Reg tab = ra_alloc1(as, ir->op1, rset_clear(allow, dest));
  Reg key = RID_NONE, type = RID_NONE, tmpnum = RID_NONE, tmp1, tmp2;
  Reg cmp64 = RID_NONE;
  IRRef refkey = ir->op2;
  IRIns *irkey = IR(refkey);
  int isk = irref_isk(refkey);
  IRType1 kt = irkey->t;
  uint32_t khash;
  MCLabel l_end, l_loop, l_next;
  rset_clear(allow, tab);
  tmp1 = ra_scratch(as, allow);
  rset_clear(allow, tmp1);
  tmp2 = ra_scratch(as, allow);
  rset_clear(allow, tmp2);

  if (irt_isnum(kt)) {
    key = ra_alloc1(as, refkey, RSET_FPR);
    tmpnum = ra_scratch(as, rset_exclude(RSET_FPR, key));
  } else {
    /* Allocate cmp64 register used for 64-bit comparisons */
    if (!isk && irt_isaddr(kt)) {
      cmp64 = tmp2;
    } else {
      int64_t k;
      if (isk && irt_isaddr(kt)) {
	k = ((int64_t)irt_toitype(kt) << 47) | irkey[1].tv.u64;
      } else {
	lj_assertA(irt_ispri(kt) && !irt_isnil(kt), "bad HREF key type");
	k = ~((int64_t)~irt_toitype(kt) << 47);
      }
      cmp64 = ra_allock(as, k, allow);
      rset_clear(allow, cmp64);
    }
    if (!irt_ispri(kt)) {
      key = ra_alloc1(as, refkey, allow);
      rset_clear(allow, key);
    }
  } 

  /* Key not found in chain: jump to exit (if merged) or load niltv. */
  l_end = emit_label(as);
  as->invmcp = NULL;
  if (merge == IR_NE)
    asm_guard(as, LOONGI_BEQ, RID_ZERO, RID_ZERO);
  else if (destused)
    emit_loada(as, dest, niltvg(J2G(as->J)));

  /* Follow hash chain until the end. */
  l_loop = --as->mcp;
  emit_move(as, dest, tmp1);
  emit_dji(as, LOONGI_LD_D, tmp1, dest, (int32_t)offsetof(Node, next)&0xfff);
  l_next = emit_label(as);

  /* Type and value comparison. */
  if (merge == IR_EQ) {  /* Must match asm_guard(). */
    l_end = asm_exitstub_addr(as);
  }
  if (irt_isnum(kt)) {
    emit_branch21(as, LOONGI_BCNEZ, 0, l_end);
    emit_dj32i(as, RID_TMP, RID_ZERO, as->snapno);
    emit_djk(as, LOONGI_FCMP_CEQ_D, 0, tmpnum, key);
    emit_branch(as, LOONGI_BEQ, tmp1, RID_ZERO, l_next);
    emit_dju(as, LOONGI_SLTUI, tmp1, tmp1, ((int32_t)LJ_TISNUM)&0xfff);
    emit_dju(as, LOONGI_SRAI_D, tmp1, tmp1, 47);
    emit_dj(as, LOONGI_MOVGR2FR_D, tmpnum, tmp1);
  } else {
    emit_branch(as, LOONGI_BEQ, tmp1, cmp64, l_end);
    emit_dj32i(as, RID_TMP, RID_ZERO, as->snapno);
  }
  emit_dji(as, LOONGI_LD_D, tmp1, dest, (int32_t)offsetof(Node, key.u64)&0xfff);
  *l_loop = LOONGI_BNE | LOONGF_J(tmp1) | LOONGF_D(RID_ZERO) | LOONGF_I(((as->mcp-l_loop) & 0xffffu));
  if (!isk && irt_isaddr(kt)) {
    type = ra_allock(as, (int64_t)irt_toitype(kt) << 47, allow);
    emit_djk(as, LOONGI_ADD_D, tmp2, key, type);
    rset_clear(allow, type);
  }

  /* Load main position relative to tab->node into dest. */
  khash = isk ? ir_khash(as, irkey) : 1;
  if (khash == 0) {
    emit_dji(as, LOONGI_LD_D, dest, tab, (int32_t)offsetof(GCtab, node)&0xfff);
  } else {
    Reg tmphash = tmp1;
    if (isk)
      tmphash = ra_allock(as, khash, allow);
    /* node = tab->node + (idx*32-idx*8) */
    emit_djk(as, LOONGI_ADD_D, dest, dest, tmp1);
    lj_assertA(sizeof(Node) == 24, "bad Node size");
    emit_djk(as, LOONGI_SUB_W, tmp1, tmp2, tmp1);
    emit_dju(as, LOONGI_SLLI_W, tmp1, tmp1, 3);
    emit_dju(as, LOONGI_SLLI_W, tmp2, tmp1, 5);
    emit_djk(as, LOONGI_AND, tmp1, tmp2, tmphash);	// idx = hi & tab->hmask
    emit_dji(as, LOONGI_LD_D, dest, tab, ((int32_t)offsetof(GCtab, node))&0xfff);
    emit_dji(as, LOONGI_LD_W, tmp2, tab, ((int32_t)offsetof(GCtab, hmask))&0xfff);
    if (isk) {
      /* Nothing to do. */
    } else if (irt_isstr(kt)) {
      emit_dji(as, LOONGI_LD_W, tmp1, key, ((int32_t)offsetof(GCstr, sid))&0xfff);
    } else {  /* Must match with hash*() in lj_tab.c. */
      emit_djk(as, LOONGI_SUB_W, tmp1, tmp1, tmp2);
      emit_dju(as, LOONGI_ROTRI_W, tmp2, tmp2, (-HASH_ROT3)&0x1f);
      emit_djk(as, LOONGI_XOR, tmp1, tmp2, tmp1);
      emit_dju(as, LOONGI_ROTRI_W, tmp1, tmp1, (-HASH_ROT2-HASH_ROT1)&0x1f);
      emit_djk(as, LOONGI_SUB_W, tmp2, tmp2, dest);
      emit_djk(as, LOONGI_XOR, tmp2, tmp2, tmp1);
      emit_dju(as, LOONGI_ROTRI_W, dest, tmp1, (-HASH_ROT1)&0x1f);
      if (irt_isnum(kt)) {
	emit_dju(as, LOONGI_SLLI_W, tmp1, tmp1, 1);
	emit_dju(as, LOONGI_SRAI_D, tmp1, tmp1, 32);	// hi
	emit_dju(as, LOONGI_SLLI_W, tmp2, tmp1, 0);	// lo
	emit_dj(as, LOONGI_MOVFR2GR_D, tmp1, key);
      } else {
	checkmclim(as);
	emit_dju(as, LOONGI_SRAI_D, tmp1, tmp1, 32);	// hi
	emit_dju(as, LOONGI_SLLI_W, tmp2, key, 0);	// lo
	emit_djk(as, LOONGI_ADD_D, tmp1, key, type);
      }
    }
  }
}

static void asm_hrefk(ASMState *as, IRIns *ir)
{
  IRIns *kslot = IR(ir->op2);
  IRIns *irkey = IR(kslot->op1);
  int32_t ofs = (int32_t)(kslot->op2 * sizeof(Node));
  int32_t kofs = ofs + (int32_t)offsetof(Node, key);
  Reg dest = (ra_used(ir)||ofs > 32736) ? ra_dest(as, ir, RSET_GPR) : RID_NONE;
  Reg node = ra_alloc1(as, ir->op1, RSET_GPR);
  RegSet allow = rset_exclude(RSET_GPR, node);
  Reg idx = node;
  Reg key = ra_scratch(as, allow);
  int64_t k;
  lj_assertA(ofs % sizeof(Node) == 0, "unaligned HREFK slot");
  if (ofs > 32736) {
    idx = dest;
    rset_clear(allow, dest);
    kofs = (int32_t)offsetof(Node, key);
  } else if (ra_hasreg(dest)) {
    emit_addk(as, dest, node, ofs, allow);
  }
  if (irt_ispri(irkey->t)) {
    lj_assertA(!irt_isnil(irkey->t), "bad HREFK key type");
    k = ~((int64_t)~irt_toitype(irkey->t) << 47);
  } else if (irt_isnum(irkey->t)) {
    k = (int64_t)ir_knum(irkey)->u64;
  } else {
    k = ((int64_t)irt_toitype(irkey->t) << 47) | (int64_t)ir_kgc(irkey);
  }
  asm_guard(as, LOONGI_BNE, key, ra_allock(as, k, allow));
  emit_lso(as, LOONGI_LD_D, key, idx, kofs, allow);
  if (ofs > 32736)
    emit_djk(as, LOONGI_ADD_D, dest, node, ra_allock(as, ofs, allow));
}

static void asm_uref(ASMState *as, IRIns *ir)
{
  Reg dest = ra_dest(as, ir, RSET_GPR);
  if (irref_isk(ir->op1)) {
    GCfunc *fn = ir_kfunc(IR(ir->op1));
    MRef *v = &gcref(fn->l.uvptr[(ir->op2 >> 8)])->uv.v;
    emit_lsptr(as, LOONGI_LD_D, dest, v, RSET_GPR);
  } else {
    Reg uv = ra_scratch(as, RSET_GPR);
    Reg func = ra_alloc1(as, ir->op1, RSET_GPR);
    if (ir->o == IR_UREFC) {
      Reg tmp = ra_scratch(as, rset_exclude(rset_exclude(RSET_GPR, dest), uv));
      asm_guard(as, LOONGI_BEQ, tmp, RID_ZERO);
      emit_dji(as, LOONGI_ADDI_D, dest, uv, ((int32_t)offsetof(GCupval, tv))&0xfff);
      emit_dji(as, LOONGI_LD_BU, tmp, uv, ((int32_t)offsetof(GCupval, closed))&0xfff);
    } else {
      emit_dji(as, LOONGI_LD_D, dest, uv, ((int32_t)offsetof(GCupval, v))&0xfff);
    }
    emit_lso(as, LOONGI_LD_D, uv, func, (int32_t)offsetof(GCfuncL, uvptr) +
      (int32_t)sizeof(MRef) * (int32_t)(ir->op2 >> 8), RSET_GPR);
  }
}

static void asm_fref(ASMState *as, IRIns *ir)
{
  UNUSED(as); UNUSED(ir);
  lj_assertA(!ra_used(ir), "unfused FREF");
}

static void asm_strref(ASMState *as, IRIns *ir)
{
  RegSet allow = RSET_GPR;
  Reg dest = ra_dest(as, ir, allow);
  Reg base = ra_alloc1(as, ir->op1, allow);
  IRIns *irr = IR(ir->op2);
  int32_t ofs = sizeof(GCstr);
  rset_clear(allow, base);
  if (irref_isk(ir->op2) && checki12(ofs + irr->i)) {
    emit_dji(as, LOONGI_ADDI_D, dest, base, (ofs + irr->i)&0xfff);
  } else {
    emit_dji(as, LOONGI_ADDI_D, dest, dest, ofs&0xfff);
    emit_djk(as, LOONGI_ADD_D, dest, base, ra_alloc1(as, ir->op2, allow));
  }
}

/* -- Loads and stores ---------------------------------------------------- */

static LOONGIns asm_fxloadins(ASMState *as, IRIns *ir)
{
  UNUSED(as);
  switch (irt_type(ir->t)) {
  case IRT_I8:
    return LOONGI_LD_B;
  case IRT_U8:
    return LOONGI_LD_BU;
  case IRT_I16:
    return LOONGI_LD_H;
  case IRT_U16:
    return LOONGI_LD_HU;
  case IRT_NUM:
    lj_assertA(!LJ_SOFTFP32, "unsplit FP op");
    return LOONGI_FLD_D;
  /* fallthrough */
  case IRT_FLOAT:
    return LOONGI_FLD_S;
  /* fallthrough */
  default:
    return irt_is64(ir->t) ? LOONGI_LD_D : LOONGI_LD_W;
  }
}

static LOONGIns asm_fxstoreins(ASMState *as, IRIns *ir)
{
  UNUSED(as);
  switch (irt_type(ir->t)) {
  case IRT_I8: case IRT_U8: return LOONGI_ST_B;
  case IRT_I16: case IRT_U16: return LOONGI_ST_H;
  case IRT_NUM:
    lj_assertA(!LJ_SOFTFP32, "unsplit FP op");
    if (!LJ_SOFTFP) return LOONGI_FST_D;
  /* fallthrough */
  case IRT_FLOAT: return LOONGI_FST_S;
  /* fallthrough */
  default: return (LJ_64 && irt_is64(ir->t)) ? LOONGI_ST_D : LOONGI_ST_W;
  }
}

static void asm_fload(ASMState *as, IRIns *ir)
{
  RegSet allow = RSET_GPR;
  Reg idx, dest = ra_dest(as, ir, allow);
  rset_clear(allow, dest);
  LOONGIns loongi = asm_fxloadins(as, ir);
  int32_t ofs;
  if (ir->op1 == REF_NIL) {  /* FLOAD from GG_State with offset. */
    idx = ra_allock(as, (int64_t)J2GG(as->J), allow);
    ofs = (int32_t)(ir->op2<<2);
  } else {
    idx = ra_alloc1(as, ir->op1, allow);
    if (ir->op2 == IRFL_TAB_ARRAY) {
      ofs = asm_fuseabase(as, ir->op1);
      if (ofs) {  /* Turn the t->array load into an add for colocated arrays. */
	emit_dji(as, LOONGI_ADDI_D, dest, idx, ofs);
	return;
      }
    }
    ofs = field_ofs[ir->op2];
  }
  rset_clear(allow, idx);
  lj_assertA(!irt_isfp(ir->t), "bad FP FLOAD");
  emit_lso(as, loongi, dest, idx, ofs, allow);
}

static void asm_fstore(ASMState *as, IRIns *ir)
{
  if (ir->r == RID_SINK)
    return;
  Reg src = ra_alloc1(as, ir->op2, RSET_GPR);
  IRIns *irf = IR(ir->op1);
  Reg idx = ra_alloc1(as, irf->op1, rset_exclude(RSET_GPR, src));
  int32_t ofs = field_ofs[irf->op2];
  lj_assertA(!irt_isfp(ir->t), "bad FP FSTORE");
  emit_dji(as, asm_fxstoreins(as, ir), src, idx, ofs&0xfff);
}

static void asm_xload(ASMState *as, IRIns *ir)
{
  Reg dest = ra_dest(as, ir, (irt_isfp(ir->t)) ? RSET_FPR : RSET_GPR);
  lj_assertA(LJ_TARGET_UNALIGNED || !(ir->op2 & IRXLOAD_UNALIGNED),
	     "unaligned XLOAD");
  asm_fusexref(as, asm_fxloadins(as, ir), dest, ir->op1, RSET_GPR, 0);
}

static void asm_xstore_(ASMState *as, IRIns *ir, int32_t ofs)
{
  if (ir->r == RID_SINK)
    return;
  Reg src = ra_alloc1(as, ir->op2, irt_isfp(ir->t) ? RSET_FPR : RSET_GPR);
  asm_fusexref(as, asm_fxstoreins(as, ir), src, ir->op1,
		 rset_exclude(RSET_GPR, src), ofs);
}

#define asm_xstore(as, ir)	asm_xstore_(as, ir, 0)

static void asm_ahuvload(ASMState *as, IRIns *ir)
{
  Reg dest = RID_NONE, type, idx;
  RegSet allow = RSET_GPR;
  int32_t ofs = 0;
  IRType1 t = ir->t;

  type = ra_scratch(as, allow);
  rset_clear(allow, type);

  if (ra_used(ir)) {
    lj_assertA((irt_isnum(ir->t)) || irt_isint(ir->t) || irt_isaddr(ir->t),
	       "bad load type %d", irt_type(ir->t));
    dest = ra_dest(as, ir, irt_isnum(t) ? RSET_FPR : allow);
    rset_clear(allow, dest);
    if (irt_isaddr(t))
      emit_djml(as, LOONGI_BSTRPICK_D, dest, dest, 46, 0);
    else if (irt_isint(t))
      emit_dju(as, LOONGI_SLLI_W, dest, dest, 0);
  }
  idx = asm_fuseahuref(as, ir->op1, &ofs, allow);
  if (ir->o == IR_VLOAD) ofs += 8 * ir->op2;
  rset_clear(allow, idx);
  if (irt_isnum(t)) {
    Reg tmp2 = ra_scratch(as, allow);
    asm_guard(as, LOONGI_BEQ, tmp2, RID_ZERO);
    emit_dju(as, LOONGI_SLTUI, tmp2, type, ((int32_t)LJ_TISNUM)&0xfff);
  } else {
    asm_guard(as, LOONGI_BNE, type,
	      ra_allock(as, (int32_t)irt_toitype(t), allow));
  }
  if (ra_hasreg(dest)) {
    if (irt_isnum(t)) {
      emit_lso(as, LOONGI_FLD_D, dest, idx, ofs, allow);
      dest = type;
    }
  } else {
    dest = type;
  }
  emit_dju(as, LOONGI_SRAI_D, type, dest, 47);
  emit_lso(as, LOONGI_LD_D, dest, idx, ofs, allow);
}

static void asm_ahustore(ASMState *as, IRIns *ir)
{
  RegSet allow = RSET_GPR;
  Reg idx, src = RID_NONE, type = RID_NONE;
  int32_t ofs = 0;
  if (ir->r == RID_SINK)
    return;
  if (irt_isnum(ir->t)) {
    src = ra_alloc1(as, ir->op2, RSET_FPR);
    idx = asm_fuseahuref(as, ir->op1, &ofs, allow);
    emit_lso(as, LOONGI_FST_D, src, idx, ofs, allow);
  } else {
    Reg tmp = RID_TMP;
    if (irt_ispri(ir->t)) {
      tmp = ra_allock(as, ~((int64_t)~irt_toitype(ir->t) << 47), allow);
      rset_clear(allow, tmp);
    } else {
      src = ra_alloc1(as, ir->op2, allow);
      rset_clear(allow, src);
      type = ra_allock(as, (int64_t)irt_toitype(ir->t) << 47, allow);
      rset_clear(allow, type);
    }
    idx = asm_fuseahuref(as, ir->op1, &ofs, allow);
    emit_lso(as, LOONGI_ST_D, tmp, idx, ofs, allow);
    if (ra_hasreg(src)) {
      if (irt_isinteger(ir->t)) {
	emit_djk(as, LOONGI_ADD_D, tmp, tmp, type);
	emit_djml(as, LOONGI_BSTRPICK_D, tmp, src, 31, 0);
      } else {
	emit_djk(as, LOONGI_ADD_D, tmp, src, type);
      }
    }
  }
}

static void asm_sload(ASMState *as, IRIns *ir)
{
  Reg dest = RID_NONE, type = RID_NONE, base;
  RegSet allow = RSET_GPR;
  IRType1 t = ir->t;
  int32_t ofs = 8*((int32_t)ir->op1-2);
  lj_assertA(!(ir->op2 & IRSLOAD_PARENT),
	     "bad parent SLOAD");  /* Handled by asm_head_side(). */
  lj_assertA(irt_isguard(ir->t) || !(ir->op2 & IRSLOAD_TYPECHECK),
	     "inconsistent SLOAD variant");
  if ((ir->op2 & IRSLOAD_CONVERT) && irt_isguard(t) && irt_isint(t)) {
    dest = ra_scratch(as, RSET_FPR);
    asm_tointg(as, ir, dest);
    t.irt = IRT_NUM;  /* Continue with a regular number type check. */
  } else if (ra_used(ir)) {
    lj_assertA((irt_isnum(ir->t)) ||
	       irt_isint(ir->t) || irt_isaddr(ir->t),
	       "bad SLOAD type %d", irt_type(ir->t));
    dest = ra_dest(as, ir, irt_isnum(t) ? RSET_FPR : allow);
    rset_clear(allow, dest);
    base = ra_alloc1(as, REF_BASE, allow);
    rset_clear(allow, base);
    if (ir->op2 & IRSLOAD_CONVERT) {
      if (irt_isint(t)) {
	Reg tmp = ra_scratch(as, RSET_FPR);
	emit_dj(as, LOONGI_MOVFR2GR_S, dest, tmp);
	emit_dj(as, LOONGI_FTINTRZ_W_D, tmp, tmp);
	dest = tmp;
	t.irt = IRT_NUM;  /* Check for original type. */
      } else {
	Reg tmp = ra_scratch(as, RSET_GPR);
	emit_dj(as, LOONGI_FFINT_D_W, dest, dest);
	emit_dj(as, LOONGI_MOVGR2FR_W, dest, tmp);
	dest = tmp;
	t.irt = IRT_INT;  /* Check for original type. */
      }
    } else if (irt_isaddr(t)) {
      /* Clear type from pointers. */
      emit_djml(as, LOONGI_BSTRPICK_D, dest, dest, 46, 0);
    } else if (irt_isint(t) && (ir->op2 & IRSLOAD_TYPECHECK)) {
      /* Sign-extend integers. */
      emit_dju(as, LOONGI_SLLI_W, dest, dest, 0);
    }
    goto dotypecheck;
  }
  base = ra_alloc1(as, REF_BASE, allow);
  rset_clear(allow, base);
dotypecheck:
  if ((ir->op2 & IRSLOAD_TYPECHECK)) {
    if (dest < RID_MAX_GPR) {
      type = dest;
    } else {
      type = ra_scratch(as, allow);
    }
    rset_clear(allow, type);
    Reg tmp1 = ra_scratch(as, allow);
    if (irt_ispri(t)) {
      asm_guard(as, LOONGI_BNE, type,
		ra_allock(as, ~((int64_t)~irt_toitype(t) << 47) , allow));
    } else if ((ir->op2 & IRSLOAD_KEYINDEX)) {
      asm_guard(as, LOONGI_BNE, tmp1,
               ra_allock(as, (int32_t)LJ_KEYINDEX, allow));
      emit_dju(as, LOONGI_SRAI_D, tmp1, type, 32);
    } else {
      if (irt_isnum(t)) {
        asm_guard(as, LOONGI_BEQ, tmp1, RID_ZERO);
        emit_dji(as, LOONGI_SLTUI, tmp1, tmp1, LJ_TISNUM&0xfff);
	if (ra_hasreg(dest)) {
	  emit_lso(as, LOONGI_FLD_D, dest, base, ofs, allow);
	}
      } else {
	asm_guard(as, LOONGI_BNE, tmp1,
		  ra_allock(as, (int32_t)irt_toitype(t), allow));
      }
      emit_dju(as, LOONGI_SRAI_D, tmp1, type, 47);
    }
    emit_lso(as, LOONGI_LD_D, type, base, ofs, allow);
  } else if (ra_hasreg(dest)) {
    if (irt_isnum(t)) {
      emit_lso(as, LOONGI_FLD_D, dest, base, ofs, allow);
    } else {
      emit_lso(as, irt_isint(t) ? LOONGI_LD_W : LOONGI_LD_D, dest, base, ofs, allow);
    }
  }
}

/* -- Allocations --------------------------------------------------------- */

#if LJ_HASFFI
static void asm_cnew(ASMState *as, IRIns *ir)
{
  CTState *cts = ctype_ctsG(J2G(as->J));
  CTypeID id = (CTypeID)IR(ir->op1)->i;
  CTSize sz;
  CTInfo info = lj_ctype_info(cts, id, &sz);
  const CCallInfo *ci = &lj_ir_callinfo[IRCALL_lj_mem_newgco];
  IRRef args[4];
  RegSet drop = RSET_SCRATCH;
  lj_assertA(sz != CTSIZE_INVALID || (ir->o == IR_CNEW && ir->op2 != REF_NIL),
	     "bad CNEW/CNEWI operands");

  as->gcsteps++;
  if (ra_hasreg(ir->r))
    rset_clear(drop, ir->r);  /* Dest reg handled below. */
  ra_evictset(as, drop);
  if (ra_used(ir))
    ra_destreg(as, ir, RID_RET);  /* GCcdata * */

  /* Initialize immutable cdata object. */
  if (ir->o == IR_CNEWI) {
    RegSet allow = (RSET_GPR & ~RSET_SCRATCH);
    emit_dji(as, sz == 8 ? LOONGI_ST_D : LOONGI_ST_W, ra_alloc1(as, ir->op2, allow),
	     RID_RET, (sizeof(GCcdata))&0xfff);
    lj_assertA(sz == 4 || sz == 8, "bad CNEWI size %d", sz);
  } else if (ir->op2 != REF_NIL) {  /* Create VLA/VLS/aligned cdata. */
    ci = &lj_ir_callinfo[IRCALL_lj_cdata_newv];
    args[0] = ASMREF_L;     /* lua_State *L */
    args[1] = ir->op1;      /* CTypeID id   */
    args[2] = ir->op2;      /* CTSize sz    */
    args[3] = ASMREF_TMP1;  /* CTSize align */
    asm_gencall(as, ci, args);
    emit_loadi(as, ra_releasetmp(as, ASMREF_TMP1), (int32_t)ctype_align(info));
    return;
  }

  /* Initialize gct and ctypeid. lj_mem_newgco() already sets marked. */
  emit_dji(as, LOONGI_ST_B, RID_RET+1, RID_RET, (offsetof(GCcdata, gct))&0xfff);
  emit_dji(as, LOONGI_ST_H, RID_TMP, RID_RET, (offsetof(GCcdata, ctypeid))&0xfff);
  emit_dji(as, LOONGI_ADDI_D, RID_RET+1, RID_ZERO, ~LJ_TCDATA&0xfff);
  emit_dj32i(as, RID_TMP, RID_ZERO, id);
  args[0] = ASMREF_L;     /* lua_State *L */
  args[1] = ASMREF_TMP1;  /* MSize size   */
  asm_gencall(as, ci, args);
  ra_allockreg(as, (int32_t)(sz+sizeof(GCcdata)), ra_releasetmp(as, ASMREF_TMP1));
}
#endif

/* -- Write barriers ------------------------------------------------------ */

static void asm_tbar(ASMState *as, IRIns *ir)
{
  Reg tab = ra_alloc1(as, ir->op1, RSET_GPR);
  Reg mark = ra_scratch(as, rset_exclude(RSET_GPR, tab));
  Reg link = RID_TMP;
  MCLabel l_end = emit_label(as);
  emit_dji(as, LOONGI_ST_D, link, tab, ((int32_t)offsetof(GCtab, gclist))&0xfff);
  emit_dji(as, LOONGI_ST_B, mark, tab, ((int32_t)offsetof(GCtab, marked))&0xfff);
  emit_setgl(as, tab, gc.grayagain);	// make tab gray again
  emit_getgl(as, link, gc.grayagain);
  emit_branch(as, LOONGI_BEQ, RID_TMP, RID_ZERO, l_end);	// black: not jump
  emit_djk(as, LOONGI_XOR, mark, mark, RID_TMP);	// mark=0: gray
  emit_dju(as, LOONGI_ANDI, RID_TMP, mark, LJ_GC_BLACK);
  emit_dji(as, LOONGI_LD_BU, mark, tab, ((int32_t)offsetof(GCtab, marked))&0xfff);
}

static void asm_obar(ASMState *as, IRIns *ir)
{
  const CCallInfo *ci = &lj_ir_callinfo[IRCALL_lj_gc_barrieruv];
  IRRef args[2];
  MCLabel l_end;
  Reg obj, val, tmp;
  /* No need for other object barriers (yet). */
  lj_assertA(IR(ir->op1)->o == IR_UREFC, "bad OBAR type");	// Closed upvalue
  ra_evictset(as, RSET_SCRATCH);
  l_end = emit_label(as);
  args[0] = ASMREF_TMP1;  /* global_State *g */
  args[1] = ir->op1;      /* TValue *tv      */
  asm_gencall(as, ci, args);
  obj = IR(ir->op1)->r;
  tmp = ra_scratch(as, rset_exclude(RSET_GPR, obj));
  emit_branch(as, LOONGI_BEQ, tmp, RID_ZERO, l_end);
  emit_addk(as, ra_releasetmp(as, ASMREF_TMP1), RID_JGL, -32768, RSET_GPR);
  emit_branch(as, LOONGI_BEQ, RID_TMP, RID_ZERO, l_end);	// black: jump
  emit_dju(as, LOONGI_ANDI, tmp, tmp, LJ_GC_BLACK);
  emit_dju(as, LOONGI_ANDI, RID_TMP, RID_TMP, LJ_GC_WHITES);
  val = ra_alloc1(as, ir->op2, rset_exclude(RSET_GPR, obj));
  emit_dji(as, LOONGI_LD_BU, tmp, obj,
	   ((int32_t)offsetof(GCupval, marked)-(int32_t)offsetof(GCupval, tv))&0xfff);
  emit_dji(as, LOONGI_LD_BU, RID_TMP, val, ((int32_t)offsetof(GChead, marked))&0xfff);
}

/* -- Arithmetic and logic operations ------------------------------------- */

static void asm_fparith(ASMState *as, IRIns *ir, LOONGIns loongi)
{
  Reg dest = ra_dest(as, ir, RSET_FPR);
  Reg right, left = ra_alloc2(as, ir, RSET_FPR);
  right = (left >> 8); left &= 255;
  emit_djk(as, loongi, dest, left, right);
}

static void asm_fpunary(ASMState *as, IRIns *ir, LOONGIns loongi)
{
  Reg dest = ra_dest(as, ir, RSET_FPR);
  Reg left = ra_hintalloc(as, ir->op1, dest, RSET_FPR);
  emit_dj(as, loongi, dest, left);
}

static void asm_fpmath(ASMState *as, IRIns *ir)
{
  IRFPMathOp fpm = (IRFPMathOp)ir->op2;
  if (fpm <= IRFPM_TRUNC)
    asm_callround(as, ir, IRCALL_lj_vm_floor + fpm);
  else if (fpm == IRFPM_SQRT)
    asm_fpunary(as, ir, LOONGI_FSQRT_D);
  else
    asm_callid(as, ir, IRCALL_lj_vm_floor + fpm);
}

static void asm_add(ASMState *as, IRIns *ir)
{
  IRType1 t = ir->t;
  if (irt_isnum(t)) {
    if (!asm_fusemadd(as, ir, LOONGI_FMADD_D, LOONGI_FMADD_D))
      asm_fparith(as, ir, LOONGI_FADD_D);
    return;
  } else {
    Reg dest = ra_dest(as, ir, RSET_GPR);
    Reg left = ra_hintalloc(as, ir->op1, dest, RSET_GPR);
    if (irref_isk(ir->op2)) {
      intptr_t k = get_kval(as, ir->op2);
      if (LOONGF_S_OK(k, 12)) {		// si12
        if (irt_is64(t)) {
          emit_dji(as, LOONGI_ADDI_D, dest, left, k&0xfff);
        } else {
	  emit_dji(as, LOONGI_ADDI_W, dest, left, k&0xfff);
        }
	return;
      }
    }
    Reg right = ra_alloc1(as, ir->op2, rset_exclude(RSET_GPR, left));
    emit_djk(as, irt_is64(t) ? LOONGI_ADD_D : LOONGI_ADD_W, dest,
	     left, right);
  }
}

static void asm_sub(ASMState *as, IRIns *ir)
{
  if (irt_isnum(ir->t)) {
    if (!asm_fusemadd(as, ir, LOONGI_FMSUB_D, LOONGI_FNMSUB_D))
      asm_fparith(as, ir, LOONGI_FSUB_D);
    return;
  } else {
    Reg dest = ra_dest(as, ir, RSET_GPR);
    Reg right, left = ra_alloc2(as, ir, RSET_GPR);
    right = (left >> 8); left &= 255;
    emit_djk(as, irt_is64(ir->t) ? LOONGI_SUB_D : LOONGI_SUB_W, dest,
	     left, right);
  }
}

static void asm_mul(ASMState *as, IRIns *ir)
{
  if (irt_isnum(ir->t)) {
    asm_fparith(as, ir, LOONGI_FMUL_D);
  } else
  {
    Reg dest = ra_dest(as, ir, RSET_GPR);
    Reg right, left = ra_alloc2(as, ir, RSET_GPR);
    right = (left >> 8); left &= 255;
    if (irt_is64(ir->t)) {
      emit_djk(as, LOONGI_MUL_D, dest, left, right);
    } else {
      emit_djk(as, LOONGI_MUL_W, dest, left, right);
    }
  }
}

static void asm_fpdiv(ASMState *as, IRIns *ir)
{
    asm_fparith(as, ir, LOONGI_FDIV_D);
}

static void asm_neg(ASMState *as, IRIns *ir)
{
  if (irt_isnum(ir->t)) {
    asm_fpunary(as, ir, LOONGI_FNEG_D);
  } else {
    Reg dest = ra_dest(as, ir, RSET_GPR);
    Reg left = ra_hintalloc(as, ir->op1, dest, RSET_GPR);
    emit_djk(as, irt_is64(ir->t) ? LOONGI_SUB_D : LOONGI_SUB_W, dest,
	     RID_ZERO, left);
  }
}

#define asm_abs(as, ir)		asm_fpunary(as, ir, LOONGI_FABS_D)

static void asm_arithov(ASMState *as, IRIns *ir)
{
  RegSet allow = RSET_GPR;
  Reg right, left, tmp, tmp2, dest = ra_dest(as, ir, allow);
  rset_clear(allow, dest);
  lj_assertA(!irt_is64(ir->t), "bad usage");
  tmp2 = ra_scratch(as, allow);
  rset_clear(allow, tmp2);
  if (irref_isk(ir->op2)) {
    int k = IR(ir->op2)->i;
    if (ir->o == IR_SUBOV) k = -k;
    if (LOONGF_S_OK(k, 12)) {	/* (dest < left) == (k >= 0 ? 1 : 0) */
      left = ra_alloc1(as, ir->op1, allow);
      asm_guard(as, k >= 0 ? LOONGI_BNE : LOONGI_BEQ, tmp2, RID_ZERO);
      emit_djk(as, LOONGI_SLT, tmp2, dest, dest == left ? tmp2 : left);
      emit_dji(as, LOONGI_ADDI_D, dest, left, k&0xfff);
      if (dest == left) emit_move(as, tmp2, left);
      return;
    }
  }
  left = ra_alloc2(as, ir, allow);
  right = (left >> 8); left &= 255;
  rset_clear(allow, right);
  rset_clear(allow, left);
  tmp = ra_scratch(as, allow);
  asm_guard(as, LOONGI_BLT, tmp2, RID_ZERO);
  emit_djk(as, LOONGI_AND, tmp2, RID_TMP, tmp);
  if (ir->o == IR_ADDOV) {  /* ((dest^left) & (dest^right)) < 0 */
    emit_djk(as, LOONGI_XOR, RID_TMP, dest, dest == right ? RID_TMP : right);
  } else {  /* ((dest^left) & (dest^~right)) < 0 */
    emit_djk(as, LOONGI_XOR, RID_TMP, RID_TMP, dest);
    emit_djk(as, LOONGI_NOR, RID_TMP, dest == right ? RID_TMP : right, RID_ZERO);
  }
  emit_djk(as, LOONGI_XOR, tmp, dest, dest == left ? RID_TMP : left);
  emit_djk(as, ir->o == IR_ADDOV ? LOONGI_ADD_W : LOONGI_SUB_W, dest, left, right);
  if (dest == left || dest == right)
    emit_move(as, RID_TMP, dest == left ? left : right);
}

#define asm_addov(as, ir)	asm_arithov(as, ir)
#define asm_subov(as, ir)	asm_arithov(as, ir)

static void asm_mulov(ASMState *as, IRIns *ir)
{
  Reg dest = ra_dest(as, ir, RSET_GPR);
  Reg tmp, tmp2, right, left = ra_alloc2(as, ir, RSET_GPR);
  right = (left >> 8); left &= 255;
  tmp = ra_scratch(as, rset_exclude(rset_exclude(rset_exclude(RSET_GPR, left),
						 right), dest));
  tmp2 = ra_scratch(as, rset_exclude(rset_exclude(rset_exclude(rset_exclude(RSET_GPR, left),
						right), dest), tmp));
  asm_guard(as, LOONGI_BNE, tmp2, tmp);
  emit_dju(as, LOONGI_SRAI_W, tmp2, dest, 31);
  emit_djk(as, LOONGI_MUL_W, dest, left, right);	// dest: [31:0]+signextend
  emit_djk(as, LOONGI_MULH_W, tmp, left, right);	// tmp: [63:32]
}

static void asm_bnot(ASMState *as, IRIns *ir)
{
  Reg left, right, dest = ra_dest(as, ir, RSET_GPR);
  IRIns *irl = IR(ir->op1);
  if (mayfuse(as, ir->op1) && irl->o == IR_BOR) {
    left = ra_alloc2(as, irl, RSET_GPR);
    right = (left >> 8); left &= 255;
  } else {
    left = ra_hintalloc(as, ir->op1, dest, RSET_GPR);
    right = RID_ZERO;
  }
  emit_djk(as, LOONGI_NOR, dest, left, right);
}

static void asm_bswap(ASMState *as, IRIns *ir)
{
  Reg dest = ra_dest(as, ir, RSET_GPR);
  Reg left = ra_alloc1(as, ir->op1, RSET_GPR);
  if (irt_is64(ir->t)) {
    emit_dj(as, LOONGI_REVH_D, dest, RID_TMP);
    emit_dj(as, LOONGI_REVB_4H, RID_TMP, left);
  } else {
    emit_dju(as, LOONGI_ROTRI_W, dest, RID_TMP, 16);
    emit_dj(as, LOONGI_REVB_2H, RID_TMP, left);
  }
}

static void asm_bitop(ASMState *as, IRIns *ir, LOONGIns loongi, LOONGIns loongik)
{
  Reg dest = ra_dest(as, ir, RSET_GPR);
  Reg right, left = ra_hintalloc(as, ir->op1, dest, RSET_GPR);
  if (irref_isk(ir->op2)) {
    intptr_t k = get_kval(as, ir->op2);
    if (checku12(k)) {
      emit_dji(as, loongik, dest, left, k&0xfff);
      return;
    }
  }
  right = ra_alloc1(as, ir->op2, rset_exclude(RSET_GPR, left));
  emit_djk(as, loongi, dest, left, right);
}

#define asm_band(as, ir)	asm_bitop(as, ir, LOONGI_AND, LOONGI_ANDI)
#define asm_bor(as, ir)		asm_bitop(as, ir, LOONGI_OR, LOONGI_ORI)
#define asm_bxor(as, ir)	asm_bitop(as, ir, LOONGI_XOR, LOONGI_XORI)

static void asm_bitshift(ASMState *as, IRIns *ir, LOONGIns loongi, LOONGIns loongik)
{
  Reg dest = ra_dest(as, ir, RSET_GPR);
  Reg left = ra_alloc1(as, ir->op1, RSET_GPR);
  uint32_t shmask = irt_is64(ir->t) ? 63 : 31;
  if (irref_isk(ir->op2)) {  /* Constant shifts. */
    uint32_t shift = (uint32_t)(IR(ir->op2)->i & shmask);
    emit_dju(as, loongik, dest, left, shift);
  } else {
    Reg right = ra_alloc1(as, ir->op2, rset_exclude(RSET_GPR, left));
    emit_djk(as, loongi, dest, left, right);  /* Shift amount is in rs. */
  }
}

#define asm_bshl(as, ir)	(irt_is64(ir->t) ? \
  asm_bitshift(as, ir, LOONGI_SLL_D, LOONGI_SLLI_D) : \
  asm_bitshift(as, ir, LOONGI_SLL_W, LOONGI_SLLI_W))
#define asm_bshr(as, ir)	(irt_is64(ir->t) ? \
  asm_bitshift(as, ir, LOONGI_SRL_D, LOONGI_SRLI_D) : \
  asm_bitshift(as, ir, LOONGI_SRL_W, LOONGI_SRLI_W))
#define asm_bsar(as, ir)	(irt_is64(ir->t) ? \
  asm_bitshift(as, ir, LOONGI_SRA_D, LOONGI_SRAI_D) : \
  asm_bitshift(as, ir, LOONGI_SRA_W, LOONGI_SRAI_W))
#define asm_brol(as, ir)	lj_assertA(0, "unexpected BROL")
#define asm_bror(as, ir)	(irt_is64(ir->t) ? \
  asm_bitshift(as, ir, LOONGI_ROTR_D, LOONGI_ROTRI_D) : \
  asm_bitshift(as, ir, LOONGI_ROTR_W, LOONGI_ROTRI_W))

static void asm_min_max(ASMState *as, IRIns *ir, int ismax)
{
  if (irt_isnum(ir->t)) {
    Reg dest = ra_dest(as, ir, RSET_FPR);
    Reg right, left = ra_alloc2(as, ir, RSET_FPR);
    right = (left >> 8); left &= 255;
    emit_djk(as, ismax ? LOONGI_FMAX_D : LOONGI_FMIN_D, dest, left, right);
  } else {
    Reg dest = ra_dest(as, ir, RSET_GPR);
    Reg left = ra_hintalloc(as, ir->op1, dest, RSET_GPR);
    Reg right = ra_alloc1(as, ir->op2, rset_exclude(RSET_GPR, left));
    emit_djk(as, LOONGI_OR, dest, dest, RID_TMP);
    if (dest != right) {
      emit_djk(as, LOONGI_MASKEQZ, RID_TMP, right, RID_TMP);
      emit_djk(as, LOONGI_MASKNEZ, dest, left, RID_TMP);
    } else {
      emit_djk(as, LOONGI_MASKNEZ, RID_TMP, left, RID_TMP);
      emit_djk(as, LOONGI_MASKEQZ, dest, right, RID_TMP);
    }
    emit_djk(as, LOONGI_SLT, RID_TMP,
	     ismax ? left : right, ismax ? right : left);
  }
}

#define asm_min(as, ir)		asm_min_max(as, ir, 0)
#define asm_max(as, ir)		asm_min_max(as, ir, 1)

/* -- Comparisons --------------------------------------------------------- */

/* FP comparisons. */
static void asm_fpcomp(ASMState *as, IRIns *ir)
{
  IROp op = ir->o;
  Reg right, left = ra_alloc2(as, ir, RSET_FPR);
  right = (left >> 8); left &= 255;
  asm_guard21(as, (op&1) ? LOONGI_BCNEZ : LOONGI_BCEQZ, 0);
  switch (op) {
    case IR_LT: case IR_UGE:
      emit_djk(as, LOONGI_FCMP_CLT_D, 0, left, right);
      break;
    case IR_GE: case IR_ULT:
      emit_djk(as, LOONGI_FCMP_CULT_D, 0, left, right);
      break;
    case IR_LE: case IR_UGT: case IR_ABC:
      emit_djk(as, LOONGI_FCMP_CLE_D, 0, left, right);
      break;
    case IR_ULE: case IR_GT:
      emit_djk(as, LOONGI_FCMP_CULE_D, 0, left, right);
      break;
    case IR_EQ: case IR_NE:
      emit_djk(as, LOONGI_FCMP_CEQ_D, 0, left, right);
      break;
    default:
      break;
  }
}

/* Integer comparisons. */
static void asm_intcomp(ASMState *as, IRIns *ir)
{
  /* ORDER IR: LT GE LE GT  ULT UGE ULE UGT. */
  /*           00 01 10 11  100 101 110 111  */
  IROp op = ir->o;
  RegSet allow = RSET_GPR;
  Reg tmp, right, left = ra_alloc1(as, ir->op1, allow);
  rset_clear(allow, left);
  if (op == IR_ABC) op = IR_UGT;
  if ((op&4) == 0 && irref_isk(ir->op2) && get_kval(as, ir->op2) == 0) {
    switch (op) {
      case IR_GT: asm_guard(as, LOONGI_BGE, RID_ZERO, left); break;
      case IR_LE: asm_guard(as, LOONGI_BLT, RID_ZERO, left); break;
      case IR_GE: asm_guard(as, LOONGI_BLT, left, RID_ZERO); break;
      case IR_LT: asm_guard(as, LOONGI_BGE, left, RID_ZERO); break;
      default: break;
    }
    return;
  }
  tmp = ra_scratch(as, allow);
  rset_clear(allow, tmp);
  if (irref_isk(ir->op2)) {
    intptr_t k = get_kval(as, ir->op2);
    if ((op&2)) k++;
    if (checki12(k)) {
      asm_guard(as, (op&1) ? LOONGI_BNE : LOONGI_BEQ, tmp, RID_ZERO);
      emit_dji(as, (op&4) ? LOONGI_SLTUI : LOONGI_SLTI, tmp, left, k&0xfff);
      return;
    }
  }
  right = ra_alloc1(as, ir->op2, allow);
  asm_guard(as, ((op^(op>>1))&1) ? LOONGI_BNE : LOONGI_BEQ, tmp, RID_ZERO);
  emit_djk(as, (op&4) ? LOONGI_SLTU : LOONGI_SLT,
           tmp, (op&2) ? right : left, (op&2) ? left : right);
}

static void asm_comp(ASMState *as, IRIns *ir)
{
  if (irt_isnum(ir->t))
    asm_fpcomp(as, ir);
  else
    asm_intcomp(as, ir);
}

static void asm_equal(ASMState *as, IRIns *ir)
{
  if (irt_isnum(ir->t)) {
    asm_fpcomp(as, ir);
  } else {
    Reg right, left = ra_alloc2(as, ir, RSET_GPR);
    right = (left >> 8); left &= 255;
    asm_guard(as, (ir->o & 1) ? LOONGI_BEQ : LOONGI_BNE, left, right);
  }
}

/* -- Split register ops -------------------------------------------------- */

/* Hiword op of a split 64 bit op. Previous op must be the loword op. */
static void asm_hiop(ASMState *as, IRIns *ir)
{
  /* HIOP is marked as a store because it needs its own DCE logic. */
  int uselo = ra_used(ir-1), usehi = ra_used(ir);  /* Loword/hiword used? */
  if (LJ_UNLIKELY(!(as->flags & JIT_F_OPT_DCE))) uselo = usehi = 1;
  if (!usehi) return;  /* Skip unused hiword op for all remaining ops. */
  switch ((ir-1)->o) {
  case IR_CALLN:
  case IR_CALLL:
  case IR_CALLS:
  case IR_CALLXS:
    if (!uselo)
      ra_allocref(as, ir->op1, RID2RSET(RID_RETLO));  /* Mark lo op as used. */
    break;
  default: lj_assertA(0, "bad HIOP for op %d", (ir-1)->o); break;
  }
}

/* -- Profiling ----------------------------------------------------------- */

static void asm_prof(ASMState *as, IRIns *ir)
{
  UNUSED(ir);
  Reg tmp = ra_scratch(as, RSET_GPR);
  asm_guard(as, LOONGI_BNE, tmp, RID_ZERO);
  emit_dju(as, LOONGI_ANDI, tmp, tmp, HOOK_PROFILE);
  emit_lsglptr2(as, LOONGI_LD_BU, tmp,
	       (int32_t)offsetof(global_State, hookmask));
}

/* -- Stack handling ------------------------------------------------------ */

/* Check Lua stack size for overflow. Use exit handler as fallback. */
static void asm_stack_check(ASMState *as, BCReg topslot,
			    IRIns *irp, RegSet allow, ExitNo exitno)
{
  /* Try to get an unused temp register, otherwise spill/restore RID_RET*. */
  Reg tmp, pbase = irp ? (ra_hasreg(irp->r) ? irp->r : RID_TMP) : RID_BASE;
  ExitNo oldsnap = as->snapno;
  rset_clear(allow, pbase);
  as->snapno = exitno;
  asm_guard(as, LOONGI_BNE, RID_R20, RID_ZERO);
  as->snapno = oldsnap;
  if (allow) {
    tmp = rset_pickbot(allow);
    ra_modified(as, tmp);
  } else {	// allow == RSET_EMPTY
    tmp = RID_RET;
    emit_dji(as, LOONGI_LD_D, tmp, RID_SP, 0);	/* Restore tmp1 register. */
  }
  lj_assertA(checki12(8*topslot), "slot offset %d does not fit in si12", 8*topslot);
  emit_dji(as, LOONGI_SLTUI, RID_R20, RID_R20, (int32_t)(8*topslot)&0xfff);
  emit_djk(as, LOONGI_SUB_D, RID_R20, tmp, pbase);
  emit_dji(as, LOONGI_LD_D, tmp, tmp, offsetof(lua_State, maxstack));
  if (pbase == RID_TMP)
    emit_getgl(as, RID_TMP, jit_base);
  emit_getgl(as, tmp, cur_L);
  if (allow == RSET_EMPTY)  /* Spill temp register. */
    emit_dji(as, LOONGI_ST_D, tmp, RID_SP, 0);
}

/* Restore Lua stack from on-trace state. */
static void asm_stack_restore(ASMState *as, SnapShot *snap)
{
  SnapEntry *map = &as->T->snapmap[snap->mapofs];
#ifdef LUA_USE_ASSERT
  SnapEntry *flinks = &as->T->snapmap[snap_nextofs(as->T, snap)-1-LJ_FR2];
#endif
  MSize n, nent = snap->nent;
  /* Store the value of all modified slots to the Lua stack. */
  for (n = 0; n < nent; n++) {
    SnapEntry sn = map[n];
    BCReg s = snap_slot(sn);
    int32_t ofs = 8*((int32_t)s-1-LJ_FR2);
    IRRef ref = snap_ref(sn);
    IRIns *ir = IR(ref);
    if ((sn & SNAP_NORESTORE))
      continue;
    if (irt_isnum(ir->t)) {
      Reg src = ra_alloc1(as, ref, RSET_FPR);
      emit_dji(as, LOONGI_FST_D, src, RID_BASE, ofs&0xfff);
    } else {
      if ((sn & SNAP_KEYINDEX)) {
        RegSet allow = rset_exclude(RSET_GPR, RID_BASE);
	int64_t kki = (int64_t)LJ_KEYINDEX << 32;
	if (irref_isk(ref)) {
	  emit_djk(as, LOONGI_STX_D,
	           ra_allock(as, kki | (int64_t)(uint32_t)ir->i, allow),
		   RID_BASE, RID_R20);
	  emit_d16i(as, RID_R20, ofs);
	} else {
	  Reg src = ra_alloc1(as, ref, allow);
	  Reg rki = ra_allock(as, kki, rset_exclude(allow, src));
	  emit_djk(as, LOONGI_STX_D, RID_TMP, RID_BASE, RID_R20);
	  emit_d16i(as, RID_R20, ofs);
	  emit_djk(as, LOONGI_ADD_D, RID_TMP, src, rki);
	}
      } else {
        asm_tvstore64(as, RID_BASE, ofs, ref);
      }
    }
    checkmclim(as);
  }
  lj_assertA(map + nent == flinks, "inconsistent frames in snapshot");
}

/* -- GC handling --------------------------------------------------------- */

/* Marker to prevent patching the GC check exit. */
#define LOONG_NOPATCH_GC_CHECK	LOONGI_OR

/* Check GC threshold and do one or more GC steps. */
static void asm_gc_check(ASMState *as)
{
  const CCallInfo *ci = &lj_ir_callinfo[IRCALL_lj_gc_step_jit];
  IRRef args[2];
  MCLabel l_end;
  Reg tmp1, tmp2;
  ra_evictset(as, RSET_SCRATCH);
  l_end = emit_label(as);
  /* Exit trace if in GCSatomic or GCSfinalize. Avoids syncing GC objects. */
  asm_guard(as, LOONGI_BNE, RID_RET, RID_ZERO);	/* Assumes asm_snap_prep() already done. */
  *--as->mcp = LOONG_NOPATCH_GC_CHECK;
  args[0] = ASMREF_TMP1;  /* global_State *g */
  args[1] = ASMREF_TMP2;  /* MSize steps     */
  asm_gencall(as, ci, args);
  tmp1 = ra_releasetmp(as, ASMREF_TMP1);
  tmp2 = ra_releasetmp(as, ASMREF_TMP2);
  ra_allockreg(as, (int64_t)(J2G(as->J)), tmp1);
  emit_loadi(as, tmp2, as->gcsteps);
  /* Jump around GC step if GC total < GC threshold. */
  emit_branch(as, LOONGI_BLTU, RID_TMP, tmp2, l_end);
  emit_getgl(as, tmp2, gc.threshold);
  emit_getgl(as, RID_TMP, gc.total);
  as->gcsteps = 0;
  checkmclim(as);
}

/* -- Loop handling ------------------------------------------------------- */

/* Fixup the loop branch. */
static void asm_loop_fixup(ASMState *as)
{
  MCode *p = as->mctop;
  MCode *target = as->mcp;
  if (as->loopinv) {  /* Inverted loop branch? */
    /* asm_guard* already inverted the bceqz/bcnez/beq/bne/blt/bge, and patched the final b. */
    uint32_t mask = (p[-2] & 0xfc000000) == 0x48000000 ? 0x1fffffu : 0xffffu;
    ptrdiff_t delta = target - (p - 2);
    if (mask == 0x1fffffu) {	/* BCEQZ  BCNEZ*/
      p[-2] = p[-2] | LOONGF_I((uint32_t)delta & 0xffffu) | (((uint32_t)delta & 0x1f0000u) >> 16);
    } else {	/* BEQ BNE BLE BGE BLTU BGEU*/
      p[-2] |= LOONGF_I(delta & 0xffffu);
    }
    if (p[-1] == 0)
      p[-1] = LOONGI_NOP;
  } else {
    /* b */
    ptrdiff_t delta = target - (p - 1);
    p[-1] = LOONGI_B | LOONGF_I(delta & 0xffffu) | ((delta & 0x3ff0000) >> 16);
  }
}

/* Fixup the tail of the loop. */
static void asm_loop_tail_fixup(ASMState *as)
{
  UNUSED(as);	/* Nothing to do. */
}

/* -- Head of trace ------------------------------------------------------- */

/* Coalesce BASE register for a root trace. */
static void asm_head_root_base(ASMState *as)
{
  IRIns *ir = IR(REF_BASE);
  Reg r = ir->r;
  if (ra_hasreg(r)) {
    ra_free(as, r);
    if (rset_test(as->modset, r) || irt_ismarked(ir->t))
      ir->r = RID_INIT;  /* No inheritance for modified BASE register. */
    if (r != RID_BASE)
      emit_move(as, r, RID_BASE);
  }
}

/* Coalesce BASE register for a side trace. */
static RegSet asm_head_side_base(ASMState *as, IRIns *irp, RegSet allow)
{
  IRIns *ir = IR(REF_BASE);
  Reg r = ir->r;
  if (ra_hasreg(r)) {
    ra_free(as, r);
    if (rset_test(as->modset, r) || irt_ismarked(ir->t))
      ir->r = RID_INIT;  /* No inheritance for modified BASE register. */
    if (irp->r == r) {
      rset_clear(allow, r);  /* Mark same BASE register as coalesced. */
    } else if (ra_hasreg(irp->r) && rset_test(as->freeset, irp->r)) {
      rset_clear(allow, irp->r);
      emit_move(as, r, irp->r);  /* Move from coalesced parent reg. */
    } else {
      emit_getgl(as, r, jit_base);  /* Otherwise reload BASE. */
    }
  }
  return allow;
}

/* -- Tail of trace ------------------------------------------------------- */

/* Fixup the tail code. */
static void asm_tail_fixup(ASMState *as, TraceNo lnk)
{
  MCode *target = lnk ? traceref(as->J,lnk)->mcode : (MCode *)lj_vm_exit_interp;
  int32_t spadj = as->T->spadjust;
  MCode *p = as->mctop - 1;
  if (spadj == 0) {
    p[-1] = LOONGI_NOP;
  } else {
    p[-1] = LOONGI_ADDI_D|LOONGF_D(RID_SP)|LOONGF_J(RID_SP)|LOONGF_I(spadj);
  }

  MCode *tmp = p;
  *p = LOONGI_B | LOONGF_I((uintptr_t)(target-tmp)&0xffffu) | (((uintptr_t)(target-tmp)&0x3ff0000u) >> 16);
}

/* Prepare tail of code. */
static void asm_tail_prep(ASMState *as)
{
  MCode *p = as->mctop - 1;  /* Leave room for exit branch. */
  if (as->loopref) {
    as->invmcp = as->mcp = p;
  } else {
    as->mcp = p-1;  /* Leave room for stack pointer adjustment. */
    as->invmcp = NULL;
  }
  *p = LOONGI_NOP;  /* Prevent load/store merging. */
}

/* -- Trace setup --------------------------------------------------------- */

/* Ensure there are enough stack slots for call arguments. */
static Reg asm_setup_call_slots(ASMState *as, IRIns *ir, const CCallInfo *ci)
{
  IRRef args[CCI_NARGS_MAX*2];
  uint32_t i, nargs = CCI_XNARGS(ci);
  int nslots = 0, ngpr = REGARG_NUMGPR, nfpr = REGARG_NUMFPR;
  asm_collectargs(as, ir, ci, args);
  for (i = 0; i < nargs; i++) {
    if (args[i] && irt_isfp(IR(args[i])->t)) {
      if (nfpr > 0)
        nfpr--;
      else if (ngpr > 0)
	ngpr--;
      else
	nslots += 2;
    } else {
      if (ngpr > 0)
	ngpr--;
      else
	nslots += 2;
    }
  }
  if (nslots > as->evenspill)  /* Leave room for args in stack slots. */
    as->evenspill = nslots;
  return REGSP_HINT(RID_RET);
}

static void asm_sparejump_setup(ASMState *as)
{
  MCode *mxp = as->mctop;
  if ((char *)mxp == (char *)as->J->mcarea + as->J->szmcarea) {
    mxp -= 4*1;
    as->mctop = mxp;
  }
}

static void asm_setup_target(ASMState *as)
{
  asm_sparejump_setup(as);
  asm_exitstub_setup(as);
}

/* -- Trace patching ------------------------------------------------------ */

/* Patch exit jumps of existing machine code to a new target. */
void lj_asm_patchexit(jit_State *J, GCtrace *T, ExitNo exitno, MCode *target)
{
  MCode *p = T->mcode;
  MCode *pe = (MCode *)((char *)p + T->szmcode);
  MCode *px = exitstub_trace_addr(T, exitno);
  MCode *cstart = NULL;
  MCode *mcarea = lj_mcode_patch(J, p, 0);

  MCode exitload = LOONGI_ADDI_D | LOONGF_D(RID_TMP) | LOONGF_J(RID_ZERO) | LOONGF_I(exitno&0xfff);

  for (; p < pe; p++) {
    if (*p == exitload) {
    /* Look for exitstub branch, replace with branch to target. */
    ptrdiff_t delta = target - p - 1;
    MCode ins = p[1];
      if (((ins ^ ((px-p-1)<<10)) & 0x3fffc00) == 0 &&
          ((ins & 0xfc000000u) == LOONGI_BEQ ||
           (ins & 0xfc000000u) == LOONGI_BNE ||
           (ins & 0xfc000000u) == LOONGI_BLT ||
           (ins & 0xfc000000u) == LOONGI_BGE ||
	   (ins & 0xfc000000u) == LOONGI_BLTU)) {
        /* Patch beq/bne/blt/bge, if within range. */
        if (p[-1] == LOONG_NOPATCH_GC_CHECK) {
	  /* nothing */
        } else if (LOONGF_S_OK(delta, 16)) {
          p[1] = (ins & 0xfc0003ffu) | LOONGF_I(delta & 0xffff);
          *p = LOONGI_NOP;
          if (!cstart) cstart = p + 1;
        }
      } else if (((ins ^ ((((px-p-1)&0xffff)<<10) + (((px-p-1)>>10)&0x1f))) & 0x3fffc1f) == 0 &&
                 ((ins & 0xfc000000u) == LOONGI_BCEQZ ||
                  (ins & 0xfc000100u) == LOONGI_BCNEZ)) {
        /* Patch bceqz/bcnez, if within range. */
        if (p[-1] == LOONG_NOPATCH_GC_CHECK) {
	  /* nothing */
        } else if (LOONGF_S_OK(delta, 21)) {
          p[1] = (ins & 0xfc0003e0u) | LOONGF_I(delta & 0xffff) | ((delta & 0x1f0000) >> 16);
          *p = LOONGI_NOP;
          if (!cstart) cstart = p + 1;
        }
      } else if (((ins ^ ((((px-p-1)&0xffff)<<10) + (((px-p-1)>>10)&0x3f))) & 0x3ffffff) == 0 &&
          ((ins & 0xfc000000u) == LOONGI_B)) {
        /* Patch b. */
        lj_assertJ(LOONGF_S_OK(delta, 26), "branch target out of range");
        p[1] = (ins & 0xfc000000u) | LOONGF_I(delta & 0xffff) | ((delta & 0x3ff0000) >> 16);
        *p = LOONGI_NOP;
        if (!cstart) cstart = p + 1;
      } else if (p+2 == pe){
         if (p[2] == LOONGI_NOP) {
            ptrdiff_t delta = target - &p[2];
            lj_assertJ(LOONGF_S_OK(delta, 26), "branch target out of range");
            p[2] = LOONGI_B | LOONGF_I(delta & 0xffff) | ((delta & 0x3ff0000) >> 16);
            *p = LOONGI_NOP;
            if (!cstart) cstart = p + 2;
         }
       }
    }
  }
  if (cstart) lj_mcode_sync(cstart, px+1);
  lj_mcode_patch(J, mcarea, 1);
}
