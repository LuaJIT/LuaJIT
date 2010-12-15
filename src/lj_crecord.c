/*
** Trace recorder for C data operations.
** Copyright (C) 2005-2010 Mike Pall. See Copyright Notice in luajit.h
*/

#define lj_ffrecord_c
#define LUA_CORE

#include "lj_obj.h"

#if LJ_HASJIT && LJ_HASFFI

#include "lj_err.h"
#include "lj_str.h"
#include "lj_ctype.h"
#include "lj_cconv.h"
#include "lj_ir.h"
#include "lj_jit.h"
#include "lj_iropt.h"
#include "lj_trace.h"
#include "lj_ffrecord.h"
#include "lj_crecord.h"
#include "lj_dispatch.h"

/* Some local macros to save typing. Undef'd at the end. */
#define IR(ref)			(&J->cur.ir[(ref)])

/* Pass IR on to next optimization in chain (FOLD). */
#define emitir(ot, a, b)	(lj_ir_set(J, (ot), (a), (b)), lj_opt_fold(J))

/* -- C type checks ------------------------------------------------------- */

static GCcdata *argv2cdata(jit_State *J, TRef trcd, TValue *o)
{
  GCcdata *cd;
  TRef trtypeid;
  if (!tviscdata(o))
    lj_trace_err(J, LJ_TRERR_BADTYPE);
  cd = cdataV(o);
  /* Specialize to the CTypeID. */
  trtypeid = emitir(IRT(IR_FLOAD, IRT_U16), trcd, IRFL_CDATA_TYPEID);
  emitir(IRTG(IR_EQ, IRT_INT), trtypeid, lj_ir_kint(J, (int32_t)cd->typeid));
  return cd;
}

/* -- Convert C type to C type -------------------------------------------- */

/*
** This code mirrors the code in lj_cconv.c. It performs the same steps
** for the trace recorder that lj_cconv.c does for the interpreter.
**
** One major difference is that we can get away with much fewer checks
** here. E.g. checks for casts, constness or correct types can often be
** omitted, even if they might fail. The interpreter subsequently throws
** an error, which aborts the trace.
**
** All operations are specialized to their C types, so the on-trace
** outcome must be the same as the outcome in the interpreter. If the
** interpreter doesn't throw an error, then the trace is correct, too.
** Care must be taken not to generate invalid (temporary) IR or to
** trigger asserts.
*/

/* Convert CType to IRType. */
static IRType crec_ct2irt(CType *ct)
{
  if (LJ_LIKELY(ctype_isnum(ct->info))) {
    if ((ct->info & CTF_FP)) {
      if (ct->size == sizeof(double))  /* NYI: float IRType. */
	return IRT_NUM;
    } else {
      uint32_t b = lj_fls(ct->size);
      if (b <= 3)
	return IRT_I8 + 2*b + ((ct->info & CTF_UNSIGNED) ? 1 : 0);
    }
  } else if (ctype_isptr(ct->info)) {
    return (LJ_64 && ct->size == 8) ? IRT_P64 : IRT_P32;
  }
  return IRT_CDATA;
}

static void crec_ct_ct(jit_State *J, CType *d, CType *s, TRef dp, TRef sp)
{
  CTState *cts = ctype_ctsG(J2G(J));
  CTSize dsize = d->size, ssize = s->size;
  CTInfo dinfo = d->info, sinfo = s->info;
  IRType dt = crec_ct2irt(d);

  if (ctype_type(dinfo) > CT_MAYCONVERT || ctype_type(sinfo) > CT_MAYCONVERT)
    goto err_conv;

  /*
  ** Note: Unlike lj_cconv_ct_ct(), sp holds the _value_ of pointers and
  ** numbers up to 8 bytes. Otherwise sp holds a pointer.
  */

  switch (cconv_idx2(dinfo, sinfo)) {
  /* Destination is a bool. */
  case CCX(B, B):
    goto xstore;  /* Source operand is already normalized. */
  case CCX(B, I):
  case CCX(B, P):
  case CCX(B, F):
  case CCX(B, C):
  case CCX(B, A):
    /* NYI: specialize to the result of a comparison against 0. */
    goto err_nyi;

  /* Destination is an integer. */
  case CCX(I, B):
  case CCX(I, I):
  conv_I_I:
    lua_assert(ssize >= 4);
    if (dsize > 8 || ssize > 8) goto err_nyi;
    if (dsize > ssize)  /* Zero-extend or sign-extend 32 to 64 bit integer. */
      sp = emitir(IRT(IR_TOI64, dt), sp,
		  (sinfo&CTF_UNSIGNED) ? IRTOINT_ZEXT64 : IRTOINT_SEXT64);
  xstore:
    emitir(IRT(IR_XSTORE, dt), dp, sp);
    break;
  case CCX(I, F):
  conv_I_F:
    if (dsize > 8 || ssize != sizeof(double)) goto err_nyi;
    if (dsize == 8) {
      if (dt == IRT_U64) goto err_nyi;
      sp = emitir(IRT(IR_TOI64, dt), sp, IRTOINT_TRUNCI64);
    } else {
      sp = emitir(IRTI(IR_TOINT), sp, IRTOINT_ANY);  /* NYI: should truncate. */
    }
    goto xstore;
  case CCX(I, C):
    if (ssize != 2*sizeof(double)) goto err_nyi;
    sp = emitir(IRT(IR_XLOAD, IRT_NUM), sp, 0);  /* Load re. */
    s = ctype_child(cts, s);
    sinfo = s->info;
    ssize = s->size;
    goto conv_I_F;  /* Just convert re. */
  case CCX(I, P):
  case CCX(I, A):
    sinfo = CTINFO(CT_NUM, CTF_UNSIGNED);
    ssize = CTSIZE_PTR;
    /*
    ** Note: Overriding the size is also required for pointers, since
    ** crec_ct_tv passes IRT_P32/IRT_P64 independently of the C type size.
    ** This avoids unnecessary zero-extensions on x64.
    */
    goto conv_I_I;

  /* Destination is a floating-point number. */
  case CCX(F, B):
  case CCX(F, I):
  conv_F_I:
    if (dsize != sizeof(double) || ssize > 4) goto err_nyi;
    if (ssize == 4 && (sinfo & CTF_UNSIGNED)) goto err_nyi;
    sp = emitir(IRTI(IR_TONUM), sp, 0);
    goto xstore;
  case CCX(F, F):
  conv_F_F:
    if (dsize != sizeof(double) || ssize != sizeof(double)) goto err_nyi;
    goto xstore;
  case CCX(F, C):
    if (ssize != 2*sizeof(double)) goto err_nyi;
    sp = emitir(IRT(IR_XLOAD, IRT_NUM), sp, 0);  /* Load re. */
    s = ctype_child(cts, s);
    sinfo = s->info;
    ssize = s->size;
    goto conv_F_F;  /* Ignore im, and convert from re. */

  /* Destination is a complex number. */
  case CCX(C, I):
  case CCX(C, F):
    d = ctype_child(cts, d);
    dinfo = d->info;
    dsize = d->size;
    if (dsize != sizeof(double)) goto err_nyi;
    {  /* Clear im. */
      TRef dpim = emitir(IRT(IR_ADD, IRT_PTR), dp, lj_ir_kintp(J, dsize));
      emitir(IRT(IR_XSTORE, IRT_NUM), dpim, lj_ir_knum(J, 0));
    }
    /* Convert to re. */
    if ((sinfo & CTF_FP)) goto conv_F_F; else goto conv_F_I;

  case CCX(C, C):
    d = ctype_child(cts, d);
    dinfo = d->info;
    dsize = d->size;
    if (dsize != sizeof(double)) goto err_nyi;
    {
      TRef spim = emitir(IRT(IR_ADD, IRT_PTR), sp, lj_ir_kintp(J, dsize));
      TRef re = emitir(IRT(IR_XLOAD, IRT_NUM), sp, 0);
      TRef im = emitir(IRT(IR_XLOAD, IRT_NUM), spim, 0);
      TRef dpim = emitir(IRT(IR_ADD, IRT_PTR), dp, lj_ir_kintp(J, dsize));
      emitir(IRT(IR_XSTORE, IRT_NUM), dp, re);
      emitir(IRT(IR_XSTORE, IRT_NUM), dpim, im);
    }
    break;

  /* Destination is a vector. */
  case CCX(V, I):
  case CCX(V, F):
  case CCX(V, C):
  case CCX(V, V):
    goto err_nyi;

  /* Destination is a pointer. */
  case CCX(P, P):
    /* Note: ok on x64, since all 32 bit ops clear the upper part of the reg. */
    goto xstore;
  case CCX(P, A):
  case CCX(P, S):
    ssize = CTSIZE_PTR;
    sinfo = CTINFO(CT_NUM, CTF_UNSIGNED);
    /* fallthrough */
  case CCX(P, I):
    dinfo = CTINFO(CT_NUM, CTF_UNSIGNED);
    goto conv_I_I;
  case CCX(P, F):
    dinfo = CTINFO(CT_NUM, CTF_UNSIGNED);
    goto conv_I_F;

  /* Destination is an array. */
  case CCX(A, A):
    goto err_nyi;

  /* Destination is a struct/union. */
  case CCX(S, S):
    goto err_nyi;

  default:
  err_conv:
  err_nyi:
    lj_trace_err(J, LJ_TRERR_NYICONV);
    break;
  }
}

/* -- Convert C type to TValue (load) ------------------------------------- */

static TRef crec_tv_ct(jit_State *J, CType *s, CTypeID sid, TRef sp)
{
  CTState *cts = ctype_ctsG(J2G(J));
  CTInfo sinfo = s->info;
  lua_assert(!ctype_isenum(sinfo));
  if (ctype_isnum(sinfo)) {
    IRType t = crec_ct2irt(s);
    if ((sinfo & CTF_BOOL))
      lj_trace_err(J, LJ_TRERR_NYICONV);  /* NYI: specialize to the result. */
    if (t == IRT_CDATA) goto copyval;
    if (t == IRT_U32) lj_trace_err(J, LJ_TRERR_NYICONV);
    return emitir(IRT(IR_XLOAD, t), sp, 0);
  } else if (ctype_isrefarray(sinfo) || ctype_isstruct(sinfo)) {
    /* Create reference. */
    CTypeID refid = lj_ctype_intern(cts, CTINFO_REF(sid), CTSIZE_PTR);
    return emitir(IRTG(IR_CNEWI, IRT_CDATA), sp, lj_ir_kint(J, refid));
  } else {
  copyval:  /* Copy value. */
    lj_trace_err(J, LJ_TRERR_NYICONV);
    return 0;
  }
}

/* -- Convert TValue to C type (store) ------------------------------------ */

static void crec_ct_tv(jit_State *J, CType *d, TRef dp, TRef sp, TValue *sval)
{
  CTState *cts = ctype_ctsG(J2G(J));
  CTypeID sid = CTID_P_VOID;
  CType *s;
  if (LJ_LIKELY(tref_isinteger(sp))) {
    sid = CTID_INT32;
  } else if (tref_isnum(sp)) {
    sid = CTID_DOUBLE;
  } else if (tref_isbool(sp)) {
    sp = lj_ir_kint(J, tref_istrue(sp) ? 1 : 0);
    sid = CTID_BOOL;
  } else if (tref_isnil(sp)) {
    sp = lj_ir_kptr(J, NULL);
  } else if (tref_isudata(sp)) {
    sp = emitir(IRT(IR_ADD, IRT_P32), sp, lj_ir_kint(J, sizeof(GCcdata)));
  } else {  /* NYI: tref_isstr(sp), tref_istab(sp), tref_islightud(sp). */
    sid = argv2cdata(J, sp, sval)->typeid;
    s = ctype_raw(cts, sid);
    if (ctype_isptr(s->info)) {
      IRType t = (LJ_64 && s->size == 8) ? IRT_P64 : IRT_P32;
      sp = emitir(IRT(IR_FLOAD, t), sp, IRFL_CDATA_INIT1);
      if (ctype_isref(s->info))
	s = ctype_rawchild(cts, s);
      else
	goto doconv;  /* The pointer value was loaded, don't load number. */
    } else {
      sp = emitir(IRT(IR_ADD, IRT_P32), sp, lj_ir_kint(J, sizeof(GCcdata)));
    }
    if (ctype_isenum(s->info)) s = ctype_child(cts, s);
    if (ctype_isnum(s->info)) {  /* Load number value. */
      IRType t = crec_ct2irt(s);
      if (t != IRT_CDATA) sp = emitir(IRT(IR_XLOAD, t), sp, 0);
    }
    goto doconv;
  }
  s = ctype_get(cts, sid);
doconv:
  crec_ct_ct(J, d, s, dp, sp);
}

/* -- C data metamethods -------------------------------------------------- */

/* This would be rather difficult in FOLD, so do it here:
** (base+k)+(idx*sz)+ofs ==> (base+idx*sz)+(ofs+k)
** (base+(idx+k)*sz)+ofs ==> (base+idx*sz)+(ofs+k*sz)
*/
static TRef crec_reassoc_ofs(jit_State *J, TRef tr, ptrdiff_t *ofsp, MSize sz)
{
  IRIns *ir = IR(tref_ref(tr));
  if (LJ_LIKELY(J->flags & JIT_F_OPT_FOLD) &&
      ir->o == IR_ADD && irref_isk(ir->op2)) {
    IRIns *irk = IR(ir->op2);
    tr = ir->op1;
#if LJ_64
    if (irk->o == IR_KINT64)
      *ofsp += (ptrdiff_t)ir_kint64(irk)->u64 * sz;
    else
#endif
      *ofsp += (ptrdiff_t)irk->i * sz;
  }
  return tr;
}

void LJ_FASTCALL recff_cdata_index(jit_State *J, RecordFFData *rd)
{
  TRef idx, ptr = J->base[0];
  ptrdiff_t ofs = sizeof(GCcdata);
  GCcdata *cd = argv2cdata(J, ptr, &rd->argv[0]);
  CTState *cts = ctype_ctsG(J2G(J));
  CType *ct = ctype_raw(cts, cd->typeid);
  CTypeID sid = 0;

  /* Resolve pointer or reference for cdata object. */
  if (ctype_isptr(ct->info)) {
    IRType t = (LJ_64 && ct->size == 8) ? IRT_P64 : IRT_P32;
    if (ctype_isref(ct->info)) ct = ctype_rawchild(cts, ct);
    ptr = emitir(IRT(IR_FLOAD, t), ptr, IRFL_CDATA_INIT1);
    ofs = 0;
  }

  idx = J->base[1];
  if (tref_isnumber(idx)) {
    /* The size of a ptrdiff_t is target-specific. */
#if LJ_64
    idx = emitir(IRT(IR_TOI64, IRT_INTP), idx,
		 tref_isinteger(idx) ? IRTOINT_SEXT64 : IRTOINT_TRUNCI64);
#else
    if (!tref_isinteger(idx))
      idx = emitir(IRT(IR_TOINT, IRT_INTP), idx, IRTOINT_ANY);
#endif
    if (ctype_ispointer(ct->info)) {
      ptrdiff_t sz = (ptrdiff_t)lj_ctype_size(cts, (sid = ctype_cid(ct->info)));
      idx = crec_reassoc_ofs(J, idx, &ofs, sz);
      idx = emitir(IRT(IR_MUL, IRT_INTP), idx, lj_ir_kintp(J, sz));
      ptr = crec_reassoc_ofs(J, ptr, &ofs, 1);
      ptr = emitir(IRT(IR_ADD, IRT_PTR), idx, ptr);
    }
  } else if (tref_isstr(idx)) {
    GCstr *name = strV(&rd->argv[1]);
    /* Always specialize to the field name. */
    emitir(IRTG(IR_EQ, IRT_STR), idx, lj_ir_kstr(J, name));
    if (ctype_isptr(ct->info)) {  /* Automatically perform '->'. */
      CType *cct = ctype_rawchild(cts, ct);
      if (ctype_isstruct(cct->info)) {
        ct = cct;
        goto index_struct;
      }
    } else if (ctype_isstruct(ct->info)) {
      CTSize fofs;
      CType *fct;
index_struct:
      fct = lj_ctype_getfield(cts, ct, name, &fofs);
      if (fct) {
	if (ctype_isconstval(fct->info)) {
	  if (fct->size >= 0x80000000u &&
	      (ctype_child(cts, fct)->info & CTF_UNSIGNED)) {
	    J->base[0] = lj_ir_knum(J, (lua_Number)(uint32_t)fct->size);
	    return;
	  }
	  J->base[0] = lj_ir_kint(J, (int32_t)fct->size);
	  return;  /* Interpreter will throw for newindex. */
	} else if (ctype_isbitfield(fct->info)) {
	  lj_trace_err(J, LJ_TRERR_NYICONV);
	} else {
	  lua_assert(ctype_isfield(fct->info));
	  sid = ctype_cid(fct->info);
	}
	ofs += (ptrdiff_t)fofs;
      }
    } else if (ctype_iscomplex(ct->info)) {
      if (strdata(name)[0] == 'i') ofs += (ct->size >> 1);
      sid = ctype_cid(ct->info);
    }
  }
  if (!sid) lj_trace_err(J, LJ_TRERR_BADTYPE);

  if (ofs)
    ptr = emitir(IRT(IR_ADD, IRT_PTR), ptr, lj_ir_kintp(J, ofs));

  /* Resolve reference for field. */
  ct = ctype_get(cts, sid);
  if (ctype_isref(ct->info))
    ptr = emitir(IRT(IR_XLOAD, IRT_PTR), ptr, 0);

  /* Skip attributes and enums. */
  while (ctype_isattrib(ct->info) || ctype_isenum(ct->info))
    ct = ctype_child(cts, ct);

  if (rd->data == 0) {  /* __index metamethod. */
    J->base[0] = crec_tv_ct(J, ct, sid, ptr);
  } else {  /* __newindex metamethod. */
    rd->nres = 0;
    crec_ct_tv(J, ct, ptr, J->base[2], &rd->argv[2]);
  }
}

#undef IR
#undef emitir

#endif
