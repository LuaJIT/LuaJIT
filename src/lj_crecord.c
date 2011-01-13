/*
** Trace recorder for C data operations.
** Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
*/

#define lj_ffrecord_c
#define LUA_CORE

#include "lj_obj.h"

#if LJ_HASJIT && LJ_HASFFI

#include "lj_err.h"
#include "lj_str.h"
#include "lj_ctype.h"
#include "lj_cparse.h"
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

#define emitconv(a, dt, st, flags) \
  emitir(IRT(IR_CONV, (dt)), (a), (st)|((dt) << 5)|(flags))

/* -- C type checks ------------------------------------------------------- */

static GCcdata *argv2cdata(jit_State *J, TRef tr, cTValue *o)
{
  GCcdata *cd;
  TRef trtypeid;
  if (!tref_iscdata(tr))
    lj_trace_err(J, LJ_TRERR_BADTYPE);
  cd = cdataV(o);
  /* Specialize to the CTypeID. */
  trtypeid = emitir(IRT(IR_FLOAD, IRT_U16), tr, IRFL_CDATA_TYPEID);
  emitir(IRTG(IR_EQ, IRT_INT), trtypeid, lj_ir_kint(J, (int32_t)cd->typeid));
  return cd;
}

static CTypeID argv2ctype(jit_State *J, TRef tr, cTValue *o)
{
  if (tref_isstr(tr)) {
    GCstr *s = strV(o);
    CPState cp;
    CTypeID oldtop;
    /* Specialize to the string containing the C type declaration. */
    emitir(IRTG(IR_EQ, IRT_STR), tr, lj_ir_kstr(J, s));
    cp.L = J->L;
    cp.cts = ctype_ctsG(J2G(J));
    oldtop = cp.cts->top;
    cp.srcname = strdata(s);
    cp.p = strdata(s);
    cp.mode = CPARSE_MODE_ABSTRACT|CPARSE_MODE_NOIMPLICIT;
    if (lj_cparse(&cp) || cp.cts->top > oldtop)  /* Avoid new struct defs. */
      lj_trace_err(J, LJ_TRERR_BADTYPE);
    return cp.val.id;
  } else {
    GCcdata *cd = argv2cdata(J, tr, o);
    return cd->typeid == CTID_CTYPEID ? *(CTypeID *)cdataptr(cd) : cd->typeid;
  }
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
      if (ct->size == sizeof(double))
	return IRT_NUM;
      else if (ct->size == sizeof(float))
	return IRT_FLOAT;
    } else {
      uint32_t b = lj_fls(ct->size);
      if (b <= 3)
	return IRT_I8 + 2*b + ((ct->info & CTF_UNSIGNED) ? 1 : 0);
    }
  } else if (ctype_isptr(ct->info)) {
    return (LJ_64 && ct->size == 8) ? IRT_P64 : IRT_P32;
  } else if (ctype_iscomplex(ct->info)) {
    if (ct->size == 2*sizeof(double))
      return IRT_NUM;
    else if (ct->size == 2*sizeof(float))
      return IRT_FLOAT;
  }
  return IRT_CDATA;
}

static void crec_ct_ct(jit_State *J, CType *d, CType *s, TRef dp, TRef sp)
{
  CTSize dsize = d->size, ssize = s->size;
  CTInfo dinfo = d->info, sinfo = s->info;
  IRType dt = crec_ct2irt(d);
  IRType st = crec_ct2irt(s);

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
  case CCX(B, F):
    /* NYI: specialize to the result of a comparison against 0. */
    goto err_nyi;

  /* Destination is an integer. */
  case CCX(I, B):
  case CCX(I, I):
  conv_I_I:
    if (dt == IRT_CDATA || st == IRT_CDATA) goto err_nyi;
#if LJ_64
    /* Sign-extend 32 to 64 bit integer. */
    if (dsize == 8 && ssize < 8 && !(sinfo & CTF_UNSIGNED))
      sp = emitconv(sp, dt, IRT_INT, IRCONV_SEXT);
    /* All other conversions are no-ops on x64. */
#else
    if (dsize == 8 && ssize < 8)  /* Extend to 64 bit integer. */
      sp = emitconv(sp, dt, ssize < 4 ? IRT_INT : st,
		    (sinfo & CTF_UNSIGNED) ? 0 : IRCONV_SEXT);
    else if (dsize < 8 && ssize == 8)  /* Truncate from 64 bit integer. */
      sp = emitconv(sp, dsize < 4 ? IRT_INT : dt, st, 0);
#endif
  xstore:
    emitir(IRT(IR_XSTORE, dt), dp, sp);
    break;
  case CCX(I, C):
    sp = emitir(IRT(IR_XLOAD, st), sp, 0);  /* Load re. */
    /* fallthrough */
  case CCX(I, F):
    if (dt == IRT_CDATA || st == IRT_CDATA) goto err_nyi;
    sp = emitconv(sp, dsize < 4 ? IRT_INT : dt, st, IRCONV_TRUNC|IRCONV_ANY);
    goto xstore;
  case CCX(I, P):
  case CCX(I, A):
    sinfo = CTINFO(CT_NUM, CTF_UNSIGNED);
    ssize = CTSIZE_PTR;
    st = IRT_UINTP;
    goto conv_I_I;

  /* Destination is a floating-point number. */
  case CCX(F, B):
  case CCX(F, I):
  conv_F_I:
    if (dt == IRT_CDATA || st == IRT_CDATA) goto err_nyi;
    sp = emitconv(sp, dt, ssize < 4 ? IRT_INT : st, 0);
    goto xstore;
  case CCX(F, C):
    sp = emitir(IRT(IR_XLOAD, st), sp, 0);  /* Load re. */
    /* fallthrough */
  case CCX(F, F):
  conv_F_F:
    if (dt == IRT_CDATA || st == IRT_CDATA) goto err_nyi;
    if (dt != st) sp = emitconv(sp, dt, st, 0);
    goto xstore;

  /* Destination is a complex number. */
  case CCX(C, I):
  case CCX(C, F):
    {  /* Clear im. */
      TRef ptr = emitir(IRT(IR_ADD, IRT_PTR), dp, lj_ir_kintp(J, (dsize >> 1)));
      emitir(IRT(IR_XSTORE, dt), ptr, lj_ir_knum(J, 0));
    }
    /* Convert to re. */
    if ((sinfo & CTF_FP)) goto conv_F_F; else goto conv_F_I;

  case CCX(C, C):
    if (dt == IRT_CDATA || st == IRT_CDATA) goto err_nyi;
    {
      TRef re, im, ptr;
      re = emitir(IRT(IR_XLOAD, st), sp, 0);
      ptr = emitir(IRT(IR_ADD, IRT_PTR), sp, lj_ir_kintp(J, (ssize >> 1)));
      im = emitir(IRT(IR_XLOAD, st), ptr, 0);
      if (dt != st) {
	re = emitconv(re, dt, st, 0);
	im = emitconv(im, dt, st, 0);
      }
      emitir(IRT(IR_XSTORE, dt), dp, re);
      ptr = emitir(IRT(IR_ADD, IRT_PTR), dp, lj_ir_kintp(J, (dsize >> 1)));
      emitir(IRT(IR_XSTORE, dt), ptr, im);
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
  case CCX(P, A):
  case CCX(P, S):
    /* There are only 32 bit pointers/addresses on 32 bit machines.
    ** Also ok on x64, since all 32 bit ops clear the upper part of the reg.
    */
    goto xstore;
  case CCX(P, I):
    if (st == IRT_CDATA) goto err_nyi;
    if (!LJ_64 && ssize == 8)  /* Truncate from 64 bit integer. */
      sp = emitconv(sp, IRT_U32, st, 0);
    goto xstore;
  case CCX(P, F):
    if (st == IRT_CDATA) goto err_nyi;
    /* The signed conversion is cheaper. x64 really has 47 bit pointers. */
    sp = emitconv(sp, (LJ_64 && dsize == 8) ? IRT_I64 : IRT_U32,
		  st, IRCONV_TRUNC|IRCONV_ANY);
    goto xstore;

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
    TRef tr;
    if ((sinfo & CTF_BOOL))
      goto err_nyi;  /* NYI: specialize to the result. */
    if (t == IRT_CDATA)
      goto err_nyi;  /* NYI: copyval of >64 bit integers. */
    tr = emitir(IRT(IR_XLOAD, t), sp, 0);
    if (t == IRT_FLOAT || t == IRT_U32) {  /* Keep uint32_t/float as numbers. */
      tr = emitconv(tr, IRT_NUM, t, 0);
    } else if (t == IRT_I64 || t == IRT_U64) {  /* Box 64 bit integer. */
      TRef dp = emitir(IRTG(IR_CNEW, IRT_CDATA), lj_ir_kint(J, sid), TREF_NIL);
      TRef ptr = emitir(IRT(IR_ADD, IRT_PTR), dp,
			lj_ir_kintp(J, sizeof(GCcdata)));
      emitir(IRT(IR_XSTORE, t), ptr, tr);
      return dp;
    }
    return tr;
  } else if (ctype_isptr(sinfo)) {
    IRType t = (LJ_64 && s->size == 8) ? IRT_P64 : IRT_P32;
    sp = emitir(IRT(IR_XLOAD, t), sp, 0);
  } else if (ctype_isrefarray(sinfo) || ctype_isstruct(sinfo)) {
    cts->L = J->L;
    sid = lj_ctype_intern(cts, CTINFO_REF(sid), CTSIZE_PTR);  /* Create ref. */
  } else if (ctype_iscomplex(sinfo)) {  /* Unbox/box complex. */
    IRType t = s->size == 2*sizeof(double) ? IRT_NUM : IRT_FLOAT;
    ptrdiff_t esz = (ptrdiff_t)(s->size >> 1);
    TRef ptr, tr1, tr2, dp;
    dp = emitir(IRTG(IR_CNEW, IRT_CDATA), lj_ir_kint(J, sid), TREF_NIL);
    tr1 = emitir(IRT(IR_XLOAD, t), sp, 0);
    ptr = emitir(IRT(IR_ADD, IRT_PTR), sp, lj_ir_kintp(J, esz));
    tr2 = emitir(IRT(IR_XLOAD, t), ptr, 0);
    ptr = emitir(IRT(IR_ADD, IRT_PTR), dp, lj_ir_kintp(J, sizeof(GCcdata)));
    emitir(IRT(IR_XSTORE, t), ptr, tr1);
    ptr = emitir(IRT(IR_ADD, IRT_PTR), dp, lj_ir_kintp(J, sizeof(GCcdata)+esz));
    emitir(IRT(IR_XSTORE, t), ptr, tr2);
    return dp;
  } else {
    /* NYI: copyval of vectors. */
  err_nyi:
    lj_trace_err(J, LJ_TRERR_NYICONV);
  }
  /* Box pointer or ref. */
  return emitir(IRTG(IR_CNEWP, IRT_CDATA), lj_ir_kint(J, sid), sp);
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
      sp = emitir(IRT(IR_FLOAD, t), sp, IRFL_CDATA_PTR);
      if (ctype_isref(s->info))
	s = ctype_rawchild(cts, s);
      else
	goto doconv;  /* The pointer value was loaded, don't load number. */
    } else {
      sp = emitir(IRT(IR_ADD, IRT_PTR), sp, lj_ir_kintp(J, sizeof(GCcdata)));
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
    ptr = emitir(IRT(IR_FLOAD, t), ptr, IRFL_CDATA_PTR);
    ofs = 0;
    ptr = crec_reassoc_ofs(J, ptr, &ofs, 1);
  }

  idx = J->base[1];
  if (tref_isnumber(idx)) {
    /* The size of a ptrdiff_t is target-specific. */
#if LJ_64
    if (tref_isnum(idx))
      idx = emitconv(idx, IRT_I64, IRT_NUM, IRCONV_TRUNC|IRCONV_ANY);
    else
      idx = emitconv(idx, IRT_I64, IRT_INT, IRCONV_SEXT);
#else
    if (tref_isnum(idx))
      idx = emitconv(idx, IRT_INT, IRT_NUM, IRCONV_TRUNC|IRCONV_ANY);
#endif
    if (ctype_ispointer(ct->info)) {
      CTSize sz = lj_ctype_size(cts, (sid = ctype_cid(ct->info)));
      idx = crec_reassoc_ofs(J, idx, &ofs, sz);
      idx = emitir(IRT(IR_MUL, IRT_INTP), idx, lj_ir_kintp(J, sz));
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
    J->needsnap = 1;
    crec_ct_tv(J, ct, ptr, J->base[2], &rd->argv[2]);
  }
}

/* Record cdata allocation. */
static void crec_alloc(jit_State *J, RecordFFData *rd, CTypeID id)
{
  CTState *cts = ctype_ctsG(J2G(J));
  CTSize sz;
  CTInfo info = lj_ctype_info(cts, id, &sz);
  TRef trid;
  if (sz == 0 || sz > 64 || (info & CTF_VLA) || ctype_align(info) > CT_MEMALIGN)
    lj_trace_err(J, LJ_TRERR_NYICONV);  /* NYI: large/special allocations. */
  trid = lj_ir_kint(J, id);
  if (ctype_isptr(info)) {
    TRef sp = J->base[1] ? J->base[1] : lj_ir_kptr(J, NULL);
    J->base[0] = emitir(IRTG(IR_CNEWP, IRT_CDATA), trid, sp);
  } else {
    CType *d = ctype_raw(cts, id);
    TRef trcd = emitir(IRTG(IR_CNEW, IRT_CDATA), trid, TREF_NIL);
    J->base[0] = trcd;
    if (J->base[1] && !J->base[2] &&
	!lj_cconv_multi_init(cts, d, &rd->argv[1])) {
      goto single_init;
    } else if (ctype_isarray(d->info)) {
      CType *dc = ctype_rawchild(cts, d);  /* Array element type. */
      CTSize ofs, esize = dc->size;
      TRef sp = 0;
      TValue *sval = NULL;
      MSize i;
      if (!(ctype_isnum(dc->info) || ctype_isptr(dc->info)))
	lj_trace_err(J, LJ_TRERR_NYICONV);  /* NYI: init array of aggregates. */
      for (i = 1, ofs = 0; ofs < sz; ofs += esize) {
	TRef dp = emitir(IRT(IR_ADD, IRT_PTR), trcd,
			 lj_ir_kintp(J, ofs + sizeof(GCcdata)));
	if (J->base[i]) {
	  sp = J->base[i];
	  sval = &rd->argv[i];
	  i++;
	} else if (i != 2) {
	  sp = ctype_isnum(dc->info) ? lj_ir_kint(J, 0) : TREF_NIL;
	}
	crec_ct_tv(J, dc, dp, sp, sval);
      }
    } else if (ctype_isstruct(d->info)) {
      CTypeID fid = d->sib;
      MSize i = 1;
      while (fid) {
	CType *df = ctype_get(cts, fid);
	fid = df->sib;
	if (ctype_isfield(df->info)) {
	  CType *dc;
	  TRef sp, dp;
	  TValue *sval;
	  if (!gcref(df->name)) continue;  /* Ignore unnamed fields. */
	  dc = ctype_rawchild(cts, df);  /* Field type. */
	  if (!(ctype_isnum(dc->info) || ctype_isptr(dc->info)))
	    lj_trace_err(J, LJ_TRERR_NYICONV);  /* NYI: init aggregates. */
	  if (J->base[i]) {
	    sp = J->base[i];
	    sval = &rd->argv[i];
	    i++;
	  } else {
	    sp = ctype_isnum(dc->info) ? lj_ir_kint(J, 0) : TREF_NIL;
	    sval = NULL;
	  }
	  dp = emitir(IRT(IR_ADD, IRT_PTR), trcd,
		      lj_ir_kintp(J, df->size + sizeof(GCcdata)));
	  crec_ct_tv(J, dc, dp, sp, sval);
	} else if (!ctype_isconstval(df->info)) {
	  /* NYI: init bitfields and sub-structures. */
	  lj_trace_err(J, LJ_TRERR_NYICONV);
	}
      }
    } else {
      TRef sp, dp;
    single_init:
      sp = J->base[1] ? J->base[1] : lj_ir_kint(J, 0);
      dp = emitir(IRT(IR_ADD, IRT_PTR), trcd, lj_ir_kintp(J, sizeof(GCcdata)));
      crec_ct_tv(J, d, dp, sp, &rd->argv[1]);
    }
  }
}

void LJ_FASTCALL recff_cdata_call(jit_State *J, RecordFFData *rd)
{
  GCcdata *cd = argv2cdata(J, J->base[0], &rd->argv[0]);
  if (cd->typeid == CTID_CTYPEID) {
    crec_alloc(J, rd, *(CTypeID *)cdataptr(cd));
  } else {
    lj_trace_err(J, LJ_TRERR_BADTYPE);
  }
}

static TRef crec_arith_int64(jit_State *J, TRef *sp, CType **s, MMS mm)
{
  if (ctype_isnum(s[0]->info) && ctype_isnum(s[1]->info)) {
    IRType dt;
    CTypeID id;
    TRef tr, dp, ptr;
    MSize i;
    if (((s[0]->info & CTF_UNSIGNED) && s[0]->size == 8) ||
	((s[1]->info & CTF_UNSIGNED) && s[1]->size == 8)) {
      dt = IRT_U64; id = CTID_UINT64;
    } else {
      dt = IRT_I64; id = CTID_INT64;
    }
    for (i = 0; i < 2; i++) {
      IRType st = tref_type(sp[i]);
      if (st == IRT_NUM || st == IRT_FLOAT)
	sp[i] = emitconv(sp[i], dt, st, IRCONV_TRUNC|IRCONV_ANY);
      else if (!(st == IRT_I64 || st == IRT_U64))
	sp[i] = emitconv(sp[i], dt, IRT_INT,
			 ((st - IRT_I8) & 1) ? 0 : IRCONV_SEXT);
    }
    if (mm == MM_pow) {
      tr = lj_ir_call(J, IRCALL_lj_cdata_powi64, sp[0], sp[1],
		      lj_ir_kint(J, (int)dt-(int)IRT_I64));
    } else {
      if (mm == MM_div || mm == MM_mod)
	return 0;  /* NYI: integer div, mod. */
      tr = emitir(IRT(mm+(int)IR_ADD-(int)MM_add, dt), sp[0], sp[1]);
    }
    dp = emitir(IRTG(IR_CNEW, IRT_CDATA), lj_ir_kint(J, id), TREF_NIL);
    ptr = emitir(IRT(IR_ADD, IRT_PTR), dp, lj_ir_kintp(J, sizeof(GCcdata)));
    emitir(IRT(IR_XSTORE, dt), ptr, tr);
    return dp;
  }
  return 0;
}

static TRef crec_arith_ptr(jit_State *J, TRef *sp, CType **s, MMS mm)
{
  CTState *cts = ctype_ctsG(J2G(J));
  CType *ctp = s[0];
  CTSize sz;
  if (!(mm == MM_add || mm == MM_sub))
    return 0;
  if (ctype_ispointer(ctp->info)) {
    sz = lj_ctype_size(cts, ctype_cid(ctp->info));
    if (mm == MM_sub && ctype_ispointer(s[1]->info)) {
      /* Pointer difference. */
      TRef tr;
      if (sz == 0 || (sz & (sz-1)) != 0)
	return 0;  /* NYI: integer division. */
      tr = emitir(IRT(IR_SUB, IRT_PTR), sp[0], sp[1]);
      tr = emitir(IRT(IR_BSAR, IRT_INTP), tr, lj_ir_kint(J, lj_fls(sz)));
#if LJ_64
      tr = emitconv(tr, IRT_NUM, IRT_INTP, 0);
#endif
      return tr;
    }
    if (!ctype_isnum(s[1]->info)) return 0;
  } else if (mm == MM_add &&
	     ctype_isnum(ctp->info) && ctype_ispointer(s[1]->info)) {
    TRef tr = sp[0]; sp[0] = sp[1]; sp[1] = tr;  /* Swap pointer and index. */
    ctp = s[1];
    sz = lj_ctype_size(cts, ctype_cid(ctp->info));
  } else {
    return 0;
  }
  {
    TRef tr = sp[1];
    IRType t = tref_type(tr);
    CTypeID id;
#if LJ_64
    if (t == IRT_NUM || t == IRT_FLOAT)
      tr = emitconv(tr, IRT_INTP, t, IRCONV_TRUNC|IRCONV_ANY);
    else if (!(t == IRT_I64 || t == IRT_U64))
      tr = emitconv(tr, IRT_INTP, IRT_INT,
		    ((t - IRT_I8) & 1) ? 0 : IRCONV_SEXT);
#else
    if (!tref_typerange(sp[1], IRT_I8, IRT_U32))
      tr = emitconv(tr, IRT_INTP, t,
		    (t == IRT_NUM || t == IRT_FLOAT) ?
		    IRCONV_TRUNC|IRCONV_ANY : 0);
#endif
    tr = emitir(IRT(IR_MUL, IRT_INTP), tr, lj_ir_kintp(J, sz));
    tr = emitir(IRT(IR_ADD, IRT_PTR), sp[0], tr);
    id = lj_ctype_intern(cts, CTINFO(CT_PTR, CTALIGN_PTR|ctype_cid(ctp->info)),
			 CTSIZE_PTR);
    return emitir(IRTG(IR_CNEWP, IRT_CDATA), lj_ir_kint(J, id), tr);
  }
}

void LJ_FASTCALL recff_cdata_arith(jit_State *J, RecordFFData *rd)
{
  CTState *cts = ctype_ctsG(J2G(J));
  TRef sp[2];
  CType *s[2];
  MSize i;
  for (i = 0; i < 2; i++) {
    TRef tr = J->base[i];
    CType *ct = ctype_get(cts, CTID_DOUBLE);
    if (tref_iscdata(tr)) {
      CTypeID id = argv2cdata(J, tr, &rd->argv[i])->typeid;
      ct = ctype_raw(cts, id);
      if (ctype_isptr(ct->info)) {  /* Resolve pointer or reference. */
	IRType t = (LJ_64 && ct->size == 8) ? IRT_P64 : IRT_P32;
	if (ctype_isref(ct->info)) ct = ctype_rawchild(cts, ct);
	tr = emitir(IRT(IR_FLOAD, t), tr, IRFL_CDATA_PTR);
      } else {
	tr = emitir(IRT(IR_ADD, IRT_PTR), tr, lj_ir_kintp(J, sizeof(GCcdata)));
      }
      if (ctype_isenum(ct->info)) ct = ctype_child(cts, ct);
      if (ctype_isnum(ct->info)) {
	IRType t = crec_ct2irt(ct);
	if (t == IRT_CDATA) goto err_type;
	tr = emitir(IRT(IR_XLOAD, t), tr, 0);
      } else if (!(ctype_isptr(ct->info) || ctype_isrefarray(ct->info))) {
	goto err_type;
      }
    } else if (tref_isnil(tr)) {
      tr = lj_ir_kptr(J, NULL);
      ct = ctype_get(cts, CTID_P_VOID);
    } else if (tref_isinteger(tr)) {
      ct = ctype_get(cts, CTID_INT32);
    } else if (!tref_isnum(tr)) {
      goto err_type;
    }
    s[i] = ct;
    sp[i] = tr;
  }
  {
    TRef tr;
    if (!(tr = crec_arith_int64(J, sp, s, (MMS)rd->data)) &&
	!(tr = crec_arith_ptr(J, sp, s, (MMS)rd->data))) {
    err_type:
      lj_trace_err(J, LJ_TRERR_BADTYPE);
    }
    J->base[0] = tr;
  }
}

/* -- FFI library functions ----------------------------------------------- */

void LJ_FASTCALL recff_ffi_new(jit_State *J, RecordFFData *rd)
{
  crec_alloc(J, rd, argv2ctype(J, J->base[0], &rd->argv[0]));
}

/* -- Miscellaneous library functions ------------------------------------- */

void LJ_FASTCALL lj_crecord_tonumber(jit_State *J, RecordFFData *rd)
{
  CTypeID sid = argv2cdata(J, J->base[0], &rd->argv[0])->typeid;
  CTState *cts = ctype_ctsG(J2G(J));
  CType *s = ctype_raw(cts, sid);
  TRef sp = J->base[0];
  if (ctype_isref(s->info)) {
    sp = emitir(IRT(IR_FLOAD, IRT_PTR), sp, IRFL_CDATA_PTR);
    s = ctype_rawchild(cts, s);
  } else {
    sp = emitir(IRT(IR_ADD, IRT_PTR), sp, lj_ir_kintp(J, sizeof(GCcdata)));
  }
  if (ctype_isenum(s->info)) s = ctype_child(cts, s);
  if (ctype_isnum(s->info) || ctype_iscomplex(s->info)) {
    IRType t = crec_ct2irt(s);
    if (t != IRT_CDATA) {
      TRef tr = emitir(IRT(IR_XLOAD, t), sp, 0);  /* Load number value. */
      if (t == IRT_FLOAT || t == IRT_U32 || t == IRT_I64 || t == IRT_U64)
	tr = emitconv(tr, IRT_NUM, t, 0);
      J->base[0] = tr;
      return;
    }
  }
  lj_trace_err(J, LJ_TRERR_BADTYPE);
}

#undef IR
#undef emitir
#undef emitconv

#endif
