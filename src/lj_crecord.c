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
#include "lj_tab.h"
#include "lj_frame.h"
#include "lj_ctype.h"
#include "lj_cdata.h"
#include "lj_cparse.h"
#include "lj_cconv.h"
#include "lj_clib.h"
#include "lj_ccall.h"
#include "lj_ir.h"
#include "lj_jit.h"
#include "lj_ircall.h"
#include "lj_iropt.h"
#include "lj_trace.h"
#include "lj_record.h"
#include "lj_ffrecord.h"
#include "lj_snap.h"
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

/* Specialize to the CTypeID held by a cdata constructor. */
static CTypeID crec_constructor(jit_State *J, GCcdata *cd, TRef tr)
{
  CTypeID id;
  lua_assert(tref_iscdata(tr) && cd->typeid == CTID_CTYPEID);
  id = *(CTypeID *)cdataptr(cd);
  tr = emitir(IRT(IR_ADD, IRT_PTR), tr, lj_ir_kintp(J, sizeof(GCcdata)));
  tr = emitir(IRT(IR_XLOAD, IRT_INT), tr, 0);
  emitir(IRTG(IR_EQ, IRT_INT), tr, lj_ir_kint(J, (int32_t)id));
  return id;
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
    return cd->typeid == CTID_CTYPEID ? crec_constructor(J, cd, tr) :
					cd->typeid;
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

/* Determine whether a passed number or cdata number is non-zero. */
static int crec_isnonzero(CType *s, void *p)
{
  if (p == (void *)0)
    return 0;
  if (p == (void *)1)
    return 1;
  if ((s->info & CTF_FP)) {
    if (s->size == sizeof(float))
      return (*(float *)p != 0);
    else
      return (*(double *)p != 0);
  } else {
    if (s->size == 1)
      return (*(uint8_t *)p != 0);
    else if (s->size == 2)
      return (*(uint16_t *)p != 0);
    else if (s->size == 4)
      return (*(uint32_t *)p != 0);
    else
      return (*(uint64_t *)p != 0);
  }
}

static TRef crec_ct_ct(jit_State *J, CType *d, CType *s, TRef dp, TRef sp,
		       void *svisnz)
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
    if (st != IRT_CDATA) {
      /* Specialize to the result of a comparison against 0. */
      TRef zero = (st == IRT_NUM  || st == IRT_FLOAT) ? lj_ir_knum(J, 0) :
		  (st == IRT_I64 || st == IRT_U64) ? lj_ir_kint64(J, 0) :
		  lj_ir_kint(J, 0);
      int isnz = crec_isnonzero(s, svisnz);
      emitir(IRTG(isnz ? IR_NE : IR_EQ, st), sp, zero);
      sp = lj_ir_kint(J, isnz);
      goto xstore;
    }
    goto err_nyi;

  /* Destination is an integer. */
  case CCX(I, B):
  case CCX(I, I):
  conv_I_I:
    if (dt == IRT_CDATA || st == IRT_CDATA) goto err_nyi;
    /* Extend 32 to 64 bit integer. */
    if (dsize == 8 && ssize < 8 && !(LJ_64 && (sinfo & CTF_UNSIGNED)))
      sp = emitconv(sp, dt, ssize < 4 ? IRT_INT : st,
		    (sinfo & CTF_UNSIGNED) ? 0 : IRCONV_SEXT);
    else if (dsize < 8 && ssize == 8)  /* Truncate from 64 bit integer. */
      sp = emitconv(sp, dsize < 4 ? IRT_INT : dt, st, 0);
    else if (st == IRT_INT)
      sp = lj_opt_narrow_toint(J, sp);
  xstore:
    if (dt == IRT_I64 || dt == IRT_U64) lj_needsplit(J);
    if (dp == 0) return sp;
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
  return 0;
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
    if (t == IRT_CDATA)
      goto err_nyi;  /* NYI: copyval of >64 bit integers. */
    tr = emitir(IRT(IR_XLOAD, t), sp, 0);
    if (t == IRT_FLOAT || t == IRT_U32) {  /* Keep uint32_t/float as numbers. */
      return emitconv(tr, IRT_NUM, t, 0);
    } else if (t == IRT_I64 || t == IRT_U64) {  /* Box 64 bit integer. */
      sp = tr;
      lj_needsplit(J);
    } else if ((sinfo & CTF_BOOL)) {
      /* Assume not equal to zero. Fixup and emit pending guard later. */
      lj_ir_set(J, IRTGI(IR_NE), tr, lj_ir_kint(J, 0));
      J->postproc = LJ_POST_FIXGUARD;
      return TREF_TRUE;
    } else {
      return tr;
    }
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
  /* Box pointer, ref or 64 bit integer. */
  return emitir(IRTG(IR_CNEWI, IRT_CDATA), lj_ir_kint(J, sid), sp);
}

/* -- Convert TValue to C type (store) ------------------------------------ */

static TRef crec_ct_tv(jit_State *J, CType *d, TRef dp, TRef sp, cTValue *sval)
{
  CTState *cts = ctype_ctsG(J2G(J));
  CTypeID sid = CTID_P_VOID;
  void *svisnz = 0;
  CType *s;
  if (LJ_LIKELY(tref_isinteger(sp))) {
    sid = CTID_INT32;
    svisnz = (void *)(intptr_t)(tvisint(sval)?(intV(sval)!=0):!tviszero(sval));
  } else if (tref_isnum(sp)) {
    sid = CTID_DOUBLE;
    svisnz = (void *)(intptr_t)(tvisint(sval)?(intV(sval)!=0):!tviszero(sval));
  } else if (tref_isbool(sp)) {
    sp = lj_ir_kint(J, tref_istrue(sp) ? 1 : 0);
    sid = CTID_BOOL;
  } else if (tref_isnil(sp)) {
    sp = lj_ir_kptr(J, NULL);
  } else if (tref_isudata(sp)) {
    sp = emitir(IRT(IR_ADD, IRT_P32), sp, lj_ir_kint(J, sizeof(GCudata)));
  } else if (tref_isstr(sp)) {
    if (ctype_isenum(d->info)) {  /* Match string against enum constant. */
      GCstr *str = strV(sval);
      CTSize ofs;
      CType *cct = lj_ctype_getfield(cts, d, str, &ofs);
      /* Specialize to the name of the enum constant. */
      emitir(IRTG(IR_EQ, IRT_STR), sp, lj_ir_kstr(J, str));
      if (cct && ctype_isconstval(cct->info)) {
	lua_assert(ctype_child(cts, cct)->size == 4);
	svisnz = (void *)(intptr_t)(cct->size != 0);
	sp = lj_ir_kint(J, (int32_t)cct->size);
	sid = ctype_cid(cct->info);
      }  /* else: interpreter will throw. */
    } else if (ctype_isrefarray(d->info)) {  /* Copy string to array. */
      lj_trace_err(J, LJ_TRERR_BADTYPE);  /* NYI */
    } else {  /* Otherwise pass the string data as a const char[]. */
      sp = emitir(IRT(IR_STRREF, IRT_P32), sp, lj_ir_kint(J, 0));
      sid = CTID_A_CCHAR;
    }
  } else {  /* NYI: tref_istab(sp), tref_islightud(sp). */
    sid = argv2cdata(J, sp, sval)->typeid;
    s = ctype_raw(cts, sid);
    svisnz = cdataptr(cdataV(sval));
    if (ctype_isptr(s->info)) {
      IRType t = (LJ_64 && s->size == 8) ? IRT_P64 : IRT_P32;
      sp = emitir(IRT(IR_FLOAD, t), sp, IRFL_CDATA_PTR);
      if (ctype_isref(s->info)) {
	svisnz = *(void **)svisnz;
	s = ctype_rawchild(cts, s);
      } else {
	goto doconv;  /* The pointer value was loaded, don't load number. */
      }
    } else if (ctype_isinteger(s->info) && s->size == 8) {
      IRType t = (s->info & CTF_UNSIGNED) ? IRT_U64 : IRT_I64;
      sp = emitir(IRT(IR_FLOAD, t), sp, IRFL_CDATA_INT64);
      lj_needsplit(J);
      goto doconv;
    } else {
      sp = emitir(IRT(IR_ADD, IRT_PTR), sp, lj_ir_kintp(J, sizeof(GCcdata)));
    }
    if (ctype_isenum(s->info)) s = ctype_child(cts, s);
    if (ctype_isnum(s->info)) {  /* Load number value. */
      IRType t = crec_ct2irt(s);
      if (t != IRT_CDATA) {
	sp = emitir(IRT(IR_XLOAD, t), sp, 0);
	if (t == IRT_I64 || t == IRT_U64) lj_needsplit(J);
      }
    }
    goto doconv;
  }
  s = ctype_get(cts, sid);
doconv:
  if (ctype_isenum(d->info)) d = ctype_child(cts, d);
  return crec_ct_ct(J, d, s, dp, sp, svisnz);
}

/* -- C data metamethods -------------------------------------------------- */

/* This would be rather difficult in FOLD, so do it here:
** (base+k)+(idx*sz)+ofs ==> (base+idx*sz)+(ofs+k)
** (base+(idx+k)*sz)+ofs ==> (base+idx*sz)+(ofs+k*sz)
*/
static TRef crec_reassoc_ofs(jit_State *J, TRef tr, ptrdiff_t *ofsp, MSize sz)
{
  IRIns *ir = IR(tref_ref(tr));
  if (LJ_LIKELY(J->flags & JIT_F_OPT_FOLD) && irref_isk(ir->op2) &&
      (ir->o == IR_ADD || ir->o == IR_ADDOV || ir->o == IR_SUBOV)) {
    IRIns *irk = IR(ir->op2);
    ptrdiff_t k;
    if (LJ_64 && irk->o == IR_KINT64)
      k = (ptrdiff_t)ir_kint64(irk)->u64 * sz;
    else
      k = (ptrdiff_t)irk->i * sz;
    if (ir->o == IR_SUBOV) *ofsp -= k; else *ofsp += k;
    tr = ir->op1;  /* Not a TRef, but the caller doesn't care. */
  }
  return tr;
}

/* Record ctype __index/__newindex metamethods. */
static void crec_index_meta(jit_State *J, CTState *cts, CType *ct,
			    RecordFFData *rd)
{
  CTypeID id = ctype_typeid(cts, ct);
  cTValue *tv = lj_ctype_meta(cts, id, rd->data ? MM_newindex : MM_index);
  if (!tv)
    lj_trace_err(J, LJ_TRERR_BADTYPE);
  if (tvisfunc(tv)) {
    J->base[-1] = lj_ir_kfunc(J, funcV(tv)) | TREF_FRAME;
    rd->nres = -1;  /* Pending tailcall. */
  } else if (rd->data == 0 && tvistab(tv) && tref_isstr(J->base[1])) {
    /* Specialize to result of __index lookup. */
    cTValue *o = lj_tab_get(J->L, tabV(tv), &rd->argv[1]);
    IRType t = itype2irt(o);
    if (tvisgcv(o))
      J->base[0] = lj_ir_kgc(J, gcV(o), t);
    else if (tvisint(o))
      J->base[0] = lj_ir_kint(J, intV(o));
    else if (tvisnum(o))
      J->base[0] = lj_ir_knumint(J, numV(o));
    else if (tvisbool(o))
      J->base[0] = TREF_PRI(t);
    else
      lj_trace_err(J, LJ_TRERR_BADTYPE);
    /* Always specialize to the key. */
    emitir(IRTG(IR_EQ, IRT_STR), J->base[1], lj_ir_kstr(J, strV(&rd->argv[1])));
  } else {
    /* NYI: resolving of non-function metamethods. */
    /* NYI: non-string keys for __index table. */
    /* NYI: stores to __newindex table. */
    lj_trace_err(J, LJ_TRERR_BADTYPE);
  }
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

again:
  idx = J->base[1];
  if (tref_isnumber(idx)) {
    idx = lj_opt_narrow_cindex(J, idx);
    if (ctype_ispointer(ct->info)) {
      CTSize sz;
  integer_key:
      if ((ct->info & CTF_COMPLEX))
	idx = emitir(IRT(IR_BAND, IRT_INTP), idx, lj_ir_kintp(J, 1));
      sz = lj_ctype_size(cts, (sid = ctype_cid(ct->info)));
      idx = crec_reassoc_ofs(J, idx, &ofs, sz);
#if LJ_TARGET_ARM || LJ_TARGET_PPC
      /* Hoist base add to allow fusion of index/shift into operands. */
      if (LJ_LIKELY(J->flags & JIT_F_OPT_LOOP) && ofs
#if LJ_TARGET_ARM
	  && (sz == 1 || sz == 4)
#endif
	  ) {
	ptr = emitir(IRT(IR_ADD, IRT_PTR), ptr, lj_ir_kintp(J, ofs));
	ofs = 0;
      }
#endif
      idx = emitir(IRT(IR_MUL, IRT_INTP), idx, lj_ir_kintp(J, sz));
      ptr = emitir(IRT(IR_ADD, IRT_PTR), idx, ptr);
    }
  } else if (tref_iscdata(idx)) {
    GCcdata *cdk = cdataV(&rd->argv[1]);
    CType *ctk = ctype_raw(cts, cdk->typeid);
    IRType t;
    if (ctype_isenum(ctk->info)) ctk = ctype_child(cts, ctk);
    if (ctype_ispointer(ct->info) &&
	ctype_isinteger(ctk->info) && (t = crec_ct2irt(ctk)) != IRT_CDATA) {
      if (ctk->size == 8) {
	idx = emitir(IRT(IR_FLOAD, t), idx, IRFL_CDATA_INT64);
      } else {
	idx = emitir(IRT(IR_ADD, IRT_PTR), idx,
		     lj_ir_kintp(J, sizeof(GCcdata)));
	idx = emitir(IRT(IR_XLOAD, t), idx, 0);
      }
      if (LJ_64 && ctk->size < sizeof(intptr_t) && !(ctk->info & CTF_UNSIGNED))
	idx = emitconv(idx, IRT_INTP, IRT_INT, IRCONV_SEXT);
      if (!LJ_64 && ctk->size > sizeof(intptr_t)) {
	idx = emitconv(idx, IRT_INTP, t, 0);
	lj_needsplit(J);
      }
      goto integer_key;
    }
  } else if (tref_isstr(idx)) {
    GCstr *name = strV(&rd->argv[1]);
    if (cd->typeid == CTID_CTYPEID)
      ct = ctype_raw(cts, crec_constructor(J, cd, ptr));
    if (ctype_isstruct(ct->info)) {
      CTSize fofs;
      CType *fct;
      fct = lj_ctype_getfield(cts, ct, name, &fofs);
      if (fct) {
	/* Always specialize to the field name. */
	emitir(IRTG(IR_EQ, IRT_STR), idx, lj_ir_kstr(J, name));
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
      if (name->len == 2 &&
	  ((strdata(name)[0] == 'r' && strdata(name)[1] == 'e') ||
	   (strdata(name)[0] == 'i' && strdata(name)[1] == 'm'))) {
	/* Always specialize to the field name. */
	emitir(IRTG(IR_EQ, IRT_STR), idx, lj_ir_kstr(J, name));
	if (strdata(name)[0] == 'i') ofs += (ct->size >> 1);
	sid = ctype_cid(ct->info);
      }
    }
  }
  if (!sid) {
    if (ctype_isptr(ct->info)) {  /* Automatically perform '->'. */
      CType *cct = ctype_rawchild(cts, ct);
      if (ctype_isstruct(cct->info)) {
	ct = cct;
	if (tref_isstr(idx)) goto again;
      }
    }
    crec_index_meta(J, cts, ct, rd);
    return;
  }

  if (ofs)
    ptr = emitir(IRT(IR_ADD, IRT_PTR), ptr, lj_ir_kintp(J, ofs));

  /* Resolve reference for field. */
  ct = ctype_get(cts, sid);
  if (ctype_isref(ct->info))
    ptr = emitir(IRT(IR_XLOAD, IRT_PTR), ptr, 0);

  while (ctype_isattrib(ct->info))
    ct = ctype_child(cts, ct);  /* Skip attributes. */

  if (rd->data == 0) {  /* __index metamethod. */
    if (ctype_isenum(ct->info)) ct = ctype_child(cts, ct);  /* Skip enums. */
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
  CType *d = ctype_raw(cts, id);
  TRef trid;
  if (sz == 0 || sz > 64 || (info & CTF_VLA) || ctype_align(info) > CT_MEMALIGN)
    lj_trace_err(J, LJ_TRERR_NYICONV);  /* NYI: large/special allocations. */
  trid = lj_ir_kint(J, id);
  /* Use special instruction to box pointer or 64 bit integer. */
  if (ctype_isptr(info) || (ctype_isinteger(info) && sz == 8)) {
    TRef sp = J->base[1] ? crec_ct_tv(J, d, 0, J->base[1], &rd->argv[1]) :
	      ctype_isptr(info) ? lj_ir_kptr(J, NULL) :
	      (lj_needsplit(J), lj_ir_kint64(J, 0));
    J->base[0] = emitir(IRTG(IR_CNEWI, IRT_CDATA), trid, sp);
  } else {
    TRef trcd = emitir(IRTG(IR_CNEW, IRT_CDATA), trid, TREF_NIL);
    cTValue *fin;
    J->base[0] = trcd;
    if (J->base[1] && !J->base[2] && !lj_cconv_multi_init(d, &rd->argv[1])) {
      goto single_init;
    } else if (ctype_isarray(d->info)) {
      CType *dc = ctype_rawchild(cts, d);  /* Array element type. */
      CTSize ofs, esize = dc->size;
      TRef sp = 0;
      TValue tv;
      TValue *sval = &tv;
      MSize i;
      tv.u64 = 0;
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
	  TValue tv;
	  TValue *sval = &tv;
	  setintV(&tv, 0);
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
      TRef dp;
    single_init:
      dp = emitir(IRT(IR_ADD, IRT_PTR), trcd, lj_ir_kintp(J, sizeof(GCcdata)));
      if (J->base[1]) {
	crec_ct_tv(J, d, dp, J->base[1], &rd->argv[1]);
      } else {
	TValue tv;
	tv.u64 = 0;
	crec_ct_tv(J, d, dp, lj_ir_kint(J, 0), &tv);
      }
    }
    /* Handle __gc metamethod. */
    fin = lj_ctype_meta(cts, id, MM_gc);
    if (fin) {
      TRef trlo = lj_ir_call(J, IRCALL_lj_cdata_setfin, trcd);
      TRef trhi = emitir(IRT(IR_ADD, IRT_P32), trlo, lj_ir_kint(J, 4));
      if (LJ_BE) { TRef tmp = trlo; trlo = trhi; trhi = tmp; }
      if (tvisfunc(fin)) {
	emitir(IRT(IR_XSTORE, IRT_P32), trlo, lj_ir_kfunc(J, funcV(fin)));
	emitir(IRTI(IR_XSTORE), trhi, lj_ir_kint(J, LJ_TFUNC));
      } else if (tviscdata(fin)) {
	emitir(IRT(IR_XSTORE, IRT_P32), trlo,
	       lj_ir_kgc(J, obj2gco(cdataV(fin)), IRT_CDATA));
	emitir(IRTI(IR_XSTORE), trhi, lj_ir_kint(J, LJ_TCDATA));
      } else {
	lj_trace_err(J, LJ_TRERR_BADTYPE);
      }
      J->needsnap = 1;
    }
  }
}

/* Record argument conversions. */
static TRef crec_call_args(jit_State *J, RecordFFData *rd,
			   CTState *cts, CType *ct)
{
  TRef args[CCI_NARGS_MAX];
  CTypeID fid;
  MSize i, n;
  TRef tr, *base;
  cTValue *o;
#if LJ_TARGET_X86
#if LJ_ABI_WIN
  TRef *arg0 = NULL, *arg1 = NULL;
#endif
  int ngpr = 0;
  if (ctype_cconv(ct->info) == CTCC_THISCALL)
    ngpr = 1;
  else if (ctype_cconv(ct->info) == CTCC_FASTCALL)
    ngpr = 2;
#endif

  /* Skip initial attributes. */
  fid = ct->sib;
  while (fid) {
    CType *ctf = ctype_get(cts, fid);
    if (!ctype_isattrib(ctf->info)) break;
    fid = ctf->sib;
  }
  args[0] = TREF_NIL;
  for (n = 0, base = J->base+1, o = rd->argv+1; *base; n++, base++, o++) {
    CTypeID did;
    CType *d;

    if (n >= CCI_NARGS_MAX)
      lj_trace_err(J, LJ_TRERR_NYICALL);

    if (fid) {  /* Get argument type from field. */
      CType *ctf = ctype_get(cts, fid);
      fid = ctf->sib;
      lua_assert(ctype_isfield(ctf->info));
      did = ctype_cid(ctf->info);
    } else {
      if (!(ct->info & CTF_VARARG))
	lj_trace_err(J, LJ_TRERR_NYICALL);  /* Too many arguments. */
      did = lj_ccall_ctid_vararg(cts, o);  /* Infer vararg type. */
    }
    d = ctype_raw(cts, did);
    if (!(ctype_isnum(d->info) || ctype_isptr(d->info) ||
	  ctype_isenum(d->info)))
      lj_trace_err(J, LJ_TRERR_NYICALL);
    tr = crec_ct_tv(J, d, 0, *base, o);
    if (ctype_isinteger_or_bool(d->info)) {
      if (d->size < 4) {
	if ((d->info & CTF_UNSIGNED))
	  tr = emitconv(tr, IRT_INT, d->size==1 ? IRT_U8 : IRT_U16, 0);
	else
	  tr = emitconv(tr, IRT_INT, d->size==1 ? IRT_I8 : IRT_I16,IRCONV_SEXT);
      }
    }
#if LJ_TARGET_X86
    /* 64 bit args must not end up in registers for fastcall/thiscall. */
#if LJ_ABI_WIN
    if (!ctype_isfp(d->info)) {
      /* Sigh, the Windows/x86 ABI allows reordering across 64 bit args. */
      if (tref_typerange(tr, IRT_I64, IRT_U64)) {
	if (ngpr) {
	  arg0 = &args[n]; args[n++] = TREF_NIL; ngpr--;
	  if (ngpr) {
	    arg1 = &args[n]; args[n++] = TREF_NIL; ngpr--;
	  }
	}
      } else {
	if (arg0) { *arg0 = tr; arg0 = NULL; n--; continue; }
	if (arg1) { *arg1 = tr; arg1 = NULL; n--; continue; }
	if (ngpr) ngpr--;
      }
    }
#else
    if (!ctype_isfp(d->info) && ngpr) {
      if (tref_typerange(tr, IRT_I64, IRT_U64)) {
	/* No reordering for other x86 ABIs. Simply add alignment args. */
	do { args[n++] = TREF_NIL; } while (--ngpr);
      } else {
	ngpr--;
      }
    }
#endif
#endif
    args[n] = tr;
  }
  tr = args[0];
  for (i = 1; i < n; i++)
    tr = emitir(IRT(IR_CARG, IRT_NIL), tr, args[i]);
  return tr;
}

/* Create a snapshot for the caller, simulating a 'false' return value. */
static void crec_snap_caller(jit_State *J)
{
  lua_State *L = J->L;
  TValue *base = L->base, *top = L->top;
  const BCIns *pc = J->pc;
  TRef ftr = J->base[-1];
  ptrdiff_t delta;
  if (!frame_islua(base-1))
    lj_trace_err(J, LJ_TRERR_NYICALL);
  J->pc = frame_pc(base-1); delta = 1+bc_a(J->pc[-1]);
  L->top = base; L->base = base - delta;
  J->base[-1] = TREF_FALSE;
  J->base -= delta; J->baseslot -= (BCReg)delta;
  J->maxslot = (BCReg)delta; J->framedepth--;
  lj_snap_add(J);
  L->base = base; L->top = top;
  J->framedepth++; J->maxslot = 1;
  J->base += delta; J->baseslot += (BCReg)delta;
  J->base[-1] = ftr; J->pc = pc;
}

/* Record function call. */
static int crec_call(jit_State *J, RecordFFData *rd, GCcdata *cd)
{
  CTState *cts = ctype_ctsG(J2G(J));
  CType *ct = ctype_raw(cts, cd->typeid);
  IRType tp = IRT_PTR;
  if (ctype_isptr(ct->info)) {
    tp = (LJ_64 && ct->size == 8) ? IRT_P64 : IRT_P32;
    ct = ctype_rawchild(cts, ct);
  }
  if (ctype_isfunc(ct->info)) {
    TRef func = emitir(IRT(IR_FLOAD, tp), J->base[0], IRFL_CDATA_PTR);
    CType *ctr = ctype_rawchild(cts, ct);
    IRType t = crec_ct2irt(ctr);
    TRef tr;
    TValue tv;
    /* Check for blacklisted C functions that might call a callback. */
    setlightudV(&tv,
		cdata_getptr(cdataptr(cd), (LJ_64 && tp == IRT_P64) ? 8 : 4));
    if (tvistrue(lj_tab_get(J->L, cts->miscmap, &tv)))
      lj_trace_err(J, LJ_TRERR_BLACKL);
    if (ctype_isvoid(ctr->info)) {
      t = IRT_NIL;
      rd->nres = 0;
    } else if (ctype_isenum(ctr->info)) {
      ctr = ctype_child(cts, ctr);
    }
    if (!(ctype_isnum(ctr->info) || ctype_isptr(ctr->info) ||
	  ctype_isvoid(ctr->info)) || t == IRT_CDATA)
      lj_trace_err(J, LJ_TRERR_NYICALL);
    if ((ct->info & CTF_VARARG)
#if LJ_TARGET_X86
	|| ctype_cconv(ct->info) != CTCC_CDECL
#endif
	)
      func = emitir(IRT(IR_CARG, IRT_NIL), func,
		    lj_ir_kint(J, ctype_typeid(cts, ct)));
    tr = emitir(IRT(IR_CALLXS, t), crec_call_args(J, rd, cts, ct), func);
    if (ctype_isbool(ctr->info)) {
      crec_snap_caller(J);
      lj_ir_set(J, IRTGI(IR_NE), tr, lj_ir_kint(J, 0));
      J->postproc = LJ_POST_FIXGUARDSNAP;
      tr = TREF_TRUE;
    } else if (t == IRT_FLOAT || t == IRT_U32) {
      tr = emitconv(tr, IRT_NUM, t, 0);
    } else if (t == IRT_I8 || t == IRT_I16) {
      tr = emitconv(tr, IRT_INT, t, IRCONV_SEXT);
    } else if (t == IRT_U8 || t == IRT_U16) {
      tr = emitconv(tr, IRT_INT, t, 0);
    } else if (t == IRT_PTR || (LJ_64 && t == IRT_P32) ||
	       (t == IRT_I64 || t == IRT_U64)) {
      TRef trid = lj_ir_kint(J, ctype_cid(ct->info));
      tr = emitir(IRTG(IR_CNEWI, IRT_CDATA), trid, tr);
      if (t == IRT_I64 || t == IRT_U64) lj_needsplit(J);
    }
    J->base[0] = tr;
    J->needsnap = 1;
    return 1;
  }
  return 0;
}

/* Record ctype call metamethod. */
static void crec_call_meta(jit_State *J, RecordFFData *rd, CTypeID id)
{
  CTState *cts = ctype_ctsG(J2G(J));
  CType *ct = ctype_raw(cts, id);
  cTValue *tv;
  if (ctype_isptr(ct->info)) id = ctype_cid(ct->info);
  tv = lj_ctype_meta(cts, id, MM_call);
  if (tv && tvisfunc(tv)) {
    J->base[-1] = lj_ir_kfunc(J, funcV(tv)) | TREF_FRAME;
    rd->nres = -1;  /* Pending tailcall. */
  } else {
    /* NYI: non-function metamethods. */
    lj_trace_err(J, LJ_TRERR_BADTYPE);
  }
}

void LJ_FASTCALL recff_cdata_call(jit_State *J, RecordFFData *rd)
{
  GCcdata *cd = argv2cdata(J, J->base[0], &rd->argv[0]);
  if (cd->typeid == CTID_CTYPEID)
    crec_alloc(J, rd, crec_constructor(J, cd, J->base[0]));
  else if (!crec_call(J, rd, cd))
    crec_call_meta(J, rd, cd->typeid);
}

static TRef crec_arith_int64(jit_State *J, TRef *sp, CType **s, MMS mm)
{
  if (ctype_isnum(s[0]->info) && ctype_isnum(s[1]->info)) {
    IRType dt;
    CTypeID id;
    TRef tr;
    MSize i;
    lj_needsplit(J);
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
    if (mm < MM_add) {
      /* Assume true comparison. Fixup and emit pending guard later. */
      IROp op;
      if (mm == MM_eq) {
	op = IR_EQ;
      } else {
	op = mm == MM_lt ? IR_LT : IR_LE;
	if (dt == IRT_U64)
	  op += (IR_ULT-IR_LT);
      }
      lj_ir_set(J, IRTG(op, dt), sp[0], sp[1]);
      J->postproc = LJ_POST_FIXGUARD;
      return TREF_TRUE;
    } else {
      tr = emitir(IRT(mm+(int)IR_ADD-(int)MM_add, dt), sp[0], sp[1]);
    }
    return emitir(IRTG(IR_CNEWI, IRT_CDATA), lj_ir_kint(J, id), tr);
  }
  return 0;
}

static TRef crec_arith_ptr(jit_State *J, TRef *sp, CType **s, MMS mm)
{
  CTState *cts = ctype_ctsG(J2G(J));
  CType *ctp = s[0];
  if (ctype_isptr(ctp->info) || ctype_isrefarray(ctp->info)) {
    if ((mm == MM_sub || mm == MM_eq || mm == MM_lt || mm == MM_le) &&
	(ctype_isptr(s[1]->info) || ctype_isrefarray(s[1]->info))) {
      if (mm == MM_sub) {  /* Pointer difference. */
	TRef tr;
	CTSize sz = lj_ctype_size(cts, ctype_cid(ctp->info));
	if (sz == 0 || (sz & (sz-1)) != 0)
	  return 0;  /* NYI: integer division. */
	tr = emitir(IRT(IR_SUB, IRT_PTR), sp[0], sp[1]);
	tr = emitir(IRT(IR_BSAR, IRT_INTP), tr, lj_ir_kint(J, lj_fls(sz)));
#if LJ_64
	tr = emitconv(tr, IRT_NUM, IRT_INTP, 0);
#endif
	return tr;
      } else {  /* Pointer comparison (unsigned). */
	/* Assume true comparison. Fixup and emit pending guard later. */
	IROp op = mm == MM_eq ? IR_EQ : mm == MM_lt ? IR_ULT : IR_ULE;
	lj_ir_set(J, IRTG(op, IRT_PTR), sp[0], sp[1]);
	J->postproc = LJ_POST_FIXGUARD;
	return TREF_TRUE;
      }
    }
    if (!((mm == MM_add || mm == MM_sub) && ctype_isnum(s[1]->info)))
      return 0;
  } else if (mm == MM_add && ctype_isnum(ctp->info) &&
	     (ctype_isptr(s[1]->info) || ctype_isrefarray(s[1]->info))) {
    TRef tr = sp[0]; sp[0] = sp[1]; sp[1] = tr;  /* Swap pointer and index. */
    ctp = s[1];
  } else {
    return 0;
  }
  {
    TRef tr = sp[1];
    IRType t = tref_type(tr);
    CTSize sz = lj_ctype_size(cts, ctype_cid(ctp->info));
    CTypeID id;
#if LJ_64
    if (t == IRT_NUM || t == IRT_FLOAT)
      tr = emitconv(tr, IRT_INTP, t, IRCONV_TRUNC|IRCONV_ANY);
    else if (!(t == IRT_I64 || t == IRT_U64))
      tr = emitconv(tr, IRT_INTP, IRT_INT,
		    ((t - IRT_I8) & 1) ? 0 : IRCONV_SEXT);
#else
    if (!tref_typerange(sp[1], IRT_I8, IRT_U32)) {
      tr = emitconv(tr, IRT_INTP, t,
		    (t == IRT_NUM || t == IRT_FLOAT) ?
		    IRCONV_TRUNC|IRCONV_ANY : 0);
    }
#endif
    tr = emitir(IRT(IR_MUL, IRT_INTP), tr, lj_ir_kintp(J, sz));
    tr = emitir(IRT(mm+(int)IR_ADD-(int)MM_add, IRT_PTR), sp[0], tr);
    id = lj_ctype_intern(cts, CTINFO(CT_PTR, CTALIGN_PTR|ctype_cid(ctp->info)),
			 CTSIZE_PTR);
    return emitir(IRTG(IR_CNEWI, IRT_CDATA), lj_ir_kint(J, id), tr);
  }
}

/* Record ctype arithmetic metamethods. */
static void crec_arith_meta(jit_State *J, CTState *cts, RecordFFData *rd)
{
  cTValue *tv = NULL;
  if (J->base[0]) {
    if (tviscdata(&rd->argv[0]))
      tv = lj_ctype_meta(cts, argv2cdata(J, J->base[0], &rd->argv[0])->typeid,
			 (MMS)rd->data);
    if (!tv && J->base[1] && tviscdata(&rd->argv[1]))
      tv = lj_ctype_meta(cts, argv2cdata(J, J->base[1], &rd->argv[1])->typeid,
			 (MMS)rd->data);
  }
  if (tv && tvisfunc(tv)) {
    J->base[-1] = lj_ir_kfunc(J, funcV(tv)) | TREF_FRAME;
    rd->nres = -1;  /* Pending tailcall. */
  } else {
    /* NYI: non-function metamethods. */
    lj_trace_err(J, LJ_TRERR_BADTYPE);
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
    if (!tr) {
      goto trymeta;
    } else if (tref_iscdata(tr)) {
      CTypeID id = argv2cdata(J, tr, &rd->argv[i])->typeid;
      ct = ctype_raw(cts, id);
      if (ctype_isptr(ct->info)) {  /* Resolve pointer or reference. */
	IRType t = (LJ_64 && ct->size == 8) ? IRT_P64 : IRT_P32;
	if (ctype_isref(ct->info)) ct = ctype_rawchild(cts, ct);
	tr = emitir(IRT(IR_FLOAD, t), tr, IRFL_CDATA_PTR);
      } else if (ctype_isinteger(ct->info) && ct->size == 8) {
	IRType t = (ct->info & CTF_UNSIGNED) ? IRT_U64 : IRT_I64;
	tr = emitir(IRT(IR_FLOAD, t), tr, IRFL_CDATA_INT64);
	lj_needsplit(J);
	goto ok;
      } else if (ctype_isfunc(ct->info)) {
	tr = emitir(IRT(IR_FLOAD, IRT_PTR), tr, IRFL_CDATA_PTR);
	ct = ctype_get(cts,
	  lj_ctype_intern(cts, CTINFO(CT_PTR, CTALIGN_PTR|id), CTSIZE_PTR));
      } else {
	tr = emitir(IRT(IR_ADD, IRT_PTR), tr, lj_ir_kintp(J, sizeof(GCcdata)));
      }
      if (ctype_isenum(ct->info)) ct = ctype_child(cts, ct);
      if (ctype_isnum(ct->info)) {
	IRType t = crec_ct2irt(ct);
	if (t == IRT_CDATA) goto trymeta;
	if (t == IRT_I64 || t == IRT_U64) lj_needsplit(J);
	tr = emitir(IRT(IR_XLOAD, t), tr, 0);
      } else if (!(ctype_isptr(ct->info) || ctype_isrefarray(ct->info))) {
	goto trymeta;
      }
    } else if (tref_isnil(tr)) {
      tr = lj_ir_kptr(J, NULL);
      ct = ctype_get(cts, CTID_P_VOID);
    } else if (tref_isinteger(tr)) {
      ct = ctype_get(cts, CTID_INT32);
    } else if (!tref_isnum(tr)) {
      goto trymeta;
    }
  ok:
    s[i] = ct;
    sp[i] = tr;
  }
  {
    TRef tr;
    if ((tr = crec_arith_int64(J, sp, s, (MMS)rd->data)) ||
	(tr = crec_arith_ptr(J, sp, s, (MMS)rd->data))) {
      J->base[0] = tr;
      /* Fixup cdata comparisons, too. Avoids some cdata escapes. */
      if (J->postproc == LJ_POST_FIXGUARD && frame_iscont(J->L->base-1) &&
	  !irt_isguard(J->guardemit)) {
	const BCIns *pc = frame_contpc(J->L->base-1) - 1;
	if (bc_op(*pc) <= BC_ISNEP) {
	  setframe_pc(&J2G(J)->tmptv, pc);
	  J2G(J)->tmptv.u32.lo = ((tref_istrue(tr) ^ bc_op(*pc)) & 1);
	  J->postproc = LJ_POST_FIXCOMP;
	}
      }
    } else {
    trymeta:
      crec_arith_meta(J, cts, rd);
    }
  }
}

/* -- C library namespace metamethods ------------------------------------- */

void LJ_FASTCALL recff_clib_index(jit_State *J, RecordFFData *rd)
{
  CTState *cts = ctype_ctsG(J2G(J));
  if (tref_isudata(J->base[0]) && tref_isstr(J->base[1]) &&
      udataV(&rd->argv[0])->udtype == UDTYPE_FFI_CLIB) {
    CLibrary *cl = (CLibrary *)uddata(udataV(&rd->argv[0]));
    GCstr *name = strV(&rd->argv[1]);
    CType *ct;
    CTypeID id = lj_ctype_getname(cts, &ct, name, CLNS_INDEX);
    cTValue *tv = lj_tab_getstr(cl->cache, name);
    rd->nres = rd->data;
    if (id && tv && !tvisnil(tv)) {
      /* Specialize to the symbol name and make the result a constant. */
      emitir(IRTG(IR_EQ, IRT_STR), J->base[1], lj_ir_kstr(J, name));
      if (ctype_isconstval(ct->info)) {
	if (ct->size >= 0x80000000u &&
	    (ctype_child(cts, ct)->info & CTF_UNSIGNED))
	  J->base[0] = lj_ir_knum(J, (lua_Number)(uint32_t)ct->size);
	else
	  J->base[0] = lj_ir_kint(J, (int32_t)ct->size);
      } else if (ctype_isextern(ct->info)) {
	CTypeID sid = ctype_cid(ct->info);
	void *sp = *(void **)cdataptr(cdataV(tv));
	TRef ptr;
	ct = ctype_raw(cts, sid);
	if (rd->data && ctype_isenum(ct->info)) ct = ctype_child(cts, ct);
	if (LJ_64 && !checkptr32(sp))
	  ptr = lj_ir_kintp(J, (uintptr_t)sp);
	else
	  ptr = lj_ir_kptr(J, sp);
	if (rd->data) {
	  J->base[0] = crec_tv_ct(J, ct, sid, ptr);
	} else {
	  J->needsnap = 1;
	  crec_ct_tv(J, ct, ptr, J->base[2], &rd->argv[2]);
	}
      } else {
	J->base[0] = lj_ir_kgc(J, obj2gco(cdataV(tv)), IRT_CDATA);
      }
    } else {
      lj_trace_err(J, LJ_TRERR_NOCACHE);
    }
  }  /* else: interpreter will throw. */
}

/* -- FFI library functions ----------------------------------------------- */

static TRef crec_toint(jit_State *J, CTState *cts, TRef sp, TValue *sval)
{
  return crec_ct_tv(J, ctype_get(cts, CTID_INT32), 0, sp, sval);
}

void LJ_FASTCALL recff_ffi_new(jit_State *J, RecordFFData *rd)
{
  crec_alloc(J, rd, argv2ctype(J, J->base[0], &rd->argv[0]));
}

void LJ_FASTCALL recff_ffi_errno(jit_State *J, RecordFFData *rd)
{
  UNUSED(rd);
  if (J->base[0])
    lj_trace_err(J, LJ_TRERR_NYICALL);
  J->base[0] = lj_ir_call(J, IRCALL_lj_vm_errno);
}

void LJ_FASTCALL recff_ffi_string(jit_State *J, RecordFFData *rd)
{
  CTState *cts = ctype_ctsG(J2G(J));
  TRef tr = J->base[0];
  if (tr) {
    TRef trlen = J->base[1];
    if (trlen) {
      trlen = crec_toint(J, cts, trlen, &rd->argv[1]);
      tr = crec_ct_tv(J, ctype_get(cts, CTID_P_CVOID), 0, tr, &rd->argv[0]);
    } else {
      tr = crec_ct_tv(J, ctype_get(cts, CTID_P_CCHAR), 0, tr, &rd->argv[0]);
      trlen = lj_ir_call(J, IRCALL_strlen, tr);
    }
    J->base[0] = emitir(IRT(IR_XSNEW, IRT_STR), tr, trlen);
  }  /* else: interpreter will throw. */
}

void LJ_FASTCALL recff_ffi_copy(jit_State *J, RecordFFData *rd)
{
  CTState *cts = ctype_ctsG(J2G(J));
  TRef trdst = J->base[0], trsrc = J->base[1], trlen = J->base[2];
  if (trdst && trsrc && (trlen || tref_isstr(trsrc))) {
    trdst = crec_ct_tv(J, ctype_get(cts, CTID_P_VOID), 0, trdst, &rd->argv[0]);
    trsrc = crec_ct_tv(J, ctype_get(cts, CTID_P_CVOID), 0, trsrc, &rd->argv[1]);
    if (trlen) {
      trlen = crec_toint(J, cts, trlen, &rd->argv[2]);
    } else {
      trlen = emitir(IRTI(IR_FLOAD), J->base[1], IRFL_STR_LEN);
      trlen = emitir(IRTI(IR_ADD), trlen, lj_ir_kint(J, 1));
    }
    lj_ir_call(J, IRCALL_memcpy, trdst, trsrc, trlen);
    emitir(IRT(IR_XBAR, IRT_NIL), 0, 0);
    rd->nres = 0;
  }  /* else: interpreter will throw. */
}

void LJ_FASTCALL recff_ffi_fill(jit_State *J, RecordFFData *rd)
{
  CTState *cts = ctype_ctsG(J2G(J));
  TRef tr = J->base[0], trlen = J->base[1], trfill = J->base[2];
  if (tr && trlen) {
    tr = crec_ct_tv(J, ctype_get(cts, CTID_P_VOID), 0, tr, &rd->argv[0]);
    trlen = crec_toint(J, cts, trlen, &rd->argv[1]);
    if (trfill)
      trfill = crec_toint(J, cts, trfill, &rd->argv[2]);
    else
      trfill = lj_ir_kint(J, 0);
    lj_ir_call(J, IRCALL_memset, tr, trfill, trlen);
    emitir(IRT(IR_XBAR, IRT_NIL), 0, 0);
    rd->nres = 0;
  }  /* else: interpreter will throw. */
}

void LJ_FASTCALL recff_ffi_istype(jit_State *J, RecordFFData *rd)
{
  argv2ctype(J, J->base[0], &rd->argv[0]);
  if (tref_iscdata(J->base[1])) {
    argv2ctype(J, J->base[1], &rd->argv[1]);
    J->postproc = LJ_POST_FIXBOOL;
    J->base[0] = TREF_TRUE;
  } else {
    J->base[0] = TREF_FALSE;
  }
}

void LJ_FASTCALL recff_ffi_abi(jit_State *J, RecordFFData *rd)
{
  if (tref_isstr(J->base[0])) {
    /* Specialize to the ABI string to make the boolean result a constant. */
    emitir(IRTG(IR_EQ, IRT_STR), J->base[0], lj_ir_kstr(J, strV(&rd->argv[0])));
    J->postproc = LJ_POST_FIXBOOL;
    J->base[0] = TREF_TRUE;
  }  /* else: interpreter will throw. */
}

/* -- Miscellaneous library functions ------------------------------------- */

void LJ_FASTCALL lj_crecord_tonumber(jit_State *J, RecordFFData *rd)
{
  CTState *cts = ctype_ctsG(J2G(J));
  CType *d, *ct = lj_ctype_rawref(cts, cdataV(&rd->argv[0])->typeid);
  if (ctype_isenum(ct->info)) ct = ctype_child(cts, ct);
  if (ctype_isnum(ct->info) || ctype_iscomplex(ct->info)) {
    if (ctype_isinteger_or_bool(ct->info) && ct->size <= 4 &&
	!(ct->size == 4 && (ct->info & CTF_UNSIGNED)))
      d = ctype_get(cts, CTID_INT32);
    else
      d = ctype_get(cts, CTID_DOUBLE);
    J->base[0] = crec_ct_tv(J, d, 0, J->base[0], &rd->argv[0]);
  } else {
    J->base[0] = TREF_NIL;
  }
}

#undef IR
#undef emitir
#undef emitconv

#endif
