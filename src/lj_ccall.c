/*
** FFI C call handling.
** Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
*/

#include "lj_obj.h"

#if LJ_HASFFI

#include "lj_gc.h"
#include "lj_err.h"
#include "lj_str.h"
#include "lj_ctype.h"
#include "lj_cconv.h"
#include "lj_cdata.h"
#include "lj_ccall.h"

/* Target-specific handling of register arguments. */
#if LJ_TARGET_X86

#define CCALL_HANDLE_REGARG \
  if (!isfp) {  /* Only non-FP values may be passed in registers. */ \
    if (n > 1) {  /* Anything > 32 bit is passed on the stack. */ \
      ngpr = maxgpr;  /* Prevent reordering. */ \
    } else if (ngpr + 1 <= maxgpr) { \
      dp = &cc->gpr[ngpr]; \
      ngpr += n; \
      goto done; \
    } \
  }

#elif LJ_TARGET_X64 && LJ_ABI_WIN

/* Windows/x64 argument registers are strictly positional (use ngpr). */
#define CCALL_HANDLE_REGARG \
  if (isfp) { \
    if (ngpr < 4) { dp = &cc->fpr[ngpr++]; nfpr = ngpr; goto done; } \
  } else { \
    if (ngpr < 4) { dp = &cc->gpr[ngpr++]; goto done; } \
  }

#elif LJ_TARGET_X64

#define CCALL_HANDLE_REGARG \
  if (isfp) {  /* Try to pass argument in FPRs. */ \
    if (nfpr + isfp <= CCALL_NARG_FPR) { \
      dp = &cc->fpr[nfpr]; \
      nfpr += isfp; \
      goto done; \
    } \
  } else {  /* Try to pass argument in GPRs. */ \
    /* Note that reordering is explicitly allowed in the x64 ABI. */ \
    if (n <= 2 && ngpr + n <= maxgpr) { \
      dp = &cc->gpr[ngpr]; \
      ngpr += n; \
      goto done; \
    } \
  }

#elif LJ_TARGET_PPCSPE

/* PPC/SPE has a softfp ABI. */
#define CCALL_HANDLE_REGARG \
  if (n > 1) {  /* Doesn't fit in a single GPR? */ \
    lua_assert(n == 2 || n == 4);  /* int64_t, double or complex (float). */ \
    if (n == 2) \
      ngpr = (ngpr + 1u) & ~1u;  /* Only align 64 bit value to regpair. */ \
    else if (ngpr + n > maxgpr) \
      ngpr = maxgpr;  /* Prevent reordering. */ \
  } \
  if (ngpr + n <= maxgpr) { \
    dp = &cc->gpr[ngpr]; \
    ngpr += n; \
    goto done; \
  }

#else
#error "missing definition for handling of register arguments"
#endif

/* Infer the destination CTypeID for a vararg argument. */
static CTypeID ccall_ctid_vararg(CTState *cts, cTValue *o)
{
  if (tvisnum(o)) {
    return CTID_DOUBLE;
  } else if (tviscdata(o)) {
    CTypeID id = cdataV(o)->typeid;
    CType *s = ctype_get(cts, id);
    if (ctype_isrefarray(s->info)) {
      return lj_ctype_intern(cts,
	       CTINFO(CT_PTR, CTALIGN_PTR|ctype_cid(s->info)), CTSIZE_PTR);
    } else if (ctype_isstruct(s->info) || ctype_isfunc(s->info)) {
      return lj_ctype_intern(cts, CTINFO(CT_PTR, CTALIGN_PTR|id), CTSIZE_PTR);
    } if (ctype_isfp(s->info) && s->size == sizeof(float)) {
      return CTID_DOUBLE;
    } else {
      return id;
    }
  } else if (tvisstr(o)) {
    return CTID_P_CCHAR;
  } else if (tvisbool(o)) {
    return CTID_BOOL;
  } else {
    return CTID_P_VOID;
  }
}

/* Setup arguments for C call. */
static int ccall_set_args(lua_State *L, CTState *cts, CType *ct,
			  CCallState *cc)
{
  int gcsteps = 0;
  TValue *o, *top = L->top;
  CTypeID fid;
  CType *ctr;
  MSize maxgpr, ngpr = 0, nsp = 0;
#if CCALL_NARG_FPR
  MSize nfpr = 0;
#endif

  /* Clear unused regs to get some determinism in case of misdeclaration. */
  memset(cc->gpr, 0, sizeof(cc->gpr));
#if CCALL_NUM_FPR
  memset(cc->fpr, 0, sizeof(cc->fpr));
#endif

#if LJ_TARGET_X86
  /* x86 has several different calling conventions. */
  cc->resx87 = 0;
  switch ((ct->info >> CTSHIFT_CCONV) & CTMASK_CCONV) {
  case CTCC_FASTCALL: maxgpr = 2; break;
  case CTCC_THISCALL: maxgpr = 1; break;
  default: maxgpr = 0; break;
  }
#else
  maxgpr = CCALL_NARG_GPR;
#endif

  /* Perform required setup for some result types. */
  ctr = ctype_rawchild(cts, ct);
  if (ctype_isvector(ctr->info)) {
    if (!(CCALL_VECTOR_REG && (ctr->size == 8 || ctr->size == 16)))
      goto err_nyi;
  } else if (ctype_iscomplex(ctr->info) || ctype_isstruct(ctr->info)) {
    /* Preallocate cdata object and anchor it after arguments. */
    CTSize sz = ctr->size;
    GCcdata *cd = lj_cdata_new(cts, ctype_cid(ct->info), sz);
    setcdataV(L, L->top++, cd);
    if (ctype_iscomplex(ctr->info)) {
      cc->retref = (sz == 2*sizeof(float)) ? CCALL_COMPLEXF_RETREF :
					     CCALL_COMPLEX_RETREF;
    } else {
#if CCALL_STRUCT_RETREF
      cc->retref = 1;  /* Return all structs by reference. */
#elif LJ_TARGET_X64
#if LJ_ABI_WIN
      /* Return structs of size 1, 2, 4 or 8 in a GPR. */
      cc->retref = !(sz == 1 || sz == 2 || sz == 4 || sz == 8);
#else
      if (sz <= 16) goto err_nyi;  /* NYI: crazy x64 rules for structs. */
      cc->retref = 1;  /* Return all bigger structs by reference. */
#endif
#else
#error "missing definition for handling of struct return values"
#endif
    }
    /* Pass reference to returned aggregate in first argument. */
    if (cc->retref) {
      if (ngpr < maxgpr)
	cc->gpr[ngpr++] = (GPRArg)cdataptr(cd);
      else
	cc->stack[nsp++] = (GPRArg)cdataptr(cd);
    }
#if LJ_TARGET_X86
  } else if (ctype_isfp(ctr->info)) {
    cc->resx87 = ctr->size == sizeof(float) ? 1 : 2;
#endif
  }

  /* Walk through all passed arguments. */
  for (fid = ct->sib, o = L->base+1; o < top; o++) {
    CTypeID did;
    CType *d;
    CTSize sz;
    MSize n, isfp = 0, isva = 0;
    void *dp, *rp = NULL;

    if (fid) {  /* Get argument type from field. */
      CType *ctf = ctype_get(cts, fid);
      fid = ctf->sib;
      lua_assert(ctype_isfield(ctf->info));
      did = ctype_cid(ctf->info);
    } else {
      if (!(ct->info & CTF_VARARG))
	lj_err_caller(L, LJ_ERR_FFI_NUMARG);  /* Too many arguments. */
      did = ccall_ctid_vararg(cts, o);  /* Infer vararg type. */
      isva = 1;
    }
    d = ctype_raw(cts, did);
    sz = d->size;

    /* Find out how (by value/ref) and where (GPR/FPR) to pass an argument. */
    if (ctype_isnum(d->info)) {
      if (sz > 8) goto err_nyi;
      if ((d->info & CTF_FP)) {
	isfp = 1;
      } else if (sz < CTSIZE_PTR) {
	d = ctype_get(cts, CTID_INT_PSZ);
      }
    } else if (ctype_isvector(d->info)) {
      if (CCALL_VECTOR_REG && (sz == 8 || sz == 16))
	isfp = 1;
      else
	goto err_nyi;
    } else if (ctype_iscomplex(d->info)) {
#if CCALL_COMPLEX_ARGREF
      rp = cdataptr(lj_cdata_new(cts, did, sz));
      sz = CTSIZE_PTR;
#else
      isfp = 2;
#endif
    } else if (ctype_isstruct(d->info)) {
      int sref = CCALL_STRUCT_ARGREF;
#if LJ_TARGET_X86
      ngpr = maxgpr;  /* Pass all structs by value on the stack. */
#elif LJ_TARGET_X64
#if LJ_ABI_WIN
      /* Pass structs of size 1, 2, 4 or 8 in a GPR by value. */
      sref = !(sz == 1 || sz == 2 || sz == 4 || sz == 8);
#else
      if (sz <= 16) goto err_nyi;  /* NYI: crazy x64 rules for structs. */
      /* Pass all bigger structs by value on the stack. */
#endif
#endif
      if (sref) {  /* Pass struct by reference. */
	rp = cdataptr(lj_cdata_new(cts, did, sz));
	sz = CTSIZE_PTR;  /* Pass all other structs by reference. */
      }
    } else {
      sz = CTSIZE_PTR;
    }
    sz = (sz + CTSIZE_PTR-1) & ~(CTSIZE_PTR-1);
    n = sz / CTSIZE_PTR;  /* Number of GPRs or stack slots needed. */

    CCALL_HANDLE_REGARG  /* Handle register arguments. */

    /* Otherwise pass argument on stack. */
    if (CCALL_ALIGN_STACKARG && !rp && (d->info & CTF_ALIGN) > CTALIGN_PTR) {
      MSize align = (1u << ctype_align(d->info-CTALIGN_PTR)) -1;
      nsp = (nsp + align) & ~align;  /* Align argument on stack. */
    }
    if (nsp + n >= CCALL_MAXSTACK) {  /* Too many arguments. */
    err_nyi:
      lj_err_caller(L, LJ_ERR_FFI_NYICALL);
    }
    dp = &cc->stack[nsp];
    nsp += n;
    isva = 0;

  done:
    if (rp) {  /* Pass by reference. */
      gcsteps++;
      *(void **)dp = rp;
      dp = rp;
    }
    lj_cconv_ct_tv(cts, d, (uint8_t *)dp, o, 0);
#if LJ_TARGET_X64 && LJ_ABI_WIN
    if (isva) {  /* Windows/x64 mirrors varargs in both register sets. */
      if (nfpr == ngpr)
	cc->gpr[ngpr-1] = cc->fpr[ngpr-1].l[0];
      else
	cc->fpr[ngpr-1].l[0] = cc->gpr[ngpr-1];
    }
#endif
  }
  if (fid) lj_err_caller(L, LJ_ERR_FFI_NUMARG);  /* Too few arguments. */

#if LJ_TARGET_X64
  cc->nfpr = nfpr;  /* Required for vararg functions. */
#endif
  cc->nsp = nsp;
  cc->spadj = (CCALL_SPS_FREE + CCALL_SPS_EXTRA)*CTSIZE_PTR;
  if (nsp > CCALL_SPS_FREE)
    cc->spadj += (((nsp-CCALL_SPS_FREE)*CTSIZE_PTR + 15u) & ~15u);
  return gcsteps;
}

/* Get results from C call. */
static int ccall_get_results(lua_State *L, CTState *cts, CType *ct,
			     CCallState *cc, int *ret)
{
  CType *ctr = ctype_rawchild(cts, ct);
  void *sp = &cc->gpr[0];
  if (ctype_isvoid(ctr->info)) {
    *ret = 0;  /* Zero results. */
    return 0;  /* No additional GC step. */
  }
  *ret = 1;  /* One result. */
  if (ctype_isstruct(ctr->info)) {
    /* Return cdata object which is already on top of stack. */
    if (!CCALL_STRUCT_RETREF && !cc->retref) {
      void *dp = cdataptr(cdataV(L->top-1));  /* Use preallocated object. */
      memcpy(dp, sp, ctr->size);  /* Copy struct return value from GPRs. */
    }
    return 1;  /* One GC step. */
  }
  if (ctype_iscomplex(ctr->info)) {
    /* Return cdata object which is already on top of stack. */
#if !CCALL_COMPLEX_RETREF || !CCALL_COMPLEXF_RETREF
    void *dp = cdataptr(cdataV(L->top-1));  /* Use preallocated object. */
#if CCALL_COMPLEX_RETREF && !CCALL_COMPLEXF_RETREF
    if (ctr->size == 2*sizeof(float))
      memcpy(dp, sp, ctr->size);  /* Copy complex float from GPRs. */
#elif CCALL_NUM_FPR
    /* Copy non-contiguous re/im part from FPRs to cdata object. */
    if (ctr->size == 2*sizeof(float)) {
      ((float *)dp)[0] = cc->fpr[0].f[0];
      ((float *)dp)[1] = cc->fpr[1].f[0];
    } else {
      ((double *)dp)[0] = cc->fpr[0].d[0];
      ((double *)dp)[1] = cc->fpr[1].d[0];
    }
#else
    memcpy(dp, sp, ctr->size);  /* Copy complex from GPRs. */
#endif
#endif
    return 1;  /* One GC step. */
  }
#if CCALL_NUM_FPR
  if (ctype_isfp(ctr->info) || ctype_isvector(ctr->info))
    sp = &cc->fpr[0];
#endif
  /* No reference types end up here, so there's no need for the CTypeID. */
  lua_assert(!(ctype_isrefarray(ctr->info) || ctype_isstruct(ctr->info)));
  return lj_cconv_tv_ct(cts, ctr, 0, L->top-1, (uint8_t *)sp);
}

/* Call C function. */
int lj_ccall_func(lua_State *L, GCcdata *cd)
{
  CTState *cts = ctype_cts(L);
  CType *ct = ctype_raw(cts, cd->typeid);
  CTSize sz = ct->size;
  void *p = cdataptr(cd);
  if (ctype_isptr(ct->info))
    ct = ctype_rawchild(cts, ct);
  if (ctype_isfunc(ct->info)) {
    CCallState cc;
    int gcsteps, ret;
    cc.func = (void (*)(void))cdata_getptr(p, sz);
    gcsteps = ccall_set_args(L, cts, ct, &cc);
    lj_vm_ffi_call(&cc);
    gcsteps += ccall_get_results(L, cts, ct, &cc, &ret);
    while (gcsteps-- > 0)
      lj_gc_check(L);
    return ret;
  }
  return -1;  /* Not a function. */
}

#endif
