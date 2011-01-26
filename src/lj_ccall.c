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
#include "lj_trace.h"

/* Target-specific handling of register arguments. */
#if LJ_TARGET_X86
/* -- x86 calling conventions --------------------------------------------- */

#if LJ_ABI_WIN

#define CCALL_HANDLE_STRUCTRET \
  /* Return structs bigger than 8 by reference (on stack only). */ \
  cc->retref = (sz > 8); \
  if (cc->retref) cc->stack[nsp++] = (GPRArg)dp;

#define CCALL_HANDLE_COMPLEXRET CCALL_HANDLE_STRUCTRET

#else

#define CCALL_HANDLE_STRUCTRET \
  cc->retref = 1;  /* Return all structs by reference (in reg or on stack). */ \
  if (ngpr < maxgpr) \
    cc->gpr[ngpr++] = (GPRArg)dp; \
  else \
    cc->stack[nsp++] = (GPRArg)dp;

#define CCALL_HANDLE_COMPLEXRET \
  /* Return complex float in GPRs and complex double by reference. */ \
  cc->retref = (sz > 8); \
  if (cc->retref) { \
    if (ngpr < maxgpr) \
      cc->gpr[ngpr++] = (GPRArg)dp; \
    else \
      cc->stack[nsp++] = (GPRArg)dp; \
  }

#endif

#define CCALL_HANDLE_COMPLEXRET2 \
  if (!cc->retref) \
    *(int64_t *)dp = *(int64_t *)sp;  /* Copy complex float from GPRs. */

#define CCALL_HANDLE_STRUCTARG \
  ngpr = maxgpr;  /* Pass all structs by value on the stack. */

#define CCALL_HANDLE_COMPLEXARG \
  isfp = 1;  /* Pass complex by value on stack. */

#define CCALL_HANDLE_REGARG \
  if (!isfp) {  /* Only non-FP values may be passed in registers. */ \
    if (n > 1) {  /* Anything > 32 bit is passed on the stack. */ \
      if (!LJ_ABI_WIN) ngpr = maxgpr;  /* Prevent reordering. */ \
    } else if (ngpr + 1 <= maxgpr) { \
      dp = &cc->gpr[ngpr]; \
      ngpr += n; \
      goto done; \
    } \
  }

#elif LJ_TARGET_X64 && LJ_ABI_WIN
/* -- Windows/x64 calling conventions ------------------------------------- */

#define CCALL_HANDLE_STRUCTRET \
  /* Return structs of size 1, 2, 4 or 8 in a GPR. */ \
  cc->retref = !(sz == 1 || sz == 2 || sz == 4 || sz == 8); \
  if (cc->retref) cc->gpr[ngpr++] = (GPRArg)dp;

#define CCALL_HANDLE_COMPLEXRET CCALL_HANDLE_STRUCTRET

#define CCALL_HANDLE_COMPLEXRET2 \
  if (!cc->retref) \
    *(int64_t *)dp = *(int64_t *)sp;  /* Copy complex float from GPRs. */

#define CCALL_HANDLE_STRUCTARG \
  /* Pass structs of size 1, 2, 4 or 8 in a GPR by value. */ \
  if (!(sz == 1 || sz == 2 || sz == 4 || sz == 8)) { \
    rp = cdataptr(lj_cdata_new(cts, did, sz)); \
    sz = CTSIZE_PTR;  /* Pass all other structs by reference. */ \
  }

#define CCALL_HANDLE_COMPLEXARG \
  /* Pass complex float in a GPR and complex double by reference. */ \
  if (sz != 2*sizeof(float)) { \
    rp = cdataptr(lj_cdata_new(cts, did, sz)); \
    sz = CTSIZE_PTR; \
  }

/* Windows/x64 argument registers are strictly positional (use ngpr). */
#define CCALL_HANDLE_REGARG \
  if (isfp) { \
    if (ngpr < 4) { dp = &cc->fpr[ngpr++]; nfpr = ngpr; goto done; } \
  } else { \
    if (ngpr < 4) { dp = &cc->gpr[ngpr++]; goto done; } \
  }

#elif LJ_TARGET_X64
/* -- POSIX/x64 calling conventions --------------------------------------- */

#define CCALL_HANDLE_STRUCTRET \
  if (sz <= 16) { \
    cc->retref = 0; \
    goto err_nyi;  /* NYI: crazy x64 rules for small structs. */ \
  } else { \
    cc->retref = 1;  /* Return all bigger structs by reference. */ \
    cc->gpr[ngpr++] = (GPRArg)dp; \
  }

#define CCALL_HANDLE_COMPLEXRET \
  /* Complex values are returned in one or two FPRs. */ \
  cc->retref = 0;

#define CCALL_HANDLE_COMPLEXRET2 \
  if (ctr->size == 2*sizeof(float)) {  /* Copy complex float from FPR. */ \
    *(int64_t *)dp = cc->fpr[0].l[0]; \
  } else {  /* Copy non-contiguous complex double from FPRs. */ \
    ((int64_t *)dp)[0] = cc->fpr[0].l[0]; \
    ((int64_t *)dp)[1] = cc->fpr[1].l[0]; \
  }

#define CCALL_HANDLE_STRUCTARG \
  if (sz <= 16) { \
    goto err_nyi;  /* NYI: crazy x64 rules for small structs. */ \
  }  /* Pass all other structs by value on stack. */

#define CCALL_HANDLE_COMPLEXARG \
  isfp = 2;  /* Pass complex in FPRs or on stack. Needs postprocessing. */

#define CCALL_HANDLE_REGARG \
  if (isfp) {  /* Try to pass argument in FPRs. */ \
    if (nfpr + n <= CCALL_NARG_FPR) { \
      dp = &cc->fpr[nfpr]; \
      nfpr += n; \
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
/* -- PPC/SPE calling conventions ----------------------------------------- */

#define CCALL_HANDLE_STRUCTRET \
  cc->retref = 1;  /* Return all structs by reference. */ \
  cc->gpr[ngpr++] = (GPRArg)dp;

#define CCALL_HANDLE_COMPLEXRET \
  /* Complex values are returned in 2 or 4 GPRs. */ \
  cc->retref = 0;

#define CCALL_HANDLE_COMPLEXRET2 \
  memcpy(dp, sp, ctr->size);  /* Copy complex from GPRs. */

#define CCALL_HANDLE_STRUCTARG \
  rp = cdataptr(lj_cdata_new(cts, did, sz)); \
  sz = CTSIZE_PTR;  /* Pass all structs by reference. */

#define CCALL_HANDLE_COMPLEXARG \
  /* Pass complex by value in 2 or 4 GPRs. */

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
#error "missing calling convention definitions for this architecture"
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
  switch (ctype_cconv(ct->info)) {
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
    void *dp = cdataptr(cd);
    setcdataV(L, L->top++, cd);
    if (ctype_isstruct(ctr->info)) {
      CCALL_HANDLE_STRUCTRET
    } else {
      CCALL_HANDLE_COMPLEXRET
    }
#if LJ_TARGET_X86
  } else if (ctype_isfp(ctr->info)) {
    cc->resx87 = ctr->size == sizeof(float) ? 1 : 2;
#endif
  }

  /* Skip initial attributes. */
  fid = ct->sib;
  while (fid) {
    CType *ctf = ctype_get(cts, fid);
    if (!ctype_isattrib(ctf->info)) break;
    fid = ctf->sib;
  }

  /* Walk through all passed arguments. */
  for (o = L->base+1; o < top; o++) {
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
    } else if (ctype_isstruct(d->info)) {
      CCALL_HANDLE_STRUCTARG
    } else if (ctype_iscomplex(d->info)) {
      CCALL_HANDLE_COMPLEXARG
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
#if LJ_TARGET_X64 && !LJ_ABI_WIN
    if (isfp == 2 && n == 2 && (uint8_t *)dp == (uint8_t *)&cc->fpr[nfpr-2]) {
      cc->fpr[nfpr-1].d[0] = cc->fpr[nfpr-2].d[1];  /* Split complex double. */
      cc->fpr[nfpr-2].d[1] = 0;
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
    if (!cc->retref) {
      void *dp = cdataptr(cdataV(L->top-1));  /* Use preallocated object. */
      memcpy(dp, sp, ctr->size);  /* Copy struct return value from GPRs. */
    }
    return 1;  /* One GC step. */
  }
  if (ctype_iscomplex(ctr->info)) {
    /* Return cdata object which is already on top of stack. */
    void *dp = cdataptr(cdataV(L->top-1));  /* Use preallocated object. */
    CCALL_HANDLE_COMPLEXRET2
    return 1;  /* One GC step. */
  }
#if CCALL_NUM_FPR
  if (ctype_isfp(ctr->info) || ctype_isvector(ctr->info))
    sp = &cc->fpr[0];
#endif
  /* No reference types end up here, so there's no need for the CTypeID. */
  lua_assert(!(ctype_isrefarray(ctr->info) || ctype_isstruct(ctr->info)));
  if (ctype_isenum(ctr->info)) ctr = ctype_child(cts, ctr);
  return lj_cconv_tv_ct(cts, ctr, 0, L->top-1, (uint8_t *)sp);
}

/* Call C function. */
int lj_ccall_func(lua_State *L, GCcdata *cd)
{
  CTState *cts = ctype_cts(L);
  CType *ct = ctype_raw(cts, cd->typeid);
  CTSize sz = CTSIZE_PTR;
  if (ctype_isptr(ct->info)) {
    sz = ct->size;
    ct = ctype_rawchild(cts, ct);
  }
  if (ctype_isfunc(ct->info)) {
    CCallState cc;
    int gcsteps, ret;
    cc.func = (void (*)(void))cdata_getptr(cdataptr(cd), sz);
    gcsteps = ccall_set_args(L, cts, ct, &cc);
    lj_vm_ffi_call(&cc);
    gcsteps += ccall_get_results(L, cts, ct, &cc, &ret);
#if LJ_TARGET_X86 && LJ_ABI_WIN
    /* Automatically detect __stdcall and fix up C function declaration. */
    if (cc.spadj && ctype_cconv(ct->info) == CTCC_CDECL) {
      CTF_INSERT(ct->info, CCONV, CTCC_STDCALL);
      lj_trace_abort(G(L));
    }
#endif
    while (gcsteps-- > 0)
      lj_gc_check(L);
    return ret;
  }
  return -1;  /* Not a function. */
}

#endif
