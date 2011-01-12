/*
** FFI library.
** Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
*/

#define lib_ffi_c
#define LUA_LIB

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "lj_obj.h"

#if LJ_HASFFI

#include "lj_gc.h"
#include "lj_err.h"
#include "lj_str.h"
#include "lj_ctype.h"
#include "lj_cparse.h"
#include "lj_cdata.h"
#include "lj_cconv.h"
#include "lj_ccall.h"
#include "lj_clib.h"
#include "lj_ff.h"
#include "lj_lib.h"

/* -- C type checks ------------------------------------------------------- */

/* Check first argument for a C type and returns its ID. */
static CTypeID ffi_checkctype(lua_State *L, CTState *cts)
{
  TValue *o = L->base;
  if (!(o < L->top)) {
  err_argtype:
    lj_err_argtype(L, 1, "C type");
  }
  if (tvisstr(o)) {  /* Parse an abstract C type declaration. */
    GCstr *s = strV(o);
    CPState cp;
    int errcode;
    cp.L = L;
    cp.cts = cts;
    cp.srcname = strdata(s);
    cp.p = strdata(s);
    cp.mode = CPARSE_MODE_ABSTRACT|CPARSE_MODE_NOIMPLICIT;
    errcode = lj_cparse(&cp);
    if (errcode) lj_err_throw(L, errcode);  /* Propagate errors. */
    return cp.val.id;
  } else {
    GCcdata *cd;
    if (!tviscdata(o)) goto err_argtype;
    cd = cdataV(o);
    return cd->typeid == CTID_CTYPEID ? *(CTypeID *)cdataptr(cd) : cd->typeid;
  }
}

/* Check argument for C data and return it. */
static GCcdata *ffi_checkcdata(lua_State *L, int narg)
{
  TValue *o = L->base + narg-1;
  if (!(o < L->top && tviscdata(o)))
    lj_err_argt(L, narg, LUA_TCDATA);
  return cdataV(o);
}

/* Convert argument to C pointer. */
static void *ffi_checkptr(lua_State *L, int narg, CTypeID id)
{
  CTState *cts = ctype_cts(L);
  TValue *o = L->base + narg-1;
  void *p;
  if (o >= L->top)
    lj_err_arg(L, narg, LJ_ERR_NOVAL);
  lj_cconv_ct_tv(cts, ctype_get(cts, id), (uint8_t *)&p, o, 0);
  return p;
}

/* -- C data arithmetic --------------------------------------------------- */

typedef struct FFIArith {
  uint8_t *p[2];
  CType *ct[2];
} FFIArith;

/* Check arguments for arithmetic metamethods. */
static void ffi_checkarith(lua_State *L, CTState *cts, FFIArith *fa)
{
  TValue *o = L->base;
  MSize i;
  if (o+1 >= L->top)
    lj_err_argt(L, 1, LUA_TCDATA);
  for (i = 0; i < 2; i++, o++) {
    if (tviscdata(o)) {
      GCcdata *cd = cdataV(o);
      CTypeID id = (CTypeID)cd->typeid;
      CType *ct = ctype_get(cts, id);
      uint8_t *p = (uint8_t *)cdataptr(cd);
      if (ctype_isref(ct->info)) {
	lua_assert(ct->size == CTSIZE_PTR);
	p = *(uint8_t **)p;
	id = ctype_cid(ct->info);
      }
      fa->ct[i] = ctype_raw(cts, id);
      fa->p[i] = p;
    } else if (tvisnum(o)) {
      fa->ct[i] = ctype_get(cts, CTID_DOUBLE);
      fa->p[i] = (uint8_t *)&o->n;
    } else {
      lj_err_optype(L, o, LJ_ERR_OPARITH);
    }
  }
}

/* Pointer arithmetic. */
static int ffi_arith_ptr(lua_State *L, CTState *cts, FFIArith *fa, MMS mm)
{
  CType *ctp = fa->ct[0];
  uint8_t *pp = fa->p[0];
  ptrdiff_t idx;
  CTSize sz;
  CTypeID id;
  GCcdata *cd;
  if (!(mm == MM_add || mm == MM_sub))
    return 0;
  if (ctype_isptr(ctp->info) || ctype_isrefarray(ctp->info)) {
    if (mm == MM_sub &&
	(ctype_isptr(fa->ct[1]->info) || ctype_isrefarray(fa->ct[1]->info))) {
      /* Pointer difference. */
      intptr_t diff;
      if (!lj_cconv_compatptr(cts, ctp, fa->ct[1], CCF_IGNQUAL))
	lj_err_caller(L, LJ_ERR_FFI_INVTYPE);
      sz = lj_ctype_size(cts, ctype_cid(ctp->info));  /* Element size. */
      if (sz == 0 || sz == CTSIZE_INVALID)
	lj_err_caller(L, LJ_ERR_FFI_INVSIZE);
      if (ctype_isptr(ctp->info))
	pp = (uint8_t *)cdata_getptr(pp, ctp->size);
      if (ctype_isptr(fa->ct[1]->info))
	fa->p[1] = (uint8_t *)cdata_getptr(fa->p[1], fa->ct[1]->size);
      diff = ((intptr_t)pp - (intptr_t)fa->p[1]) / (int32_t)sz;
      /* All valid pointer differences on x64 are in (-2^47, +2^47),
      ** which fits into a double without loss of precision.
      */
      setnumV(L->top-1, (lua_Number)diff);
      return 1;
    }
    if (!ctype_isnum(fa->ct[1]->info)) return 0;
    lj_cconv_ct_ct(cts, ctype_get(cts, CTID_INT_PSZ), fa->ct[1],
		   (uint8_t *)&idx, fa->p[1], 0);
    if (mm == MM_sub) idx = -idx;
  } else if (mm == MM_add &&
      (ctype_isptr(fa->ct[1]->info) || ctype_isrefarray(fa->ct[1]->info))) {
    if (!ctype_isnum(ctp->info)) return 0;
    /* Swap pointer and index. */
    ctp = fa->ct[1]; pp = fa->p[1];
    lj_cconv_ct_ct(cts, ctype_get(cts, CTID_INT_PSZ), fa->ct[0],
		   (uint8_t *)&idx, fa->p[0], 0);
  } else {
    return 0;
  }
  sz = lj_ctype_size(cts, ctype_cid(ctp->info));  /* Element size. */
  if (sz == CTSIZE_INVALID)
    lj_err_caller(L, LJ_ERR_FFI_INVSIZE);
  if (ctype_isptr(ctp->info))
    pp = (uint8_t *)cdata_getptr(pp, ctp->size);
  pp += idx*(int32_t)sz;  /* Compute pointer + index. */
  id = lj_ctype_intern(cts, CTINFO(CT_PTR, CTALIGN_PTR|ctype_cid(ctp->info)),
		       CTSIZE_PTR);
  cd = lj_cdata_new(cts, id, CTSIZE_PTR);
  *(uint8_t **)cdataptr(cd) = pp;
  setcdataV(L, L->top-1, cd);
  lj_gc_check(L);
  return 1;
}

/* 64 bit integer arithmetic. */
static int ffi_arith_int64(lua_State *L, CTState *cts, FFIArith *fa, MMS mm)
{
  if (ctype_isnum(fa->ct[0]->info) && fa->ct[0]->size <= 8 &&
      ctype_isnum(fa->ct[1]->info) && fa->ct[1]->size <= 8) {
    CTypeID id = (((fa->ct[0]->info & CTF_UNSIGNED) && fa->ct[0]->size == 8) ||
		  ((fa->ct[1]->info & CTF_UNSIGNED) && fa->ct[1]->size == 8)) ?
		 CTID_UINT64 : CTID_INT64;
    CType *ct = ctype_get(cts, id);
    GCcdata *cd;
    uint64_t u0, u1, *up;
    lj_cconv_ct_ct(cts, ct, fa->ct[0], (uint8_t *)&u0, fa->p[0], 0);
    if (mm != MM_unm)
      lj_cconv_ct_ct(cts, ct, fa->ct[1], (uint8_t *)&u1, fa->p[1], 0);
    if ((mm == MM_div || mm == MM_mod)) {
      if (u1 == 0) {  /* Division by zero. */
	if (u0 == 0)
	  setnanV(L->top-1);
	else if (id == CTID_INT64 && (int64_t)u0 < 0)
	  setminfV(L->top-1);
	else
	  setpinfV(L->top-1);
	return 1;
      } else if (id == CTID_INT64 && (int64_t)u1 == -1 &&
		 u0 == U64x(80000000,00000000)) {  /* MIN64 / -1. */
	if (mm == MM_div) id = CTID_UINT64; else u0 = 0;
	mm = MM_unm;  /* Result is 0x8000000000000000ULL or 0LL. */
      }
    }
    cd = lj_cdata_new(cts, id, 8);
    up = (uint64_t *)cdataptr(cd);
    setcdataV(L, L->top-1, cd);
    switch (mm) {
    case MM_add: *up = u0 + u1; break;
    case MM_sub: *up = u0 - u1; break;
    case MM_mul: *up = u0 * u1; break;
    case MM_div:
      if (id == CTID_INT64)
	*up = (uint64_t)((int64_t)u0 / (int64_t)u1);
      else
	*up = u0 / u1;
      break;
    case MM_mod:
      if (id == CTID_INT64)
	*up = (uint64_t)((int64_t)u0 % (int64_t)u1);
      else
	*up = u0 % u1;
      break;
    case MM_pow: *up = lj_cdata_powi64(u0, u1, (id == CTID_UINT64)); break;
    case MM_unm: *up = (uint64_t)-(int64_t)u0; break;
    default: lua_assert(0); break;
    }
    lj_gc_check(L);
    return 1;
  }
  return 0;
}

/* cdata arithmetic. */
static int ffi_arith(lua_State *L)
{
  CTState *cts = ctype_cts(L);
  FFIArith fa;
  MMS mm = (MMS)(curr_func(L)->c.ffid - (int)FF_ffi_meta___add + (int)MM_add);
  ffi_checkarith(L, cts, &fa);
  if (!ffi_arith_int64(L, cts, &fa, mm) &&
      !ffi_arith_ptr(L, cts, &fa, mm)) {
    const char *repr[2];
    int i;
    for (i = 0; i < 2; i++)
      repr[i] = strdata(lj_ctype_repr(L, ctype_typeid(cts, fa.ct[i]), NULL));
    lj_err_callerv(L, LJ_ERR_FFI_BADARITH, repr[0], repr[1]);
  }
  return 1;
}

/* -- C type metamethods -------------------------------------------------- */

#define LJLIB_MODULE_ffi_meta

LJLIB_CF(ffi_meta___index)	LJLIB_REC(cdata_index 0)
{
  CTState *cts = ctype_cts(L);
  CTInfo qual = 0;
  CType *ct;
  uint8_t *p;
  TValue *o = L->base;
  if (!(o+1 < L->top && tviscdata(o)))  /* Also checks for presence of key. */
    lj_err_argt(L, 1, LUA_TCDATA);
  ct = lj_cdata_index(cts, cdataV(o), o+1, &p, &qual);
  if (lj_cdata_get(cts, ct, L->top-1, p))
    lj_gc_check(L);
  return 1;
}

LJLIB_CF(ffi_meta___newindex)	LJLIB_REC(cdata_index 1)
{
  CTState *cts = ctype_cts(L);
  CTInfo qual = 0;
  CType *ct;
  uint8_t *p;
  TValue *o = L->base;
  if (!(o+2 < L->top && tviscdata(o)))  /* Also checks for key and value. */
    lj_err_argt(L, 1, LUA_TCDATA);
  ct = lj_cdata_index(cts, cdataV(o), o+1, &p, &qual);
  lj_cdata_set(cts, ct, p, o+2, qual);
  return 0;
}

/* Forward declaration. */
static int lj_cf_ffi_new(lua_State *L);

LJLIB_CF(ffi_meta___call)	LJLIB_REC(cdata_call)
{
  GCcdata *cd = ffi_checkcdata(L, 1);
  int ret;
  if (cd->typeid == CTID_CTYPEID)
    return lj_cf_ffi_new(L);
  if ((ret = lj_ccall_func(L, cd)) < 0)
    lj_err_callerv(L, LJ_ERR_FFI_BADCALL,
		   strdata(lj_ctype_repr(L, cd->typeid, NULL)));
  return ret;
}

LJLIB_CF(ffi_meta___add)	LJLIB_REC(cdata_arith MM_add)
{
  return ffi_arith(L);
}

LJLIB_CF(ffi_meta___sub)	LJLIB_REC(cdata_arith MM_sub)
{
  return ffi_arith(L);
}

LJLIB_CF(ffi_meta___mul)	LJLIB_REC(cdata_arith MM_mul)
{
  return ffi_arith(L);
}

LJLIB_CF(ffi_meta___div)	LJLIB_REC(cdata_arith MM_div)
{
  return ffi_arith(L);
}

LJLIB_CF(ffi_meta___mod)	LJLIB_REC(cdata_arith MM_mod)
{
  return ffi_arith(L);
}

LJLIB_CF(ffi_meta___pow)	LJLIB_REC(cdata_arith MM_pow)
{
  return ffi_arith(L);
}

LJLIB_CF(ffi_meta___unm)	LJLIB_REC(cdata_arith MM_unm)
{
  return ffi_arith(L);
}

LJLIB_CF(ffi_meta___tostring)
{
  GCcdata *cd = ffi_checkcdata(L, 1);
  const char *msg = "cdata<%s>: %p";
  CTypeID id = cd->typeid;
  if (id == CTID_CTYPEID) {
    msg = "ctype<%s>";
    id = *(CTypeID *)cdataptr(cd);
  } else {
    CType *ct = ctype_raw(ctype_cts(L), id);
    if (ctype_iscomplex(ct->info)) {
      setstrV(L, L->top-1, lj_ctype_repr_complex(L, cdataptr(cd), ct->size));
      goto checkgc;
    } else if (ct->size == 8 && ctype_isinteger(ct->info)) {
      setstrV(L, L->top-1, lj_ctype_repr_int64(L, *(uint64_t *)cdataptr(cd),
					       (ct->info & CTF_UNSIGNED)));
      goto checkgc;
    }
  }
  lj_str_pushf(L, msg, strdata(lj_ctype_repr(L, id, NULL)), cdataptr(cd));
checkgc:
  lj_gc_check(L);
  return 1;
}

#include "lj_libdef.h"

/* -- C library metamethods ----------------------------------------------- */

#define LJLIB_MODULE_ffi_clib

/* Index C library by a name. */
static TValue *ffi_clib_index(lua_State *L)
{
  TValue *o = L->base;
  CLibrary *cl;
  if (!(o < L->top && tvisudata(o) && udataV(o)->udtype == UDTYPE_FFI_CLIB))
    lj_err_argt(L, 1, LUA_TUSERDATA);
  cl = (CLibrary *)uddata(udataV(o));
  if (!(o+1 < L->top && tvisstr(o+1)))
    lj_err_argt(L, 2, LUA_TSTRING);
  return lj_clib_index(L, cl, strV(o+1));
}

LJLIB_CF(ffi_clib___index)
{
  TValue *tv = ffi_clib_index(L);
  if (tviscdata(tv)) {
    CTState *cts = ctype_cts(L);
    GCcdata *cd = cdataV(tv);
    CType *s = ctype_get(cts, cd->typeid);
    if (ctype_isextern(s->info)) {
      CTypeID sid = ctype_cid(s->info);
      void *sp = *(void **)cdataptr(cd);
      if (lj_cconv_tv_ct(cts, ctype_raw(cts, sid), sid, L->top-1, sp))
	lj_gc_check(L);
      return 1;
    }
  }
  copyTV(L, L->top-1, tv);
  return 1;
}

LJLIB_CF(ffi_clib___newindex)
{
  TValue *tv = ffi_clib_index(L);
  TValue *o = L->base+2;
  if (o < L->top && tviscdata(tv)) {
    CTState *cts = ctype_cts(L);
    GCcdata *cd = cdataV(tv);
    CType *d = ctype_get(cts, cd->typeid);
    if (ctype_isextern(d->info)) {
      CTInfo qual = 0;
      for (;;) {  /* Skip attributes and collect qualifiers. */
	d = ctype_child(cts, d);
	if (!ctype_isattrib(d->info)) break;
	if (ctype_attrib(d->info) == CTA_QUAL) qual |= d->size;
      }
      if (!((d->info|qual) & CTF_CONST)) {
	lj_cconv_ct_tv(cts, d, *(void **)cdataptr(cd), o, 0);
	return 0;
      }
    }
  }
  lj_err_caller(L, LJ_ERR_FFI_WRCONST);
  return 0;  /* unreachable */
}

LJLIB_CF(ffi_clib___gc)
{
  TValue *o = L->base;
  if (o < L->top && tvisudata(o) && udataV(o)->udtype == UDTYPE_FFI_CLIB)
    lj_clib_unload((CLibrary *)uddata(udataV(o)));
  return 0;
}

#include "lj_libdef.h"

/* -- FFI library functions ----------------------------------------------- */

#define LJLIB_MODULE_ffi

LJLIB_CF(ffi_cdef)
{
  GCstr *s = lj_lib_checkstr(L, 1);
  CPState cp;
  int errcode;
  cp.L = L;
  cp.cts = ctype_cts(L);
  cp.srcname = strdata(s);
  cp.p = strdata(s);
  cp.mode = CPARSE_MODE_MULTI|CPARSE_MODE_DIRECT;
  errcode = lj_cparse(&cp);
  if (errcode) lj_err_throw(L, errcode);  /* Propagate errors. */
  lj_gc_check(L);
  return 0;
}

LJLIB_CF(ffi_new)	LJLIB_REC(.)
{
  CTState *cts = ctype_cts(L);
  CTypeID id = ffi_checkctype(L, cts);
  CTSize sz;
  CTInfo info = lj_ctype_info(cts, id, &sz);
  TValue *o = L->base+1;
  GCcdata *cd;
  if ((info & CTF_VLA)) {
    o++;
    sz = lj_ctype_vlsize(cts, ctype_raw(cts, id),
			 (CTSize)lj_lib_checkint(L, 2));
  }
  if (sz == CTSIZE_INVALID)
    lj_err_arg(L, 1, LJ_ERR_FFI_INVSIZE);
  if (!(info & CTF_VLA) && ctype_align(info) <= CT_MEMALIGN)
    cd = lj_cdata_new(cts, id, sz);
  else
    cd = lj_cdata_newv(cts, id, sz, ctype_align(info));
  setcdataV(L, o-1, cd);  /* Anchor the uninitialized cdata. */
  lj_cconv_ct_init(cts, ctype_raw(cts, id), sz, cdataptr(cd),
		   o, (MSize)(L->top - o));  /* Initialize cdata. */
  L->top = o;  /* Only return the cdata itself. */
  lj_gc_check(L);
  return 1;
}

LJLIB_CF(ffi_typeof)
{
  CTState *cts = ctype_cts(L);
  CTypeID id = ffi_checkctype(L, cts);
  GCcdata *cd = lj_cdata_new(cts, CTID_CTYPEID, 4);
  *(CTypeID *)cdataptr(cd) = id;
  setcdataV(L, L->top-1, cd);
  lj_gc_check(L);
  return 1;
}

LJLIB_CF(ffi_sizeof)
{
  CTState *cts = ctype_cts(L);
  CTypeID id = ffi_checkctype(L, cts);
  CTSize sz;
  if (LJ_UNLIKELY(tviscdata(L->base) && cdataisv(cdataV(L->base)))) {
    sz = cdatavlen(cdataV(L->base));
  } else {
    CType *ct = lj_ctype_rawref(cts, id);
    if (ctype_isvltype(ct->info))
      sz = lj_ctype_vlsize(cts, ct, (CTSize)lj_lib_checkint(L, 2));
    else
      sz = ctype_hassize(ct->info) ? ct->size : CTSIZE_INVALID;
    if (LJ_UNLIKELY(sz == CTSIZE_INVALID)) {
      setnilV(L->top-1);
      return 1;
    }
  }
  setintV(L->top-1, (int32_t)sz);
  return 1;
}

LJLIB_CF(ffi_alignof)
{
  CTState *cts = ctype_cts(L);
  CTypeID id = ffi_checkctype(L, cts);
  CTSize sz = 0;
  CTInfo info = lj_ctype_info(cts, id, &sz);
  setintV(L->top-1, 1 << ctype_align(info));
  return 1;
}

LJLIB_CF(ffi_offsetof)
{
  CTState *cts = ctype_cts(L);
  CTypeID id = ffi_checkctype(L, cts);
  GCstr *name = lj_lib_checkstr(L, 2);
  CType *ct = lj_ctype_rawref(cts, id);
  CTSize ofs;
  if (ctype_isstruct(ct->info) && ct->size != CTSIZE_INVALID) {
    CType *fct = lj_ctype_getfield(cts, ct, name, &ofs);
    if (fct) {
      setintV(L->top-1, ofs);
      if (ctype_isfield(fct->info)) {
	return 1;
      } else if (ctype_isbitfield(fct->info)) {
	setintV(L->top++, ctype_bitpos(fct->info));
	setintV(L->top++, ctype_bitbsz(fct->info));
	return 3;
      }
    }
  }
  return 0;
}

LJLIB_CF(ffi_cast)
{
  CTState *cts = ctype_cts(L);
  CTypeID id = ffi_checkctype(L, cts);
  TValue *o = lj_lib_checkany(L, 2);
  L->top = o+1;  /* Make sure this is the last item on the stack. */
  if (!(tviscdata(o) && cdataV(o)->typeid == id)) {
    CTSize sz = lj_ctype_size(cts, id);
    GCcdata *cd;
    if (sz == CTSIZE_INVALID)
      lj_err_caller(L, LJ_ERR_FFI_INVSIZE);
    cd = lj_cdata_new(cts, id, sz);  /* Create destination cdata. */
    lj_cconv_ct_tv(cts, ctype_raw(cts, id), cdataptr(cd), o, CCF_CAST);
    setcdataV(L, o, cd);
    lj_gc_check(L);
  }
  return 1;
}

LJLIB_CF(ffi_string)
{
  CTState *cts = ctype_cts(L);
  TValue *o = lj_lib_checkany(L, 1);
  size_t sz = (size_t)(CTSize)lj_lib_optint(L, 2, (int32_t)CTSIZE_INVALID);
  CType *ct = ctype_get(cts, sz==CTSIZE_INVALID ? CTID_P_CVOID : CTID_P_CCHAR);
  const char *p;
  L->top = o+1;  /* Make sure this is the last item on the stack. */
  lj_cconv_ct_tv(cts, ct, (uint8_t *)&p, o, 0);
  if (sz == CTSIZE_INVALID) sz = strlen(p);
  setstrV(L, o, lj_str_new(L, p, sz));
  lj_gc_check(L);
  return 1;
}

LJLIB_CF(ffi_copy)
{
  void *dp = ffi_checkptr(L, 1, CTID_P_VOID);
  void *sp = ffi_checkptr(L, 2, CTID_P_CVOID);
  TValue *o = L->base+1;
  CTSize sz;
  if (tvisstr(o) && o+1 >= L->top) {
    sz = strV(o)->len+1;  /* Copy Lua string including trailing '\0'. */
  } else {
    sz = (CTSize)lj_lib_checkint(L, 3);
    if (tvisstr(o) && sz > strV(o)->len+1)
      sz = strV(o)->len+1;  /* Max. copy length is string length. */
  }
  memcpy(dp, sp, sz);
  return 0;
}

LJLIB_CF(ffi_fill)
{
  void *dp = ffi_checkptr(L, 1, CTID_P_VOID);
  CTSize sz = (CTSize)lj_lib_checkint(L, 2);
  int32_t fill = lj_lib_optint(L, 3, 0);
  memset(dp, fill, sz);
  return 0;
}

#define H_(le, be)	LJ_ENDIAN_SELECT(0x##le, 0x##be)

/* Test ABI string. */
LJLIB_CF(ffi_abi)
{
  GCstr *s = lj_lib_checkstr(L, 1);
  int b = 0;
  switch (s->hash) {
#if LJ_64
  case H_(849858eb,ad35fd06): b = 1; break;  /* 64bit */
#else
  case H_(662d3c79,d0e22477): b = 1; break;  /* 32bit */
#endif
#if LJ_ARCH_HASFPU
  case H_(e33ee463,e33ee463): b = 1; break;  /* fpu */
#endif
#if LJ_ABI_SOFTFP
  case H_(61211a23,c2e8c81c): b = 1; break;  /* softfp */
#else
  case H_(539417a8,8ce0812f): b = 1; break;  /* hardfp */
#endif
#if LJ_ABI_EABI
  case H_(2182df8f,f2ed1152): b = 1; break;  /* eabi */
#endif
#if LJ_ABI_WIN
  case H_(4ab624a8,4ab624a8): b = 1; break;  /* win */
#endif
  case H_(3af93066,1f001464): b = 1; break;  /* le/be */
  default:
    break;
  }
  setboolV(L->top-1, b);
  return 1;
}

#undef H_

LJLIB_PUSH(top-5) LJLIB_SET(!)  /* Store clib metatable in func environment. */

LJLIB_CF(ffi_load)
{
  GCstr *name = lj_lib_checkstr(L, 1);
  int global = (L->base+1 < L->top && tvistruecond(L->base+1));
  lj_clib_load(L, tabref(curr_func(L)->c.env), name, global);
  return 1;
}

LJLIB_PUSH(top-4) LJLIB_SET(C)
LJLIB_PUSH(top-3) LJLIB_SET(os)
LJLIB_PUSH(top-2) LJLIB_SET(arch)

#include "lj_libdef.h"

/* ------------------------------------------------------------------------ */

LUALIB_API int luaopen_ffi(lua_State *L)
{
  lj_ctype_init(L);
  LJ_LIB_REG_(L, NULL, ffi_meta);
  /* NOBARRIER: basemt is a GC root. */
  setgcref(basemt_it(G(L), LJ_TCDATA), obj2gco(tabV(L->top-1)));
  LJ_LIB_REG_(L, NULL, ffi_clib);
  lj_clib_default(L, tabV(L->top-1));  /* Create ffi.C default namespace. */
  lua_pushliteral(L, LJ_OS_NAME);
  lua_pushliteral(L, LJ_ARCH_NAME);
  LJ_LIB_REG_(L, NULL, ffi);  /* Note: no global "ffi" created! */
  return 1;
}

#endif
