/*
** Buffer library.
** Copyright (C) 2005-2021 Mike Pall. See Copyright Notice in luajit.h
*/

#define lib_buffer_c
#define LUA_LIB

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "lj_obj.h"

#if LJ_HASBUFFER
#include "lj_gc.h"
#include "lj_err.h"
#include "lj_buf.h"
#include "lj_str.h"
#include "lj_tab.h"
#include "lj_udata.h"
#include "lj_meta.h"
#if LJ_HASFFI
#include "lj_ctype.h"
#include "lj_cdata.h"
#include "lj_cconv.h"
#endif
#include "lj_strfmt.h"
#include "lj_serialize.h"
#include "lj_lib.h"

/* ------------------------------------------------------------------------ */

#define LJLIB_MODULE_buffer_method

/* Check that the first argument is a string buffer. */
static SBufExt *buffer_tobuf(lua_State *L)
{
  if (!(L->base < L->top && tvisbuf(L->base)))
    lj_err_argtype(L, 1, "buffer");
  return bufV(L->base);
}

/* Ditto, but for writers. */
static LJ_AINLINE SBufExt *buffer_tobufw(lua_State *L)
{
  SBufExt *sbx = buffer_tobuf(L);
  setsbufXL_(sbx, L);
  return sbx;
}

LJLIB_CF(buffer_method_free)
{
  SBufExt *sbx = buffer_tobuf(L);
  lj_bufx_free(G(L), sbx);
  lj_bufx_init(L, sbx);
  L->top = L->base+1;  /* Chain buffer object. */
  return 1;
}

LJLIB_CF(buffer_method_reset)
{
  SBufExt *sbx = buffer_tobuf(L);
  lj_bufx_reset(sbx);
  L->top = L->base+1;  /* Chain buffer object. */
  return 1;
}

LJLIB_CF(buffer_method_skip)
{
  SBufExt *sbx = buffer_tobuf(L);
  MSize n = (MSize)lj_lib_checkintrange(L, 2, 0, LJ_MAX_BUF);
  MSize len = sbufxlen(sbx);
  if (n < len) {
    sbx->r += n;
  } else {
    sbx->r = sbx->w = sbx->b;
  }
  L->top = L->base+1;  /* Chain buffer object. */
  return 1;
}

LJLIB_CF(buffer_method_set)
{
  SBufExt *sbx = buffer_tobuf(L);
  const char *p;
  MSize len;
#if LJ_HASFFI
  if (tviscdata(L->base+1)) {
    CTState *cts = ctype_cts(L);
    lj_cconv_ct_tv(cts, ctype_get(cts, CTID_P_CVOID), (uint8_t *)&p,
		   L->base+1, CCF_ARG(2));
    len = (MSize)lj_lib_checkintrange(L, 3, 0, LJ_MAX_BUF);
  } else
#endif
  {
    GCstr *str = lj_lib_checkstrx(L, 2);
    p = strdata(str);
    len = str->len;
  }
  lj_bufx_free(G(L), sbx);
  lj_bufx_init_cow(L, sbx, p, len);
  setgcref(sbx->cowref, gcV(L->base+1));
  L->top = L->base+1;  /* Chain buffer object. */
  return 1;
}

LJLIB_CF(buffer_method_put)
{
  SBufExt *sbx = buffer_tobufw(L);
  ptrdiff_t arg, narg = L->top - L->base;
  for (arg = 1; arg < narg; arg++) {
    cTValue *o = &L->base[arg], *mo = NULL;
  retry:
    if (tvisstr(o)) {
      lj_buf_putstr((SBuf *)sbx, strV(o));
    } else if (tvisint(o)) {
      lj_strfmt_putint((SBuf *)sbx, intV(o));
    } else if (tvisnum(o)) {
      lj_strfmt_putfnum((SBuf *)sbx, STRFMT_G14, numV(o));
    } else if (tvisbuf(o)) {
      SBufExt *sbx2 = bufV(o);
      lj_buf_putmem((SBuf *)sbx, sbx2->r, sbufxlen(sbx2));
    } else if (!mo && !tvisnil(mo = lj_meta_lookup(L, o, MM_tostring))) {
      /* Call __tostring metamethod inline. */
      copyTV(L, L->top++, mo);
      copyTV(L, L->top++, o);
      lua_call(L, 1, 1);
      o = &L->base[arg];  /* The stack may have been reallocated. */
      copyTV(L, &L->base[arg], L->top-1);
      L->top = L->base + narg;
      goto retry;  /* Retry with the result. */
    } else {
      lj_err_argtype(L, arg+1, "string/number/__tostring");
    }
    /* Probably not useful to inline other __tostring MMs, e.g. FFI numbers. */
  }
  L->top = L->base+1;  /* Chain buffer object. */
  lj_gc_check(L);
  return 1;
}

LJLIB_CF(buffer_method_putf)
{
  SBufExt *sbx = buffer_tobufw(L);
  lj_strfmt_putarg(L, (SBuf *)sbx, 2, 2);
  L->top = L->base+1;  /* Chain buffer object. */
  lj_gc_check(L);
  return 1;
}

LJLIB_CF(buffer_method_get)
{
  SBufExt *sbx = buffer_tobuf(L);
  ptrdiff_t arg, narg = L->top - L->base;
  if (narg == 1) {
    narg++;
    setnilV(L->top++);  /* get() is the same as get(nil). */
  }
  for (arg = 1; arg < narg; arg++) {
    TValue *o = &L->base[arg];
    MSize n = tvisnil(o) ? LJ_MAX_BUF :
	      (MSize) lj_lib_checkintrange(L, arg+1, 0, LJ_MAX_BUF);
    MSize len = sbufxlen(sbx);
    if (n > len) n = len;
    setstrV(L, o, lj_str_new(L, sbx->r, n));
    sbx->r += n;
  }
  if (sbx->r == sbx->w) sbx->r = sbx->w = sbx->b;
  lj_gc_check(L);
  return narg-1;
}

#if LJ_HASFFI
LJLIB_CF(buffer_method_putcdata)
{
  SBufExt *sbx = buffer_tobufw(L);
  const char *p;
  MSize len;
  if (tviscdata(L->base+1)) {
    CTState *cts = ctype_cts(L);
    lj_cconv_ct_tv(cts, ctype_get(cts, CTID_P_CVOID), (uint8_t *)&p,
		   L->base+1, CCF_ARG(2));
  } else {
    lj_err_argtype(L, 2, "cdata");
  }
  len = (MSize)lj_lib_checkintrange(L, 3, 0, LJ_MAX_BUF);
  lj_buf_putmem((SBuf *)sbx, p, len);
  L->top = L->base+1;  /* Chain buffer object. */
  return 1;
}

LJLIB_CF(buffer_method_reserve)
{
  SBufExt *sbx = buffer_tobufw(L);
  MSize len = (MSize)lj_lib_checkintrange(L, 2, 0, LJ_MAX_BUF);
  GCcdata *cd;
  lj_buf_more((SBuf *)sbx, len);
  ctype_loadffi(L);
  cd = lj_cdata_new_(L, CTID_P_UINT8, CTSIZE_PTR);
  *(void **)cdataptr(cd) = sbx->w;
  setcdataV(L, L->top++, cd);
  setintV(L->top++, sbufleft(sbx));
  return 2;
}

LJLIB_CF(buffer_method_commit)
{
  SBufExt *sbx = buffer_tobuf(L);
  MSize len = (MSize)lj_lib_checkintrange(L, 2, 0, LJ_MAX_BUF);
  if (len > sbufleft(sbx)) lj_err_arg(L, 2, LJ_ERR_NUMRNG);
  sbx->w += len;
  L->top = L->base+1;  /* Chain buffer object. */
  return 1;
}

LJLIB_CF(buffer_method_ref)
{
  SBufExt *sbx = buffer_tobuf(L);
  GCcdata *cd;
  ctype_loadffi(L);
  cd = lj_cdata_new_(L, CTID_P_UINT8, CTSIZE_PTR);
  *(void **)cdataptr(cd) = sbx->r;
  setcdataV(L, L->top++, cd);
  setintV(L->top++, sbufxlen(sbx));
  return 2;
}
#endif

LJLIB_CF(buffer_method_encode)
{
  SBufExt *sbx = buffer_tobufw(L);
  cTValue *o = lj_lib_checkany(L, 2);
  lj_serialize_put(sbx, o);
  lj_gc_check(L);
  L->top = L->base+1;  /* Chain buffer object. */
  return 1;
}

LJLIB_CF(buffer_method_decode)
{
  SBufExt *sbx = buffer_tobufw(L);
  setnilV(L->top++);
  lj_serialize_get(sbx, L->top-1);
  lj_gc_check(L);
  return 1;
}

LJLIB_CF(buffer_method___gc)
{
  SBufExt *sbx = buffer_tobuf(L);
  lj_bufx_free(G(L), sbx);
  lj_bufx_init(L, sbx);
  return 0;
}

LJLIB_CF(buffer_method___tostring)
{
  SBufExt *sbx = buffer_tobuf(L);
  setstrV(L, L->top-1, lj_str_new(L, sbx->r, sbufxlen(sbx)));
  lj_gc_check(L);
  return 1;
}

LJLIB_CF(buffer_method___len)
{
  SBufExt *sbx = buffer_tobuf(L);
  setintV(L->top-1, (int32_t)sbufxlen(sbx));
  return 1;
}

LJLIB_PUSH("buffer") LJLIB_SET(__metatable)
LJLIB_PUSH(top-1) LJLIB_SET(__index)

/* ------------------------------------------------------------------------ */

#define LJLIB_MODULE_buffer

LJLIB_PUSH(top-2) LJLIB_SET(!)  /* Set environment. */

LJLIB_CF(buffer_new)
{
  MSize sz = L->base == L->top ? 0u :
	     (MSize)lj_lib_checkintrange(L, 1, 0, LJ_MAX_BUF);
  GCtab *env = tabref(curr_func(L)->c.env);
  GCudata *ud = lj_udata_new(L, sizeof(SBufExt), env);
  SBufExt *sbx = (SBufExt *)uddata(ud);
  ud->udtype = UDTYPE_BUFFER;
  /* NOBARRIER: The GCudata is new (marked white). */
  setgcref(ud->metatable, obj2gco(env));
  setudataV(L, L->top++, ud);
  lj_bufx_init(L, sbx);
  if (sz > 0) lj_buf_need2((SBuf *)sbx, sz);
  return 1;
}

LJLIB_CF(buffer_encode)
{
  cTValue *o = lj_lib_checkany(L, 1);
  SBufExt sbx;
  lj_bufx_init_borrow(L, &sbx, &G(L)->tmpbuf);
  lj_serialize_put(&sbx, o);
  setstrV(L, L->top++, lj_buf_str(L, (SBuf *)&sbx));
  lj_gc_check(L);
  return 1;
}

LJLIB_CF(buffer_decode)
{
  GCstr *str = lj_lib_checkstrx(L, 1);
  SBufExt sbx;
  lj_bufx_init_cow(L, &sbx, strdata(str), str->len);
  /* No need to set sbx.cowref here. */
  setnilV(L->top++);
  lj_serialize_get(&sbx, L->top-1);
  lj_gc_check(L);
  if (sbx.r != sbx.w) lj_err_caller(L, LJ_ERR_BUFFER_LEFTOV);
  return 1;
}

/* ------------------------------------------------------------------------ */

#include "lj_libdef.h"

int luaopen_string_buffer(lua_State *L)
{
  LJ_LIB_REG(L, NULL, buffer_method);
  lua_getfield(L, -1, "__tostring");
  lua_setfield(L, -2, "tostring");
  LJ_LIB_REG(L, NULL, buffer);
  return 1;
}

#endif
