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
#include "lj_buf.h"
#include "lj_serialize.h"
#include "lj_lib.h"

/* ------------------------------------------------------------------------ */

#define LJLIB_MODULE_buffer

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
  GCstr *str = lj_lib_checkstr(L, 1);
  SBufExt sbx;
  lj_bufx_init_cow(L, &sbx, strdata(str), str->len);
  /* No need to set sbx.cowref here. */
  setnilV(L->top++);
  lj_serialize_get(&sbx, L->top-1);
  lj_gc_check(L);
  return 1;
}

/* ------------------------------------------------------------------------ */

#include "lj_libdef.h"

int luaopen_string_buffer(lua_State *L)
{
  LJ_LIB_REG(L, NULL, buffer);
  return 1;
}

#endif
