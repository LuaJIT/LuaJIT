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

/* Note: this uses interim structs until the SBuf reorg. */

LJLIB_CF(buffer_encode)
{
  cTValue *o = lj_lib_checkany(L, 1);
  StrBuf sbuf;
  sbuf.sb = lj_buf_tmp_(L);
  lj_serialize_put(&sbuf, o);
  setstrV(L, L->top++, lj_buf_str(L, sbuf.sb));
  lj_gc_check(L);
  return 1;
}

LJLIB_CF(buffer_decode)
{
  GCstr *str = lj_lib_checkstr(L, 1);
  char *p = (char *)strdata(str);
  SBuf sb;
  StrBuf sbuf;
  setsbufL(&sb, L);
  sb.b = p;
  sb.w = sb.e = p + str->len;
  sbuf.sb = &sb;
  sbuf.r = p;
  setnilV(L->top++);
  lj_serialize_get(&sbuf, L->top-1);
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
