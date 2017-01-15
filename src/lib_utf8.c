/*
** utf8 library.
** Copyright (C) 2019 moonjit developers. https://github.com/moonjit/moonjit
*/

#define LUA_LIB

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "lj_arch.h"
#include "utf8.h"

/* ------------------------------------------------------------------------ */

LUALIB_API int luaopen_utf8(lua_State *L)
{
  luaL_loadbufferx(L, (const char *)luaJIT_BC_utf8, luaJIT_BC_utf8_SIZE, "utf8", "b");
  lua_pcall(L, 0, 1, lua_gettop(L));
  lua_pushvalue(L, -1);
  lua_setglobal(L, LUA_UTF8LIBNAME);
  return 1;
}

