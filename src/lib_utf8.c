/*
** utf8 library.
** Copyright (C) 2005-2016 Mike Pall. See Copyright Notice in luajit.h
**
** Contributed by Francois Perrad.
*/

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

/* ------------------------------------------------------------------------ */

LUALIB_API int luaopen_utf8(lua_State *L)
{
  lua_getglobal(L, "require");
  lua_pushliteral(L, "utf8");
  lua_call(L, 1, 1);
  lua_pushvalue(L, -1);
  lua_setglobal(L, LUA_UTF8LIBNAME);
  return 1;
}

