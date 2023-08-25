/*
** Library initialization.
** Copyright (C) 2005-2023 Mike Pall. See Copyright Notice in luajit.h
**
** Major parts taken verbatim from the Lua interpreter.
** Copyright (C) 1994-2008 Lua.org, PUC-Rio. See Copyright Notice in lua.h
*/

#define lib_init_c
#define LUA_LIB

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "lj_arch.h"

#include <stdlib.h>

static const luaL_Reg lj_lib_load[] = {
  { "",			luaopen_base },
  { LUA_LOADLIBNAME,	luaopen_package },
  { LUA_TABLIBNAME,	luaopen_table },
  { LUA_IOLIBNAME,	luaopen_io },
  { LUA_OSLIBNAME,	luaopen_os },
  { LUA_STRLIBNAME,	luaopen_string },
  { LUA_MATHLIBNAME,	luaopen_math },
  { LUA_DBLIBNAME,	luaopen_debug },
  { LUA_BITLIBNAME,	luaopen_bit },
  { LUA_JITLIBNAME,	luaopen_jit },
  { NULL,		NULL }
};

static const luaL_Reg lj_lib_preload[] = {
#if LJ_HASFFI
  { LUA_FFILIBNAME,	luaopen_ffi },
#endif
  { NULL,		NULL }
};

LUALIB_API void luaL_openlibs(lua_State *L)
{
  const luaL_Reg *lib;
  for (lib = lj_lib_load; lib->func; lib++) {
    lua_pushcfunction(L, lib->func);
    lua_pushstring(L, lib->name);
    lua_call(L, 1, 0);
  }
  luaL_findtable(L, LUA_REGISTRYINDEX, "_PRELOAD",
		 sizeof(lj_lib_preload)/sizeof(lj_lib_preload[0])-1);
  for (lib = lj_lib_preload; lib->func; lib++) {
    lua_pushcfunction(L, lib->func);
    lua_setfield(L, -2, lib->name);
  }
  lua_pop(L, 1);
#ifdef LJ_DS
#ifdef LJ_DS_STRING_DUMP_FIX
  const char* dump_fix = 
"local util = require 'jit.util'\n"
"local std_fns = {}\n"
"for name, mod in pairs(package.loaded) do\n"
"    if type(mod) == 'table' then\n"
"        for fn_name, fn in pairs(mod) do\n"
"            if type(fn) == 'function' then\n"
"                if pcall(util.funck, fn, 0) then\n"
"                    std_fns[fn] = true\n"
"                end\n"
"            end\n"
"        end\n"
"    end\n"
"end\n"
"local dump = string.dump\n"
"string.dump = function (f, strip)\n"
"    if std_fns[f] then\n"
"        error('unable to dump given function', 2)\n"
"    end\n"
"    return dump(f, strip)\n"
"end\n";
  luaL_dostring(L, dump_fix);
#endif
#ifdef DO_LUA_INIT
  void handle_luainit(lua_State *L);
  handle_luainit(L);
#endif
#endif
}

static void handle_luainit(lua_State *L)
{
  const char *init = getenv(LUA_INIT);
  if (init == NULL)
    return;
  if (init[0] == '@')
    luaL_dofile(L, init+1);
  else
    luaL_dostring(L, init);
}


