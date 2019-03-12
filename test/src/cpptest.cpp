
#include <stdio.h>

extern "C" {
#define LUA_LIB
#include "lua.h"
#include "lauxlib.h"
#include "luajit.h"
}

static int testobj_alloc;

class TestObj {
public:
  TestObj(int x) { foo = x; testobj_alloc = 1; }
  ~TestObj() { testobj_alloc = 0; }
private:
  int foo;
};

static int ct_alloc(lua_State *L)
{
  TestObj foo(1);
  lua_pushlightuserdata(L, (void *)&foo);
  lua_call(L, lua_gettop(L)-1, LUA_MULTRET);
  if (lua_iscfunction(L, -1)) {
    lua_CFunction f = lua_tocfunction(L, -1);
    lua_pop(L, 1);
    f(L);
  }
  return lua_gettop(L);
}

static int ct_isalloc(lua_State *L)
{
  lua_pushboolean(L, testobj_alloc);
  return 1;
}

static int ct_usereg(lua_State *L)
{
  int n = luaL_checkint(L, 1);
  int m = luaL_checkint(L, 2);
  int i;
  int a = 0, b = 0, c = 0, d = 0, e = 0, f = 0;
  for (i = 0; i < n; i++) {
    a = (a + 1) ^ 0x12345678;
    b = (b + 2) ^ 0x12345678;
    c = (c + 3) ^ 0x12345678;
    d = (d + 4) ^ 0x12345678;
    e = (e + 5) ^ 0x12345678;
    f = (f + 5) ^ 0x12345678;
    if (i == m) {
      if (i & 1)
	lua_pcall(L, 1, 0, 0);
      else
	lua_call(L, 1, 0);
    }
  }
  lua_pushinteger(L, a);
  lua_pushinteger(L, b);
  lua_pushinteger(L, c);
  lua_pushinteger(L, d);
  lua_pushinteger(L, e);
  lua_pushinteger(L, f);
  return 6;
}

static int ct_catch(lua_State *L)
{
  try {
    lua_call(L, lua_gettop(L)-1, LUA_MULTRET);
    return lua_gettop(L);
  } catch (const char *s) {
    lua_pushstring(L, s);
  } catch (...) {
    lua_pushliteral(L, "catch ...");
  }
  return 1;
}

static int ct_throw(lua_State *L)
{
  const char *s = lua_tostring(L, 1);
  throw(s);
  return 0;
}

static int ct_wrap(lua_State *L, lua_CFunction f)
{
  try {
    return f(L);
  } catch (const char *s) {
    lua_pushstring(L, s);
  }
  return lua_error(L);
}

static int ct_wrapon(lua_State *L)
{
  lua_pushlightuserdata(L, (void *)ct_wrap);
  luaJIT_setmode(L, -1, LUAJIT_MODE_WRAPCFUNC|LUAJIT_MODE_ON);
  return 0;
}

static int ct_wrapoff(lua_State *L)
{
  luaJIT_setmode(L, 0, LUAJIT_MODE_WRAPCFUNC|LUAJIT_MODE_OFF);
  return 0;
}

static luaL_Reg ct_funcs[] = {
  {"isalloc",	ct_isalloc },
  {"alloc",	ct_alloc },
  {"usereg",	ct_usereg },
  {"catch",	ct_catch },
  {"throw",	ct_throw },
  {"wrapon",	ct_wrapon },
  {"wrapoff",	ct_wrapoff },
  {NULL, NULL}
};

extern "C" {
LUA_API int luaopen_cpptest(lua_State *L)
{
  luaL_register(L, "cpptest", ct_funcs);
  return 1;
}
}
