local ffi = require("ffi")

ffi.cdef[[
// libc/libm
int sprintf(char *buf, const char *fmt, ...);
double pow(double x, double y);
int rmdir(const char *name);
int errno;

// Windows
unsigned int GetSystemDirectoryA(char *buf, unsigned int sz);
char *CharUpperA(char *str);
int GdiFlush(void);
int _rmdir(const char *name);
static const int _O_TEXT = 0x4000;
static const int _O_BINARY = 0x8000;
int *_errno(void);
int _fmode;

// Lua/C API
typedef struct lua_State lua_State;
typedef double lua_Number;
lua_State *luaL_newstate(void);
void luaL_openlibs(lua_State *L);
void lua_close(lua_State *L);
int luaL_loadstring(lua_State *L, const char *s);
int lua_pcall(lua_State *L, int nargs, int nresults, int errfunc);
lua_Number lua_tonumber(lua_State *L, int idx);
]]

local C = ffi.C

do
  local buf = ffi.new("char[?]", 100)
  local n = C.sprintf(buf, "test %g %s", 12.5, "foo")
  assert(ffi.string(buf, n) == "test 12.5 foo")
end

assert(ffi.C.pow(2.5, 5) == 97.65625)

if ffi.abi("win") then
  do
    local buf = ffi.new("char[?]", 4, "abc")
    C.CharUpperA(buf)
    assert(ffi.string(buf) == "ABC")
  end

  do
    local buf = ffi.new("char[?]", 256)
    local len = C.GetSystemDirectoryA(buf, 255)
    local s = ffi.string(buf, len)
    assert(string.find(string.lower(s), "\\system32"))
  end

  assert(C.GdiFlush() == 1)

  assert(ffi.C._rmdir("/tmp/does_not_exist") == -1)
  assert(ffi.C._errno()[0] == 2)

  ffi.C._fmode = ffi.C._O_BINARY
  assert(ffi.C._fmode == ffi.C._O_BINARY)
  ffi.C._fmode = ffi.C._O_TEXT
else
  assert(ffi.C.rmdir("/tmp/does_not_exist") == -1)
  assert(ffi.C.errno == 2)

  ffi.C.errno = 17
  assert(ffi.C.errno == 17)
  ffi.C.errno = 0
end

do
  local L = C.luaL_newstate()
  local s = "local x = 0; for i=1,100 do x=x+i end; return x"
  C.luaL_openlibs(L)
  assert(C.luaL_loadstring(L, s) == 0)
  assert(C.lua_pcall(L, 0, 1, 0) == 0)
  assert(C.lua_tonumber(L, -1) == 5050)
  C.lua_close(L)
end

do
  if not (ffi.os == "Windows" or ffi.os == "Other") then
    ffi.load("pthread")
  end
end

