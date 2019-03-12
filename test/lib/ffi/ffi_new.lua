local ffi = require("ffi")
local bit = require("bit")

dofile("../common/ffi_util.inc")

ffi.cdef([[
typedef struct { int a,b,c; } foo1_t;
typedef int foo2_t[?];
void *malloc(size_t size);
void free(void *ptr);
]])

do
  assert(ffi.sizeof("foo1_t") == 12)
  local cd = ffi.new("foo1_t")
  assert(ffi.sizeof(cd) == 12)
  local foo1_t = ffi.typeof("foo1_t")
  assert(ffi.sizeof(foo1_t) == 12)
  cd = foo1_t()
  assert(ffi.sizeof(cd) == 12)
end

do
  assert(ffi.sizeof("foo2_t", 3) == 12)
  local cd = ffi.new("foo2_t", 3)
  assert(ffi.sizeof(cd) == 12)
  local foo2_t = ffi.typeof("foo2_t")
  fails(ffi.sizeof, foo2_t)
  assert(ffi.sizeof(foo2_t, 3) == 12)
  cd = foo2_t(3)
  assert(ffi.sizeof(cd) == 12)
end

do
  local tpi = ffi.typeof("int")
  local tpb = ffi.typeof("uint8_t")
  local t = {}
  for i=1,200 do t[i] = tpi end
  t[100] = tpb
  local x = 0
  for i=1,200 do x = x + tonumber(ffi.new(t[i], 257)) end
  assert(x == 199*257 + 1)
end

do
  local oc = collectgarbage("count")
  for al=0,15 do
    local align = 2^al -- 1, 2, 4, ..., 32768
    local ct = ffi.typeof("struct { char __attribute__((aligned("..align.."))) a; }")
    for i=1,100 do
      local cd = ct()
      local addr = tonumber(ffi.cast("intptr_t", ffi.cast("void *", cd)))
      assert(bit.band(addr, align-1) == 0)
    end
  end
  local nc = collectgarbage("count")
  assert(nc < oc + 3000, "GC step missing for ffi.new")
end

do
  local t = {}
  for i=1,100 do t[i] = ffi.new("int[?]", i) end
  assert(ffi.sizeof(t[100]) == 400)
  for i=0,99 do assert(t[100][i] == 0) end
end

do
  local t = {}
  local ct = ffi.typeof("struct { double x; int y[?];}")
  for i=1,100 do t[i] = ct(i) end
  assert(ffi.sizeof(t[100]) == 408)
  for i=0,99 do assert(t[100].y[i] == 0) end
end

do
  local ct = ffi.typeof("struct __attribute__((aligned(16))) { int x; }")
  local y
  for i=1,200 do
    local x = ct()
    if i == 150 then y = x end
  end
  assert(bit.band(ffi.cast("intptr_t", ffi.cast("void *", y)), 15) == 0)
end

do
  local q
  local p = ffi.gc(ffi.new("int[1]"), function(x) q = x end)
  p = nil
  collectgarbage()
  assert(type(q) == "cdata")
  q = nil
  collectgarbage()
  assert(q == nil)
end

do
  local p = ffi.gc(ffi.C.malloc(2^20), ffi.C.free)
  p = nil
  collectgarbage()
end

do
  local p = ffi.gc(ffi.new("int[1]"), function(x) assert(type(x) == "cdata") end)
  -- test for lua_close() cleanup.
end

