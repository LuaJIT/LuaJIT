local ffi = require("ffi")

dofile("../common/ffi_util.inc")

ffi.cdef[[
typedef struct { int a,b,c; } foo1_t;
void free(void *);
void *malloc(size_t);
struct incomplete;
]]

do
  local a = ffi.new("int[10]")
  local p1 = a+0
  p1[0] = 1;
  p1[1] = 2;
  assert(a[0] == 1)
  assert(a[1] == 2)
  assert(a == p1)
  assert(not (a ~= p1))
  assert(p1 <= a)
  assert(a <= p1)
  assert(not (p1 < a))
  assert(not (a < p1))
  assert(a ~= nil)
  assert(not (a == nil))
  assert(p1 ~= nil)
  assert(not (p1 == nil))

  local p2 = a+2
  p2[0] = 3;
  p2[1] = 4;
  assert(a[2] == 3)
  assert(a[3] == 4)
  assert(p2 - p1 == 2)
  assert(p1 - p2 == -2)
  assert(p1 ~= p2)
  assert(not (p1 == p2))
  assert(p1 < p2)
  assert(p2 > p1)
  assert(not (p1 > p2))
  assert(not (p2 < p1))
  assert(p1 <= p2)
  assert(p2 >= p1)
  assert(not (p1 >= p2))
  assert(not (p2 <= p1))

  local p3 = a-2
  assert(p3[2] == 1)
  assert(p3[3] == 2)
  local p4 = a+(-3)
  assert(p4[5] == 3)
  assert(p4[6] == 4)
  -- bad: adding two pointers or subtracting a pointer
  fails(function(p1, p2) return p1 + p2 end, p1, p2)
  fails(function(p1) return 1 - p1 end, p1)
  -- bad: subtracting different pointer types
  fails(function(p1) return p1 - ffi.new("char[1]") end, p1)
  -- but different qualifiers are ok
  local b = ffi.cast("const int *", a+5)
  assert(b - a == 5)
end

do
  local p1 = ffi.cast("void *", 0)
  local p2 = ffi.cast("int *", 1)
  assert(p1 == p1)
  assert(p2 == p2)
  assert(p1 ~= p2)
  assert(p1 == nil)
  assert(p2 ~= nil)
end

do
  local f1 = ffi.C.free
  local f2 = ffi.C.malloc
  local p1 = ffi.cast("void *", f1)
  assert(f1 == f1)
  assert(f1 ~= nil)
  assert(f1 ~= f2)
  assert(p1 == f1)
  assert(p1 ~= f2)
  assert(f1 < f2 or f1 > f2)
  fails(function(f1) return f1 + 1 end, f1)
end

do
  local s = ffi.new("foo1_t[10]")
  local p1 = s+3
  p1.a = 1; p1.b = 2; p1.c = 3
  p1[1].a = 4; p1[1].b = 5; p1[1].c = 6
  assert(s[3].a == 1 and s[3].b == 2 and s[3].c == 3)
  assert(s[4].a == 4 and s[4].b == 5 and s[4].c == 6)
  local p2 = s+6
  assert(p2 - p1 == 3)
  assert(p1 - p2 == -3)
end

do
  local mem = ffi.new("int[1]")
  local p = ffi.cast("struct incomplete *", mem)
  fails(function(p) return p+1 end, p)
  local ok, err = pcall(function(p) return p[1] end, p)
  assert(not ok and err:match("size.*unknown"))
end

