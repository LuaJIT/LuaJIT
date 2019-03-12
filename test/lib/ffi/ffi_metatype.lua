local ffi = require("ffi")

dofile("../common/ffi_util.inc")

ffi.cdef[[
typedef struct { int x; } idx1_t;
typedef struct { int x; } idx2_t;
typedef struct { int x; } idx3_t;
typedef struct { int x,y; } arith_t;
typedef struct { void *p; } gc_t;
]]

local function ptreq(a, b)
  return ffi.cast("void *", a) == ffi.cast("void *", b)
end

do
  local nidx = {}
  local tp = ffi.metatype("idx1_t", {
    __index = { foo = 99, method = function(c, v) return v end },
    __newindex = nidx,
  })

  fails(function() ffi.metatype("idx1_t", {}) end)

  local s = tp(1234)
  assert(s.foo == 99)
  assert(s.x == 1234)
  -- bad field in __index metatable
  fails(function(s) local x = s.bar end, s)
  assert(s:method(123) == 123)
  s.bar = 42
  assert(nidx.bar == 42)

  local cs = ffi.new("const idx1_t", 9876)
  assert(cs.foo == 99)
  assert(cs.x == 9876)
  -- write to const struct
  fails(function(cs) cs.bar = 42 end, cs)

  local cp = ffi.new("const idx1_t *", cs)
  assert(cp.foo == 99)
  assert(cp.x == 9876)
  -- write to const struct pointer
  fails(function(cp) cp.bar = 42 end, cp)
end

do
  local uc, uk, uv
  local tp = ffi.metatype("idx2_t", {
    __index = function(c, k, x, y)
       assert(x == nil and y == nil)
       uc, uk = c, k; return 99
    end,
    __newindex = function(c, k, v) uc, uk, uv = c, k, v end,
  })

  local s = tp(1234)
  assert(s.foo == 99)
  assert(ptreq(uc, s) and uk == "foo" and uv == nil); uc,uk,uv=nil,nil,nil
  assert(s.x == 1234)
  assert(uc == nil and uk == nil and uv == nil); uc,uk,uv=nil,nil,nil

  s.bar = 42
  assert(ptreq(uc, s) and uk == "bar" and uv == 42); uc,uk,uv=nil,nil,nil
  s[10] = 11
  assert(ptreq(uc, s) and uk == 10 and uv == 11); uc,uk,uv=nil,nil,nil

  local p = ffi.new("idx2_t *", s)
  assert(p.foo == 99)
  assert(ptreq(uc, p) and uk == "foo" and uv == nil); uc,uk,uv=nil,nil,nil
  assert(p.x == 1234)
  assert(uc == nil and uk == nil and uv == nil); uc,uk,uv=nil,nil,nil
  -- pointer dereference has precedence
  assert(ptreq(p[0], p))
  assert(uc == nil and uk == nil and uv == nil); uc,uk,uv=nil,nil,nil
  -- pointer dereference has precedence
  fails(function(p) p[0] = 11 end, p)
end

do
  local uc, uk, uv
  local ti, tn = {}, {}
  local tp = ffi.metatype("idx3_t", {
    __index = setmetatable(ti,
      { __index = function(c, k) uc, uk = c, k; return 99 end }),
    __newindex = setmetatable(tn,
      { __newindex = function(c, k, v) uc, uk, uv = c, k, v end }),
  })

  local s = tp(1234)
  assert(s.foo == 99)
  assert(uc == ti and uk == "foo" and uv == nil)
  uc, uk, uv = nil, nil, nil
  assert(s.x == 1234)
  assert(uc == nil and uk == nil and uv == nil)

  s.bar = 42
  assert(uc == tn and uk == "bar" and uv == 42)
  uc, uk, uv = nil, nil, nil
  s[10] = 11
  assert(uc == tn and uk == 10 and uv == 11)
  uc, uk, uv = nil, nil, nil
end

do
  local tp
  tp = ffi.metatype("arith_t", {
    __add = function(a, b) return tp(a.x+b.x, a.y+b.y) end,
    __sub = function(a, b) return tp(a.x-b.x, a.y-b.y) end,
    __mul = function(a, z) return tp(a.x*z, a.y*z) end,
    __div = function(z, a) return tp(a.x*z, a.y*z) end,
    __concat = setmetatable({}, { __call = function(x) return 99 end }),
    __len = function(x) return 2 end,
    __call = function(a) return a.x+a.y end,
    __tostring = function(a) return "foo" end,
    __newindex = function(a, k, v) a.y = v end,
    __index = {
      diff = function(a) return a.x-a.y end,
    },
  })

  local a = tp(10, 20)
  local b = tp(1, 2)
  local c = a + b
  assert(c.x == 11 and c.y == 22)
  assert(c:diff() == -11)
  assert(c() == 33)
  local d = a - b
  assert(d.x == 9 and d.y == 18)
  assert(d:diff() == -9)
  assert(d() == 27)
  local e = a * 3
  assert(e.x == 30 and e.y == 60)
  local f = 3LL / a
  assert(f.x == 30 and f.y == 60)
  assert(1 .. c == 99)
  assert(c .. 1 == 99)
  assert(c .. d == 99)
  assert(tostring(c) == "foo")
  assert(tostring(ffi.cast("arith_t *", c)) == "foo")
  c.foo = 42
  assert(c.y == 42)

  local p = ffi.new("arith_t *", a)
  local g1 = p + p
  assert(g1.x == 20 and g1.y == 40)
  local g2 = p[0] + p[0]
  assert(g2.x == 20 and g2.y == 40)
  assert(p() == 30)

  local q = ffi.new("arith_t &", a)
  fails(function(p) local y = q[0] + q[0] end, q)
  local h = q + q
  assert(h.x == 20 and h.y == 40)

  local diff = 0
  for i=1,100 do diff = a:diff() end
  assert(diff == -10)

  for i=1,100 do c.foo = i end
  assert(c.y == 100)

  local z = tp(1, 3)
  for i=1,100 do z = z + a end
  assert(z.x == 1001 and z.y == 2003)

  local x = 0
  for i=1,100 do x = x + #a end
  assert(x == 200)

  local x = 0
  for i=1,100 do x = x + p() end
  assert(x == 3000)
end

do
  local count = 0
  local tp = ffi.metatype("gc_t", {
    __gc = function(x) count = count + 1 end,
  })

  local a = tp()
  a = nil
  collectgarbage()
  assert(count == 1)
  local b,c = tp(), tp()
  b = nil
  collectgarbage()
  assert(count == 2)
  c = nil
  collectgarbage()
  assert(count == 3)

  local z
  for i=1,100 do z = tp() end
  z = nil
  collectgarbage()
  assert(count == 103)

  local t = {}
  for i=1,100 do t[i] = tp() end
  for i=1,100 do ffi.gc(t[i], nil) end
  t = nil
  collectgarbage()
  assert(count == 103)
end

do
  local tp = ffi.metatype([[
struct {
  static const int Z42 = 42;
  enum { Z39 = 39 };
  int x;
}]], {
    __new = function(tp, x)
      return ffi.new(tp, x or -1)
    end,
    __index = { test = function(x) return x+1 end, x = "hello" }
  })
  assert(tp.Z42 == 42)
  assert(tp.Z39 == 39)
  assert(tp.test(99) == 100)
  fails(function() tp.Z42 = 1 end)
  fails(function() tp.Z39 = 1 end)
  assert(tp.x == "hello") -- Not sure this is a good idea to allow that.
  fails(function() tp.x = 1 end)
  local o = tp()
  assert(o.Z42 == 42)
  assert(o.Z39 == 39)
  assert(o.test(55) == 56)
  fails(function() o.Z42 = 1 end)
  fails(function() o.Z39 = 1 end)
  assert(o.x == -1)
  o.x = 5
  assert(o.x == 5)
end

do
  local fb = ffi.new("struct { int x; }", 99)
  local xt = ffi.metatype("struct { }", { __index = fb })
  local o = xt()
  assert(o.x == 99)
end

