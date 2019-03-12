
local cp = require("cpptest")

do
  local a, b = pcall(cp.catch, function() return "x" end)
  assert(a == true and b == "x")
end

do
  local a, b = pcall(function() cp.throw("foo") end)
  assert(a == false and b == "C++ exception")
end

local unwind
do
  local a, b = pcall(cp.catch, function() cp.throw("foo") end)
  unwind = a
  assert((a == false and b == "C++ exception") or (a == true and b == "foo"))
end

do
  local st = cp.alloc(function() return cp.isalloc() end)
  assert(st == true)
  assert(cp.isalloc() == false)
end

do
  local a, b = pcall(cp.alloc, function()
    assert(cp.isalloc() == true)
    return "foo", cp.throw
  end)
  assert(a == false and b == "C++ exception")
  assert(cp.isalloc() == false)
end

if unwind then
  local a, b = pcall(cp.alloc, function()
    assert(cp.isalloc() == true)
    return "foo", error
  end)
  assert(a == false and b == "foo")
  assert(cp.isalloc() == false)
end

do
  local a,b,c,d,e,f = cp.usereg(100, 50, function() end, false)
  assert(a==164 and b==312 and c==428 and d==3696 and e==404 and f==404)
end

do
  local function test()
    cp.usereg(100, 40, error, "foo")
  end
  local a,b,c,d,e,f = cp.usereg(100, 51, test, false)
  assert(a==164 and b==312 and c==428 and d==3696 and e==404 and f==404)
end

do
  local t = {};
  t.t = t;
  local function foo()
    for i=1,100 do
      local a,b,c,d,e,f = t, t.t, t.t.t, t.t.t.t, t.t.t.t.t, t.t.t.t.t.t
      local g,h,j,k,l = f.t, f.t.t, f.t.t.t, f.t.t.t.t, f.t.t.t.t.t
      local m = { a,b,c,d,e,f,g,h,j,k,l }
    end
  end
  local a,b,c,d,e,f = cp.usereg(100, 50, foo, false)
  assert(a==164 and b==312 and c==428 and d==3696 and e==404 and f==404)
end

