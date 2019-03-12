
local cp = require("cpptest")
cp.wrapon()

do
  local a, b = pcall(cp.catch, function() return "x" end)
  assert(a == true and b == "x")
end

do
  local a, b = pcall(function() cp.throw("foo") end)
  assert(a == false and b == "foo")
end

local unwind
do
  local a, b = pcall(cp.catch, function() cp.throw("foo") end)
  unwind = a
  assert((a == false and b == "foo") or (a == true and b == "catch ..."))
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
  assert(a == false and b == "foo")
  assert(cp.isalloc() == false)
end

do
  local a, b = pcall(cp.alloc, function()
    assert(cp.isalloc() == true)
    return "foo", error
  end)
  assert(a == false and b == "foo")
  if unwind then assert(cp.isalloc() == false) end
end

