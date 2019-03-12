
local t = {}
for i=1,26 do t[i] = string.char(96+i) end

local function tcheck(t1, t2)
  assert(#t1 == #t2)
  for i=1,#t1 do assert(t1[i] == t2[i]) end
end

local function foo1(...) -- VARG RETM
  return ...
end

local function foo2(...) -- VARG UCLO RETM
  local function dummy() end
  return ...
end

local function foo3(...) -- VARG UCLO -> RETM
  do return ... end
  local function dummy() end
end

local function foo4() -- UCLO UCLO RET
  do
    local x
    local function dummy() return x end
  end
end

called = false
debug.sethook(function() local x = "wrong"; called = true end, "", 1)
tcheck(t, {foo1(unpack(t))}) -- CALLM TSETM
assert(called)
called = false
tcheck(t, {foo2(unpack(t))})
assert(called)
called = false
tcheck(t, {foo2(unpack(t))})
assert(called)
called = false
foo4()
assert(called)

debug.sethook(function()
  local name, val = debug.getlocal(2, 1)
  assert(name == "a" and val == nil)
  debug.setlocal(2, 1, "bar")
  debug.sethook(nil)
end, "c")
local function foo5(a)
  assert(a == "bar")
end
foo5()

