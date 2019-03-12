
local function test_for()
  local z1, z2
  for i=1,10 do
    local function f() return i end
    if z1 then z2 = f else z1 = f end
  end
  assert(z1() == 1)
  assert(z2() == 10)
end

local function test_while()
  local z1, z2
  local i = 1
  while i <= 10 do
    local j = i
    local function f() return j end
    if z1 then z2 = f else z1 = f end
    i = i + 1
  end
  assert(z1() == 1)
  assert(z2() == 10)
end

local function test_repeat()
  local z1, z2
  local i = 1
  repeat
    local j = i
    local function f() return j end
    if z1 then z2 = f else z1 = f end
    i = i + 1
  until i > 10
  assert(z1() == 1)
  assert(z2() == 10)
end

local function test_func()
  local function ff(x)
    return function() return x end
  end
  local z1, z2
  for i=1,10 do
    local f = ff(i)
    if z1 then z2 = f else z1 = f end
  end
  assert(z1() == 1)
  assert(z2() == 10)
end

test_for()
test_while()
test_repeat()
test_func()

do
  local function f1(a)
    if a > 0 then
      local b = f1(a - 1)
      return function()
	if type(b) == "function" then
	  return a + b()
	end
	return a + b
      end
    end
    return a
  end

  local function f2(a)
    return f1(a)()
  end

  for i = 1, 41 do
    local r = f2(4) + f2(4)
  end
end

-- Don't mark upvalue as immutable if written to after prototype definition.
do
  local x = 1
  local function f()
    local y = 0
    for i=1,100 do y=y+x end
    return y
  end
  assert(f() == 100)
  x = 2
  assert(f() == 200)
end

