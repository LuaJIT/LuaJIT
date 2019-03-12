local loadstring = loadstring or load

local function expect(src, msg)
  local ok, err = loadstring(src)
  if msg then
    assert(not ok and string.find(err, msg), err)
  else
    assert(ok, err)
  end
end
  
do --- Basic goto and label semantics.
  -- Error: duplicate label.
  expect("::a:: ::a::", "'a'")
  expect("::a:: ::b:: do ::b:: end ::a::", "'a'")

  -- Error: undefined label.
  expect("goto a", "'a'")
  expect("goto a; ::b::", "'a'")
  expect("do ::a:: end; goto a", "'a'")
  expect("goto a; do ::a:: end", "'a'")
  expect("break", "break")
  expect("if x then break end", "break")

  -- Error: goto into variable scope.
  expect("goto a; local x; ::a:: local y", "'x'")
  expect("do local v,w; goto a; end; local x; ::a:: local y", "'x'")
  expect("repeat goto a; local x; ::a:: until x", "'x'")

  ::a:: do goto a; ::a:: end -- Forward jump, not an infinite loop.
end

do --- Goto is not a keyword. -compat5.2 !lex !private_G
  goto = 1
end

do --- Goto is a keyword. +compat5.2
  expect("goto = 1", "<name>")
end

do --- Trailing label is considered to be out of scope.
  local x = 11
  do
    goto a
    goto a
    local y = 22
    x = y
    ::a::
    ::b::
  end
  assert(x == 11)
end

do --- Trailing labels and empty statements are considered to be out of scope. +compat5.2 !lex
  local x = 11
  do
    goto a
    goto a
    local y = 22
    x = y
    ::a:: ;;
    ::b:: ;;
  end
  assert(x == 11)
end

do --- Simple loop with cross-jumping.
  local x = 1
  while true do
    goto b
    ::a:: if x < 100 then goto c end
    goto d
    ::b:: x = x + 1; goto a
    ::c::
  end
  ::d::
  assert(x == 100)
end

do --- Backwards goto must close upval.
  local t = {}
  local i = 1
  ::a::
  local x
  t[i] = function() return x end
  x = i
  i = i + 1
  if i <= 2 then goto a end
  assert(t[1]() == 1)
  assert(t[2]() == 2)
end

do --- Break must close upval, even if closure is parsed after break.
  local foo
  repeat
    local x
    ::a::
    if x then break end
    function foo() return x end
    x = true
    goto a
  until false
  assert(foo() == true)
end

do --- Label prevents joining to KNIL. -lua==5.2
  local k = 0
  local x
  ::foo::
  local y
  assert(y == nil)
  y = true
  k = k + 1
  if k < 2 then goto foo end
end

do --- Break resolved from the right scope.
  local function p(lvl)
     lvl = lvl or 1
     while true do
	lvl = lvl + 1
	if lvl == nil then break end
	local idx = 1
	while true do
	   if key == nil then break end
	   idx = idx + 1
	end
     end
  end
end

do --- Do not join twice with UCLO.
  while true do
    do
      local x
      local function f() return x end
    end
    break
  end

  while true do
    do
      local x
      local function f() return x end
    end
    goto foo
  end
  ::foo::
end
