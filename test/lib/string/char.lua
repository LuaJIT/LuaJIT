local char = string.char

do --- jit one char
  local y
  for i=1,100 do y = char(65) end
  assert(y == "A")
  local x = 97
  for i=1,100 do y = char(x) end
  assert(y == "a")
  x = "98"
  for i=1,100 do y = char(x) end
  assert(y == "b")
  for i=1,100 do y = char(32+i) end
  assert(y == "\132")
end

do --- jit until out of bounds
  local y
  assert(not pcall(function()
    for i=1,200 do y = char(100+i) end
  end))
  assert(y == "\255")
end

do --- jit five chars
  local y
  for i=1,100 do y = char(65, 66, i, 67, 68) end
  assert(y == "ABdCD")
end
