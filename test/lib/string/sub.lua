local band, bor = bit and bit.band, bit and bit.bor
local sub = string.sub
local expect_error = require"common.expect_error"

do --- smoke
  assert(sub("abc", 2) == "bc")
  assert(sub(123, "2") == "23")
end

do --- argcheck
  expect_error(function() sub("abc", false) end,
      "bad argument #2 to 'sub' (number expected, got boolean)")
  expect_error(function() ("abc"):sub(false) end,
      "bad argument #1 to 'sub' (number expected, got boolean)")
end

do --- all bar substrings
  local subs = {
    {"b", "ba", "bar"},
    { "",  "a",  "ar"},
    { "",   "",   "r"}
  }
  for i = 1, 3 do
    for j = 1, 3 do
      assert(sub("bar", i, j) == subs[i][j])
      assert(sub("bar", -4+i, j) == subs[i][j])
      assert(sub("bar", i, -4+j) == subs[i][j])
      assert(sub("bar", -4+i, -4+j) == subs[i][j])
    end
  end
end

do --- Positive slice [i,len] or overflow +bit
  local s = "abc"
  local x
  for j=100,107 do
    for i=1,j do x = sub("abc", band(i, 7)) end
    assert(x == sub("abc", band(j, 7)))
  end
  for j=100,107 do
    for i=1,j do x = sub(s, band(i, 7)) end
    assert(x == sub(s, band(j, 7)))
  end
end

do --- Negative slice [-i,len] or underflow +bit
  local s = "abc"
  local x
  for j=-100,-107,-1 do
    for i=-1,j,-1 do x = sub("abc", bor(i, -8)) end
    assert(x == sub("abc", bor(j, -8)))
  end
  for j=-100,-107,-1 do
    for i=-1,j,-1 do x = sub(s, bor(i, -8)) end
    assert(x == sub(s, bor(j, -8)))
  end
end

do --- Positive slice [1,i] or overflow +bit
  local s = "abc"
  local x
  for j=100,107 do
    for i=1,j do x = sub("abc", 1, band(i, 7)) end
    assert(x == sub("abc", 1, band(j, 7)))
  end
  for j=100,107 do
    for i=1,j do x = sub(s, 1, band(i, 7)) end
    assert(x == sub(s, 1, band(j, 7)))
  end
end

do --- Negative slice [1,-i] or underflow +bit
  local s = "abc"
  local x
  for j=-100,-107,-1 do
    for i=-1,j,-1 do x = sub("abc", 1, bor(i, -8)) end
    assert(x == sub("abc", 1, bor(j, -8)))
  end
  for j=-100,-107,-1 do
    for i=-1,j,-1 do x = sub(s, 1, bor(i, -8)) end
    assert(x == sub(s, 1, bor(j, -8)))
  end
end

do --- jit sub 1 eq
  local s = "abcde"
  local x = 0
  for i=1,100 do
    if sub(s, 1, 1) == "a" then x = x + 1 end
  end
  assert(x == 100)
end

do --- jit sub 1 ne (contents)
  local s = "abcde"
  local x = 0
  for i=1,100 do
    if sub(s, 1, 1) == "b" then x = x + 1 end
  end
  assert(x == 0)
end

do --- jit sub 1 ne (rhs too long)
  local s = "abcde"
  local x = 0
  for i=1,100 do
    if sub(s, 1, 1) == "ab" then x = x + 1 end
  end
  assert(x == 0)
end

do --- jit sub 1,2 ne
  local s = "abcde"
  local x = 0
  for i=1,100 do
    if sub(s, 1, 2) == "a" then x = x + 1 end
  end
  assert(x == 0)
end

do --- jit sub 1,k eq
  local s = "abcde"
  local x = 0
  local k = 1
  for i=1,100 do
    if sub(s, 1, k) == "a" then x = x + 1 end
  end
  assert(x == 100)
end

do --- jit sub 1,k ne (contents)
  local s = "abcde"
  local x = 0
  local k = 1
  for i=1,100 do
    if sub(s, 1, k) == "b" then x = x + 1 end
  end
  assert(x == 0)
end

do --- jit sub 1,k ne (rhs too long)
  local s = "abcde"
  local x = 0
  local k = 1
  for i=1,100 do
    if sub(s, 1, k) == "ab" then x = x + 1 end
  end
  assert(x == 0)
end

do --- jit sub 1,2 eq
  local s = "abcde"
  local x = 0
  for i=1,100 do
    if sub(s, 1, 2) == "ab" then x = x + 1 end
  end
  assert(x == 100)
end

do --- jit sub 1,3 eq
  local s = "abcde"
  local x = 0
  for i=1,100 do
    if sub(s, 1, 3) == "abc" then x = x + 1 end
  end
  assert(x == 100)
end

do --- jit sub 1,4 eq
  local s = "abcde"
  local x = 0
  for i=1,100 do
    if sub(s, 1, 4) == "abcd" then x = x + 1 end
  end
  assert(x == 100)
end

do --- jit sub i,i
  local t = {}
  local line = string.rep("..XX", 100)
  local i = 1
  local c = line:sub(i, i)
  while c ~= "" and c ~= "Z" do
    t[i] = c == "X" and "Y" or c
    i = i + 1
    c = line:sub(i, i)
  end
  assert(table.concat(t) == string.rep("..YY", 100))
end
