do --- smoke
  assert(("abc123DEF_<>"):lower() == "abc123def_<>")
  assert(("abc123DEF_<>"):upper() == "ABC123DEF_<>")
end

do --- repeated
  local l = "the quick brown fox..."
  local u = "THE QUICK BROWN FOX..."
  local s = l
  for i = 1, 75 do
    s = s:upper()
    assert(s == u)
    s = s:lower()
    assert(s == l)
  end
end

do --- repeated with growing string
  local y, z
  local x = "aBcDe"
  for i=1,100 do
    y = string.upper(x)
    z = y.."fgh"
  end
  assert(y == "ABCDE")
  assert(z == "ABCDEfgh")
end

do --- misc upper
  local y
  for i=1,100 do y = string.upper("aBc9") end
  assert(y == "ABC9")
  local x = ":abCd+"
  for i=1,100 do y = string.upper(x) end
  assert(y == ":ABCD+")
  x = 1234
  for i=1,100 do y = string.upper(x) end
  assert(y == "1234")
end

do --- misc lower
  local y
  for i=1,100 do y = string.lower("aBc9") end
  assert(y == "abc9")
  local x = ":abcd+"
  for i=1,100 do y = string.lower(x) end
  assert(y == ":abcd+")
  x = 1234
  for i=1,100 do y = string.lower(x) end
  assert(y == "1234")
end
