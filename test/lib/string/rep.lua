local rep = string.rep

do --- smoke
  assert(("p"):rep(0) == "")
  assert(("a"):rep(3) == "aaa")
  assert(("x\0z"):rep(4) == "x\0zx\0zx\0zx\0z")
end

do --- versus concat
  local s = ""
  for i = 1, 75 do
    s = s .. "{}"
    assert(s == ("{}"):rep(i))
  end
end

do --- misc
  local y
  for i=1,100 do y = rep("a", 10) end
  assert(y == "aaaaaaaaaa")
  for i=1,100 do y = rep("ab", 10) end
  assert(y == "abababababababababab")
  local x = "a"
  for i=1,100 do y = rep(x, 10) end
  assert(y == "aaaaaaaaaa")
  local n = 10
  for i=1,100 do y = rep(x, n) end
  assert(y == "aaaaaaaaaa")
  x = "ab"
  for i=1,100 do y = rep(x, n) end
  assert(y == "abababababababababab")
  x = 12
  n = "10"
  for i=1,100 do y = rep(x, n) end
  assert(y == "12121212121212121212")
end

do --- separator +goto
  local y
  for i=1,100 do y = rep("ab", 10, "c") end
  assert(y == "abcabcabcabcabcabcabcabcabcab")
end

do --- iterate to table
  local t = {}
  for i=1,100 do t[i] = rep("ab", i-85) end
  assert(t[100] == "ababababababababababababababab")
end

do --- iterate to table with sep +goto
  local t = {}
  for i=1,100 do t[i] = rep("ab", i-85, "c") end
  assert(t[85] == "")
  assert(t[86] == "ab")
  assert(t[87] == "abcab")
  assert(t[100] == "abcabcabcabcabcabcabcabcabcabcabcabcabcabcab")
end

do --- iterate and concat
  local y, z
  local x = "ab"
  for i=1,100 do
    y = rep(x, i-90)
    z = y.."fgh"
  end
  assert(y == "abababababababababab")
  assert(z == "ababababababababababfgh")
end
