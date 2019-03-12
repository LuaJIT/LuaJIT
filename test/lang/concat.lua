do --- Constant folding
  local y
  for i=1,100 do y = "a".."b" end
  assert(y == "ab")
  for i=1,100 do y = "ab"..(1).."cd"..(1.5) end
  assert(y == "ab1cd1.5")
end

do --- Fuse conversions to strings
  local y
  local x = "a"
  for i=1,100 do y = x..i end
  assert(y == "a100")
  x = "a"
  for i=1.5,100.5 do y = x..i end
  assert(y == "a100.5")
end

do --- Fuse string construction
  local y
  local x = "abc"
  for i=1,100 do y = "x"..string.sub(x, 2) end
  assert(y == "xbc")
end

do --- CSE, sink
  local y
  local x = "a"
  for i=1,100 do y = x.."b" end
  assert(y == "ab")
end

do --- CSE, two buffers in parallel, no sink
  local y, z
  local x1, x2 = "xx", "yy"
  for i=1,100 do y = x1.."a"..x1; z = x1.."a"..x2 end
  assert(y == "xxaxx")
  assert(z == "xxayy")
  x1 = "xx"
  for i=1,100 do y = x1.."a"..x1; z = x1.."b"..x1 end
  assert(y == "xxaxx")
  assert(z == "xxbxx")
end

do --- Append, CSE
  local y, z
  local x = "a"
  for i=1,100 do
    y = x.."b"
    y = y.."c"
  end
  assert(y == "abc")
  x = "a"
  for i=1,100 do
    y = x.."b"
    z = y.."c"
  end
  assert(y == "ab")
  assert(z == "abc")
  x = "a"
  for i=1,100 do
    y = x.."b"
    z = y..i
  end
  assert(y == "ab")
  assert(z == "ab100")
end

do --- Append, FOLD
  local a, b = "x"
  for i=1,100 do b = (a.."y").."" end
  assert(b == "xy")
end

do --- Append to buffer, sink
  local x = "a"
  for i=1,100 do x = x.."b" end
  assert(x == "a"..string.rep("b", 100))
  x = "a"
  for i=1,100 do x = x.."bc" end
  assert(x == "a"..string.rep("bc", 100))
end

do --- Append to two buffers in parallel, no append, no sink
  local y, z = "xx", "yy"
  for i=1,100 do y = y.."a"; z = z.."b" end
  assert(y == "xx"..string.rep("a", 100))
  assert(z == "yy"..string.rep("b", 100))
end

do --- Sink into side-exit
  local x = "a"
  local z
  for i=1,200 do
    local y = x.."b"
    if i > 100 then
      z = y..i
    end
  end
  assert(z == "ab200")
end

do --- Very long strings
  for i, s in ipairs{"a", "bc", "def"} do
    for n = 1, 20 do
      s = s .. s
    end
    assert(#s == 2^20*i)
    assert(s:sub(1, 6) == s:sub(7, 12))
    assert(s:sub(1, 6) == s:sub(-6, -1))
  end
end
