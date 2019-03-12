local function create(cat, v1, v2)
  local meta = { __concat = cat }
  return setmetatable({v1}, meta), setmetatable({v2}, meta)
end

do --- default
  local a, b, c = "foo", "bar", "baz"
  assert(a..b == "foobar")
  assert(a..b..c == "foobarbaz")
end

do --- lhs
  local a, b = create(function(a, b) return a end)
  assert(a..b == a)
  assert(b..a == b)
  assert(a..b..b == a)
  assert(a..a..b == a)
  assert(a..b..a == a)
  assert(a..b..b..b..b..b..b..b == a)
end

do --- rhs
  local a, b = create(function(a, b) return b end)
  assert(a..b == b)
  assert(b..a == a)
  assert(a..b..b == b)
  assert(a..a..b == b)
  assert(b..b..a == a)
  assert(a..a..a..a..a..a..a..b == b)
end

do --- mixed types
  local a, b = create(function(a, b)
    return (type(a) == "string" and a or a[1])..
	   (type(b) == "string" and b or b[1])
  end, "a", "b")
  assert(a..b == "ab")
  assert(a..b == "ab")
  assert(a..b..b == "abb")
  assert(a..b..a == "aba")
  assert(a..a..a..a..a..a..a..b == "aaaaaaab")
  assert(a..a..a.."x".."x"..a..a..b == "aaaxxaab")
  assert("x"..a..a..a..a..a..a..b == "xaaaaaab")
  assert(a..b..a..b..a.."x".."x".."x" == "ababaxxx")
end

do --- jit mixed types
  local a, b = create(function(a, b)
    if a ~= b then local x = gg end
    return (type(a) == "string" and a or a[1])..
	   (type(b) == "string" and b or b[1])
  end, "a", "b")
  local y
  for i=1,100 do y = a..b end
  assert(y == "ab")
  for i=1,100 do y = a..b.."x" end
  assert(y == "abx")
  for i=1,100 do y = a..b.. 1 .. "z" end
  assert(y == "ab1z")
end

