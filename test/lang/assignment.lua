local assert = assert

do --- local
  local a, b, c
  a, b, c = 0, 1
  assert(a == 0)
  assert(b == 1)
  assert(c == nil)
  a, b = a+1, b+1, a+b
  assert(a == 1)
  assert(b == 2)
  a, b, c = 0
  assert(a == 0)
  assert(b == nil)
  assert(c == nil)
end

do --- global !private_G
  a, b, c = 0, 1
  assert(a == 0)
  assert(b == 1)
  assert(c == nil)
  a, b = a+1, b+1, a+b
  assert(a == 1)
  assert(b == 2)
  a, b, c = 0
  assert(a == 0)
  assert(b == nil)
  assert(c == nil)
end

do --- local lhs in key on lhs
  local a = {}
  local i = 3
  i, a[i] = i+1, 20
  assert(i == 4)
  assert(a[3] == 20)
end

do --- global lhs in key on lhs !private_G
  a = {}
  i = 3
  i, a[i] = i+1, 20
  assert(i == 4)
  assert(a[3] == 20)
end
