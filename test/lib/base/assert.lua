do --- pass through one
  assert(assert(true) == true)
  assert(assert(3) == 3)
  assert(assert(1.5) == 1.5)
  assert(assert("x") == "x")
  local f = function() end
  assert(assert(f) == f)
  local t = {}
  assert(assert(t) == t)
end

do --- pass through many
  local b, c = assert("b", "c")
  assert(b == "b")
  assert(c == "c")
  local d, e, f, g = assert("d", 5, true, false)
  assert(d == "d")
  assert(e == 5)
  assert(f == true)
  assert(g == false)
end

do --- raise on nil
  local ok, err = pcall(assert, nil)
  assert(ok == false)
  assert(err == "assertion failed!")
end

do --- raise on false
  local ok, err = pcall(assert, false, "msg")
  assert(ok == false)
  assert(err == "msg")
end
