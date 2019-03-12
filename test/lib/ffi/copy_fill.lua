local ffi = require("ffi")

do --- misc
  local arr = ffi.typeof("char[11]")
  local a = arr()
  local b = arr()
  local c = arr()

  for i=0,9 do a[i] = 97+i; b[i] = 106-i end
  a[10] = 0; b[10] = 0;

  ffi.copy(c, a, 11)
  for i=0,9 do assert(c[i] == 97+i) end
  assert(ffi.string(c) == "abcdefghij")

  ffi.copy(c, b, 5)
  for i=0,4 do assert(c[i] == 106-i) end
  for i=5,9 do assert(c[i] == 97+i) end
  assert(ffi.string(c) == "jihgffghij")

  c[7] = 0
  assert(ffi.string(c) == "jihgffg")

  c[10] = 1
  ffi.copy(c, "ABCDEFGHIJ")
  for i=0,9 do assert(c[i] == 65+i) end
  assert(c[10] == 0)
  assert(ffi.string(c) == "ABCDEFGHIJ")

  ffi.copy(c, "abcdefghij", 5)
  assert(ffi.string(c) == "abcdeFGHIJ")

  ffi.fill(c, 10, 65)
  assert(ffi.string(c) == "AAAAAAAAAA")
  for i=10,0,-1 do ffi.fill(c, i, 96+i) end
  assert(ffi.string(c) == "abcdefghij")
  ffi.fill(c, 10)
  assert(c[0] == 0)
  assert(c[9] == 0)

  -- test length parameter to ffi.string
  ffi.fill(c, 10, 65)
  assert(ffi.string(c, 5) == "AAAAA")
end

do --- jit char[10]
  local a = ffi.new("char[10]", 64)
  local x
  for i=1,100 do a[0] = i; x = ffi.string(a, 10) end
  assert(x == "d@@@@@@@@@")
end

do --- jit char[1]
  local a = ffi.new("char[1]")
  local x, y
  for i=1,100 do
    a[0] = i
    x = ffi.string(a, 1)
    a[0] = 126
    y = ffi.string(a, 1)
  end
  assert(x == "d" and y == "~")
end

