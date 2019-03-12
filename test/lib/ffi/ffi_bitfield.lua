local ffi = require("ffi")

dofile("../common/ffi_util.inc")

do
  local x = ffi.new([[
    union {
      uint32_t u;
      struct { int a:10,b:10,c:11,d:1; };
      struct { unsigned int e:10,f:10,g:11,h:1; };
      struct { int8_t i:4,j:5,k:5,l:3; };
      struct { _Bool b0:1,b1:1,b2:1,b3:1; };
    }
  ]])

  -- bitfield access
  x.u = 0xffffffff
  assert(x.a == -1 and x.b == -1 and x.c == -1 and x.d == -1)
  assert(x.e == 1023 and x.f == 1023 and x.g == 2047 and x.h == 1)
  assert(x.i == -1 and x.j == -1 and x.k == -1 and x.l == -1)
  assert(x.b0 == true and x.b1 == true and x.b2 == true and x.b3 == true)
  x.u = 0x12345678
  if ffi.abi("le") then
    assert(x.a == -392 and x.b == 277 and x.c == 291 and x.d == 0)
    assert(x.e == 632 and x.f == 277 and x.g == 291 and x.h == 0)
    assert(x.i == -8 and x.j == -10 and x.k == -12 and x.l == 1)
    assert(x.b0 == false and x.b1 == false and x.b2 == false and x.b3 == true)
  else
    assert(x.a == 72 and x.b == -187 and x.c == 828 and x.d == 0)
    assert(x.e == 72 and x.f == 837 and x.g == 828 and x.h == 0)
    assert(x.i == 1 and x.j == 6 and x.k == 10 and x.l == -2)
    assert(x.b0 == false and x.b1 == false and x.b2 == false and x.b3 == true)
  end
  x.u = 0xe8d30edc
  if ffi.abi("le") then
    assert(x.a == -292 and x.b == 195 and x.c == -371 and x.d == -1)
    assert(x.e == 732 and x.f == 195 and x.g == 1677 and x.h == 1)
    assert(x.i == -4 and x.j == 14 and x.k == -13 and x.l == -2)
    assert(x.b0 == false and x.b1 == false and x.b2 == true and x.b3 == true)
  else
    assert(x.a == -93 and x.b == 304 and x.c == -146 and x.d == 0)
    assert(x.e == 931 and x.f == 304 and x.g == 1902 and x.h == 0)
    assert(x.i == -2 and x.j == -6 and x.k == 1 and x.l == -2)
    assert(x.b0 == true and x.b1 == true and x.b2 == true and x.b3 == false)
  end

  -- bitfield insert
  x.u = 0xffffffff
  x.a = 0
  if ffi.abi("le") then
    assert(x.u == 0xfffffc00)
  else
    assert(x.u == 0x003fffff)
  end
  x.u = 0
  x.a = -1
  if ffi.abi("le") then
    assert(x.u == 0x3ff)
  else
    assert(x.u == 0xffc00000)
  end
  x.u = 0xffffffff
  x.b = 0
  if ffi.abi("le") then
    assert(x.u == 0xfff003ff)
  else
    assert(x.u == 0xffc00fff)
  end
  x.u = 0
  x.b = -1
  if ffi.abi("le") then
    assert(x.u == 0x000ffc00)
  else
    assert(x.u == 0x003ff000)
  end

  -- cumulative bitfield insert
  x.u = 0xffffffff
  if ffi.abi("le") then
    x.a = -392; x.b = 277; x.c = 291; x.d = 0
  else
    x.a = 72; x.b = -187; x.c = 828; x.d = 0
  end
  assert(x.u == 0x12345678)
  x.u = 0
  if ffi.abi("le") then
    x.a = -392; x.b = 277; x.c = 291; x.d = 0
  else
    x.a = 72; x.b = -187; x.c = 828; x.d = 0
  end
  assert(x.u == 0x12345678)
  x.u = 0xffffffff
  x.b0 = true; x.b1 = false; x.b2 = true; x.b3 = false
  if ffi.abi("le") then
    assert(x.u == 0xfffffff5)
  else
    assert(x.u == 0xafffffff)
  end
  x.u = 0
  x.b0 = true; x.b1 = false; x.b2 = true; x.b3 = false
  if ffi.abi("le") then
    assert(x.u == 0x00000005)
  else
    assert(x.u == 0xa0000000)
  end

end

