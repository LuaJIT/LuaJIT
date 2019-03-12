local ffi = require("ffi")

local ctest = require("ctest")

do
  local s = ffi.new("struct { int32_t x; }")
  s.x = -0x12345678
  for i=1,100 do
    s.x = s.x + 1 -- narrowed
  end
  assert(s.x == -0x12345678+100)
end

do
  local s = ffi.new("struct { uint32_t x; }")
  s.x = 0x81234567
  for i=1,100 do
    s.x = s.x + 1 -- CONV.num.u32, CONV.u32.num (no narrowing yet)
  end
  assert(s.x == 0x81234567+100)
end

do
  local s = ffi.new("struct { int8_t x; }")
  s.x = 42
  for i=1,100 do
    s.x = s.x + 1
    assert(s.x >= -128 and s.x <= 127) -- fwd -> CONV.int.i8
  end
  assert(s.x == 142-256)
end

do
  local s = ffi.new("struct { uint8_t x; }")
  s.x = 200
  for i=1,100 do
    s.x = s.x + 1
    assert(s.x >= 0 and s.x <= 255) -- fwd -> CONV.int.u8
  end
  assert(s.x == 300-256)
end

do
  local s = ffi.new("struct { int16_t x; }")
  s.x = 32700
  for i=1,100 do
    s.x = s.x + 1
    assert(s.x >= -32768 and s.x <= 32767) -- fwd -> CONV.int.i16
  end
  assert(s.x == 32800-65536)
end

do
  local s = ffi.new("struct { uint16_t x; }")
  s.x = 65450
  for i=1,100 do
    s.x = s.x + 1
    assert(s.x >= 0 and s.x <= 65535) -- fwd -> CONV.int.u16
  end
  assert(s.x == 65550-65536)
end

do
  local s = ffi.new("union { int32_t x; uint32_t y; }")
  s.x = 0x7fffffff - 60
  local x,y = 0,0
  for i=1,100 do
    if s.x == 0x7fffffff then s.x = -0x80000000 else s.x = s.x + 1 end
    x = x + s.x -- fwd -> CONV.num.int
    y = y + s.y -- fwd -> CONV.num.u32
  end
  assert(s.x == 0x7fffffff - 60 + 100 - 2^32)
  assert(s.y == 0x7fffffff - 60 + 100)
  assert(y == (0x7fffffff - 60) * 100 + 5050)
  assert(x == y - 40*2^32)
end

do
  local s = ffi.new("union { int32_t x; uint32_t y; }")
  local x, z = 0, 2^31 + 42
  for i=1,100 do
    s.y = z
    x = x + s.x -- fwd -> CONV.int.u32 (dummy)
  end
  assert(x == 100*(-2^31 + 42))
end

do
  local s = ffi.new("union { int8_t x; uint8_t y; }")
  s.x = 42
  local x,y = 0,0
  for i=1,100 do
    s.x = s.x + 1
    x = x + s.x -- fwd -> CONV.int.i8, CONV.num.int
    y = y + s.y -- fwd -> CONV.int.u8, CONV.num.int
  end
  assert(s.x == 142 - 256)
  assert(s.y == 142)
  assert(y == 42 * 100 + 5050)
  assert(x == y - (100-(127-42))*256)
end

do
  local a = ffi.new("uint32_t[?]", 101)
  for i=1,100 do a[i] = 0x80000000+i end
  local x = 0
  for i=1,100 do
    x = bit.bxor(x, a[i]) -- FOLD TOBIT + CONV.num.u32
  end
  assert(x == 100)
end

do
  local a = ffi.new("uint32_t[?]", 101)
  for i=1,100 do a[i] = 0x80000000+i end
  local x = 0
  for i=1,100 do
    x = bit.bxor(a[i], 0) -- FOLD TOBIT + CONV.num.u32
  end
  assert(x == -0x80000000+100)
end

do
  local v = ffi.new("float", 12.5)
  local x = 0
  for i=1,100 do
    x = x + tonumber(v) -- CONV.num.flt
  end
  assert(x == 100*12.5)
end

do
  local v = ffi.new("uint32_t", 0x80000000)
  local x = 0
  for i=1,100 do
    x = x + tonumber(v) -- CONV.num.u32
  end
  assert(x == 100*0x80000000)
end

do
  local v = ffi.new("int64_t", 0x1234567800000000ll)
  local x = 0
  for i=1,100 do
    x = x + tonumber(v) -- CONV.num.i64
  end
  assert(x == 100*0x12345678*2^32)
end

do
  local v = ffi.new("uint64_t", 0x89abcdef00000000ull)
  local x = 0
  for i=1,100 do
    x = x + tonumber(v) -- CONV.num.u64
  end
  assert(x == 100*0x89abcdef*2^32)
end

do
  local a = ffi.new("int64_t[?]", 101)
  for i=1,100 do a[i] = -i end
  local x = 0
  for i=1,100 do
    x = x + tonumber(a[i]) -- CONV.num.i64
  end
  assert(x == -5050)
end

do
  local a = ffi.new("uint64_t[?]", 101)
  for i=1,100 do a[i] = 2^63+2^32*i end
  local x = 0
  for i=1,100 do
    x = x + tonumber(a[i]) -- CONV.num.u64
  end
  assert(x == 2^63*100+2^32*5050)
end

do
  local v = ffi.new("complex", 12.5, -3.25)
  local x = 0
  for i=1,100 do
    x = x + tonumber(v)
  end
  assert(x == 100*12.5)
end

do
  local s = ffi.new("struct { int64_t x;}")
  for i=1,100 do
    s.x = 0x123456789abcdef0LL
  end
  assert(tonumber(s.x) == tonumber(0x123456789abcdef0LL))
end

do
  local s = ffi.new("struct { uint64_t x;}")
  for i=1,100 do
    s.x = 0x823456789abcdef0ULL
  end
  assert(tonumber(s.x) == tonumber(0x823456789abcdef0ULL))
end

do
  ffi.cdef[[
  typedef enum { AA, BB, CC = -42 } foo_i;
  typedef enum { DD, EE, FF = 0x80000000u } foo_u;
  ]]
  local s = ffi.new("struct { foo_i x; foo_u y;}")
  for i=1,100 do
    s.x = "CC"
    assert(s.x == -42)
    s.x = "BB"
    assert(s.x == 1)
    s.y = "FF"
    assert(s.y == 0x80000000)
  end
  local st = ffi.typeof(s)
  for i=1,100 do s = st() end
  assert(s.x == 0 and s.y == 0)
  for i=1,100 do s = st("CC", "EE") end
  assert(s.x == -42 and s.y == 1)
  local ei = ffi.new("foo_i", "CC")
  local eu = ffi.new("foo_u", "EE")
  for i=1,100 do s = st(ei, eu) end
  assert(s.x == -42 and s.y == 1)
  local x
  for i=1,100 do x = tonumber(ei) end
  assert(x == -42)
end

do
  local s = ffi.new("struct { const char *x; const char *y;}")
  local a, tmp = "abcd", "ab"
  for i=1,100 do
    s.x = "abc"
    s.y = string.sub(a, 1, 2)
  end
  assert(ffi.string(s.x) == "abc")
  assert(ffi.string(s.y) == "ab")
end

do
  local s = ffi.new("struct { bool b[200]; int i[200]; double d[200];}")
  for i=0,199 do s.i[i] = i-100; s.d[i] = i-100 end
  for i=0,99 do s.b[i] = 0 end
  for i=100,199 do s.b[i] = 1 end
  for i=0,99 do assert(s.b[i] == false) end
  for i=100,199 do assert(s.b[i] == true) end
  for i=0,199 do s.b[i] = s.i[i] end
  for i=0,199 do assert(s.b[i] == (i ~= 100)) end
  for i=0,199 do s.b[i] = s.d[i] end
  for i=0,199 do assert(s.b[i] == (i ~= 100)) end
end

do
  local a = ffi.new("int16_t[100]", 1)
  for i=1,99 do a[i] = a[i] + a[i-1] end
  assert(a[99] == 100)
end

do
  local ud = ctest.lightud(12345678)
  local s = ffi.new("struct { void *p; }")
  for i=1,100 do
    assert(ffi.cast("uintptr_t", ud) == 12345678)
    s.p = ud
  end
  assert(ffi.cast("uintptr_t", s.p) == 12345678)
end

do
  local x = ffi.new("struct { int & x;}", ffi.new("int[1]", 42))
  local z
  for i=1,100 do z = x.x end
  assert(z == 42)
end
