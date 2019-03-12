local ffi = require("ffi")

do --- smoke
  local types = {
    "int8_t", "uint8_t",
    "int16_t", "uint16_t",
    "int32_t", "uint32_t",
    "int64_t", "uint64_t",
    "float", "double",
  }
  for j,tp in ipairs(types) do
    local t = ffi.new(tp.."[?]", 301)
    for i=1,300 do t[i] = 1 end
    for i=1,300 do assert(t[i] == 1) end
    for i=1,300 do t[i] = t[i-1] end -- reassoc across PHIs, a[i-1] forwarding
    for i=1,300 do assert(t[i] == 0) end
    for i=1,300 do t[i] = i end
    local x = 0
    for i=1,300 do x = x + t[i] end
    if tp == "int8_t" then assert(x == 862)
    elseif tp == "uint8_t" then assert(x == 33630)
    else assert(x == 45150) end
  end
end

do --- int array pointer arithmetic
  local a = ffi.new("int[?]", 101)
  local p = a+1;
  for i=1,100 do
    p[0] = i
    assert(p - a == i) -- pointer difference
    p = p + 1 -- pointer increment by 4 bytes
  end
  for i=1,100 do assert(a[i] == i) end
  for i=1,100 do assert((a+i)[0] == i) end -- pointer arithmetic
  for i=1,100 do assert((i+a)[0] == i) end -- pointer arithmetic
end

do --- double array pointer arithmetic
  local a = ffi.new("double[?]", 101)
  local p = a+1;
  for i=1,100 do
    p[0] = i
    p = p + 1 -- pointer increment by 8 bytes
  end
  for i=1,100 do assert(a[i] == i) end
  for i=1,100 do assert((a+i)[0] == i) end -- pointer arithmetic
end

do --- double array pointer comparisons +bit
  local a = ffi.new("double[?]", 201)
  local p = a+3
  for i=1,200 do local j = bit.band(i, 7); assert((a+j == p) == (j == 3)) end
  p = a+100;
  for i=1,200 do assert((a+i < p) == (i < 100)) end
  for i=1,200 do assert((a+i <= p) == (i <= 100)) end
end

do --- constant offset in double array index
  local a = ffi.new("double[?]", 100)
  for i=1,100 do a[i-1LL] = i end
  for i=1,100 do assert(a[100LL-i] == 101-i) end
end

do --- fixed index of minus one
  local a = ffi.new("int[10]")
  local p = a+1
  local k = ffi.new("int", -1)
  a[0] = 42
  for i=1,100 do assert(p[-1] == 42); assert(p[k] == 42) end
end

do --- uint8_t array element comparisons
  local a = ffi.new("uint8_t[?]", 256)
  for i=0,255 do a[i] = i end
  for i=1,255 do assert(a[i] >= 1) end
  for i=0,254 do assert(a[i] <= 254) end
end

do --- int32_t array bit/bswap tricks +bit
  local a = ffi.new("int32_t[?]", 256)
  local tobit, bswap, shl = bit.tobit, bit.bswap, bit.lshift
  for i=0,255 do a[i] = bswap(i+0x12345600) end
  for i=0,255 do assert(a[i] == tobit(shl(i, 24)+0x00563412)) end
  for i=0,255 do assert(bswap(a[i]) == tobit(i+0x12345600)) end
end

do --- int32_t shift/rotate/and +bit
  local a = ffi.new("int32_t[?]", 256)
  local shl, shr, rol, band = bit.lshift, bit.rshift, bit.rol, bit.band
  for i=0,255 do a[i] = i + shl(i, 8) + shl(i, 16) end

  for i=0,255 do assert(shl(band(a[i], 0xff), 8) == shl(i, 8)) end
  for i=0,255 do assert(band(shl(a[i], 8), 0xff00) == shl(i, 8)) end

  for i=0,255 do assert(shr(band(a[i], 0xff00), 8) == i) end
  for i=0,255 do assert(band(shr(a[i], 8), 255) == i) end

  for i=0,255 do assert(rol(band(a[i], 0xff), 8) == shl(i, 8)) end
  for i=0,255 do assert(band(rol(a[i], 8), 0xff00) == shl(i, 8)) end

  for i=0,255 do assert(shl(band(a[i], 0x000000ff), 24) == shl(i, 24)) end
  for i=0,255 do assert(shr(band(a[i], 0xffff0000), 16) == i) end
end
