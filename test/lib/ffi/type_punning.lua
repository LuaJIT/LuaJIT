local ffi = require("ffi")

local u = ffi.new([[
union {
  int8_t i8[8];
  uint8_t u8[8];
  int16_t i16[4];
  uint16_t u16[4];
  int32_t i32[2];
  uint32_t u32[2];
  int64_t i64[1];
  uint64_t u64[1];
  void *v[2];
  float f[2];
  double d[1];
}
]])

do --- float -> u32 type punning at same offset
  local x = 0LL
  for i=1,100 do
    u.f[0] = i
    x = x + u.u32[0]
  end
  assert(x == 110888222720LL)
end

do --- double -> u64 type punning at same offset
  local x = 0LL
  for i=1,100 do
    u.d[0] = i
    x = x + u.u64[0]
  end
  assert(x == 1886586031403171840ULL)
end

do --- i8 -> u8 type punning at same offset (fwd -> CONV.int.u8)
  local x = 0
  for i=-100,100 do
    u.i8[0] = i
    x = x + u.u8[0]
  end
  assert(x == 25600)
end

do --- p32/p64 -> u64 type punning at same offset (32 bit: different size)
  local x = 0LL
  u.u64[0] = 0
  for i=-100,150 do
    u.v[0] = ffi.cast("void *", ffi.cast("ptrdiff_t", i))
    x = x + u.u64[0]
  end
  assert(x == (ffi.abi"64bit" and 6275ULL or
	       (ffi.abi"le" and 0x6400001883ULL or 0x188300000000ULL)))
end

do --- u16 -> u8 type punning at overlapping offsets [0]
  local x = 0
  for i=255,520 do
    u.u16[0] = i
    x = x + u.u8[0]
  end
  assert(x == (ffi.abi"be" and 274 or 32931))
end

do --- u16 -> u8 type punning at overlapping offsets [1]
  local x = 0
  for i=255,520 do
    u.u16[0] = i
    x = x + u.u8[1]
  end
  assert(x == (ffi.abi"le" and 274 or 32931))
end

do --- i16 -> i32 type punning at overlapping offsets [0]
  local x = 0
  u.i32[0] = 0
  for i=-100,150 do
    u.i16[0] = i
    x = x + u.i32[0]
  end
  assert(x == (ffi.abi"be" and 411238400 or 6559875))
end

do --- i16 -> i32 type punning at overlapping offsets [1]
  local x = 0
  u.i32[0] = 0
  for i=-100,150 do
    u.i16[1] = i
    x = x + u.i32[0]
  end
  assert(x == (ffi.abi"le" and 411238400 or 6559875))
end

do --- double -> i32 type punning at overlapping offsets [0]
  local x = 0
  for i=1.5,120,1.1 do
    u.d[0] = i
    x = x + u.i32[0]
  end
  assert(x == (ffi.abi"be" and 116468870297 or -858993573))
end

do --- double -> i32 type punning at overlapping offsets [1]
  local x = 0
  for i=1.5,120,1.1 do
    u.d[0] = i
    x = x + u.i32[1]
  end
  assert(x == (ffi.abi"le" and 116468870297 or -858993573))
end

do --- u32 -> u64 type punning, constify u, 32 bit SPLIT: fold KPTR
  local u = ffi.new("union { struct { uint32_t lo, hi; }; uint64_t u64; }")

  local function conv(lo, hi)
    u.lo = lo
    u.hi = hi
    return u.u64
  end

  local x = 0ll
  for i=1,100 do
    x = x + conv(i, i)
  end
  assert(x == 21689584849850ULL)
end

do --- u64 -> u32 -> u64 type punning with KPTR
  local s = ffi.new("union { int64_t q; int32_t i[2]; }")
  local function f()
    s.q = 0
    s.i[1] = 1
    return s.q
  end
  for i=1,50 do f() f() f() end
  assert(f() ~= 0)
end
