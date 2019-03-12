local ffi = require("ffi")

do
  local a = ffi.new("int64_t[?]", 101)
  for i=1,100 do a[i] = -2 end
  for i=1,100 do a[i] = i end
  local x, y, m = 0ll, 0ll, 0ll
  for i=1,100 do x = x + a[i]; y = y - a[i]; m = -a[i] end
  assert(x == 5050)
  assert(y == -5050)
  assert(m == -100)
  local z, z0 = 1ll, 3ll
  for i=1,100 do z = a[i] * z0 end
  assert(z == 300)
  for i=1,100 do z = a[i] * 4ll end -- test MUL -> BSHL rule
  assert(z == 400)
  z, z0 = 1ll, 0x123456789abcdef0ll
  for i=1,100 do z = z0 / a[i] end
  assert(z == 0x123456789abcdef0ll / 100)
  z, z0 = 1ll, 0x123456789abcdef0ll
  for i=1,100 do z = z0 % a[i] end
  assert(z == 0x123456789abcdef0ll % 100)
  -- use multiple 64 bit PHIs
  local t, u, v, w = 0ll, 0ll, 0ll, 0ll
  for i=1,100 do t = t + a[i]; u = u + a[i]; v = v + a[i]; w = w + a[i] end
  assert(t == 5050)
  assert(u == 5050)
  assert(v == 5050)
  assert(w == 5050)
end

do
  local a = ffi.new("uint64_t[?]", 101)
  for i=1,100 do a[i] = i end
  local x, y, m = 0ull, 0ull, 0ull
  for i=1,100 do x = x + a[i]; y = y - a[i]; m = -a[i] end
  assert(x == 5050)
  assert(y == 0ull-5050)
  assert(m == -100ull)
  local z, z0 = 1ull, 3ll
  for i=1,100 do z = a[i] * z0 end
  assert(z == 300)
  z, z0 = 1ull, 0x123456789abcdef0ull
  for i=1,100 do z = z0 / a[i] end
  assert(z == 0x123456789abcdef0ull / 100)
  z, z0 = 1ull, 0x123456789abcdef0ull
  for i=1,100 do z = z0 % a[i] end
  assert(z == 0x123456789abcdef0ull % 100)
end

do
  local x = 0ll
  for i=1,100 do x = x + (-2ll) ^ (bit.band(i, 15)+1ll) end
  assert(x == 262120)
end

do
  local x, a = 0ll, -2ll
  for i=1,100 do x = x + a ^ (bit.band(i, 15)+1ll) end
  assert(x == 262120)
end

do
  local x = 0ull
  for i=1,100 do x = x + (-2ll) ^ (bit.band(i, 15)+1ull) end
  assert(x == 262120)
end

do
  for i=1,200 do local j = bit.band(i, 7); assert((j == 0ll) == (j == 0)) end
  for i=1,200 do assert((i < 100ll) == (i < 100)) end
  for i=1,200 do assert((i <= 100ll) == (i <= 100)) end
  for i=-100,100 do assert((i > 100ull) == (i < 0)) end
end

do
  local a = ffi.new("int64_t[?]", 100)
  for i=0,99 do
    a[i] = math.random(0, 2^32)*0x100000000LL + math.random(0, 2^32)
  end
  a[92] = 0x10000000LL
  a[93] = 0x10000001LL
  a[94] = math.random(0, 2^32)
  a[95] = a[94] + 0x100000000LL
  a[96] = a[94] + 0x100000001LL
  a[97] = a[20]
  a[98] = 0
  a[99] = -1

  local function cksum(b)
    local bxor, rol = bit.bxor, bit.rol
    local x = 0
    for i=0,#b do x = rol(bxor(x, (b[i] and i or 0)), 7) end
    return x
  end

  local s = [[
    local a, b = ...
    local k = 0
    for i=0,99 do
      for j=0,99 do
	b[k] = a[i] %s a[j]
	k = k + 1
      end
    end
  ]]

  local ap = ffi.new("int64_t *", a)
  local b = {}
  for i=1,2 do
    for _,cmp in ipairs{ "==", "~=", "<", "<=", ">", ">=" } do
      local f = assert(loadstring(string.format(s, cmp), "operator"..cmp))
      f(ap, b)
      local r1 = cksum(b)
      jit.off(f)
      f(ap, b)
      local r2 = cksum(b)
      assert(r1 == r2)
    end
    ap = ffi.new("uint64_t *", a)
  end
end

do
  local a, b = ffi.new("char *"), ffi.new("char *")
  local z
  for i=1,100 do z = a-b end
end

do
  local x = true
  local abc = ffi.cast("const char *", "abc")
  for i=1,100 do x = abc == "abc" end
  assert(x == true)
  for i=1,100 do x = abc == "xyz" end
  assert(x == false)
  for i=1,100 do x = 0LL == "" end
  assert(x == false)
  for i=1,100 do x = 0LL == false end
  assert(x == false)
  for i=1,100 do x = 0LL == nil end
  assert(x == false)
end

-- ra_destpair
do
  local x, y = 0, 0
  for i=1,100 do
    x = x + i/3LL
    y = y + i/5LL
  end
  assert(x == 1650)
  assert(y == 970)
end

