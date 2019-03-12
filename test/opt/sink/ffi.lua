local ffi = require("ffi")

do --- incrementing
  local x = 10000000000000ll
  for i=1,100 do x=x+1 end
  assert(x == 10000000000100ll)
end

do --- hoistable increment !private_G
  local x = 10000000000000ll
  local z
  for i=1,100 do z=x+1 end
  assert(z == 10000000000001ll)
  for i=1,100 do local y=x; z=x+1; g=y end
  assert(z == 10000000000001ll)
  assert(g == 10000000000000ll)
end

do --- escaping hoistable increment
  local x = 10000000000000ll
  for i=1,100 do local y=x+1; if i == 90 then x=y end end
  assert(x == 10000000000001ll)
end

do --- escaping addition
  local x = 10000000000000ll
  for i=1,100 do local y=x+i; if i == 90 then x=y end end
  assert(x == 10000000000090ll)
end

do --- conditional addition / incrementing
  local x = 10000000000000ll
  for i=1,200 do local y=x+i; if i > 100 then x=y end end
  assert(x == 10000000015050ll)
end

do --- incrementing pointer
  local a = ffi.new("int[?]", 100)
  local p = a
  for i=0,99 do p[0]=i; p=p+1 end
  assert(p == a+100)
  for i=0,99 do assert(a[i] == i) end
end

do --- mutating complex
  local cx = ffi.typeof("complex")
  local x = cx(1, 2)
  local k = cx(3, 4)
  for i=1,100 do x = cx(x.re+k.re, x.im+k.im) end
  assert(x.re == 301)
  assert(x.im == 402)
end

do --- mutating struct
  local st = ffi.typeof("struct { int a; int64_t b; double c; }")
  local x = st(1, 20000000000LL, 3.5)
  local k = st(3, 4, 5.0)
  for i=1,100 do x = st(x.a+k.a, x.b+k.b, x.c+k.c) end
  assert(x.a == 301)
  assert(x.b == 20000000400LL)
  assert(x.c == 503.5)
  local y, z
  for i=1,100 do
    local x = st(i, i, i)
    if i == 90 then y = st(x.a, x.b, x.c) end
    x.b = x.b + 20000000000LL
    if i == 95 then z = st(x.a, x.b, x.c) end
  end
  assert(y.a == 90)
  assert(y.b == 90)
  assert(y.c == 90)
  assert(z.a == 95)
  assert(z.b == 20000000095LL)
  assert(z.c == 95)
  for i=1,200 do
    local x = st(i, i, i)
    if i > 100 then y = st(x.a, x.b, x.c) end
    x.b = x.b + 20000000000LL
    if i > 150 then z = st(x.a, x.b, x.c) end
  end
  assert(y.a == 200)
  assert(y.b == 200)
  assert(y.c == 200)
  assert(z.a == 200)
  assert(z.b == 20000000200LL)
  assert(z.c == 200)
end

do --- mutating struct 2
  local st = ffi.typeof("struct { int64_t a; double b; float c; }")
  local x = st(1, 2.5, 3.25)
  local k = st(3, 4, 5)
  for i=1,100 do x = st(x.a+k.a, x.b+k.b, x.c+k.c) end
  assert(x.a == 301)
  assert(x.b == 402.5)
  assert(x.c == 503.25)
end

do --- escaping loop counter to float
  local st = ffi.typeof("struct { float a; }")
  local x
  for i=1,200 do
    local y = st(i)
    if i > 100 then x = y end
  end
  assert(x.a == 200)
end

do --- 64 bit crash bug !private_G
  local t = {}
  for i=1,200 do t[i] = "abcd" end
  local r
  for i=1,200 do
    local a,b,c,d
    local g = t[201-i]                                -- Non-zero stack slot above.
    local v = ffi.cast("const char *", t[i])          -- Uses 32 bit stack slot!
    a,b,c,d = {v[0]},{v[1]},{v[2]},{v[3]}             -- Force above to spill.
    r = {{i}}                                         -- Spill due to call.
    if i > 100 then z = v[0]+a[1]+b[1]+c[1]+d[1] end  -- Crash for 64 bit ptr v.
  end
end
