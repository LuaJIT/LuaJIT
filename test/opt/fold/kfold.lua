do --- operators
  local y = 0
  for i=1,100 do local a, b = 23, 11; y = a+b end; assert(y == 23+11)
  for i=1,100 do local a, b = 23, 11; y = a-b end; assert(y == 23-11)
  for i=1,100 do local a, b = 23, 11; y = a*b end; assert(y == 23*11)
  for i=1,100 do local a, b = 23, 11; y = a/b end; assert(y == 23/11)
  for i=1,100 do local a, b = 23, 11; y = a%b end; assert(y == 23%11)
  for i=1,100 do local a, b = 23, 11; y = a^b end; assert(y == 23^11)

  for i=1,100 do local a, b = 23.5, 11.5; y = a+b end; assert(y == 23.5+11.5)
  for i=1,100 do local a, b = 23.5, 11.5; y = a-b end; assert(y == 23.5-11.5)
  for i=1,100 do local a, b = 23.5, 11.5; y = a*b end; assert(y == 23.5*11.5)
  for i=1,100 do local a, b = 23.5, 11.5; y = a/b end; assert(y == 23.5/11.5)
  for i=1,100 do local a, b = 23.5, 11.5; y = a%b end; assert(y == 23.5%11.5)
end

do --- abs
  local y = 0
  for i=1,100 do local a=23; y = math.abs(a) end; assert(y == math.abs(23))
  for i=1,100 do local a=-23; y = math.abs(a) end; assert(y == math.abs(-23))
  for i=1,100 do local a=23.5; y = math.abs(a) end; assert(y == math.abs(23.5))
  for i=1,100 do local a=-23.5; y = math.abs(a) end; assert(y==math.abs(-23.5))
  for i=1,100 do local a=-2^31; y = math.abs(a) end; assert(y==math.abs(-2^31))
end

do --- atan2 ldexp
  local y = 0
  for i=1,100 do local a, b = 23, 11; y = math.atan2(a, b) end
  assert(y == math.atan2(23, 11))
  for i=1,100 do local a, b = 23, 11; y = math.ldexp(a, b) end
  assert(y == math.ldexp(23, 11))
end

do --- minmax
  local y = 0
  for i=1,100 do local a, b = 23, 11; y = math.min(a, b) end
  assert(y == math.min(23, 11))
  for i=1,100 do local a, b = 23, 11; y = math.max(a, b) end
  assert(y == math.max(23, 11))
  for i=1,100 do local a, b = 23.5, 11.5; y = math.min(a, b) end
  assert(y == math.min(23.5, 11.5))
  for i=1,100 do local a, b = 23.5, 11.5; y = math.max(a, b) end
  assert(y == math.max(23.5, 11.5))
  for i=1,100 do local a, b = 11, 23; y = math.min(a, b) end
  assert(y == math.min(11, 23))
  for i=1,100 do local a, b = 11, 23; y = math.max(a, b) end
  assert(y == math.max(11, 23))
  for i=1,100 do local a, b = 11.5, 23.5; y = math.min(a, b) end
  assert(y == math.min(11.5, 23.5))
  for i=1,100 do local a, b = 11.5, 23.5; y = math.max(a, b) end
  assert(y == math.max(11.5, 23.5))
end

do --- floorceil
  local y = 0
  for i=1,100 do local a=23; y=math.floor(a) end assert(y==math.floor(23))
  for i=1,100 do local a=23.5; y=math.floor(a) end assert(y==math.floor(23.5))
  for i=1,100 do local a=-23; y=math.floor(a) end assert(y==math.floor(-23))
  for i=1,100 do local a=-23.5; y=math.floor(a) end assert(y==math.floor(-23.5))
  for i=1,100 do local a=-0; y=math.floor(a) end assert(y==math.floor(-0))
  for i=1,100 do local a=23; y=math.ceil(a) end assert(y==math.ceil(23))
  for i=1,100 do local a=23.5; y=math.ceil(a) end assert(y==math.ceil(23.5))
  for i=1,100 do local a=-23; y=math.ceil(a) end assert(y==math.ceil(-23))
  for i=1,100 do local a=-23.5; y=math.ceil(a) end assert(y==math.ceil(-23.5))
  for i=1,100 do local a=-0; y=math.ceil(a) end assert(y==math.ceil(-0))
end

do --- sqrt exp log trig
  local y = 0
  for i=1,100 do local a=23; y=math.sqrt(a) end assert(y==math.sqrt(23))
  for i=1,100 do local a=23; y=math.exp(a) end assert(y==math.exp(23))
  for i=1,100 do local a=23; y=math.log(a) end assert(y==math.log(23))
  for i=1,100 do local a=23; y=math.log10(a) end assert(y==math.log10(23))
  for i=1,100 do local a=23; y=math.sin(a) end assert(y==math.sin(23))
  for i=1,100 do local a=23; y=math.cos(a) end assert(y==math.cos(23))
  for i=1,100 do local a=23; y=math.tan(a) end assert(y==math.tan(23))
end

do --- exp -luajit==2.0
  assert((10^-2 - 0.01) == 0)
end
