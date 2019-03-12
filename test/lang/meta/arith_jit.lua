
do --- assert rhs
  local t = {}
  local mt = {
    __add = function(a, b) assert(b == t); return a+11 end,
    __sub = function(a, b) assert(b == t); return a+12 end,
    __mul = function(a, b) assert(b == t); return a+13 end,
    __div = function(a, b) assert(b == t); return a+14 end,
    __mod = function(a, b) assert(b == t); return a+15 end,
    __pow = function(a, b) assert(b == t); return a+16 end,
    __unm = function(a, b) assert(a == t and b == t); return 17 end,
  }
  t = setmetatable(t, mt)
  do local x = 0; for i=1,100 do x = x + t end; assert(x == 1100); end
  do local x = 0; for i=1,100 do x = x - t end; assert(x == 1200); end
  do local x = 0; for i=1,100 do x = x * t end; assert(x == 1300); end
  do local x = 0; for i=1,100 do x = x / t end; assert(x == 1400); end
  do local x = 0; for i=1,100 do x = x % t end; assert(x == 1500); end
  do local x = 0; for i=1,100 do x = x ^ t end; assert(x == 1600); end
  do local x = 0; for i=1,100 do x = x + (-t) end; assert(x == 1700); end
end

do --- assert lhs
  local t = {}
  local mt = {
    __add = function(a, b) assert(a == t); return b+11 end,
    __sub = function(a, b) assert(a == t); return b+12 end,
    __mul = function(a, b) assert(a == t); return b+13 end,
    __div = function(a, b) assert(a == t); return b+14 end,
    __mod = function(a, b) assert(a == t); return b+15 end,
    __pow = function(a, b) assert(a == t); return b+16 end,
  }
  t = setmetatable(t, mt)
  do local x = 0; for i=1,100 do x = t + x end; assert(x == 1100); end
  do local x = 0; for i=1,100 do x = t - x end; assert(x == 1200); end
  do local x = 0; for i=1,100 do x = t * x end; assert(x == 1300); end
  do local x = 0; for i=1,100 do x = t / x end; assert(x == 1400); end
  do local x = 0; for i=1,100 do x = t % x end; assert(x == 1500); end
  do local x = 0; for i=1,100 do x = t ^ x end; assert(x == 1600); end
end

do --- assert both sides
  local t = {}
  local mt = {
    __add = function(a, b) assert(a == t and b == t); return 11 end,
    __sub = function(a, b) assert(a == t and b == t); return 12 end,
    __mul = function(a, b) assert(a == t and b == t); return 13 end,
    __div = function(a, b) assert(a == t and b == t); return 14 end,
    __mod = function(a, b) assert(a == t and b == t); return 15 end,
    __pow = function(a, b) assert(a == t and b == t); return 16 end,
  }
  t = setmetatable(t, mt)
  do local x = 0; for i=1,100 do x = t + t end; assert(x == 11); end
  do local x = 0; for i=1,100 do x = t - t end; assert(x == 12); end
  do local x = 0; for i=1,100 do x = t * t end; assert(x == 13); end
  do local x = 0; for i=1,100 do x = t / t end; assert(x == 14); end
  do local x = 0; for i=1,100 do x = t % t end; assert(x == 15); end
  do local x = 0; for i=1,100 do x = t ^ t end; assert(x == 16); end
end

do --- adjust no result to one result
  local t = {}
  local mt = { __add = function(a, b) end }
  t = setmetatable(t, mt)
  local x
  for i=1,100 do x = t+t end
  assert(x == nil)
end
