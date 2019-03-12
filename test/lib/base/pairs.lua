
do --- nometatable
  local t = {}
  for i=1,10 do t[i] = i+100 end
  local a, b = 0, 0
  for j=1,100 do for k,v in ipairs(t) do a = a + k; b = b + v end end
  assert(a == 5500)
  assert(b == 105500)
  a, b = 0, 0
  for j=1,100 do for k,v in pairs(t) do a = a + k; b = b + v end end
  assert(a == 5500)
  assert(b == 105500)
end

do --- empty metatable
  local t = setmetatable({}, {})
  for i=1,10 do t[i] = i+100 end
  local a, b = 0, 0
  for j=1,100 do for k,v in ipairs(t) do a = a + k; b = b + v end end
  assert(a == 5500)
  assert(b == 105500)
  a, b = 0, 0
  for j=1,100 do for k,v in pairs(t) do a = a + k; b = b + v end end
  assert(a == 5500)
  assert(b == 105500)
end

do --- metamethods +compat5.2
  local function iter(t, i)
    i = i + 1
    if t[i] then return i, t[i]+2 end
  end
  local function itergen(t)
    return iter, t, 0
  end
  local t = setmetatable({}, { __pairs = itergen, __ipairs = itergen })
  for i=1,10 do t[i] = i+100 end
  local a, b = 0, 0
  for j=1,100 do for k,v in ipairs(t) do a = a + k; b = b + v end end
  assert(a == 5500)
  assert(b == 107500)
  a, b = 0, 0
  for j=1,100 do for k,v in pairs(t) do a = a + k; b = b + v end end
  assert(a == 5500)
  assert(b == 107500)
end

do --- _G
  local n = 0
  for k,v in pairs(_G) do
    assert(_G[k] == v)
    n = n + 1
  end
  assert(n >= 35)
end

do --- count
  local function count(t)
    local n = 0
    for i,v in pairs(t) do
      n = n + 1
    end
    return n;
  end
  assert(count({ 4,5,6,nil,8,nil,10}) == 5)
  assert(count({ [0] = 3, 4,5,6,nil,8,nil,10}) == 6)
  assert(count({ foo=1, bar=2, baz=3 }) == 3)
  assert(count({ foo=1, bar=2, baz=3, boo=4 }) == 4)
  assert(count({ 4,5,6,nil,8,nil,10, foo=1, bar=2, baz=3 }) == 8)
  local t = { foo=1, bar=2, baz=3, boo=4 }
  t.bar = nil; t.boo = nil
  assert(count(t) == 2)
end
