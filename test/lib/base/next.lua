do --- _G 1
  local ok, err = pcall(next, _G, 1)
  assert(not ok)
  local ok, err = pcall(function() next(_G, 1) end)
  assert(not ok)
end

do --- as iterator
  local t = { foo = 9, bar = 10, 4, 5, 6 }
  local r = {}
  local function dummy() end
  local function f(next)
    for k,v in next,t,nil do r[#r+1] = k; if v == 5 then f(dummy) end end
  end
  f(next)
  assert(#r == 5)
end
