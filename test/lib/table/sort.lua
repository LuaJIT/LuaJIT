-- Really a test for lua_lessthan()
local N = 1000

do --- numbers
  math.randomseed(42)
  local t = {}
  for i=1,N do t[i] = math.random(N) end
  table.sort(t)
  for i=2,N do assert(t[i-1] <= t[i]) end
end

do --- strings
  math.randomseed(42)
  local t = {}
  for i=1,N do t[i] = math.random(1, N/10).."" end
  table.sort(t)
  for i=2,N do assert(t[i-1] <= t[i]) end
end

do --- tables
  math.randomseed(42)
  local mt = { __lt = function(a,b) return a[1] < b[1] end }
  local t = {}
  for i=1,N do t[i] = setmetatable({ math.random(N) }, mt) end
  table.sort(t)
  for i=2,N do assert(t[i-1][1] <= t[i][1]) end
end
