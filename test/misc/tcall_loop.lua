local function f(i)
  if i > 0 then return f(i-1) end
  return 1
end

local x = 0
for i=1,100 do x = x + f(1000) end
assert(x == 100)
