
local function nsieve(p, m)
  for i=2,m do p[i] = true end
  local count = 0
  for i=2,m do
    if p[i] then
      for k=i+i,m,i do p[k] = false end
      count = count + 1
    end
  end
  return count
end

local N = tonumber(arg and arg[1]) or 1
if N < 2 then N = 2 end
local primes = {}

for i=0,2 do
  local m = (2^(N-i))*10000
  io.write(string.format("Primes up to %8d %8d\n", m, nsieve(primes, m)))
end
