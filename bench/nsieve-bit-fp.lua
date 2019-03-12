
local floor, ceil = math.floor, math.ceil

local precision = 50 -- Maximum precision of lua_Number (minus safety margin).
local onebits = (2^precision)-1

local function nsieve(p, m)
  local cm = ceil(m/precision)
  do local onebits = onebits; for i=0,cm do p[i] = onebits end end
  local count, idx, bit = 0, 2, 2
  for i=2,m do
    local r = p[idx] / bit
    if r - floor(r) >= 0.5 then -- Bit set?
      local kidx, kbit = idx, bit
      for k=i+i,m,i do
        kidx = kidx + i
        while kidx >= cm do kidx = kidx - cm; kbit = kbit + kbit end
        local x = p[kidx]
        local r = x / kbit
        if r - floor(r) >= 0.5 then p[kidx] = x - kbit*0.5 end -- Clear bit.
      end
      count = count + 1
    end
    idx = idx + 1
    if idx >= cm then idx = 0; bit = bit + bit end
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
