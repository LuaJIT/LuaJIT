
local bit = require("bit")
local bnot, bor, band = bit.bnot, bit.bor, bit.band
local shl, shr = bit.lshift, bit.rshift

local N = tonumber(arg and arg[1]) or 10000000
local cache, m, n = { 1 }, 1, 1
if arg and arg[2] then cache = nil end
for i=2,N do
  local j = i
  for len=1,1000000000 do
    j = bor(band(shr(j,1), band(j,1)-1), band(shl(j,1)+j+1, bnot(band(j,1)-1)))
    if cache then
      local x = cache[j]; if x then j = x+len; break end
    elseif j == 1 then
      j = len+1; break
    end
  end
  if cache then cache[i] = j end
  if j > m then m, n = j, i end
end
io.write("Found ", n, " (chain length: ", m, ")\n")
