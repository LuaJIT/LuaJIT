
local bit = require("bit")

local Rm = {}
for i=0,16 do Rm[i] = 0 end

for k=1,10 do
  local seed = 1
  for i=16,0,-1 do
    seed = bit.band(seed*9069, 0x7fffffff)
    Rm[i] = seed
  end
  assert(seed == 1952688301)
end

local retindex = 0
local retdata = { 3, 1, 1, 1, 0, 3, 1, 0, 0, 2, 0, 2, 0, 0, 3, 1, 1, 1, 1 }

local function get_bits()
  retindex = retindex + 1
  return retdata[retindex]
end

local hufcodes = { [0] = true, [4] = true, [11] = true, [36] = true, [68] = true }

local maskhuf = { 0x0002, 0x0003, 0x0004, 0x0005, }

local function decodeCode()
  local lookup = get_bits()
  local code = hufcodes[lookup]
  local z = {1,1,1,1}
  if not code then
    for i = 1, 4 do
      lookup = bit.bor(lookup, bit.lshift(get_bits(), i + 1))
      -- need PHI for CONV num.int of lookup, used in snapshot
      code = hufcodes[lookup + maskhuf[i]]
      if code then break end
    end
  end
  assert(code)
  return code
end

local function test()
  for i = 1, 6 do
    decodeCode()
  end
end

if jit and jit.status and jit.status() then jit.opt.start("hotloop=1") end

test()

