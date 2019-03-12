local random = math.random
local MAX_SEED = 10

do --- generally uniform in range [0, 1)
  local N = 100
  local min, max = math.min, math.max
  for j=1,MAX_SEED do
    math.randomseed(j)
    local lo, hi, sum = math.huge, -math.huge, 0
    for i=1,N do
      local x = random()
      assert(0 <= x and x < 1)
      sum = sum + x
      lo = min(lo, x)
      hi = max(hi, x)
    end
    assert(lo*N < 15 and (1-hi)*N < 15)
    assert(sum > N*0.45 and sum < N*0.55)
  end
end

do --- all in range [1, 10]
  math.randomseed(1)
  local counts = setmetatable({}, {__index = function() return 0 end})
  for i = 1, 100 do
    local n = random(10)
    counts[n] = counts[n] + 1
  end
  for i = 1, 10 do
    assert(counts[i])
    counts[i] = nil
  end
  assert(not next(counts))
end

do --- all in range [-3, 11]
  math.randomseed(1)
  local seen = setmetatable({}, {__index = function() return false end})
  for i = 1, 120 do
    seen[random(-3, 11)] = true
  end
  for i = -3, 11 do
    assert(seen[i])
    seen[i] = nil
  end
  assert(not next(seen))
end
