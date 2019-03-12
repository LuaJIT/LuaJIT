
do --- square sum
  local function f(x) return x*x end
  local x = 0
  for i=1,100 do
    local ok1, ok2, ok3, y = pcall(pcall, pcall, f, i)
    if not ok1 or not ok2 or not ok3 then break end
    x = x + y
  end
  assert(x == 338350)
end

do --- sqrt square sum
  local x = 0
  for i=1,100 do
    local ok1, ok2, ok3, y = pcall(pcall, pcall, math.sqrt, i*i)
    if not ok1 or not ok2 or not ok3 then break end
    x = x + y
  end
  assert(x == 5050)
end

do --- sum with error
  local function f(x)
    if x >= 150 then error("test", 0) end
    return x end
  local x = 0
  for i=1,200 do
    local ok1, ok2, ok3, y = pcall(pcall, pcall, f, i)
    if not ok1 or not ok2 or not ok3 then
      assert(ok1 and ok2 and not ok3)
      assert(y == "test")
      break
    end
    x = x + y
  end
  assert(x == 11175)
end

do --- sum or square
  local function f(x)
    if x >= 150 then return x*x end
    return x
  end
  local x = 0
  for i=1,200 do
    local ok1, ok2, ok3, y = pcall(pcall, pcall, f, i)
    if not ok1 or not ok2 or not ok3 then break end
    x = x + y
  end
  assert(x == 1584100)
end

do --- sum or square with error
  local function f(x)
    if x >= 150 then
      if x >= 175 then error("test", 0) end
      return x*x
    end
    return x
  end
  local x = 0
  for i=1,200 do
    local ok1, ok2, ok3, y = pcall(pcall, pcall, f, i)
    if not ok1 or not ok2 or not ok3 then
      assert(ok1 and ok2 and not ok3)
      assert(y == "test")
      -- note: no break, so we get an exit to interpreter
    else
      x = x + y
    end
  end
  assert(x == 668575)
end
