local ctest = require("ctest")

local lightud = ctest.lightud
local assert = assert

-- x64 lightud tests
if jit and jit.arch == "x64" then
  do
    local ud1 = lightud(0x12345678)
    local ud2 = lightud(0x12345678)
    assert(ud1 == ud2)
    assert(tostring(ud1) == "userdata: 0x12345678")
  end
  do
    local ud1 = lightud(1)
    local ud2 = lightud(2)
    assert(ud1 ~= ud2)
  end
  do
    local ud1 = lightud(2^47-1)
    local ud2 = lightud(2^47-1)
    assert(ud1 == ud2)
    assert(tostring(ud1) == "userdata: 0x7fffffffffff")
  end
  do
    local ud1 = lightud(0x12345678+123*2^32)
    local ud2 = lightud(0x12345678+456*2^32)
    for i=1,100 do assert(ud1 ~= ud2) end
  end
  assert(tostring(lightud(0x5abc*2^32 + 0xdef01234)) == "userdata: 0x5abcdef01234")
  assert(pcall(lightud, 2^47) == false)
  assert(pcall(lightud, 2^64-2048) == false)
end

assert(getmetatable(lightud(1)) == nil)

-- lightuserdata SLOAD value and HREF key
do
  local ud = lightud(12345)
  local t = {[ud] = 42}
  for i=1,100 do
    assert(t[ud] == 42)
  end
end

-- lightuserdata NEWREF key
do
  local ud = lightud(12345)
  for i=1,100 do
    local t = {[ud] = 42}
    assert(t[ud] == 42)
  end
end

-- lightuserdata ASTORE/HSTORE value
do
  local ud = lightud(12345)
  local t = {}
  for i=1,100 do
    t[i] = ud
  end
  assert(t[100] == ud)
end

-- lightuserdata sync to stack
do
  local ud = lightud(12345)
  local x = nil
  for j=1,20 do
    for i=1,50 do
      x = ud
    end
    assert(x == ud)
  end
end

-- lightuserdata vs. number type check
do
  local t = {}
  for i=1,200 do t[i] = i end
  t[180] = lightud(12345)
  local x = 0
  assert(not pcall(function(t)
    for i=1,200 do x = x + t[i] end
  end, t))
  assert(x == 16110)
end

