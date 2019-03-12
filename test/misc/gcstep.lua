
local function testgc(what, func)
  collectgarbage()
  local oc = gcinfo()
  func()
  local nc = gcinfo()
  assert(nc < oc*4, "GC step missing for "..what)
end

testgc("TNEW", function()
  for i=1,10000 do
    local t = {}
  end
end)

testgc("TDUP", function()
  for i=1,10000 do
    local t = {1}
  end
end)

testgc("FNEW", function()
  for i=1,10000 do
    local function f() end
  end
end)

testgc("CAT", function()
  for i=1,10000 do
    local s = "x"..i
  end
end)

