local loadstring = loadstring or load

do --- Must unpatch modified bytecode with ILOOP/JLOOP etc.
  local function foo()
    local t = {}
    for i=1,100 do t[i] = i end
    for a,b in ipairs(t) do end
    local m = 0
    while m < 100 do m = m + 1 end
  end

  local d1 = string.dump(foo)
  foo()
  assert(string.dump(foo) == d1)
  if jit then jit.off(foo) end
  foo()
  assert(string.dump(foo) == d1)
  local d2 = string.dump(loadstring(d1, ""), true)
  local d3 = string.dump(assert(loadstring(d2, "")), true)
  assert(d2 == d3)
  assert(loadstring(string.dump(assert(loadstring(d2, "")))))
end

do --- roundtrip constants
  local function f1() return -0x80000000 end
  local function f2() return 0.971234567 end
  assert(f1() == -0x80000000)
  assert(loadstring(string.dump(f1), "")() == -0x80000000)
  assert(f2() == 0.971234567)
  assert(loadstring(string.dump(f2), "")() == 0.971234567)
end
