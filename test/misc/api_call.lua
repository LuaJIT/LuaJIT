local ctest = require("ctest")

local function ret0() end
local function ret1() return 1 end
local function ret2() return 1,2 end
local function ret3() return 1,2,3 end
local function retva(...) return ... end
local function ret1va(...) return 1,... end

local function pack(...)
  return { n = select('#', ...), ... }
end

local function ck(res, ...)
  local ok = pack(...)
  if res.n ~= ok.n then error("nresults wrong: "..res.n.." ~= "..ok.n, 2) end
  for i=1,res.n do
    if res[i] ~= ok[i] then
      error("result["..i.."] wrong: "..tostring(res[i]).." ~= "..tostring(ok[i]), 2)
    end
  end
end

local function test_adjust_results(testfunc)

  local function cc(nres, f, ...)
    return pack(testfunc(nres, f, ...))
  end

  ck(cc(0, ret0))
  ck(cc(0, ret1))
  ck(cc(0, ret2))
  ck(cc(0, ret3))
  ck(cc(0, retva))

  ck(cc(1, ret0), nil)
  ck(cc(1, ret1), 1)
  ck(cc(1, ret2), 1)
  ck(cc(1, ret3), 1)
  ck(cc(1, retva), nil)
  ck(cc(1, retva, 1), 1)

  ck(cc(2, ret0), nil, nil)
  ck(cc(2, ret1), 1, nil)
  ck(cc(2, ret2), 1, 2)
  ck(cc(2, ret3), 1, 2)
  ck(cc(2, retva), nil, nil)
  ck(cc(2, retva, 1), 1, nil)
  ck(cc(2, retva, 1, 2), 1, 2)

  ck(cc(-1, ret0))
  ck(cc(-1, ret1), 1)
  ck(cc(-1, ret2), 1, 2)
  ck(cc(-1, ret3), 1, 2, 3)
  ck(cc(-1, retva))
  ck(cc(-1, retva, 1), 1)
  ck(cc(-1, retva, 1, 2), 1, 2)
end

test_adjust_results(ctest.call)
test_adjust_results(ctest.pcall_err)


local function gcshrink()
  for i=1,10 do collectgarbage() end
end

assert(select('#', ctest.call(2000, gcshrink)) == 2000)
gcshrink()
assert(select('#', ctest.call(7000, gcshrink)) == 7000)
gcshrink()

local function test_yield(resume, yield)
  local function inpcall()
    ck(pack(yield(6, 7)), 18, 19)
  end
  local co = coroutine.create(function(...)
    ck(pack(...), 11, 12)
    ck(pack(yield(1, 2)))
    ck(pack(yield()), 13, 14, 15)
    ck(pack(yield(3, 4, 5)), 16, 17)
    assert(pcall(inpcall) == true)
    return 8, 9
  end)

  ck(pack(resume(co, 11, 12)), true, 1, 2)
  ck(pack(resume(co)), true)
  ck(pack(resume(co, 13, 14, 15)), true, 3, 4, 5)
  ck(pack(resume(co, 16, 17)), true, 6, 7)
  ck(pack(resume(co, 18, 19)), true, 8, 9)
  assert(resume(co) == false)
end

test_yield(coroutine.resume, coroutine.yield)
test_yield(ctest.resume, coroutine.yield)
test_yield(coroutine.resume, ctest.yield)
test_yield(ctest.resume, ctest.yield)

