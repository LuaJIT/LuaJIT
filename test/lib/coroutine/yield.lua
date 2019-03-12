local create = coroutine.create
local wrap = coroutine.wrap
local resume = coroutine.resume
local yield = coroutine.yield

do --- Stack overflow on return (create)
  wrap(function()
    local co = create(function()
      yield(string.byte(string.rep(" ", 100), 1, 100))
    end)
    assert(select('#', resume(co)) == 101)
  end)()
end

do --- Stack overflow on return (wrap)
  wrap(function()
    local f = wrap(function()
      yield(string.byte(string.rep(" ", 100), 1, 100))
    end)
    assert(select('#', f()) == 100)
  end)()
end

do --- cogen
  local function cogen(x)
    return wrap(function(n) repeat x = x+n; n = yield(x) until false end),
	   wrap(function(n) repeat x = x*n; n = yield(x) until false end)
  end

  local a,b=cogen(3)
  local c,d=cogen(5)
  assert(d(b(c(a(d(b(c(a(1)))))))) == 168428160)
end

do --- cofunc +luajit
  local function verify(what, expect, ...)
    local got = {...}
    for i=1,100 do
      if expect[i] ~= got[i] then
        error("FAIL " .. what)
      end
      if expect[i] == nil then
        break
      end
    end
  end

  local function cofunc(...)
    verify("call", { 1, "foo" }, ...)
    verify("yield", { "bar" }, yield(2, "test"))
    verify("pcall yield", { true, "again" }, pcall(yield, "from pcall"))
    return "end"
  end

  local co = create(cofunc)
  verify("resume", { true, 2, "test" }, resume(co, 1, "foo"))
  verify("resume pcall", { true, "from pcall" }, resume(co, "bar"))
  verify("resume end", { true, "end" }, resume(co, "again"))
end

do --- assorted +luajit
  local function verify(expect, func, ...)
    local co = create(func)
    for i=1,100 do
      local ok, res = resume(co, ...)
      if not ok then
        if expect[i] ~= nil then
          error("too few results: ["..i.."] = "..tostring(expect[i]).." (got: "..tostring(res)..")")
        end
        break
      end
      if expect[i] ~= res then
        error("bad result: ["..i.."] = "..tostring(res).." (should be: "..tostring(expect[i])..")")
      end
    end
  end

  verify({ 42, 99 },
    function(x) pcall(yield, x) return 99 end,
    42)

  verify({ 42, 99 },
    function(x) pcall(function(y) yield(y) end, x) return 99 end,
    42)

  verify({ 42, 99 },
    function(x) xpcall(yield, debug.traceback, x) return 99 end,
    42)

  verify({ 45, 44, 43, 42, 99 },
    function(x, y)
      for i in
        function(o, k)
          yield(o+k)
          if k ~= 0 then return k-1 end
        end,x,y do
      end
      return 99
    end,
    42, 3)

  verify({ 84, 99 },
    function(x)
      local o = setmetatable({ x },
        {__add = function(a, b) yield(a[1]+b[1]) return 99 end })
      return o+o
    end,
    42)
end
