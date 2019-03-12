
local function f()
  f()
end

local err, s = xpcall(f, debug.traceback)
assert(err == false)

local first = string.match(s, "[^\n]+")
local line = debug.getinfo(f, "S").linedefined+1
assert(string.match(first, ":"..line..": stack overflow$"))

local n = 1
for _ in string.gmatch(s, "\n") do n = n + 1 end
assert(n == 1+1+11+1+10)

local function g(i)
  g(i)
end

local err, s = xpcall(g, debug.traceback, 1)
assert(err == false)

--[[
-- too slow
local function vtail(...)
  return vtail(1, ...)
end

local err, s = xpcall(vtail, debug.traceback, 1)
assert(err == false)
--]]

local function vcall(...)
  vcall(1, ...)
end

local err, s = xpcall(vcall, debug.traceback, 1)
assert(err == false)

