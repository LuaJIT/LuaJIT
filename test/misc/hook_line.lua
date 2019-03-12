local lines = {}
local function hook()
  lines[#lines+1] = debug.getinfo(2).currentline
end

local function dummy()
end -- <-- line 7

debug.sethook(hook, "l", 0)
-- <-- line 10
local x
dummy()
local y = 1
dummy() dummy()
local z = 2; local r = true
while y < 4 do y = y + 1 end
while z < 4 do
  z = z + 1
end
-- <-- line 20
local v
debug.sethook(nil, "", 0)

assert(#lines > 0)
while lines[1] < 10 do table.remove(lines, 1) end
while lines[#lines] > 20 do table.remove(lines) end

local s = table.concat(lines, " ")
assert(s == "11 12 7 13 14 7 7 15 16 16 16 16 17 18 17 18 17" or
       s == "11 12 7 13 14 7 14 7 15 16 16 16 16 17 18 17 18 17")

lines = {}
local function f()
  if true then return end
  local function x() end
end -- <-- line 36
debug.sethook(hook, "l", 0)
f()
debug.sethook(nil, "", 0)
for i=1,#lines do assert(lines[i] ~= 36) end

