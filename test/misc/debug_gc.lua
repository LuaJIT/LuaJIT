
-- Do not run this test unless the JIT compiler is turned off.
if jit and jit.status and jit.status() then return end

local caught, caught_line, caught_mm

local function gcmeta()
  if caught ~= "end" then
--    print(debug.traceback())
    -- This may point to the wrong instruction if in a JIT trace.
    -- But there's no guarantee if, when or where any GC steps occur.
    local dbg = debug.getinfo(2)
    caught_line = dbg.currentline
    caught_mm = debug.getinfo(1).name
    caught = true
  end
end

local function testgc(mm, f)
  collectgarbage()
  caught = false
  local u = newproxy(true)
  getmetatable(u).__gc = gcmeta
  u = nil
  for i=1,100000 do
    f(i)
    -- This check may be hoisted. __gc is not supposed to have side-effects.
    if caught then break end
  end
  if not caught then
    error(mm.." metamethod not called", 2)
  end
  if type(caught_line) ~= "number" or caught_line < 0 then
    error("bad linenumber in debug info", 2)
  end
  if caught_mm ~= mm then
    error("bad name for metamethod in debug info", 2)
  end
end

local x
testgc("__gc", function(i) x = {} end)
testgc("__gc", function(i) x = {1} end)
testgc("__gc", function(i) x = function() end end)
testgc("__concat", function(i) x = i.."" end)

caught = "end"
