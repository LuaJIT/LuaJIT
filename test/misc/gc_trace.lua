
if not jit or not jit.status or not jit.status() then return end

collectgarbage()
for j=1,100 do
  loadstring("for i=1,100 do end")()
end
local jutil = require("jit.util")
assert(jutil.traceinfo(90) == nil)
collectgarbage()
assert(jutil.traceinfo(1) == nil)
assert(jutil.traceinfo(2) == nil)
assert(jutil.traceinfo(3) == nil)

do
  local f
  local function reccb(tr)
    if f == nil then
      collectgarbage()
      local info = jutil.traceinfo(tr)
      jutil.tracek(tr, -info.nk)
      -- Error in lj_ir_kvalue() if KGC not marked.
      -- Only caught with assertions or Valgrind.
    end
  end
  jit.attach(reccb, "record")
  for i=1,200 do
    if i % 5 == 0 then
      f = function() end
    elseif f then
      f()
      f = nil
    end
  end
  jit.attach(reccb)
end

