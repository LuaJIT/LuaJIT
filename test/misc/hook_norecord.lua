
if not jit or not jit.status or not jit.status() then return end

local called = false
local function f() local x = "wrong"; called = true end
jit.off(f)
debug.sethook(f, "", 5)
for i=1,1000 do local a,b,c,d,e,f=1,2,3,4,5,6 end
assert(called)
-- Check that no trace was generated.
assert(require("jit.util").traceinfo(1) == nil)

