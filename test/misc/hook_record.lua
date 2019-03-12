
if not jit or not jit.status or not jit.status() then return end

debug.sethook(function() for i=1,100 do end end, "", 10)
for i=1,10 do end
debug.sethook()
assert((require("jit.util").traceinfo(1)))

