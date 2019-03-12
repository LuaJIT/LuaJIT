-- DSE of USTORE must eliminate OBAR, too.

if jit and jit.opt then pcall(jit.opt.start, "-sink") end

local f
do
  local x
  f = function()
    local y = 0
    for i=1,10000 do
      x = {1}
      if y > 0 then end
      x = 1
    end
  end
end

collectgarbage()
collectgarbage("setstepmul", 1)
collectgarbage("restart")
f()

