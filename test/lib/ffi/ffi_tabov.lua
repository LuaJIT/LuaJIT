local ffi = require("ffi")

local last = 0

assert(pcall(function()
  for i=1,65536 do
    last = i
    ffi.typeof"struct {}"
  end
end) == false)

assert(last > 20000)
