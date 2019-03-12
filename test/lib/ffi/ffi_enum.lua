
local ffi = require("ffi")

dofile("../common/ffi_util.inc")

ffi.cdef[[
typedef enum enum_i { FOO_I = -1, II = 10 } enum_i;
typedef enum enum_u { FOO_U = 1, UU = 10 } enum_u;

enum_i call_ei_i(int a) asm("call_i");
enum_u call_eu_i(int a) asm("call_i");
int call_i_ei(enum_i a) asm("call_i");
int call_i_eu(enum_u a) asm("call_i");
]]

local C = ffi.load("../clib/ctest")

do

  local t = ffi.new("enum_i[100]")
  for i=0,99 do t[i] = "II" end
  for i=0,99 do assert(t[i] == "II") end
  for i=0,99 do assert(t[i] >= "II") end
  for i=0,99 do t[i] = -10 end
  for i=0,99 do assert(t[i] == -10) end
  for i=0,99 do assert(t[i] ~= 2147483648) end
  for i=1,99 do assert(t[i] == t[i-1]) end
  assert(t[0]+1 == -9)
  assert(t[0] ~= "BB")
  fails(function() return t[0] > "BB" end)

  local u = ffi.new("enum_u[100]")
  for i=0,99 do u[i] = "UU" end
  for i=0,99 do assert(u[i] == "UU") end
  for i=0,99 do assert(u[i] >= "UU") end
  for i=0,99 do u[i] = 4294967296-10 end
  for i=0,99 do assert(u[i] == 4294967296-10) end
  for i=0,99 do assert(u[i] ~= -10) end
  for i=1,99 do assert(u[i] == u[i-1]) end
  assert(u[0]+1 == 4294967296-9)

  for i=0,99 do assert(t[i] ~= u[i]) end
end

do
  for i=0,99 do assert(C.call_ei_i(9) == "II") end
  for i=0,99 do assert(C.call_eu_i(9) == "UU") end
  for i=0,99 do assert(C.call_i_ei("II") == 11) end
  for i=0,99 do assert(C.call_i_eu("UU") == 11) end
end

do
  local f = ffi.cast("bool (*)(enum_i)", function(e) return e == "II" end)
  assert(f("II"))
  assert(not f(0))
end

