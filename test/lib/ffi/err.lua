local ffi = require("ffi")

do --- error in FFI metamethod: don't print metamethod frame.
  local ok, err = xpcall(function()
    local x = (1ll).foo
  end, debug.traceback)
  assert(ok == false)
  assert(not string.find(err, "__index"))
end

do --- tailcall in regular metamethod: keep metamethod frame.
  local ok, err = xpcall(function()
    local t = setmetatable({}, {__index = function() return rawget("x") end })
    local y = t[1]
  end, debug.traceback)
  assert(ok == false)
  assert(string.find(err, "__index"))
end

do --- error in FFI metamethod: set correct PC.
  ffi.cdef[[
typedef struct { int x; int y; } ffi_err_point;
ffi_err_point ffi_err_strchr(ffi_err_point* op1, ffi_err_point* op2) asm("strchr");
]]
  local point = ffi.metatype("ffi_err_point", { __add = ffi.C.ffi_err_strchr })
  local function foo()
    local p = point{ 3, 4 }
    local r = p + p
    local r = p + 5
  end
  local ok, err = xpcall(foo, debug.traceback)
  local line = debug.getinfo(foo).linedefined+3
  assert(string.match(err, "traceback:[^:]*:"..line..":"))
end

