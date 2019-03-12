-- This should be turned into a proper module and not use globals.
-- Or combined into a generiv test utility module. With FFI
-- functionality turned off, if the FFI module is not built-in.

local ffi = require("ffi")

function checkfail(t, f)
  f = f or ffi.typeof
  for i=1,1e9 do
    local tp = t[i]
    if not tp then break end
    assert(pcall(f, tp) == false, tp)
  end
end

function checktypes(t)
  for i=1,1e9,3 do
    local tp = t[i+2]
    if not tp then break end
    local id = ffi.typeof(tp)
    assert(ffi.sizeof(id) == t[i], tp)
    assert(ffi.alignof(id) == t[i+1], tp)
  end
end

function fails(f, ...)
  if pcall(f, ...) ~= false then error("failure expected", 2) end
end

local incroot = os.getenv("INCROOT") or "/usr/include"
local cdefs = os.getenv("CDEFS") or ""

function include(name)
  local flags = ffi.abi("32bit") and "-m32" or "-m64"
  if string.sub(name, 1, 1) ~= "/" then name = incroot.."/"..name end
  local fp = assert(io.popen("cc -E -P "..flags.." "..cdefs.." "..name))
  local s = fp:read("*a")
  fp:close()
  ffi.cdef(s)
end

