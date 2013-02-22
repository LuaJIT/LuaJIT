----------------------------------------------------------------------------
-- Lua script to dump the bytecode of the library functions written in Lua.
-- The resulting 'buildvm_libbc.h' is used for the build process of LuaJIT.
----------------------------------------------------------------------------
-- Copyright (C) 2005-2013 Mike Pall. All rights reserved.
-- Released under the MIT license. See Copyright Notice in luajit.h
----------------------------------------------------------------------------

local function usage()
  io.stderr:write("Usage: ", arg and arg[0] or "genlibbc", " lib_*.c\n")
  os.exit(1)
end

local function read_source()
  if not (arg and arg[1]) then usage() end
  local src = ""
  for _,name in ipairs(arg) do
    local fp = assert(io.open(name))
    src = src .. fp:read("*a")
    fp:close()
  end
  return src
end

local function find_defs(src)
  local defs = {}
  for name, code in string.gmatch(src, "LJLIB_LUA%(([^)]*)%)%s*/%*(.-)%*/") do
    local env = {}
    local func = assert(load("return "..code, "", nil, env))()
    local d = string.dump(func, true)
    local ofs = 6
    while string.byte(d, ofs) > 127 do ofs = ofs + 1 end
    defs[name] = string.sub(d, ofs+1, -2)
    defs[#defs+1] = name
  end
  return defs
end

local function write_defs(fp, defs)
  fp:write("/* This is a generated file. DO NOT EDIT! */\n\n")
  fp:write("static const int libbc_endian = ",
	   string.byte(string.dump(function() end), 5) % 2, ";\n\n")
  local s = ""
  for _,name in ipairs(defs) do
    s = s .. defs[name]
  end
  fp:write("static const uint8_t libbc_code[] = {\n")
  local n = 0
  for i=1,#s do
    local x = string.byte(s, i)
    fp:write(x, ",")
    n = n + (x < 10 and 2 or (x < 100 and 3 or 4))
    if n >= 75 then n = 0; fp:write("\n") end
  end
  fp:write("0\n};\n\n")
  fp:write("static const struct { const char *name; int ofs; } libbc_map[] = {\n")
  local m = 0
  for _,name in ipairs(defs) do
    fp:write('{"', name, '",', m, '},\n')
    m = m + #defs[name]
  end
  fp:write("{NULL,", m, "}\n};\n\n")
  fp:flush()
end

local src = read_source()
local defs = find_defs(src)
write_defs(io.stdout, defs)
