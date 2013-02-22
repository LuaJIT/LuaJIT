----------------------------------------------------------------------------
-- Lua script to dump the bytecode of the library functions written in Lua.
-- The resulting 'buildvm_libbc.h' is used for the build process of LuaJIT.
----------------------------------------------------------------------------
-- Copyright (C) 2005-2013 Mike Pall. All rights reserved.
-- Released under the MIT license. See Copyright Notice in luajit.h
----------------------------------------------------------------------------

local function usage(arg)
  io.stderr:write("Usage: ", arg and arg[0] or "genlibbc",
		  " [-o buildvm_libbc.h] lib_*.c\n")
  os.exit(1)
end

local function parse_arg(arg)
  local outfile = "-"
  if not (arg and arg[1]) then
    usage(arg)
  end
  if arg[1] == "-o" then
    outfile = arg[2]
    if not outfile then usage(arg) end
    table.remove(arg, 1)
    table.remove(arg, 1)
  end
  return outfile
end

local function read_files(names)
  local src = ""
  for _,name in ipairs(names) do
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

local function gen_header(defs)
  local t = {}
  local function w(x) t[#t+1] = x end
  w("/* This is a generated file. DO NOT EDIT! */\n\n")
  w("static const int libbc_endian = ")
  w(string.byte(string.dump(function() end), 5) % 2)
  w(";\n\n")
  local s = ""
  for _,name in ipairs(defs) do
    s = s .. defs[name]
  end
  w("static const uint8_t libbc_code[] = {\n")
  local n = 0
  for i=1,#s do
    local x = string.byte(s, i)
    w(x); w(",")
    n = n + (x < 10 and 2 or (x < 100 and 3 or 4))
    if n >= 75 then n = 0; w("\n") end
  end
  w("0\n};\n\n")
  w("static const struct { const char *name; int ofs; } libbc_map[] = {\n")
  local m = 0
  for _,name in ipairs(defs) do
    w('{"'); w(name); w('",'); w(m) w('},\n')
    m = m + #defs[name]
  end
  w("{NULL,"); w(m); w("}\n};\n\n")
  return table.concat(t)
end

local function write_file(name, data)
  if name == "-" then
    assert(io.write(data))
    assert(io.flush())
  else
    local fp = io.open(name)
    if fp then
      local old = fp:read("*a")
      fp:close()
      if data == old then return end
    end
    fp = assert(io.open(name, "w"))
    assert(fp:write(data))
    assert(fp:close())
  end
end

local outfile = parse_arg(arg)
local src = read_files(arg)
local defs = find_defs(src)
local hdr = gen_header(defs)
write_file(outfile, hdr)

