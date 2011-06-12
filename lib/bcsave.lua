----------------------------------------------------------------------------
-- LuaJIT module to save/list bytecode.
--
-- Copyright (C) 2005-2011 Mike Pall. All rights reserved.
-- Released under the MIT/X license. See Copyright Notice in luajit.h
----------------------------------------------------------------------------
--
-- This module saves or lists the bytecode for an input file.
-- It's run by the -b command line option.
--
------------------------------------------------------------------------------

-- Cache some library functions and objects.
local jit = require("jit")
assert(jit.version_num == 20000, "LuaJIT core/library version mismatch")

------------------------------------------------------------------------------

local function usage()
  io.stderr:write[[
Save LuaJIT bytecode: luajit -b[options] input output
  -l        Only list bytecode.
  -s        Strip debug info (default).
  -g        Keep debug info.
  -e chunk  Use chunk string as input.
  --        Stop handling options.
  -         Use stdin as input and/or stdout as output.
]]
  os.exit(1)
end

local function readfile(input)
  if type(input) == "function" then return input end
  if input == "-" then input = nil end
  local f, err = loadfile(input)
  if not f then
    io.stderr:write("luajit: ", err, "\n")
    os.exit(1)
  end
  return f
end

local function readstring(input)
  local f, err = loadstring(input)
  if not f then
    io.stderr:write("luajit: ", err, "\n")
    os.exit(1)
  end
  return f
end

local function savefile(name, mode)
  if name == "-" then return io.stdout end
  local fp, err = io.open(name, mode)
  if not fp then
    io.stderr:write("luajit: cannot write ", err, "\n")
    os.exit(1)
  end
  return fp
end

------------------------------------------------------------------------------

local function bclist(input, output)
  local f = readfile(input)
  require("jit.bc").dump(f, savefile(output, "w"), true)
end

local function bcsave(input, output, strip)
  local f = readfile(input)
  local s = string.dump(f, strip)
  local fp = savefile(output, "wb")
  local ok, err = fp:write(s)
  if ok and output ~= "-" then ok, err = fp:close() end
  if not ok then
    io.stderr:write("luajit: cannot write ", arg[2], ": ", err, "\n")
    os.exit(1)
  end
end

local function docmd(...)
  local arg = {...}
  local n = 1
  local list = false
  local strip = true
  while n <= #arg do
    local a = arg[n]
    if type(a) == "string" and string.sub(a, 1, 1) == "-" and a ~= "-" then
      if a == "--" then table.remove(arg, n); break end
      for m=2,#a do
	local opt = string.sub(a, m, m)
	if opt == "l" then
	  list = true
	elseif opt == "s" then
	  strip = true
	elseif opt == "g" then
	  strip = false
	elseif opt == "e" then
	  if n ~= 1 or #arg < 2 or m ~= #a then usage() end
	  arg[2] = readstring(arg[2])
	else
	  usage()
	end
      end
      table.remove(arg, n)
    else
      n = n + 1
    end
  end
  if list then
    if #arg == 0 or #arg > 2 then usage() end
    bclist(arg[1], arg[2] or "-")
  else
    if #arg ~= 2 then usage() end
    bcsave(arg[1], arg[2], strip)
  end
end

------------------------------------------------------------------------------

-- Public module functions.
module(...)

start = docmd -- Process -b command line option.

