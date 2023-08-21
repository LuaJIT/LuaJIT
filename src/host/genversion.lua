----------------------------------------------------------------------------
-- Lua script to embed the rolling release version in luajit.h.
----------------------------------------------------------------------------
-- Copyright (C) 2005-2023 Mike Pall. All rights reserved.
-- Released under the MIT license. See Copyright Notice in luajit.h
----------------------------------------------------------------------------

local FILE_INPUT_H = "luajit_rolling.h"
local FILE_INPUT_R = "luajit_relver.txt"
local FILE_OUTPUT_H = "luajit.h"

local function file_read(file)
  local fp = assert(io.open(file, "rb"), "run from the wrong directory")
  local data = assert(fp:read("*a"))
  fp:close()
  return data
end

local function file_write_mod(file, data)
  local fp = io.open(file, "rb")
  if fp then
    local odata = assert(fp:read("*a"))
    fp:close()
    if odata == data then return end
  end
  fp = assert(io.open(file, "wb"))
  assert(fp:write(data))
  assert(fp:close())
end

local text = file_read(FILE_INPUT_H)
local relver = file_read(FILE_INPUT_R):match("(%d+)")

if relver then
  text = text:gsub("ROLLING", relver)
else
  io.stderr:write([[
**** WARNING Cannot determine rolling release version from git log.
**** WARNING The 'git' command must be available during the build.
]])
end

file_write_mod(FILE_OUTPUT_H, text)
