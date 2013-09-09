----------------------------------------------------------------------------
-- LuaJIT profiler.
--
-- Copyright (C) 2005-2013 Mike Pall. All rights reserved.
-- Released under the MIT license. See Copyright Notice in luajit.h
----------------------------------------------------------------------------
--
-- This module is a simple command line interface to the built-in
-- low-overhead profiler of LuaJIT.
--
-- The lower-level API of the profiler is accessible via the "jit.profile"
-- module or the luaJIT_profile_* C API.
--
-- Example usage:
--
--   luajit -jp myapp.lua
--   luajit -jp=s myapp.lua
--   luajit -jp=-s myapp.lua
--   luajit -jp=vl myapp.lua
--   luajit -jp=G,profile.txt myapp.lua
--
-- The following dump features are available:
--
--   f  Stack dump: function name, Otherwise module:line. Default mode
--   F  Stack dump: ditto, but always prepend module.
--   l  Stack dump: module:line.
--   <number> stack dump depth (callee < caller). Default: 1.
--   -<number> Inverse stack dump depth (caller > callee).
--   s  Split stack dump after first stack level. Implies abs(depth) >= 2.
--   p  Show full path for module names.
--   v  Show VM states. Can be combined with stack dumps, e.g. vf or fv.
--   z  Show zones. Can be combined with stack dumps, e.g. zf or fz.
--   r  Show raw sample counts. Default: show percentages.
--   G  Produce output suitable for graphical tools (e.g. flame graphs).
--   m<number> Minimum sample percentage to be shown. Default: 3.
--   i<number> Sampling interval in milliseconds. Default: 10.
--
----------------------------------------------------------------------------

-- Cache some library functions and objects.
local jit = require("jit")
assert(jit.version_num == 20100, "LuaJIT core/library version mismatch")
local profile = require("jit.profile")
local vmdef = require("jit.vmdef")
local pairs, tonumber, floor, min = pairs, tonumber, math.floor, math.min
local sort, format = table.sort, string.format
local stdout = io.stdout
local zone -- Load jit.zone module on demand.

-- Output file handle.
local out

------------------------------------------------------------------------------

local prof_ud
local prof_states, prof_split, prof_min, prof_raw, prof_fmt, prof_depth
local prof_count1, prof_count2, prof_samples

local map_vmmode = {
  N = "Compiled",
  I = "Interpreted",
  C = "C code",
  G = "Garbage Collector",
  J = "JIT Compiler",
}

-- Profiler callback.
local function prof_cb(th, samples, vmmode)
  prof_samples = prof_samples + samples
  local key_stack, key_stack2, key_state
  -- Collect keys for sample.
  if prof_states then
    if prof_states == "v" then
      key_state = map_vmmode[vmmode] or vmmode
    else
      key_state = zone:get() or "(none)"
    end
  end
  if prof_fmt then
    key_stack = profile.dumpstack(th, prof_fmt, prof_depth)
    key_stack = key_stack:gsub("%[builtin#(%d+)%]", function(x)
      return vmdef.ffnames[tonumber(x)]
    end)
    if prof_split == 2 then
      local k1, k2 = key_stack:match("(.-) [<>] (.*)")
      if k2 then key_stack, key_stack2 = k1, k2 end
    end
  end
  -- Order keys.
  local k1, k2
  if prof_split == 1 then
    if key_state then
      k1 = key_state
      if key_stack then k2 = key_stack end
    end
  elseif key_stack then
    k1 = key_stack
    if key_stack2 then k2 = key_stack2 elseif key_state then k2 = key_state end
  end
  -- Coalesce samples in one or two levels.
  if k1 then
    local t1 = prof_count1
    t1[k1] = (t1[k1] or 0) + samples
    if k2 then
      local t2 = prof_count2
      local t3 = t2[k1]
      if not t3 then t3 = {}; t2[k1] = t3 end
      t3[k2] = (t3[k2] or 0) + samples
    end
  end
end

------------------------------------------------------------------------------

-- Show top N list.
local function prof_top(count1, count2, samples, indent)
  local t, n = {}, 0
  for k, v in pairs(count1) do
    n = n + 1
    t[n] = k
  end
  sort(t, function(a, b) return count1[a] > count1[b] end)
  local raw = prof_raw
  for i=1,n do
    local k = t[i]
    local v = count1[k]
    local pct = floor(v*100/samples + 0.5)
    if pct < prof_min then break end
    if not raw then
      out:write(format("%s%2d%%  %s\n", indent, pct, k))
    elseif raw == "r" then
      out:write(format("%s%5d  %s\n", indent, v, k))
    else
      out:write(format("%s %d\n", k, v))
    end
    if count2 then
      local r = count2[k]
      if r then
	prof_top(r, nil, v, prof_depth < 0 and "  -> " or "  <- ")
      end
    end
  end
end

------------------------------------------------------------------------------

-- Finish profiling and dump result.
local function prof_finish()
  if prof_ud then
    profile.stop()
    local samples = prof_samples
    if samples == 0 then
      if prof_raw ~= true then out:write("[no samples collected]\n") end
      return
    end
    prof_top(prof_count1, prof_count2, samples, "")
    prof_count1 = nil
    prof_count2 = nil
    prof_ud = nil
  end
end

-- Start profiling.
local function prof_start(mode)
  local interval = ""
  mode = mode:gsub("i%d*", function(s) interval = s; return "" end)
  prof_min = 3
  mode = mode:gsub("m(%d+)", function(s) prof_min = tonumber(s); return "" end)
  prof_depth = 1
  mode = mode:gsub("%-?%d+", function(s) prof_depth = tonumber(s); return "" end)
  local m = {}
  for c in mode:gmatch(".") do m[c] = c end
  prof_states = m.z or m.v
  if prof_states == "z" then zone = require("jit.zone") end
  local scope = m.l or m.f or m.F or (prof_states and "" or "f")
  local flags = (m.p or "")
  prof_raw = m.r
  if m.s then
    prof_split = 2
    if prof_depth == -1 or m["-"] then prof_depth = -2
    elseif prof_depth == 1 then prof_depth = 2 end
  else
    prof_split = (scope == "" or mode:find("[zv].*[lfF]")) and 1 or 0
  end
  if m.G and scope ~= "" then
    prof_fmt = flags..scope.."Z;"
    prof_depth = -100
    prof_raw = true
    prof_min = 0
  elseif scope == "" then
    prof_fmt = false
  else
    prof_fmt = flags..scope..(prof_depth >= 0 and "Z < " or "Z > ")
  end
  prof_count1 = {}
  prof_count2 = {}
  prof_samples = 0
  profile.start(scope:lower()..interval, prof_cb)
  prof_ud = newproxy(true)
  getmetatable(prof_ud).__gc = prof_finish
end

------------------------------------------------------------------------------

local function start(mode, outfile)
  if not outfile then outfile = os.getenv("LUAJIT_PROFILEFILE") end
  if outfile then
    out = outfile == "-" and stdout or assert(io.open(outfile, "w"))
  else
    out = stdout
  end
  prof_start(mode or "f")
end

-- Public module functions.
return {
  start = start, -- For -j command line option.
  stop = prof_finish
}

