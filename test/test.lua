local assert, io_open, io_lines, io_write, load, type, xpcall =
      assert, io.open, io.lines, io.write, load, type, xpcall
local debug_traceback, math_random, tonumber, loadstring =
      debug.traceback, math.random, tonumber, loadstring or load

local dirsep = package.config:match"^(.-)\n"
local own_file = debug.getinfo(1, "S").source:match"^@(.*)" or arg[0]
local own_dir = own_file:match("^.*[/".. dirsep .."]")

local function default_tags()
  local tags = {}
  
  -- Lua version and features
  tags.lua = tonumber(_VERSION:match"%d+%.%d+")
  if table.pack then
    tags["compat5.2"] = true
  end
  if loadstring"return 0xep+9" then
    tags.hexfloat = true
  end
  if loadstring"goto x ::x::" then
    tags["goto"] = true
  end

  -- Libraries
  for _, lib in ipairs{"bit", "ffi", "jit.profile", "table.new"} do
    if pcall(require, lib) then
      tags[lib] = true
    end
  end

  -- LuaJIT-specific
  if jit then
    tags.luajit = tonumber(jit.version:match"%d+%.%d+")
    tags[jit.arch:lower()] = true
    if jit.os ~= "Other" then
      tags[jit.os:lower()] = true
    end
    if jit.status() then
      tags.jit = true
    end
    for _, flag in ipairs{select(2, jit.status())} do
      tags[flag:lower()] = true
    end
  end
  
  -- Environment
  if dirsep == "\\" then
    tags.windows = true
  end
  if tags.ffi then
    local abi = require"ffi".abi
    for _, param in ipairs{"le", "be", "fpu", "softfp", "hardfp", "eabi"} do
      if abi(param) then
        tags[param] = true
      end
    end
    if abi"win" then tags.winabi = true end
    if abi"32bit" then tags.abi32 = true end
    if abi"64bit" then tags.abi64 = true end
  else
    local bytecode = string.dump(function()end)
    if bytecode:find"^\27Lua[\80-\89]" then
      tags[bytecode:byte(7, 7) == 0 and "be" or "le"] = true
      tags["abi".. (bytecode:byte(9, 9) * 8)] = true
    end
  end
  
  return tags
end

local function want_meta(opts, meta)
  if not opts.want_meta_cache then
    opts.want_meta_cache = setmetatable({}, {__index = function(t, meta)
      local result = true
      for polarity, tag, cond in meta:gmatch"([+-])([^ <>=]+)([<>=0-9.]*)" do
        local tagval = opts.tags[tag]
        local condresult
        if cond == "" or not tagval then
          condresult = tagval
        else
          condresult = assert(loadstring("return (...) ".. cond))(tagval)
        end
        if polarity == "-" then
          condresult = not condresult
        end
        if not condresult then
          result = false
          break
        end
      end
      t[meta] = result
      return result
    end})
  end
  return opts.want_meta_cache[meta]
end

local function parse_args(t)
  local opts = {
    tags = default_tags(),
    want_meta = want_meta,
  }
  local result = opts
  
  local i, tlen = 1, #t
  local joinedval = ""
  local function flagval()
    local val
    if joinedval ~= "" then
      val = joinedval:sub(2)
      joinedval = ""
    else
      val = t[i]
      if not val then error("Expected value after ".. t[i-1]) end
      i = i + 1
    end
    return val
  end
  
  while i <= tlen do
    local arg = t[i]
    i = i + 1
    if arg:sub(1, 2) == "--" then
      arg, joinedval = arg:match"^([^=]+)(=?.*)$"
      if arg == "--quiet" then
        opts.quiet = true
      elseif arg == "--shuffle" then
        local seed = tonumber(flagval())
        if not seed then error("Expected numeric seed after --shuffle") end
        opts.shuffle = seed
      elseif arg == "--shard" then
        local i, s = flagval():match"^(%d+)/(%d+)$"
        if not s then error("Expected integer/integer after --shard") end
        opts.shard = {initial = tonumber(i), step = tonumber(s)}
      elseif arg == "--version" then
        io_write("LuaJIT test-suite runner v0.1\n")
        result = nil
      elseif arg == "--help" then
        io_write("Usage: ", _G and _G.arg and _G.arg[-1] or "luajit", " ")
        io_write(own_file, " [flags] [tags] [root] [numbers]\n")
        io_write"\n"
        io_write"Root specifies either a directory of tests, or the name of\n"
        io_write"a particular .lua test file, defaulting to all tests if not given.\n"
        io_write"Tags are specified in the form +tag_name or -tag_name, and\n"
        io_write"are used to turn on or off groups of tests. For example,\n"
        io_write"pass -ffi to skip tests relating to the ffi library, or\n"
        io_write"pass +slow to enable running of slow tests.\n"
        io_write"Numbers can be passed to only run particular tests.\n"
        io_write"The available flags are:\n"
        io_write"  --quiet\n"
        io_write"  --shuffle=SEED\n"
        io_write"  --shard=INDEX/NUM_SHARDS\n"
        io_write"  --version\n"
        io_write"  --help\n"
        result = nil
      else
        error("Unsupported flag: ".. arg)
      end
      if joinedval ~= "" then
        error(arg .." does not expect an argument")
      end
    elseif arg:find"^[-+]" then
      opts.tags[arg:sub(2)] = (arg:sub(1, 1) == "+")
    elseif arg:find"^%d+$" then
      if not opts.numbers_to_run then
        opts.numbers_to_run = {}
      end
      opts.numbers_to_run[tonumber(arg)] = true
    elseif not opts.root then
      opts.root = arg
    else
      error("Unexpected argument ".. arg)
    end
  end
  return result
end

local function scan_tests(path, opts)
  if path:sub(-4, -4) == "." then
    local f = assert(io_open(path, "rb"))
    local contents = f:read"*a"
    f:close()
    local prefix = "return {"
    local code =  contents:gsub("()(do +%-%-%- +)([^\r\n]+)",
      function(pos, marker, info)
        if pos ~= 1 then
          pos = pos - 1
          if contents:sub(pos, pos) ~= "\n" then
            return marker .. info
          end
        end
        local result = ("%s%q,function()"):format(prefix, info)
        prefix = ","
        if info:find" !lex" and not opts:want_meta(info:sub((info:find" +[-+@!]"))) then
          result = result .."end--[========["
          prefix = "]========]".. prefix
        end
        return result
      end)
    if prefix:sub(-1) ~= "," then
      error("No tests found in ".. path)
    end
    prefix = prefix .."}"
    return assert(load(function()
      local result = code
      code = code ~= prefix and prefix or nil
      return result
    end, "@".. path))()
  else
    if path ~= "" and path:sub(-1) ~= "/" and path:sub(-1) ~= dirsep then
      path = path .. dirsep
    end
    local result = {}
    local i = 1
    for line in io_lines(path .."index") do
      if line ~= "" then
        local metaidx = line:find" +[-+@]"
        local name = line
        local want_these = true
        if metaidx then
          name = line:sub(1, metaidx - 1)
          want_these = opts:want_meta(line:sub(metaidx))
        end
        if want_these then
          result[i] = line
          result[i+1] = scan_tests(path .. name, opts)
          i = i + 2
        end
      end
    end
    return result
  end
end

local function upvalue_iterator(f, i)
  i = i + 1
  local name, val = debug.getupvalue(f, i)
  return name and i, name, val
end

local function upvalues_of(f)
  return upvalue_iterator, f, 0
end

local function append_tree_to_plan(test_tree, opts, plan, prefix)
  local prefi
  for i = 1, #test_tree, 2 do
    local info = test_tree[i]
    local name = info
    local want_these = true
    local metaidx = info:find" +[-+@!]"
    if metaidx then
      name = info:sub(1, metaidx - 1)
      want_these = opts:want_meta(info:sub(metaidx))
    end
    local planlen = #plan
    if want_these then
      local test = test_tree[i+1]
      if type(test) == "table" then
        append_tree_to_plan(test, opts, plan, prefix .. name .. dirsep)
      else
        if not prefi then
          prefi = prefix:sub(1, -2)
        end
        plan[#plan+1] = {prefi, name, test}
      end
    end
    if metaidx and info:find"!" then
      for modifier in info:gmatch"!([^ ]+)" do
        if modifier == "private_G" then
          local G = setmetatable({}, {__index = _G})
          G._G = G
          local function Gfn() return G end
          for i = planlen+1, #plan do
            local test = plan[i][3]
            if setfenv then
              setfenv(test, G)
            else
              for i, name in upvalues_of(test) do
                if name == "_ENV" then
                  debug.upvaluejoin(test, i, Gfn, 1)
                  break
                end
              end
            end
          end
        elseif modifier == "lex" then
          -- Handled during test scanning
        else
          error("Unsupported modifier \"".. modifier .."\" in ".. prefix)
        end
      end
    end
  end
  return plan
end

local function seal_globals()
  local sealed_mt = {__newindex = function()
    error("Tests should not mutate global state", 3)
  end}
  local function seal(t)
    if getmetatable(t) then return end
    setmetatable(t, sealed_mt)
    for k, v in pairs(t) do
      if type(v) == "table" then seal(v) end
    end
  end
  seal(_G)
  
  if getmetatable(package.loaded) == sealed_mt then
    setmetatable(package.loaded, nil)
  end
end

local function check_package_path()
  local ok, res = pcall(require, "common.test_runner_canary")
  if not ok then
    if own_dir then
      local _, psep, placeholder = package.config:match"^(.-)\n(.-)\n(.-)\n"
      package.path = package.path .. psep .. own_dir .. placeholder ..".lua"
      ok, res = pcall(require, "common.test_runner_canary")
    end
    if not ok then
      error(res)
    end
  end
  assert(res == "canary is alive")
end

local function mutate_plan(plan, opts)
  if opts.shuffle then
    math.randomseed(opts.shuffle)
    for i = #plan, 2, -1 do
      local n = math_random(1, i)
      plan[i], plan[n] = plan[n], plan[i]
    end
  end
  if opts.shard then
    local shard_plan = {}
    for i = opts.shard.initial, #plan, opts.shard.step do
      shard_plan[#shard_plan + 1] = plan[i]
    end
    plan = shard_plan
  end
  if opts.numbers_to_run then
    for i = 1, #plan do
      if not opts.numbers_to_run[i] then
        plan[i][3] = false
      end
    end
    for k in pairs(opts.numbers_to_run) do
      if not plan[k] then
        error("Test number ".. k .." is not part of the plan")
      end
    end
  end
  return plan
end

local function execute_plan(plan, opts)
  if #plan == 0 then
    error("No tests selected")
  end
  local progress_format = ("[%%%dd/%d] "):format(#tostring(#plan), #plan)
  local num_tests_run = 0
  local fail_numbers = {}
  for i = 1, #plan do
    local plan_i = plan[i]
    local test = plan_i[3]
    if test then
      local file, name = plan_i[1], plan_i[2]
      if not opts.quiet then
        io_write(progress_format:format(i), file)
        io_write(file == "" and "" or " --- ", name, "\n")
      end
      local ok, err = xpcall(test, debug_traceback)
      if not ok then
        if opts.quiet then
          io_write(progress_format:format(i), file)
          io_write(file == "" and "" or " --- ", name, "\n")
        end
        fail_numbers[#fail_numbers + 1] = i
        io_write(err, "\n")
      end
      num_tests_run = num_tests_run + 1
    end
  end
  if #fail_numbers == 0 then
    io_write(num_tests_run, " passed\n")
    return true
  else
    io_write(num_tests_run - #fail_numbers, " passed, ")
    io_write(#fail_numbers, " failed\n")
    if not opts.quiet and num_tests_run ~= #fail_numbers then
      io_write("to run just failing tests, pass command line arguments: ")
      io_write(table.concat(fail_numbers, " "), "\n")
    end
    return false
  end
end

local opts = parse_args{...}
if not opts then
  return
end
seal_globals()
check_package_path()
local test_tree = scan_tests(opts.root or own_dir or "", opts)
local plan = append_tree_to_plan(test_tree, opts, {}, "")
plan = mutate_plan(plan, opts)
local all_good = execute_plan(plan, opts)
if not all_good then
  os.exit(1)
end
