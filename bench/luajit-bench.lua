
local function do_bench(bench, scriptargs, duration)
  local f = assert(loadfile(bench..'.lua'))
  local stdout = io.output()
  io.output('/dev/null')
  arg = scriptargs
  local start_time = os.time()
  local end_time = start_time + duration
  local ctr = 0
  local cur_time = start_time
  repeat
    pcall(f)
    ctr = ctr + 1
    cur_time = os.time()
  until(cur_time >= end_time)
  io.output(stdout)
  io.write(bench..':'..tostring(ctr)..':'..tostring(ctr / (cur_time - start_time))..'\n')
end

-- Parse arguments.
local script_arg = false
local concurrency = false
local set_duration = false
local nthreads = 0
local duration = 10
local bench = nil
local scriptargs = {}
for k,a in ipairs(arg) do
  if (script_arg) then
    scriptargs[#scriptargs + 1] = a
    script_arg = false
  elseif (concurrency) then
    nthreads = tonumber(a)
    concurrency = false
  elseif (set_duration) then
    duration = tonumber(a)
    set_duration = false
  elseif (a == '-a') then
    script_arg = true
  elseif (a == '-c') then
    concurrency = true
  elseif (a == '-t') then
    set_duration = true
  else
    bench = a
  end
end

if (not bench) then
  io.write('Script needs a benchmark name argument with the .lua extension')
else
  do_bench(bench, scriptargs, duration)
end
