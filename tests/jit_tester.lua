local jit = require("jit")
local jutil = require("jit.util")
local vmdef = require("jit.vmdef")
local tracker = require("tracetracker")
local funcinfo, funcbc, traceinfo = jutil.funcinfo, jutil.funcbc, jutil.traceinfo
local traceinfo, traceir, tracek = jutil.traceinfo, jutil.traceir, jutil.tracek
local band = bit.band
local unpack = unpack
--set a high count for now to work around hot counter backoff
local testloopcount = 80 
local sub = string.sub

local function fmtfunc(func, pc)
  local fi = funcinfo(func, pc)
  if fi.loc then
    return fi.loc
  elseif fi.ffid then
    return vmdef.ffnames[fi.ffid]
  elseif fi.addr then
    return string.format("C:%x", fi.addr)
  else
    return "(?)"
  end
end

local bcnames = {}


for i=1,#vmdef.bcnames/6 do
  bcnames[i] = string.sub(vmdef.bcnames, i+1, i+6)
end

local irlookup = {}

for i=0,(#vmdef.irnames)/6 do
  local ir = sub(vmdef.irnames, (i*6)+1, (i+1) * 6)
  ir = ir:match("^%s*(.-)%s*$")
  irlookup[ir] = i
end

local function getiroptype(tr, ref)

  local m, ot = traceir(tr, ref)
  
  return shr(ot, 8)
end

function checkir(tr, ir, exop1, exop2, start)

  local info = traceinfo(tr)
  assert(info)
  
  local findop = irlookup[ir]
  assert(findop, "No IR opcode named "..tostring(ir))
  
  local op1kind = (exop1 and irlookup[exop1]) or -1
  assert(op1kind == -1 or exop1 == nil, "op1 no IR opcode named "..tostring(exop1))
  
  local op2kind = (exop2 and irlookup[exop2]) or -1
  assert(op2kind == -1 or exop2 == nil, "op2 no IR opcode named "..tostring(exop2))
  
  start = start or 1
  local err = false
  
  for ins=start,info.nins do
    local m, ot, op1, op2, ridsp = traceir(tr, ins)
    local op, t = shr(ot, 8), band(ot, 31)
    
    if op == findop then
      local match = true
    
      if op1kind ~= -1 and op1 ~= op1kind then
        match = false
      end
      
    
      return ins
    end    
  end

  trerror("Trace contained no %s IR opcodes", ir)
end


local expectedlnk = "return"

local function trerror(s, a1, ...)

  tracker.print_savedevevents()

  if(a1) then
    error(string.format(s, a1, ...), 4)
  else
    error(s, 4)
  end

end

local function asserteq(result, expected, info)
  if result ~= expected then
    error(string.format("expected %q but got %q - %s", tostring(expected), tostring(result), info or "", 3))
  end
end

local function checktrace(func, tr, mode)

  local traces = tracker.traces()

  if tr == nil then

    if #traces == 0 then
      trerror("no traces were started for test")
    else
      --TODO: Filter out trace that are from checker code
      tr = traces[1]
    end
  end

  if tr.abort then
    trerror("trace aborted with error %s at %s", abort, fmtfunc(tr.stopfunc, tr.stoppc))
  end
  
  local info = traceinfo(tr.traceno)
  
  if info.linktype == "stitch" and expectedlnk ~= "stitch" then
    trerror("trace did not cover full function stitched at %s", fmtfunc(tr.stopfunc, tr.stoppc))
  end
  
  if tr.startfunc ~= func then
    trerror("trace did not start in tested function. started in %s", fmtfunc(tr.startfunc, tr.startpc))
  end

  if tr.stopfunc ~= func then
    trerror("trace did not stop in tested function. stoped in %s", fmtfunc(tr.stopfunc, tr.stoppc))
  end
  
  if info.linktype ~= expectedlnk then
    trerror("expect trace link '%s but got %s", expectedlnk, info.linktype)
  end
  
  if mode == "root" then
    if tracker.hasexits() then
      trerror("unexpect traces exits ")
    end
      
    if #traces > 1 then
      trerror("unexpect extra traces were started for test ")
    end
  end
end

local started_tracker = false

local function begintest(func)
  if not started_tracker then
    tracker.start()
    started_tracker = true
  end
  
  jit.flush()
  jit.on(func, true) --clear any interpreter only function/loop headers that may have been caused by other tests
  tracker.clear()
end

local function trerror2(s, a1, ...)

  tracker.print_savedevevents()

  if(a1) then
    error(string.format(s, a1, ...), 3)
  else
    error(s, 3)
  end
end

function testsingle(expected, func, ...)

  begintest(func)

  local expectedval = expected
  
  for i=1, testloopcount do
  
    local result
  
    if type(expected) == "function" then
      expectedval, result = expected(i, func(...))
    else
      result = func(...)
    end
    
    if (result ~= expectedval) then
      local jitted, anyjited = tracker.isjited(func)
      tracker.print_savedevevents()
      trerror2("expected %q but got %q - %s", tostring(expected), tostring(result), (jitted and "JITed") or "Interpreted")
    end
    
  end

  checktrace(func, tr, "root")
  
  return true
end

function testwithchecker(checker, func, ...)
  begintest(func)

  jit.off(checker)
  
  for i=1, testloopcount do
  
    local expected, result = checker(i, func(i, ...))
      
    if (result ~= expected) then
      local jitted, anyjited = tracker.isjited(func)
      tracker.print_savedevevents()
      trerror2("expected %q but got %q - %s", tostring(expected), tostring(result), (jitted and "JITed") or "Interpreted")
    end
    
  end

  checktrace(func, nil, "root")
  
  return true
end

  
local state = {
  WaitFirstTrace = 1,
  CheckNoExits = 2,
  RunNextConfig = 3,
  CheckCompiledSideTrace = 4,
}

--FIXME: the side traces that happen for config 2 will always abort because they trace out into this function which has jit turned off
local function testexits(func, config1, config2)

  begintest(func)
  
  local jitted = false
  local trcount = 0
  local config, expected, shoulderror = config1, config1.expected, config1.shoulderror 
  local state = 1
  local sidestart = 0
  
  for i=1, testloopcount do
    local status, result
    
    if not shoulderror then
      result = func(unpack(config.args))
    else
      status, result = pcall(func, unpack(config.args))
      
      if(status) then
        tracker.print_savedevevents()
        trerror2("expected call to trigger error but didn't "..tostring(i))
      end
    end
    
    if state == 2 then
      if tracker.hasexits() then
        trerror2("trace exited on first run after being compiled "..expected)
      end
      state = 3
    end
    
    local newtraces = tracker.traceattemps() ~= trcount
    
    if newtraces then
      trcount = tracker.traceattemps()
      jitted, anyjited = tracker.isjited(func)  
      
      if state == 1 then
      --let the trace be executed once before we switch to the next arguments
        state = 2
      elseif state == 4 and not tracker.traces()[trcount].abort  then
        state = 5
        sidestart = tracker.exitcount()
        print("side trace compiled ".. tostring(expected))
      end
    end
    
    if not shoulderror and result ~= expected then
      tracker.print_savedevevents()
      error(string.format("expected %q but got %q - %s", tostring(expected), tostring(result), (jitted and "JITed") or "Interpreted"), 2)
    end
    
    if state == 3 then
      config = config2
      expected = config2.expected
      shoulderror = config2.shoulderror
      state = 4
    end
    
  end
  
  local traces = tracker.traces()
  
  if #traces == 0 then
    trerror2("no traces were started for test "..expected)
  end
  
  local tr = traces[1]

  checktrace(func, tr)
  
  if not tracker.hasexits() then
    trerror2("Expect trace to exit to interpreter")
  end
  
  if sidestart ~= 0 and tracker.exitcount() > sidestart then
    trerror2("Unexpected exits from side trace")
  end
  
  assert(state >= 4)
  
  return true
end

local function texiterror(msg)
  tracker.printexits()
  error(msg, 4)
end

local function testexit(expected, func, ...)
  
  tracker.clearexits()
  local result = func(...)

  if not tracker.hasexits() then
    texiterror("Expected trace to exit but didn't")
  end
  
  asserteq(result, expected)
  
  return true
end

local function testnoexit(expected, func, ...)
  
  tracker.clearexits()
  local result = func(...)
  
  if tracker.hasexits() then
    texiterror("Unexpected trace exits")
  end
  
  asserteq(result, expected)
  
  return true
end

local function testexiterr(func, ...)

  tracker.clearexits()
  local status, result = pcall(func, ...)

  if not tracker.hasexits() then
    texiterror("Expected trace to exit but didn't")
  end
  
  if(status) then
    texiterror("Expected call to trigger error but didn't ")
  end
  
  return true
end

jit.off(true, true)

local function setasserteq(func)
  jit.off(func)
  
  --force interpreter only func header bc
  for i=1, 30 do
    func(1, 1)
  end
  asserteq = func
end

require("jit.opt").start("hotloop=2")
--force the loop and function header in testjit to abort and be patched
local dummyfunc = function() return "" end
for i=1,30 do
  pcall(testsingle, "", dummyfunc, "")
end

require("jit.opt").start("hotloop=6")

return {
  testsingle = testsingle,
  testwithchecker = testwithchecker,
  testexit = testexit,
  testnoexit = testnoexit,
  testexiterr = testexiterr,
  testexits = testexits,
  testloopcount = testloopcount,
  setasserteq = setasserteq,
}