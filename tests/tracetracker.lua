local jit = require("jit")
local jutil = require("jit.util")
local vmdef = require("jit.vmdef")
local funcinfo, funcbc, traceinfo = jutil.funcinfo, jutil.funcbc, jutil.traceinfo
local band = bit.band

local lib 
local traces, texits = {}, {}

local printevents = false 

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

-- Format trace error message.
local function fmterr(err, info)
  if type(err) == "number" then
    if type(info) == "function" then info = fmtfunc(info) end
    err = string.format(vmdef.traceerr[err], info)
  end
  return err
end


local function print_trevent(tr, printall)
  
  if printall or not tr.stopfunc then
    print(string.format("\n[TRACE(%d) start at %s]", tr.traceno, fmtfunc(tr.startfunc, tr.startpc)))
  end

  if tr.abort then
    print(string.format("[TRACE(%d) abort at %s, error = %s]", tr.traceno, fmtfunc(tr.stopfunc, tr.stoppc), tr.abort))
  elseif tr.stopfunc then
    print(string.format("[TRACE(%d) stop at %s]", tr.traceno, fmtfunc(tr.stopfunc, tr.stoppc)))
  end
end

local function trace_event(what, tr, func, pc, otr, oex)

  local trace

  if what == "flush" then
    return
  end
  
  if what == "start" then
    trace = {
      traceno = tr,
      startfunc = func,
      startpc = pc,
    }
    traces[#traces+1] = trace
  elseif what == "abort" or what == "stop" then
    trace = traces[#traces]
    assert(trace and trace.traceno == tr)
    
    trace.stopfunc = func
    trace.stoppc = pc
    
    if what == "abort" then
      lib.aborts = lib.aborts+1
      trace.abort = fmterr(otr, oex)
    end
  else
    assert(false, what)
  end
  
  if printevents then
    print_trevent(trace)
  end
end

local function trace_exit(tr, ex, ngpr, nfpr, ...)
  texits[#texits+1] = {
    tr = tr,
    exitno = ex,
    order = #traces
  }
  
  if printevents then
    print("---- TRACE ", tr, " exit ", ex)
  end
end

local function isjited(func, starti)

  local hasany = false

  starti = starti or 1
  
  for i=starti,#traces do
  
    local tr = traces[i]
  
    if not tr.abort then
      hasany = true
      if tr.startfunc == func or tr.stopfunc == func then
        return i, true
      end
    end
  end
  
  return false,hasany
end

function hastraces(optfunc, starti)

  if not optfunc then
    return traces[1] ~= nil
  end

  starti = starti or 1
  
  for i=starti,#traces do
  
    if tr.startfunc == func or tr.stopfunc == func then
      return i, true
    end  
    if not tr.abort then
      hasany = true
    end
  end
end

local active = false

local function start()
  if not active then
    active = true
    jit.attach(trace_event, "trace")
    jit.attach(trace_exit, "texit")
  end
end

local function stop()
  if active then
    active = false
    jit.attach(trace_event)
    jit.attach(trace_exit)
    --jit.attach(dump_trace)
  end
end

local function clear()
  traces = {}
  texits = {}
  lib.aborts = 0
end

local function print_savedevevents()

  local nextexit = texits[1] and texits[1].order
  local exi = 1

  for i,tr in ipairs(traces) do
    print_trevent(tr, true)
    
    if nextexit and i >= nextexit then
      for i=exi,#texits do
        
        local exit = texits[i]
        
        if exit.order > i then 
          exi = i
          break
        end
      
        print("---- TRACE ", exit.tr, " exit ", exit.exitno)
      end
    end
    
  end
end

lib = {
  start = start,
  stop = stop,
  clear = clear,
  
  isjited = isjited,
  hasexits = function() return texits[1] ~= nil end,
  setprintevents = function(enabled) printevents = enabled end,
  traces = function() return traces end,
  exitcount = function() return #texits end,
  traceattemps = function() return #traces end,
  print_savedevevents = print_savedevevents,
  aborts = 0,
}

jit.off(true, true)

return lib