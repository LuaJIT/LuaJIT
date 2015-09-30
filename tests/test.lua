package.path = package.path .. ";../src/?.lua"

local jit = require("jit")
local jutil = require("jit.util")
local vmdef = require("jit.vmdef")
local tracker = require("tracetracker")
local funcinfo, funcbc, traceinfo = jutil.funcinfo, jutil.funcbc, jutil.traceinfo
local band = bit.band
local unpack = unpack
local buf, buf2
local testloopcount = 30

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

jit.off(fmtfunc)

local bcnames = {}

for i=1,#vmdef.bcnames/6 do
  bcnames[i] = string.sub(vmdef.bcnames, i+1, i+6)
end


local expectedlnk = "return"

function trerror(s, a1, ...)

  tracker.print_savedevevents()

  if(a1) then
    error(string.format(s, a1, ...), 4)
  else
    error(s, 4)
  end

end

local function checktrace(tr, func)

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
end

jit.off(checktrace)

local function begin_jittest(func)
  jit.flush()
  jit.on(func, true) --clear any interpreter only function/loop headers that may have been caused by other tests
  tracker.clear()
end

function trerror2(s, a1, ...)

  tracker.print_savedevevents()

  if(a1) then
    error(string.format(s, a1, ...), 3)
  else
    error(s, 3)
  end

end

function testjit(expected, func, ...)

  begin_jittest(func)

  for i=1, testloopcount do
  
    local result = func(...)

    if (result ~= expected) then
      local jitted, anyjited = tracker.isjited(func)
      tracker.print_savedevevents()
      trerror2("expected '%s' but got '%s' - %s", tostring(expected), tostring(result), (jitted and "JITed") or "Interpreted")
    end
  end

  local traces = tracker.traces()
  
  if #traces == 0 then
    trerror2("no traces were started for test "..expected, 2)
  end
  
  local tr = traces[1]

  checktrace(tr, func)

  if tracker.hasexits() then
    trerror2("unexpect traces exits "..expected)
  end
    
  if #traces > 1 then
    trerror2("unexpect extra traces were started for test "..expected)
  end
  --trace stop event doesn't provide a pc so would need to save the last pc traced
  --[[
  local stopbc = bcnames[band(funcbc(tr.stopfunc, tr.stoppc), 0xff)]
  
  if stopbc:find("RET") ~= 1 then
    error(string.format("trace stoped at unexpected bytecode"), 2)
  end
  ]]
  
  jit.flush()  
end

jit.off(testjit)

--FIXME: the side traces that happen for config 2 will always abort because they trace out into this function which has jit turned off
local function testjit2(func, config1, config2)

  begin_jittest(func)
  
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
      error(string.format("expected '%s' but got '%s' - %s", tostring(expected), tostring(result), (jitted and "JITed") or "Interpreted"), 2)
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

  checktrace(tr, func)
  
  if not tracker.hasexits() then
    trerror2("Expect trace to exit to interpreter")
  end
  
  if sidestart ~= 0 and tracker.exitcount() > sidestart then
    trerror2("Unexpected exits from side trace")
  end
  
  assert(state >= 4)
end

jit.off(testjit2)

require("jit.opt").start("hotloop=2")
--force the loop and function header in testjit to abort and be patched
local dummyfunc = function() return "" end
for i=1,30 do
  pcall(testjit, "", dummyfunc, "")
end

require("jit.opt").start("hotloop=5")

function reset_write(buf, ...)
  buf:reset()
  buf:write(...)
    
  return buf
end

function asserteq(result, expected)

  if (result ~= expected) then 
    error("expected \""..tostring(expected).."\" but got \""..tostring(result).."\"", 2)
  end

  return result
end


local tostringobj = setmetatable({}, {
    __tostring = function(self) 
        return "tostring_result"
    end
})

local tostringerr = setmetatable({}, {
    __tostring = function() 
        return error("throwing tostring")
    end
})

local tostring_turtle = setmetatable({}, {
    __tostring = function(self) 
        return self.turtle
    end
})


local buf_empty = string.createbuffer()
local buf_a = string.createbuffer()
buf_a:write("a")
local buf_abc = string.createbuffer()
buf_abc:write("abc")

buf = string.createbuffer()
buf2 = string.createbuffer()


tests = {}

function tests.createbuffer()

  --the internal buffer should always be allocated
  assert(string.createbuffer():capacity() ~= 0)
  -- test providing the initial capacity
  assert(string.createbuffer(512):capacity() >= 512)
end

local function bufcapacity(buf)
  return (buf:capacity())
end

local function bufleft(buf)
  return buf:capacity()-buf:len()
end

function tests.capacity()
  local capacity = buf:capacity()
  testjit(capacity, bufcapacity, buf)
  
  capacity = buf_a:capacity()
  testjit(capacity-1, bufleft, buf_a)
end

function tests.setcapacity()
  reset_write(buf, "foobar")
  buf:setcapacity(120)
  asserteq(buf:capacity(), 120)
  asserteq(buf:tostring(), "foobar")
  
  buf:setcapacity(32)
  asserteq(buf:capacity(), 32)
  asserteq(buf:tostring(), "foobar")
  
  --check clamping to min capacity
  buf:setcapacity(0)
  assert(buf:capacity() ~= 0)
  asserteq(buf:tostring(), "foobar")

  assert(not pcall(buf.setcapacity, buf, 0x7fffff01))
end

local function bufsize(buf)
  return (buf:len())
end

local function bufsizechange(buf, s)
  local size1 = buf:len()
  buf:write(s)
  return buf:len()-size1
end

function tests.len()

  asserteq(#buf_empty, 0)
  asserteq(#buf_a, 1)
  
  testjit(0, bufsize, buf_empty)
  testjit(1, bufsize, buf_a)
  
  --check buffer pointers are reloaded when getting the size before and after an append to the buffer
  testjit(3, bufsizechange, buf, "foo")
end

function tests.setlength()
  
  buf:setlength(1)
  asserteq(buf:len(), 1)
  
  buf:setlength(0)
  asserteq(buf:len(), 0)

  --setting size larger than the buffer should throw an error
  assert(not pcall(buf.setlength, buf, buf:capacity()+1))
  assert(not pcall(buf.setlength, buf, -1))

  --check truncating contents in the buffer
  buf:write("foobar1")
  asserteq(buf:len(), 7)
  buf:setlength(6)
  asserteq(buf:len(), 6)
  asserteq(buf:tostring(), "foobar")
  
  --Check setting size to capacity
  local minsize = buf:capacity()
  buf:setlength(buf:capacity())
  buf:write("a")
  assert(buf:len() > minsize)
  
  buf:setlength(6)
  asserteq(buf:tostring(), "foobar")
end

function tests.reserve()
  local capacity = buf:capacity()
  
  buf:setlength(buf:capacity()-1)
  
  buf:reserve(0)
  asserteq(buf:capacity(), capacity)
  
  buf:reserve(1)
  asserteq(buf:capacity(), capacity)
  
  buf:reserve(2)
  assert(buf:capacity() > capacity)
  
  assert(not pcall(buf.reserve, buf, -1))
end

function tests.equals()

  assert(buf_a:equals("a"))
  assert(not buf_a:equals("b"))
  assert(not buf_a:equals("aa"))
  
  assert(buf_empty:equals(""))
  assert(not buf_empty:equals("a"))
  
  --compare buffer to buffer
  reset_write(buf, "foo")
  assert(buf:equals(buf))
  assert(buf_empty:equals(buf_empty))
  assert(not buf:equals(buf_empty))
  
  reset_write(buf2, "foo")
  assert(buf:equals(buf2))
end

local function getbyte(buf, i)
  return (buf:byte(i))
end

function tests.byte()

  local a = string.byte("a")
  local b = string.byte("b")
  local c = string.byte("c")
  
  assert(not pcall(getbyte, buf_a, 0))
  assert(not pcall(getbyte, buf_empty, 0))
  assert(not pcall(getbyte, buf_empty, 1))
  assert(not pcall(getbyte, buf_empty, -1))
  
  testjit(a, getbyte, buf_a, 1)
  testjit(b, getbyte, buf_abc, 2)
  testjit(a, getbyte, buf_a, -1)
  testjit(a, getbyte, buf_abc, -3)
  
  --check guard for index changing from negative to positive 
  testjit2(getbyte, {args = {buf_abc, -3}, expected = a}, 
                    {args = {buf_abc, 2}, expected = b})

  local start = {args = {buf_a, 1}, expected = a}
  testjit2(getbyte, start, {args = {buf_abc, -3}, expected = a})
  testjit2(getbyte, start, {args = {buf_abc, -2}, expected = b})
  testjit2(getbyte, start, {args = {buf_abc, -1}, expected = c})
  
  testjit2(getbyte, start, {args = {buf_empty, 0}, shoulderror = true})
  testjit2(getbyte, start, {args = {buf_abc, 0}, shoulderror = true})
  testjit2(getbyte, start, {args = {buf_a, 0}, shoulderror = true})
  testjit2(getbyte, start, {args = {buf_a, 2}, shoulderror = true})
  testjit2(getbyte, start, {args = {buf_a, -2}, shoulderror = true})

  --check when the buffer changed to empty
  start = {args = {buf_a, -1}, expected = a}
  testjit2(getbyte, start, {args = {buf_empty, 0}, shoulderror = true})
  testjit2(getbyte, start, {args = {buf_empty, 1}, shoulderror = true})
  testjit2(getbyte, start, {args = {buf_empty, -1}, shoulderror = true})
end
--tracker.setprintevents(true)
--singletest = tests.byte

local function fixslash(buf, path)

  local slash = string.byte("\\")

  buf:reset()
  buf:write(path)
  
  for i=1,#buf do
    if buf:byte(i) == slash then
      buf:setbyte(i, "/")
    end
  end
  
  return (buf:tostring())
end

local function setbyte(buf, i, b)
  buf:setbyte(i, b)
  return (buf:tostring())
end

function tests.setbyte()
  reset_write(buf, "a")

  asserteq(setbyte(buf, 1, "b"), "b")
  asserteq(setbyte(buf, -1, "c"), "c")
  asserteq(setbyte(buf, 1, 97), "a")
  
  --check error for index out of range
  reset_write(buf, "a")
  assert(not pcall(setbyte, buf, 2, "b"))
  assert(buf:equals("a"))
  
  assert(not pcall(setbyte, -2, "b"))
  assert(buf:equals("a"))
  
  --TODO: refactor jittest for this
  --testjit("a/bar/c/d/e/foo/a/b/c/d/e/f", fixslash, buf, "a\\bar\\c\\d\\e\\foo\\a\\b\\c\\d\\e\\f")
  asserteq("a/bar/c/d/e/foo/a/b/c/d/e/f", fixslash(buf, "a\\bar\\c\\d\\e\\foo\\a\\b\\c\\d\\e\\f"))
end

function testwrite(a1, a2, a3, a4)
  buf:reset()

  if(a2 == nil) then
    buf:write(a1)
  elseif(a3 == nil) then
    buf:write(a1, a2)
  elseif(a4 == nil) then
    buf:write(a1, a2, a3)
  else 
    buf:write(a1, a2, a3, a4)
  end
 
  return (buf:tostring())
end

function tests.write()
  asserteq(testwrite("a"), "a")
  asserteq(testwrite(""), "")

  testjit("bar", testwrite, "bar")
  testjit("1234567890", testwrite, 1234567890)
  testjit("12345.9375", testwrite, 12345.9375)
  testjit("foo2bar", testwrite, "foo", 2, "bar")
  testjit("true1false", testwrite, true, 1, false)
  
  asserteq(testwrite(tostringobj), "tostring_result")
  
  --Make sure the buffer is unmodifed if an error is thrown
  reset_write(buf, "foo")
  local status, err = pcall(function(buff, s, tostr) 
    buff:write(s, tostr)
  end, buf, "end", tostringerr)
  
  assert(not status and err == "throwing tostring")
  --buffer is not reset to starting state if an error is thrown during a write/format call
  asserteq(buf:tostring() , "fooend")
  
  --appending one buff to another
  reset_write(buf2, "buftobuf")
  testjit("foobuftobuf", testwrite, "foo", buf2)
end

local function testwriteln(a1)
    buf:reset()
    
    if(a1 == nil) then
      buf:writeln()
    else
      buf:writeln(a1)
    end
    
    return (buf:tostring())
end

function tests.writeln()
  testjit("\n", testwriteln)
  testjit("foo\n", testwriteln, "foo")
end

local function testwritesub(base, s, ofs, len)
  buf:reset()
  buf:write(base)
    
  if(len == nil) then
    buf:writesub(s, ofs)
  else
    buf:writesub(s, ofs, len)
  end

  return (buf:tostring())
end

function tests.writesub()
  --test writing a sub string
  testjit("n1234567", testwritesub, "n", "01234567",  2)
  testjit("n67",      testwritesub, "n", "01234567", -2)
  testjit("n12345",   testwritesub, "n", "01234567",  2, -3) 

  ----check overflow clamping
  testjit("n1234567",  testwritesub, "n", "01234567",   2, 20)
  testjit("n01234567", testwritesub, "n", "01234567", -20, 8)
  
  --test writing a sub string where the source is another buffer
  reset_write(buf2, "01234567")
  testjit("n1234567", testwritesub, "n", buf2,  2)
  testjit("n67",      testwritesub, "n", buf2, -2)
  testjit("n12345",   testwritesub, "n", buf2,  2, -3)
end

tracker.start()
--tracker.setprintevents(true)
collectgarbage("stop")

function testjit(expected, func, ...)

  local result = func(...)

  if (result ~= expected) then 
    error("expected \""..tostring(expected).."\" but got \""..tostring(result).."\"", 2)
  end
end

testjit2 = function(func, config1, config2)

  local config, expected, shoulderror = config1, config1.expected, config1.shoulderror 
  local state = 1
  
  for i=1, 3 do
    local status, result
    
    if not shoulderror then
      result = func(unpack(config.args))
    else
      status, result = pcall(func, unpack(config.args))
      
      if(status) then
        trerror2("expected call to trigger error but didn't "..tostring(i))
      end
    end
    
    if not shoulderror and result ~= expected then
      trerror2("expected '%s' but got '%s' - %s", tostring(expected), tostring(result), (jitted and "JITed") or "Interpreted")
    end
    
    config = config2
    expected = config2.expected
    shoulderror = config2.shoulderror 
  end
end

skip = {
  setcapacity = true,
}

if singletest then
  print("Running: single test")
  singletest()
else
  --should really order these by line
  for name,func in pairs(tests) do
    if not skip[name] then
      print("Running: "..name)
      func()
    else
      print("Skipping: "..name)
    end
   
  end
end

print("tests past")








