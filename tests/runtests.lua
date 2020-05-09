local tester = require("jit_tester")
local testjit = tester.testsingle
local telescope = require("telescope")
local ffi = require("ffi")
local C = ffi.C

telescope.make_assertion("jit", "", tester.testsingle)
telescope.make_assertion("jitchecker", "", tester.testwithchecker)
telescope.make_assertion("noexit", "", tester.testnoexit)
telescope.make_assertion("exit", "", tester.testexit)

telescope.make_assertion("cdef", "", function(cdef, name) 
  assert(not name or type(name) == "string")
  ffi.cdef(cdef)
  if name then assert(C[name]) end
  return true
end)

telescope.make_assertion("cdeferr", "expected cdef '%s' to error", function(cdef, msg) 
  local success, ret = pcall(ffi.cdef, cdef)
  if success then return false end
  if msg then
    local found = string.find(ret, msg)
    
    if not found then
      error(string.format('cdef error message did not containt string: \n  "%s" \nerror was\n  "%s"', msg, ret))
    end
  end
  return true
end)

telescope.make_assertion("v4eq", "", function(v, x, y, z, w) 
 
  if v[0] ~= x then
    error(string.format("expected v[0] to equal %s was %s", tostring(x), tostring(v[0])))
  elseif v[1] ~= y then                                                 
    error(string.format("expected v[1] to equal %s was %s", tostring(y), tostring(v[1])))
  elseif v[2] ~= z then                                                 
    error(string.format("expected v[2] to equal %s was %s", tostring(z), tostring(v[2])))
  elseif v[3] ~= w then                                                 
    error(string.format("expected v[3] to equal %s was %s", tostring(w), tostring(v[3])))
  end
 
  return true
end)

filter = filter or ""

local callbacks = {}

local function printfail()
  print("  Failed!")
end

callbacks.err = printfail
callbacks.fail = printfail

function callbacks.before(t) 
  print("running", t.name) 
end

local contexts = {}
local files = {"intrinsic_spec.lua"}

for _, file in ipairs(files) do 
  telescope.load_contexts(file, contexts) 
end

local buffer = {}
local testfilter

if filter then

  if(type(filter) == "table") then  
    testfilter = function(t) 
      for _,patten in ipairs(filter) do
        if t.name:match(patten) then
          return true
        end
      end
      
      return false
    end
  elseif(type(filter) == "number") then
    local count = 0
    local reverse = filter < 0
    testfilter = function(t)
      count = count+1
      if ((not reverse and count > filter) or (reverse and (count+filter) < 0)) then
        return false 
      end
      
      return true
    end
  elseif(filter ~= "") then 
    testfilter = function(t) return t.name:match(filter) end
  end
end

local results = telescope.run(contexts, callbacks, testfilter)
local summary, data = telescope.summary_report(contexts, results)
table.insert(buffer, summary)
local report = telescope.error_report(contexts, results)

if report then
  table.insert(buffer, "")
  table.insert(buffer, report)
end

if #buffer > 0 then 
  print(table.concat(buffer, "\n"))
end