local ctest = require("ctest")

local called = 0
local function clearhook() debug.sethook(nil, "", 0) end

-- Return from pcall with active hook must prepend true. FF pcall.
called = 0
debug.sethook(function() called=called+1; assert(pcall(function() end) == true); clearhook() end, "", 1)
do local x = 1 end
assert(called == 1)

-- Hook with special caught error must not unblock hooks. FF pcall.
called = 0
debug.sethook(function() called=called+1; pcall(nil); clearhook() end, "", 1)
do local x = 1 end
assert(called == 1)

-- Hook with caught error must not unblock hooks. FF pcall.
called = 0
local function p2() error("") end
debug.sethook(function() called=called+1; pcall(p2); clearhook() end, "", 1)
do local x = 1 end
assert(called == 1)

-- Hook with special caught error must not unblock hooks. C pcall.
called = 0
debug.sethook(function() called=called+1; ctest.pcall(nil); clearhook() end, "", 1)
do local x = 1 end
assert(called == 1)

-- Hook with caught error must not unblock hooks. C pcall
called = 0
local function p2() error("") end
debug.sethook(function() called=called+1; ctest.pcall(p2); clearhook() end, "", 1)
do local x = 1 end
assert(called == 1)

-- Regular pcall must not block hooks.
debug.sethook(function() called=called+1 end, "", 1)
pcall(function() end)
called = 0
do local x = 1 end
assert(called > 0)
pcall(function() error("") end)
called = 0
do local x = 1 end
assert(called > 0)
ctest.pcall(function() end)
called = 0
do local x = 1 end
assert(called > 0)
ctest.pcall(function() error("") end)
called = 0
do local x = 1 end
assert(called > 0)
clearhook()

-- Hook with uncaught error must unblock hooks. FF pcall
called = 0
pcall(function()
  debug.sethook(function()
    local old = called
    called = 1
    if old == 0 then error("") end
  end, "", 1)
  do local x = 1 end
end)
assert(called == 1)
called = 2
do local x = 1 end
assert(called == 1, "hook not unblocked after uncaught error")
clearhook()
called = 2
do local x = 1 end
assert(called == 2)

-- Hook with uncaught error must unblock hooks. C pcall
called = 0
ctest.pcall(function()
  debug.sethook(function()
    local old = called
    called = 1
    if old == 0 then error("") end
  end, "", 1)
  do local x = 1 end
end)
assert(called == 1)
called = 2
do local x = 1 end
assert(called == 1, "hook not unblocked after uncaught error")
clearhook()
called = 2
do local x = 1 end
assert(called == 2)

