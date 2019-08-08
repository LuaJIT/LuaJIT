local exdata = require "thread.exdata"
local ffi = require "ffi"

local function nargs(...)
  return select('#', ...)
end

--[[ These tests need to be first so that they read the default value and not
     the value updated by the tests that follow. --]]
do --- default value: JIT off
  jit.off()
  local saved_q
  for i = 1, 5 do
    local q = exdata()
    if saved_q then
      assert(q == saved_q)
    end
    saved_q = q
  end
  assert(saved_q == nil)
end

do --- default value: JIT on +jit
  jit.opt.start("minstitch=100000", "hotloop=2")
  jit.on()
  local saved_q
  for i = 1, 5 do
    local q = exdata()
    if saved_q then
      assert(q == saved_q)
    end
    saved_q = q
  end
  assert(saved_q == nil)
end

do --- sanity: JIT off
  jit.off()
  local u64 = ffi.new("uintptr_t", 0xefdeaddeadbeefLL)
  local ptr = ffi.cast("void *", u64)
  local saved_q
  for i = 1, 5 do
    exdata(u64)
    local q = exdata()
    if saved_q then
      assert(q == saved_q)
    end
    saved_q = q
  end
  assert(ptr == saved_q)
end

do --- coroutines: JIT off
  jit.off()
  local u64 = ffi.new("uintptr_t", 0xefdeadbeefLL)
  local ptr = ffi.cast("void *", u64)
  local ptr2 = ffi.cast("void *", u64 + 1)
  local ptr3 = ffi.cast("void *", u64 - 2)
  local saved_q
  local function f()
    coroutine.yield(exdata())
    exdata(ptr2)
    coroutine.yield(exdata())
    coroutine.yield(exdata())
  end

  exdata(u64)

  local co = coroutine.create(f)

  local ok, data = coroutine.resume(co)
  assert(ok)
  assert(data == ptr)

  ok, data = coroutine.resume(co)
  assert(ok)
  assert(data == ptr2)

  exdata(ptr3)

  ok, data = coroutine.resume(co)
  assert(ok)
  assert(data == ptr2)
  assert(exdata() == ptr3)
end

do --- reading: JIT on +jit
  jit.opt.start("minstitch=100000", "hotloop=2")
  local u64 = ffi.new("uintptr_t", 0xefdeaddeadbeefLL)
  local ptr = ffi.cast("void *", u64)
  local saved_q
  exdata(u64)
  for i = 1, 10 do
    local q = exdata()
    if saved_q then
      assert(q == saved_q)
    end
    saved_q = q
  end
  assert(ptr == saved_q)
end

do --- writing: JIT on +jit
  jit.opt.start("minstitch=100000", "hotloop=2")
  local u64 = ffi.new("uintptr_t", 0xefdeaddeadbeefLL)
  local ptr = ffi.cast("void *", u64)
  local saved_q
  for i = 1, 10 do
    exdata(u64)
    local q = exdata()
    if saved_q then
      assert(q == saved_q)
    end
    saved_q = q
  end
  assert(ptr == saved_q)
end

do --- Check number of arguments: JIT off
  jit.off()
  local select = select
  local u64 = ffi.new("uintptr_t", 0xefdeaddeadbeefLL)
  local ptr = ffi.cast("void *", u64)

  assert(nargs(exdata(ptr)) == 0)
  assert(nargs(exdata()) == 1)
end

do --- Check number of arguments: JIT on +jit
  jit.opt.start("minstitch=100000", "hotloop=2")
  local select = select
  local u64 = ffi.new("uintptr_t", 0xefdeaddeadbeefLL)
  local ptr = ffi.cast("void *", u64)

  local total = 0
  for i = 1, 10 do
    total = total + nargs(exdata(ptr))
  end
  assert(total == 0)

  for i = 1, 10 do
    total = total + nargs(exdata())
  end
  assert(total == 10)
end

