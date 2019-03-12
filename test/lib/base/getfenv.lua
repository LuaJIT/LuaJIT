do --- untitled
  local x
  local function f()
    x = getfenv(0)
  end
  local co = coroutine.create(f)
  local t = {}
  debug.setfenv(co, t)
  for i=1,50 do f() f() f() end
  assert(x == getfenv(0))
  coroutine.resume(co)
  assert(x == t)
end
