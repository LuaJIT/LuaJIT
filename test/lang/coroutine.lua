do --- traceback
  local co = coroutine.create(function()
    local x = nil
    local y = x.x
  end)
  assert(coroutine.resume(co) == false)
  debug.traceback(co)
end
