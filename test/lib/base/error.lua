do --- no message
  local ok, msg = pcall(error)
  assert(ok == false)
  assert(msg == nil)
end

do --- level 0
  local ok, msg = pcall(error, "emsg", 0)
  assert(ok == false)
  assert(msg == "emsg")
end

do --- default level
  local ok, msg = pcall(error, "emsg")
  assert(ok == false)
  assert(msg == "emsg")
end

do --- default level in xpcall
  local line
  local ok, msg = xpcall(function()
    local x
    line = debug.getinfo(1, "l").currentline; error("emsg")
  end, function(m)
    assert(debug.getlocal(3, 1) == "x")
    return m .."xp"
  end)
  assert(ok == false)
  assert(msg:find("^.-:".. line ..": emsgxp$"))
end

do --- level 2 in xpcall
  local line
  local ok, msg = xpcall(function()
    local function f() error("emsg", 2) end
    line = debug.getinfo(1, "l").currentline; f()
  end, function(m)
    assert(debug.getlocal(4, 1) == "f")
    return m .."xp2"
  end)
  assert(ok == false)
  assert(msg:find("^.-:".. line ..": emsgxp2$"))
end
