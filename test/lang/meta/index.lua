do --- table 1
  local t=setmetatable({}, {__index=function(t,k)
    return 100-k
  end})

  for i=1,100 do assert(t[i] == 100-i) end

  for i=1,100 do t[i] = i end
  for i=1,100 do assert(t[i] == i) end

  for i=1,100 do t[i] = nil end
  for i=1,100 do assert(t[i] == 100-i) end
end

do --- table 2
  local x
  local t2=setmetatable({}, {__index=function(t,k)
    x = k
  end})

  assert(t2[1] == nil)
  assert(x == 1)

  assert(t2.foo == nil)
  assert(x == "foo")
end

do --- userdata +lua<5.2
  local u = newproxy(true)
  getmetatable(u).__index = { foo = u, bar = 42 }

  local x = 0
  for i=1,100 do
    x = x + u.bar
    u = u.foo
  end
  assert(x == 4200)

  x = 0
  for i=1,100 do
    u = u.foo
    x = x + u.bar
  end
  assert(x == 4200)
end

do --- string
  local s = "foo"
  local mt = debug.getmetatable(s)
  debug.setmetatable(s, {__index = {s = s, len = string.len}})
  local x = 0
  local t = {}
  for i=1,100 do
    x = x + s:len()
    s = s.s
    t[s] = t -- Hash store with same type prevents hoisting
  end
  debug.setmetatable(s, mt)
  assert(x == 300)
end
