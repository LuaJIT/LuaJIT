
local function callmeta(o, a, b)
  return o, a, b
end

local meta = { __call = callmeta }

do --- table
  local t = setmetatable({}, meta)
  local o,a,b = t()
  assert(o == t and a == nil and b == nil)
  local o,a,b = t("foo")
  assert(o == t and a == "foo" and b == nil)
  local o,a,b = t("foo", "bar")
  assert(o == t and a == "foo" and b == "bar")
end

do --- userdata +lua<5.2
  local u = newproxy(true)
  getmetatable(u).__call = callmeta

  local o,a,b = u()
  assert(o == u and a == nil and b == nil)
  local o,a,b = u("foo")
  assert(o == u and a == "foo" and b == nil)
  local o,a,b = u("foo", "bar")
  assert(o == u and a == "foo" and b == "bar")
end

do --- number
  debug.setmetatable(0, meta)
  local o,a,b = (42)()
  assert(o == 42 and a == nil and b == nil)
  local o,a,b = (42)("foo")
  assert(o == 42 and a == "foo" and b == nil)
  local o,a,b = (42)("foo", "bar")
  assert(o == 42 and a == "foo" and b == "bar")
  debug.setmetatable(0, nil)
end

do --- table with changing metamethod
  local tc = setmetatable({}, { __call = function(o,a,b) return o end})
  local ta = setmetatable({}, { __add = tc})
  local o,a = ta + ta
  assert(o == tc and a == nil)

  getmetatable(tc).__call = function(o,a,b) return a end
  local o,a = ta + ta
  assert(o == ta and a == nil)
end

do --- jit table
  local t = setmetatable({}, { __call = function(t, a) return 100-a end })
  for i=1,100 do assert(t(i) == 100-i) end
end

do --- jit table rawget as metamethod
  local t = setmetatable({}, { __call = rawget })
  for i=1,100 do t[i] = 100-i end
  for i=1,100 do assert(t(i) == 100-i) end
end

do --- jit number
  debug.setmetatable(0, { __call = function(n) return 100-n end })
  for i=1,100 do assert((i)() == 100-i) end
  debug.setmetatable(0, nil)
end

do --- jit newindex pcall
  local t = setmetatable({}, { __newindex = pcall, __call = rawset })
  for i=1,100 do t[i] = 100-i end
  for i=1,100 do assert(t[i] == 100-i) end
end

do --- jit index pcall
  local t = setmetatable({}, {
    __index = pcall, __newindex = rawset,
    __call = function(t, i) t[i] = 100-i end,
  })
  for i=1,100 do assert(t[i] == true and rawget(t, i) == 100-i) end
end
