
do --- tonumber int
  local x = 0
  for i=1,100 do x = x + tonumber(i) end
  assert(x == 5050)
end

do --- tonumber float
  local x = 0
  for i=1.5,100.5 do x = x + tonumber(i) end
  assert(x == 5100)
end

do --- tostring int / tonumber
  local t = {}
  for i=1,100 do t[i] = tostring(i) end
  local x = 0
  for i=1,100 do assert(type(t[i]) == "string"); x = x + tonumber(t[i]) end
  assert(x == 5050)
end

do --- tostring float / tonumber
  local t = {}
  for i=1,100 do t[i] = tostring(i+0.5) end
  local x = 0
  for i=1,100 do assert(type(t[i]) == "string"); x = x + tonumber(t[i]) end
  assert(x == 5100)
end

do --- tonumber table
  for i=1,100 do assert(tonumber({}) == nil) end
end

do --- tostring int / tostring
  local t = {}
  for i=1,100 do t[i] = tostring(i) end
  for i=1,100 do t[i] = tostring(t[i]) end
  local x = 0
  for i=1,100 do assert(type(t[i]) == "string"); x = x + t[i] end
  assert(x == 5050)
end

do --- tostring table __tostring
  local mt = { __tostring = function(t) return tostring(t[1]) end }
  local t = {}
  for i=1,100 do t[i] = setmetatable({i}, mt) end
  for i=1,100 do t[i] = tostring(t[i]) end
  local x = 0
  for i=1,100 do assert(type(t[i]) == "string"); x = x + t[i] end
  assert(x == 5050)
end

do --- tostring table __tostring __call
  local r = setmetatable({},
			 { __call = function(x, t) return tostring(t[1]) end })
  local mt = { __tostring = r }
  local t = {}
  for i=1,100 do t[i] = setmetatable({i}, mt) end
  for i=1,100 do t[i] = tostring(t[i]) end
  local x = 0
  for i=1,100 do assert(type(t[i]) == "string"); x = x + t[i] end
  assert(x == 5050)
end

do --- print calls overridden tostring +lua<5.2
  local x = false
  local co = coroutine.create(function() print(1) end)
  debug.setfenv(co, setmetatable({}, { __index = {
    tostring = function() x = true end }}))
  coroutine.resume(co)
  assert(x == true)
end

do --- tonumber base 2
  assert(tonumber(111, 2) == 7)
end

do --- __tostring must be callable
  local t = setmetatable({}, { __tostring = "" })
  assert(pcall(function() tostring(t) end) == false)
end
