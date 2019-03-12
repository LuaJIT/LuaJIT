do --- octal
  local tonumber = tonumber
  local function octal(s) return tonumber(s, 8) end
  for i=1,100 do
    octal("1")
    octal("1")
    octal("1")
  end
end

do --- coroutines
  local t = {
    [0] = function() end,
    coroutine.wrap(function() while true do coroutine.yield() end end),
  }
  for i=1,100 do
    t[i % 2]()
  end
end
