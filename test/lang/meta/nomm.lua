
do --- untitled
  local keys = {}
  for i=1,100 do keys[i] = "foo" end
  keys[95] = "__index"
  local function fidx(t, k) return 12345 end
  local mt = { foo = 1, __index = "" }
  local t = setmetatable({ 1 }, mt)
  t[1] = nil
  mt.__index = nil
  local x = nil
  for i=1,100 do
    mt[keys[i]] = fidx
    if t[1] then
      if not x then x = i end
      assert(t[1] == 12345)
    end
  end
  assert(x == 95)
end

