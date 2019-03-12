local tremove = table.remove
local assert = assert

do --- table.remove(t) removes correct entries
  local t = {}
  for i=1,200 do t[i] = i end
  for i=1,100 do tremove(t) end
  assert(#t == 100 and t[100] == 100)
end

do --- table.remove(t) returns the removed entry
  local t = {}
  for i=1,200 do t[i] = i end
  for i=1,100 do assert(tremove(t) == 201-i) end
  assert(#t == 100 and t[100] == 100)
end

do --- table.remove(t, 1) removes and returns the first entry
  local t = {}
  for i=1,200 do t[i] = i end
  for i=1,100 do assert(tremove(t, 1) == i) end
  assert(#t == 100 and t[100] == 200)
end

do --- TSETR hash part +table.new
  local tnew = require"table.new"
  local t = tnew(0, 16)
  for i=10,1,-1 do t[i] = i+3 end
  for i=10,1,-1 do assert(tremove(t) == i+3) end
  assert(#t == 0)
end

do --- TSETR write barrier +table.new
  local tnew = require"table.new"
  for _, t in ipairs{{}, tnew(0, 16)} do
    for i = 1, 10 do t[i] = {i} end
    for i = 1, 10 do
      collectgarbage()
      assert(tremove(t, 1)[1] == i)
    end
  end
end
