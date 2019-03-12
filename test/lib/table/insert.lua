local tinsert = table.insert
local assert = assert

do --- table.insert(t,i)
  local t = {}
  for i=1,100 do t[i] = i end
  for i=1,100 do tinsert(t, i) end
  assert(#t == 200 and t[100] == 100 and t[200] == 100)
end

do --- table.insert(t,i,i)
  local t = {}
  for i=1,200 do t[i] = i end
  for i=101,200 do tinsert(t, i, i) end
  assert(#t == 300 and t[101] == 101 and t[200] == 200 and t[300] == 200)
end

