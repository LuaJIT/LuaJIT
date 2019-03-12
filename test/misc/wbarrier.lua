local t={}
for i=1,20000 do
  t[i] = tostring(i)
end
for i=1,#t do
  assert(t[i] == tostring(i))
end
