
local a,b,c = 10,1,-1
for i=1,20 do
  if c == -1 then
    a,b,c = 1,10,1
  else
    a,b,c = 10,1,-1
  end
  local x = 0
  for i=a,b,c do for j=1,10 do end x=x+1 end
  assert(x == 10)
end

