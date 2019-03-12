
do
  local t = {[0]={}}
  for i=1,1e5 do t[i] = {t[i-1]} end
  for i=1,1e5 do assert(t[i][1] == t[i-1]) end
end

do
  local f
  do
    local x = 0
    function f()
      for i=1,1e5 do x = {i} end
    end
  end
  f()
end

