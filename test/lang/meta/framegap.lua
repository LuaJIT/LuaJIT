do --- untitled
  local t = setmetatable({}, { __add = function(a, b)
    if b > 200 then
      for j=1,10 do end
      return b+3
    elseif b > 100 then
      return b+2
    else
      return b+1
    end
  end })

  local function f(t, i)
    do return t+i end
    -- Force large frame with unassigned slots below mm.
    do local a,b,c,d,e,f,g,h,i,j,k end
  end

  local x = 0
  for i=1,300 do
    x = f(t, i)
  end
  assert(x == 303)
end
