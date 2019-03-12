do --- gcexit
  local x = 0
  local t
  for i=1,1000 do
    if i >= 100 then
      -- causes an exit for atomic phase
      -- must not merge snapshot #0 with comparison since it has the wrong PC
      if i < 150 then x=x+1 end
      t = {i}
    end
  end
  assert(x == 50)
  assert(t[1] == 1000)
end


do --- top !private_G
  function randomtable(entries, depth)
    if depth == 0 then
      return tostring(math.random(2)) -- snapshot between return and CALLMT
    end
    local t = {}
    for k=1,entries do
      t[k] = randomtable(entries, depth-1)
    end
    return t
  end

  local t = randomtable(10, 2)
end

do --- top2
  local function f()
    gcinfo()
    local _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
    local _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
    local _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
    local _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
    local _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
    local _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_
  end

  for i=1,100 do
    f()
    if i % 3 == 0 then collectgarbage() end
  end
end
