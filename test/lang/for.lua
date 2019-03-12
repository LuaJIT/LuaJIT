do --- direction
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
end

do --- coerce to integer at 13
  local n = 1
  local x = 0
  for i=1,20 do
    for j=n,100 do x = x + 1 end
    if i == 13 then n = "2" end
  end
  assert(x == 1993)
end

do --- coerce to integer at 10
  local n = 1
  local x = 0
  for i=1,20 do
    for j=n,100 do x = x + 1 end
    if i == 10 then n = "2" end
  end
  assert(x == 1990)
end

do --- cannot coerce to integer at 10
  local function f()
    local n = 1
    local x = 0
    for i=1,20 do
      for j=n,100 do x = x + 1 end
      if i == 10 then n = "x" end
    end
  end
  assert(not pcall(f))
end
