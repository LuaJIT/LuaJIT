do --- tables as keys in tables
  local fwd, bck = {}, {}
  for i = 1,100 do
    local v = {}
    fwd[i] = v
    bck[v] = i
  end
  for i = 1,100 do
    local v = fwd[i]
    assert(type(v) == "table")
    assert(bck[v] == i)
  end
end

do --- some tables as keys in tables
  local fwd, bck = {}, {}
  for i = 1,100 do
    local v = {}
    fwd[i] = v
    if i > 90 then
      bck[v] = i
    end
  end
  local n = 0
  for i = 1, 100 do
    local v = fwd[i]
    if bck[v] then
      n = n + 1
    end
  end
  assert(n == 10)
end
