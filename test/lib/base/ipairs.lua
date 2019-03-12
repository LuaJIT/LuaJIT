do --- small integer values
  local t = { 4,5,6,7,8,9,10 }
  local n = 0
  for i,v in ipairs(t) do
    assert(v == i+3)
    n = n + 1
  end
  assert(n == 7)
end

do --- jit key=value
  local t = {}
  for i=1,100 do t[i]=i end
  local n = 0
  for i,v in ipairs(t) do
    assert(i == v)
    n = n + 1
  end
  assert(n == 100)
end

do --- untitled
  local t = {}
  local o = {{}, {}}
  for i=1,100 do
    local c = i..""
    t[i] = c
    o[1][c] = i
    o[2][c] = i
  end
  o[1]["90"] = nil

  local n = 0
  for _, c in ipairs(t) do
    for i = 1, 2 do
      o[i][c] = o[i][c] or 1
      n = n + 1
    end
  end
  assert(n == 200)
end
