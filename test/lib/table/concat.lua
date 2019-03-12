local concat, assert, pcall = table.concat, assert, pcall

do --- table.concat
  local t = {a=1,b=2,c=3,d=4,e=5}
  t[1] = 4
  t[3] = 6
  local ok, err = pcall(concat, t, "", 1, 3)
  assert(not ok and err:match("index 2 "))
  local q = {}
  for i=1,100 do q[i] = {9,8,7} end
  q[90] = t
  for i=1,100 do
    assert(pcall(concat, q[i], "", 1, 3) == (i ~= 90))
  end
  t[2] = 5 -- index 1 - 3 in hash part
  q[91] = {}
  q[92] = {9}
  for i=1,100 do q[i] = concat(q[i], "x") end
  assert(q[90] == "4x5x6")
  assert(q[91] == "")
  assert(q[92] == "9")
  assert(q[93] == "9x8x7")
end

do --- table.concat must inhibit CSE and DSE
  local t = {1,2,3}
  local y, z
  for i=1,100 do
    y = concat(t, "x", 1, 3)
    t[2] = i
    z = concat(t, "x", 1, 3)
  end
  assert(y == "1x99x3")
  assert(z == "1x100x3")
end

do --- table.concat must inhibit CSE and DSE 2
  local y
  for i=1,100 do
    local t = {1,2,3}
    t[2] = 4
    y = concat(t, "x")
    t[2] = 9
  end
  assert(y == "1x4x3")
end

do --- table.concat must inhibit CSE and DSE 3
  local t = {[0]={}, {}, {}, {}}
  for i=1,30 do
    for j=3,0,-1 do
      t[j].x = t[j-1]
    end
  end
end
