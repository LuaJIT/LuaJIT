do --- coverage
  local lt, le = false, false
  local t, u = {}, {}
  local x, ax, bx
  local function ck(xx, a, b)
    if x ~= xx then error("bad x", 2) end
    if ax ~= a then error("bad ax", 2) end
    if bx ~= b then error("bad bx", 2) end
  end
  local mt = {
    __lt = function(a, b) ax=a; bx=b; return lt end,
    __le = function(a, b) ax=a; bx=b; return le end,
  }
  t = setmetatable(t, mt)
  u = setmetatable(u, mt)
  lt, le = false, false
  x = 0; for i=1,100 do x = t < u and 2 or 1 end        ck(1, t, u)
  x = 0; for i=1,100 do x = t <= u and 2 or 1 end       ck(1, t, u)
  x = 0; for i=1,100 do x = t > u and 2 or 1 end        ck(1, u, t)
  x = 0; for i=1,100 do x = t >= u and 2 or 1 end       ck(1, u, t)
  x = 0; for i=1,100 do x = not (t < u) and 2 or 1 end  ck(2, t, u)
  x = 0; for i=1,100 do x = not (t <= u) and 2 or 1 end ck(2, t, u)
  x = 0; for i=1,100 do x = not (t > u) and 2 or 1 end  ck(2, u, t)
  x = 0; for i=1,100 do x = not (t >= u) and 2 or 1 end ck(2, u, t)
  lt, le = false, true
  x = 0; for i=1,100 do x = t < u and 2 or 1 end        ck(1, t, u)
  x = 0; for i=1,100 do x = t <= u and 2 or 1 end       ck(2, t, u)
  x = 0; for i=1,100 do x = t > u and 2 or 1 end        ck(1, u, t)
  x = 0; for i=1,100 do x = t >= u and 2 or 1 end       ck(2, u, t)
  x = 0; for i=1,100 do x = not (t < u) and 2 or 1 end  ck(2, t, u)
  x = 0; for i=1,100 do x = not (t <= u) and 2 or 1 end ck(1, t, u)
  x = 0; for i=1,100 do x = not (t > u) and 2 or 1 end  ck(2, u, t)
  x = 0; for i=1,100 do x = not (t >= u) and 2 or 1 end ck(1, u, t)
  lt, le = true, false
  x = 0; for i=1,100 do x = t < u and 2 or 1 end        ck(2, t, u)
  x = 0; for i=1,100 do x = t <= u and 2 or 1 end       ck(1, t, u)
  x = 0; for i=1,100 do x = t > u and 2 or 1 end        ck(2, u, t)
  x = 0; for i=1,100 do x = t >= u and 2 or 1 end       ck(1, u, t)
  x = 0; for i=1,100 do x = not (t < u) and 2 or 1 end  ck(1, t, u)
  x = 0; for i=1,100 do x = not (t <= u) and 2 or 1 end ck(2, t, u)
  x = 0; for i=1,100 do x = not (t > u) and 2 or 1 end  ck(1, u, t)
  x = 0; for i=1,100 do x = not (t >= u) and 2 or 1 end ck(2, u, t)
  lt, le = true, true
  x = 0; for i=1,100 do x = t < u and 2 or 1 end        ck(2, t, u)
  x = 0; for i=1,100 do x = t <= u and 2 or 1 end       ck(2, t, u)
  x = 0; for i=1,100 do x = t > u and 2 or 1 end        ck(2, u, t)
  x = 0; for i=1,100 do x = t >= u and 2 or 1 end       ck(2, u, t)
  x = 0; for i=1,100 do x = not (t < u) and 2 or 1 end  ck(1, t, u)
  x = 0; for i=1,100 do x = not (t <= u) and 2 or 1 end ck(1, t, u)
  x = 0; for i=1,100 do x = not (t > u) and 2 or 1 end  ck(1, u, t)
  x = 0; for i=1,100 do x = not (t >= u) and 2 or 1 end ck(1, u, t)
  mt.__le = nil
  lt = false
  x = 0; for i=1,100 do x = t < u and 2 or 1 end        ck(1, t, u)
  x = 0; for i=1,100 do x = t <= u and 2 or 1 end       ck(2, u, t)
  x = 0; for i=1,100 do x = t > u and 2 or 1 end        ck(1, u, t)
  x = 0; for i=1,100 do x = t >= u and 2 or 1 end       ck(2, t, u)
  x = 0; for i=1,100 do x = not (t < u) and 2 or 1 end  ck(2, t, u)
  x = 0; for i=1,100 do x = not (t <= u) and 2 or 1 end ck(1, u, t)
  x = 0; for i=1,100 do x = not (t > u) and 2 or 1 end  ck(2, u, t)
  x = 0; for i=1,100 do x = not (t >= u) and 2 or 1 end ck(1, t, u)
  lt = true
  x = 0; for i=1,100 do x = t < u and 2 or 1 end        ck(2, t, u)
  x = 0; for i=1,100 do x = t <= u and 2 or 1 end       ck(1, u, t)
  x = 0; for i=1,100 do x = t > u and 2 or 1 end        ck(2, u, t)
  x = 0; for i=1,100 do x = t >= u and 2 or 1 end       ck(1, t, u)
  x = 0; for i=1,100 do x = not (t < u) and 2 or 1 end  ck(1, t, u)
  x = 0; for i=1,100 do x = not (t <= u) and 2 or 1 end ck(2, u, t)
  x = 0; for i=1,100 do x = not (t > u) and 2 or 1 end  ck(1, u, t)
  x = 0; for i=1,100 do x = not (t >= u) and 2 or 1 end ck(2, t, u)
end

do --- Mixed metamethods for ordered comparisons.
  local mt1 = { __lt = function(a, b) return a[1] < b[1] end }
  local mt2 = { __lt = function(a, b) return a[1] < b[1] end }
  local t1 = setmetatable({1}, mt1)
  local t2 = setmetatable({2}, mt2)
  do
    local x
    for i=1,100 do x = t1 <= t1 end
    assert(x == true)
  end
  local ok, ret = pcall(function()
    local x
    for i=1,100 do x = t1 < t2 end
    return x
  end)
  if table.pack then
    assert(ok and ret == true)
  else
    assert(not ok)
  end
  local ok, ret = pcall(function()
    local x
    for i=1,100 do x = t1 <= t2 end
    return x
  end)
  if table.pack then
    assert(ok and ret == true)
  else
    assert(not ok)
  end
end

