
do --- 1
  local function f(a, b, c, ...)
    assert(c == nil)
    assert(a == 100-b)
    return 100-a, 100-b
  end
  for i=1,100 do
    local x, y = f(i, 100-i)
    assert(x == 100-i)
    assert(y == i)
  end
end

do --- 2
  local function f(a, b, ...)
    if a > b then return b end
    return a
  end
  local x = 0
  for i=1,200 do
    x = x + f(i, 100, 99, 88, 77)
  end
  assert(x == 15050)
end

do --- 3
  local function f(a, b, ...)
    local c, d = ...
    if c > d then return d end
    return c
  end
  local x = 0
  for i=1,200 do
    x = x + f(77, 88, i, 100)
  end
  assert(x == 15050)
end

do --- 4
  local function f(a, b, ...)
    if a > b then end
    return ...
  end
  local x = 0
  for i=1,200 do
    x = x + f(i, 100, i, 100)
    assert(f(i, 100) == nil)
    assert(f(i, 100, 2) == 2)
  end
  assert(x == 20100)
end

do --- 5
  local function f(a, ...)
    local x, y = 0, 0
    for i=1,100 do
      local b, c = ...
      x = x + b
      y = y + c
    end
    assert(x == 200 and y == 300)
  end
  f(1, 2, 3)
end

do --- 6
  local function f(a, ...)
    local t = {[0]=9, 9}
    local v, w, x, y = 0, 0, 0, 0
    for i=1,100 do
      v, w = ...
      t[0] = 9; t[1] = 9;
      x, y = ...
    end
    assert(v == 2 and w == 3 and x == 2 and y == 3)
  end
  f(1, 2, 3)
end

do --- 7
  local function f(a, b, ...)
    for i=1,100 do
      local c, d = ...
      assert(a == c);
      assert(b == d);
    end
  end
  f(2, 3, 2, 3)
  f(2, nil, 2)
  f(nil, nil)
  f(nil)
  f()
end

