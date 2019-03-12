do --- global assignments !private_G
  g = 0
  gf = 1
  gz = 2

  local function f(i)
    if i == 90 then
      gf = gf + 1
      return true
    end
    g = g + 1
  end

  local function z(i)
    if f(i) then
      gz = gz + 1
    end
  end

  for j=1,5 do
    for i=1,100 do z(i) end
  end

  assert(g == 495)
  assert(gf == 6)
  assert(gz == 7)
end

do --- mutual recursion
  local f, g
  function f(j)
    if j >= 0 then return g(j-1) end
  end
  function g(j)
    for i=1,200 do
      if i > 100 then return f(j) end
    end
  end
  for k=1,20 do g(20) end
end

do --- multi-path mutual recursion
  local f, g
  function f(j, k)
    if j >= 0 then return g(j-1, k) end
    if k >= 0 then return g(20, k-1) end
  end
  function g(j, k)
    for i=1,200 do
      if i > 100 then return f(j, k) end
    end
  end
  g(20, 20)
end

do --- late mutual recursion
  local k = 0
  local f, g

  function g(a)
    -- 'a' is an SLOAD #1 from f's frame and still at slot #1
    -- Avoid losing a in exit if the SLOAD is ignored
    if k > 10 then k = 0 end
    k= k + 1
    return f(a)
  end

  function f(a,b,c,d,e)
    if not e then e =1 end
    a=a+1
    if a > 1000 then return end
    for i=1,100 do
      e=e+1
      if i > 90 then return g(a) end
    end
  end

  f(1,2,3,4,5)
end
