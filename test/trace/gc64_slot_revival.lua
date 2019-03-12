do --- BC_KNIL
  local function f(x, y) end
  for i = 1,100 do
    f(i, i)
    f(nil, nil)
  end
end

do --- BC_VARG
  local function f() end
  local function g(...)
    f()
    f(...)
  end
  for i = 1,100 do
    g()
  end
end
