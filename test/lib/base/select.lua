
do --- select #
-- Test whether select("#", 3, 4) returns the correct number of arguments. 
  local x = 0
  for i=1,100 do
    x = x + select("#", 3, 4)
  end
  assert(x == 200)
end

do --- select modf
-- Test whether select("#", func()) also works with func returning multiple values
  local x = 0
  math.frexp(3)
  for i=1,100 do
    x = x + select("#", math.modf(i))
  end
  assert(x == 200)
end

do --- select 1 
  local x = 0
  for i=1,100 do
    x = x + select(1, i)
  end
  assert(x == 5050)
end

do --- select 2
  local x, y = 0, 0
  for i=1,100 do
    local a, b = select(2, 1, i, i+10)
    x = x + a
    y = y + b
  end
  assert(x == 5050 and y == 6050)
end

do --- select vararg #
  local function f(a, ...)
    local x = 0
    for i=1,select('#', ...) do
      x = x + select(i, ...)
    end
    assert(x == a)
  end
  for i=1,100 do
    f(1, 1)
    f(3, 1, 2)
    f(15, 1, 2, 3, 4, 5)
    f(0)
    f(3200, string.byte(string.rep(" ", 100), 1, 100))
  end
end

do --- select vararg i
  local function f(a, ...)
    local x = 0
    for i=1,20 do
      local b = select(i, ...)
      if b then x = x + b else x = x + 9 end
    end
    assert(x == a)
  end
  for i=1,100 do
    f(172, 1)
    f(165, 1, 2)
    f(150, 1, 2, 3, 4, 5)
    f(180)
    f(640, string.byte(string.rep(" ", 100), 1, 100))
  end
end

do --- select vararg 4
  local function f(a, ...)
    local x = 0
    for i=1,20 do
      local b = select(4, ...)
      if b then x = x + b else x = x + 9 end
    end
    assert(x == a)
  end
  for i=1,100 do
    f(180, 1)
    f(180, 1, 2)
    f(80, 1, 2, 3, 4, 5)
    f(180)
    f(640, string.byte(string.rep(" ", 100), 1, 100))
  end
end

do --- varg-select specialisation requires guard against select
  local select = select
  local exptyp = "number"
  local function f(...)
    for i = 1, 100 do
      assert(type((select('#', ...))) == exptyp)
      if i == 75 then
        select = function() return "" end
        exptyp = "string"
      end
    end
  end
  f(1)
end
