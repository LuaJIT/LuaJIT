
do
  local function sum(n)
    if n == 1 then return 1 end
    return n + sum(n-1)
  end
  assert(sum(200) == 20100)
end

do
  local pcall = pcall
  local tr1
  local x = 0
  function tr1(n)
    if n <= 0 then return end
    x = x + 1
    return pcall(tr1, n-1)
  end
  assert(tr1(200) == true and x == 200)
end

do
  local function fib(n)
    if n < 2 then return 1 end
    return fib(n-2) + fib(n-1)
  end
  assert(fib(15) == 987)
end

