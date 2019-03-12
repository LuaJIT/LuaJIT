do --- ack
  local function Ack(m, n)
    if m == 0 then return n+1 end
    if n == 0 then return Ack(m-1, 1) end
    return Ack(m-1, (Ack(m, n-1))) -- The parentheses are deliberate.
  end
  
  assert(Ack(3,5) == 253)
end

do --- ack notail
  local function Ack(m, n)
    if m == 0 then return n+1 end
    if n == 0 then return Ack(m-1, 1) end
    return (Ack(m-1, (Ack(m, n-1)))) -- The parentheses are deliberate.
  end
  
  assert(Ack(3,5) == 253)
end

do --- fac
  local function fac(n)
    local x = 1
    for i=2,n do
      x = x * i
    end
    return x
  end
  
  assert(fac(10) == 3628800)
end

do --- ffib
  local function ffib(n)
    if n <= 2 then return n,1 end
    if n % 2 == 1 then
      local a,b = ffib((n-1)/2)
      local aa = a*a
      return aa+a*(b+b), aa+b*b
    else
      local a,b = ffib(n/2-1)
      local ab = a+b
      return ab*ab+a*a, (ab+b)*a
    end
  end

  local function fib(n)
    return (ffib(n))
  end

  assert(fib(40) == 165580141)
  assert(fib(39) == 102334155)
  assert(fib(77) == 8944394323791464)
end

do --- fib
  local function fib(n)
    if n < 2 then return 1 end
    return fib(n-2) + fib(n-1)
  end

  assert(fib(27) == 317811)
end

do --- nsieve
  local function nsieve(m)
    local isPrime = {}
    for i=2,m do isPrime[i] = true end
    local count = 0
    for i=2,m do
      if isPrime[i] then
        for k=i+i,m,i do isPrime[k] = false end
        count = count + 1
      end
    end
    return count
  end
  
  assert(nsieve(100) == 25)
  assert(nsieve(12345) == 1474)
end

do --- recsum
  local function sum(n)
    if n == 1 then return 1 end
    return n + sum(n-1)
  end
  
  for i=1, 100 do
    assert(sum(i) == i*(i+1)/2)
  end
end

do --- recsump
  local abs = math.abs
  local function sum(n)
    if n == 1 then return 1 end
    return abs(n + sum(n-1))
  end
  
  for i=1, 100 do
    assert(sum(i) == i*(i+1)/2)
  end
end

do --- tak
  local function tak(x, y, z)
    if y >= x then return z end
    return tak(tak(x-1, y, z), tak(y-1, z, x), (tak(z-1, x, y)))
  end

  assert(tak(21, 14, 7) == 14)
end
