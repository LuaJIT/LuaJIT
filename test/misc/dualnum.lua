

do --- Positive overflow
  local x = 0
  for i=2147483446,2147483647,2 do x = x + 1 end
  assert(x == 101)
end


do --- Negative overflow
  local x = 0
  for i=-2147483447,-2147483648,-2 do x = x + 1 end
  assert(x == 101)
end


do --- SLOAD with number to integer conversion.
  local k = 1
  local a, b, c = 1/k, 20/k, 1/k
  for i=1,20 do
    for j=a,b,c do end
  end
end

do --- Number comparison
  local function fmin(a, b)
    for i=1,100 do a = math.min(a, b) end
    return a
  end
  local function fmax(a, b)
    for i=1,100 do a = math.max(a, b) end
    return a
  end
  assert(fmin(1, 3) == 1)
  assert(fmin(3, 1) == 1)
  assert(fmin(-1, 3) == -1)
  assert(fmin(3, -1) == -1)
  assert(fmin(-1, -3) == -3)
  assert(fmin(-3, -1) == -3)
  assert(fmax(1, 3) == 3)
  assert(fmax(3, 1) == 3)
  assert(fmax(-1, 3) == 3)
  assert(fmax(3, -1) == 3)
  assert(fmax(-1, -3) == -1)
  assert(fmax(-3, -1) == -1)
end

