do --- type instability on loop unroll -> record unroll
  local flip = true
  for i=1,100 do flip = not flip end
  assert(flip == true)
end

do --- untitled
  local t = {}
  local a, b, c = 1, "", t
  for i=1,100 do a,b,c=b,c,a end
  assert(c == 1 and a == "" and b == t)
end

do --- FAILFOLD on loop unroll -> LJ_TRERR_GFAIL -> record unroll
  local t = { 1, 2 }
  local k = 2
  local x = 0
  for i=1,200 do
    x = x + t[k]
    k = k == 1 and 2 or 1
  end
  assert(x == 300 and k == 2)
end

do --- Unroll if inner loop aborts.
  local j = 0
  for i = 1,100 do
    repeat
      j = j+1
    until true
  end
end
