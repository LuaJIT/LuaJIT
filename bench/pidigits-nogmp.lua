
-- Start of dynamically compiled chunk.
local chunk = [=[

-- Factory function for multi-precision number (mpn) operations.
local function fmm(fa, fb)
  return loadstring([[
    return function(y, a, ka, b, kb)
      local carry, n = 0, #a ]]..(fb == 0 and "" or [[
      local na, nb = n, #b -- Need to adjust lengths. 1 element suffices here.
      if na > nb then b[na] = 0 elseif na < nb then a[nb] = 0; n = nb end
    ]])..[[
      for i=1,n do -- Sum up all elements and propagate carry.
        local x = a[i] ]]..(fa == 2 and "*ka" or "")..
          (fb == 2 and "+b[i]*kb" or (fb == 1 and "+b[i]" or ""))..[[ + carry
        if x < RADIX and x >= 0 then carry = 0; y[i] = x -- Check for overflow.
        else local d = x % RADIX; carry = (x-d) / RADIX; y[i] = d end
      end
      y[n+1] = nil -- Truncate target. 1 element suffices here.
      if carry == 0 then while n > 0 and y[n] == 0 do y[n] = nil end
      elseif carry == -1 then y[n] = y[n] - RADIX else y[n+1] = carry end
    ]]..(fb == 0 and "" or [[ -- Undo length adjustment.
      if na > nb then b[na] = nil elseif na < nb and y ~= a then a[nb] = nil end
    ]])..[[
      return y
    end]])()
end

-- Generate needed mpn functions.
local mm_kk, mm_k1, mm_k0, mm_11 = fmm(2, 2), fmm(2, 1), fmm(2, 0), fmm(1, 1)

-- Choose the most efficient mpn function for y = a*ka + b*kb at run-time.
local function mm(y, a, ka, b, kb)
  local f = mm_kk
  if kb == 0 or #b == 0 then if ka == 1 then return a else f = mm_k0 end
  elseif kb == 1 then if ka == 1 then f = mm_11 else f = mm_k1 end end
  return f(y, a, ka, b, kb)
end

-- Compose matrix with numbers on the right.
local function compose_r(aq,ar,as,at, bq,br,bs,bt)
  mm(ar, ar,bq, at,br) mm(at, at,bt, ar,bs)
  mm(as, as,bt, aq,bs) mm(aq, aq,bq, nil,0)
end

-- Compose matrix with numbers on the left.
local function compose_l(aq,ar,as,at, bq,br,bs,bt)
  mm(ar, ar,bt, aq,br) mm(at, at,bt, as,br)
  mm(as, as,bq, at,bs) mm(aq, aq,bq, nil,0)
end

-- Extract one digit.
local u, v, jj = {}, {}, 0
local function extract(q,r,s,t, j)
  local u = j == jj + 1 and mm(u, u,1, q,1) or mm(u, q,j, r,1); jj = j
  local v = mm(v, t,1, s,j)
  local nu, nv, y = #u, #v
  if nu == nv then
    if nu == 1 then y = u[1] / v[1]
    else y = (u[nu]*RADIX + u[nu-1]) / (v[nv]*RADIX + v[nv-1]) end
  elseif nu == nv+1 then y = (u[nu]*RADIX + u[nv]) / v[nv]
  else return 0 end
  return math.floor(y)
end

-- Coroutine which yields successive digits of PI.
return coroutine.wrap(function()
  local q, r, s, t, k = {1}, {}, {}, {1}, 1
  repeat
    local y = extract(q,r,s,t, 3)
    if y == extract(q,r,s,t, 4) then
      coroutine.yield(y)
      compose_r(q,r,s,t,  10, -10*y, 0, 1)
    else
      compose_l(q,r,s,t,   k, 4*k+2, 0, 2*k+1)
      k = k + 1
    end
  until false
end)

]=] -- End of dynamically compiled chunk.

local N = tonumber(arg and arg[1]) or 27
local RADIX = N < 6500 and 2^36 or 2^32 -- Avoid overflow.

-- Substitute radix and compile chunk.
local pidigit = loadstring(string.gsub(chunk, "RADIX", tostring(RADIX)))()

-- Print lines with 10 digits.
for i=10,N,10 do
  for j=1,10 do io.write(pidigit()) end
  io.write("\t:", i, "\n")
end

-- Print remaining digits (if any).
local n10 = N % 10
if n10 ~= 0 then
  for i=1,n10 do io.write(pidigit()) end
  io.write(string.rep(" ", 10-n10), "\t:", N, "\n")
end
