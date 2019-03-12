
local inp = { 0, -"0", 0.5, -0.5, 1, -1, 1/0, -1/0, 0/0 }

local function tostr(n)
  if n == 0 and 1/n < 0 then return "-0"
  elseif 1/n == 0 then return n < 0 and "-inf" or "+inf"
  elseif n ~= n then return "nan"
  else return string.format("%+1.5g", n) end
end

local function check(f, expected)
  local inp = inp
  local out = {}
  for i=1,#inp do out[i] = tostr(f(inp[i])) end
  local got = table.concat(out, " ")
  if got ~= expected then
    error("got: \""..got.."\"\nexpected: \""..expected.."\"", 2)
  end
end

check(function(x) return x end, "+0 -0 +0.5 -0.5 +1 -1 +inf -inf nan")

local powcheck = {
  "+1 +1 +1 +1 +1 +1 +1 +1 +1",
  "+1 +1 +1 +1 +1 +1 +1 +1 +1",
  "+0 +0 +0.70711 nan +1 nan +inf +inf nan",
  "+inf +inf +1.4142 nan +1 nan +0 +0 nan",
  "+0 -0 +0.5 -0.5 +1 -1 +inf -inf nan",
  "+inf -inf +2 -2 +1 -1 +0 -0 nan",
  "+0 +0 +0 +0 +1 +1 +inf +inf nan",
  "+inf +inf +inf +inf +1 +1 +0 +0 nan",
  "nan nan nan nan +1 nan nan nan nan",
}
for j=1,#inp do
  local y = inp[j]
  check(function(x) return x^y end, powcheck[j])
end

check(math.abs, "+0 +0 +0.5 +0.5 +1 +1 +inf +inf nan")
check(math.floor, "+0 -0 +0 -1 +1 -1 +inf -inf nan")
check(math.ceil, "+0 -0 +1 -0 +1 -1 +inf -inf nan")
check(math.sqrt, "+0 -0 +0.70711 nan +1 nan +inf nan nan")
check(math.sin, "+0 -0 +0.47943 -0.47943 +0.84147 -0.84147 nan nan nan")
check(math.cos, "+1 +1 +0.87758 +0.87758 +0.5403 +0.5403 nan nan nan")
check(math.tan, "+0 -0 +0.5463 -0.5463 +1.5574 -1.5574 nan nan nan")
check(math.asin, "+0 -0 +0.5236 -0.5236 +1.5708 -1.5708 nan nan nan")
check(math.acos, "+1.5708 +1.5708 +1.0472 +2.0944 +0 +3.1416 nan nan nan")
check(math.atan, "+0 -0 +0.46365 -0.46365 +0.7854 -0.7854 +1.5708 -1.5708 nan")
check(math.log, "-inf -inf -0.69315 nan +0 nan +inf nan nan")
check(math.log10, "-inf -inf -0.30103 nan +0 nan +inf nan nan")
check(math.exp, "+1 +1 +1.6487 +0.60653 +2.7183 +0.36788 +inf +0 nan")

-- Pointless: deg, rad, min, max, pow
-- LATER: %, fmod, frexp, ldexp, modf, sinh, cosh, tanh

