do --- smoke
  local x = ((1 or false) and true) or false
  assert(x == true)
end

do --- allcases
  local basiccases = {
    {"nil", nil},
    {"false", false},
    {"true", true},
    {"10", 10},
  }

  local mem = {basiccases}    -- for memoization

  local function allcases (n)
    if mem[n] then return mem[n] end
    local res = {}
    -- include all smaller cases
    for _, v in ipairs(allcases(n - 1)) do
      res[#res + 1] = v
    end
    for i = 1, n - 1 do
      for _, v1 in ipairs(allcases(i)) do
        for _, v2 in ipairs(allcases(n - i)) do
    res[#res + 1] = {
      "(" .. v1[1] .. " and " .. v2[1] .. ")",
      v1[2] and v2[2]
    }
    res[#res + 1] = {
      "(" .. v1[1] .. " or " .. v2[1] .. ")",
      v1[2] or v2[2]
    }
        end
      end
    end
    mem[n] = res   -- memoize
    return res
  end

  for _, v in pairs(allcases(4)) do
    local res = (loadstring or load)("return " .. v[1])()
    if res ~= v[2] then
      error(string.format("bad conditional eval\n%s\nexpected: %s\ngot: %s",
        v[1], tostring(v[2]), tostring(res)))
    end
  end
end

do --- tracefib
  -- 0001    KSHORT   1   2
  -- 0002    ISGE     0   1
  -- 0003    JMP      1 => 0006
  -- 0004    KSHORT   1   1
  -- 0005    JMP      1 => 0013
  --                 ^^^ must be 2
  -- fix in jmp_patchtestreg
  local function fib(n) return (n < 2) and 1 or fib(n-1)+fib(n-2) end
  assert(fib(5) == 8)
  assert(fib(10) == 89)
end
