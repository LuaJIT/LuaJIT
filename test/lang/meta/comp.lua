
local function create(comp, v1, v2)
  local meta = {
    __lt=function(a,b) return comp("lt", a, b) end,
    __le=function(a,b) return comp("le", a, b) end,
  }
  return setmetatable({v1}, meta), setmetatable({v2}, meta)
end

do --- __lt and __le xop
  local xop
  local a, b = create(function(op,a,b) xop = op; return "" end)
  assert(a<b == true and xop == "lt"); xop = nil
  assert(a>b == true and xop == "lt"); xop = nil
  assert(a<=b == true and xop == "le"); xop = nil
  assert(a>=b == true and xop == "le"); xop = nil

  assert(not (a<b) == false and xop == "lt"); xop = nil
  assert(not (a>b) == false and xop == "lt"); xop = nil
  assert(not (a<=b) == false and xop == "le"); xop = nil
  assert(not (a>=b) == false and xop == "le"); xop = nil

  -- __le metamethod is optional and substituted with arg+res inverted __lt.
  local f = getmetatable(a).__le
  getmetatable(a).__le = nil
  assert(a<b == true and xop == "lt"); xop = nil
  assert(a>b == true and xop == "lt"); xop = nil
  assert(a<=b == false and xop == "lt"); xop = nil
  assert(a>=b == false and xop == "lt"); xop = nil

  assert(not (a<b) == false and xop == "lt"); xop = nil
  assert(not (a>b) == false and xop == "lt"); xop = nil
  assert(not (a<=b) == true and xop == "lt"); xop = nil
  assert(not (a>=b) == true and xop == "lt"); xop = nil
  getmetatable(a).__le = f

  -- Different metatable, but same metamethod works, too.
  setmetatable(b, { __lt = getmetatable(b).__lt, __le = getmetatable(b).__le })
  assert(a<b == true and xop == "lt"); xop = nil
  assert(a>b == true and xop == "lt"); xop = nil
  assert(a<=b == true and xop == "le"); xop = nil
  assert(a>=b == true and xop == "le"); xop = nil

  assert(not (a<b) == false and xop == "lt"); xop = nil
  assert(not (a>b) == false and xop == "lt"); xop = nil
  assert(not (a<=b) == false and xop == "le"); xop = nil
  assert(not (a>=b) == false and xop == "le"); xop = nil
end

do --- __lt and __le values
  local a, b = create(function(op,a,b)
    if op == "lt" then return a[1]<b[1] else return a[1]<=b[1] end end, 1, 2)
  assert(a<b == true)
  assert(a>b == false)
  assert(a<=b == true)
  assert(a>=b == false)

  assert(not (a<b) == false)
  assert(not (a>b) == true)
  assert(not (a<=b) == false)
  assert(not (a>=b) == true)

  b[1] = 1
  assert(a<b == false)
  assert(a>b == false)
  assert(a<=b == true)
  assert(a>=b == true)

  assert(not (a<b) == true)
  assert(not (a>b) == true)
  assert(not (a<=b) == false)
  assert(not (a>=b) == false)

  a[1] = 2
  assert(a<b == false)
  assert(a>b == true)
  assert(a<=b == false)
  assert(a>=b == true)

  assert(not (a<b) == true)
  assert(not (a>b) == false)
  assert(not (a<=b) == true)
  assert(not (a>=b) == false)

  -- __le metamethod is optional and substituted with arg+res inverted __lt.
  getmetatable(a).__le = nil
  a[1] = 1
  b[1] = 2
  assert(a<b == true)
  assert(a>b == false)
  assert(a<=b == true)
  assert(a>=b == false)

  assert(not (a<b) == false)
  assert(not (a>b) == true)
  assert(not (a<=b) == false)
  assert(not (a>=b) == true)

  b[1] = 1
  assert(a<b == false)
  assert(a>b == false)
  assert(a<=b == true)
  assert(a>=b == true)

  assert(not (a<b) == true)
  assert(not (a>b) == true)
  assert(not (a<=b) == false)
  assert(not (a>=b) == false)

  a[1] = 2
  assert(a<b == false)
  assert(a>b == true)
  assert(a<=b == false)
  assert(a>=b == true)

  assert(not (a<b) == true)
  assert(not (a>b) == false)
  assert(not (a<=b) == true)
  assert(not (a>=b) == false)
end
