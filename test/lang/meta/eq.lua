local function create(equal, v1, v2)
  local meta = { __eq = equal }
  return setmetatable({v1}, meta), setmetatable({v2}, meta)
end

do --- __eq xop
  local xop
  local a, b = create(function(a,b) xop = "eq" return "" end)
  assert(a==b == true and xop == "eq"); xop = nil
  assert(a~=b == false and xop == "eq"); xop = nil

  -- Different metatable, but same metamethod works, too.
  setmetatable(b, { __eq = getmetatable(b).__eq })
  assert(a==b == true and xop == "eq"); xop = nil
  assert(a~=b == false and xop == "eq"); xop = nil
end

do --- __eq values
  local a, b = create(function(a,b) return a[1] == b[1] end, 1, 2)
  assert(a==b == false)
  assert(a~=b == true)

  b[1] = 1
  assert(a==b == true)
  assert(a~=b == false)

  a[1] = 2
  assert(a==b == false)
  assert(a~=b == true)
end
