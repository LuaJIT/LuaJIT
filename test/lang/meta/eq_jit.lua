
do --- coverage
  local eq = false
  local t, u = {}, {}
  local x, ax, bx
  local function ck(xx, a, b)
    if x ~= xx then error("bad x", 2) end
    if ax ~= a then error("bad ax", 2) end
    if bx ~= b then error("bad bx", 2) end
  end
  local mt = {
    __eq = function(a, b) ax=a; bx=b; return eq end,
  }
  t = setmetatable(t, mt)
  u = setmetatable(u, mt)
  eq = false
  x = 0; for i=1,100 do x = t == u and 2 or 1 end       ck(1, t, u)
  x = 0; for i=1,100 do x = t ~= u and 2 or 1 end       ck(2, t, u)
  x = 0; for i=1,100 do x = not (t == u) and 2 or 1 end ck(2, t, u)
  x = 0; for i=1,100 do x = not (t ~= u) and 2 or 1 end ck(1, t, u)
  eq = true
  x = 0; for i=1,100 do x = t == u and 2 or 1 end       ck(2, t, u)
  x = 0; for i=1,100 do x = t ~= u and 2 or 1 end       ck(1, t, u)
  x = 0; for i=1,100 do x = not (t == u) and 2 or 1 end ck(1, t, u)
  x = 0; for i=1,100 do x = not (t ~= u) and 2 or 1 end ck(2, t, u)
end

do --- non-constant objects +bit
  local bit = require("bit")
  local mt = { __eq = function(a, b) return true end }
  local tt = { [0] = setmetatable({}, mt), setmetatable({}, mt) }
  for i=0,100 do
    assert(tt[0] == tt[bit.band(i, 1)])
  end
end
