local ffi = require("ffi")

local cx = ffi.typeof("complex")
local cxf = ffi.typeof("complex float")

ffi.cdef[[
typedef struct jit_complex_chain_t {
  struct jit_complex_chain_t *next;
  complex c;
} jit_complex_chain_t;
]]

do --- field access
  local c = cx(1, 2)
  local x
  for i=1,100 do
    x = c.re + c.im
  end
  assert(x == 3)
end

do --- one element circular chain, named indexing
  local cp = ffi.new("jit_complex_chain_t")
  local p = cp
  p.next = p
  p.c = cx(1, 2)
  local x,y = 0,0
  for i=1,100 do
    x = x + p.c.re
    y = y + p.c.im
    p = p.next
  end
  assert(x == 100)
  assert(y == 200)
end

do --- one element circular chain, array indexing
  local cp = ffi.new("jit_complex_chain_t")
  local p = cp
  p.next = p
  p.c = cx(1, 2)
  local x,y = 0,0
  for i=1,100 do
    x = x + p.c[0]
    y = y + p.c[1]
    p = p.next
  end
  assert(x == 100)
  assert(y == 200)
end

do --- one-arg initialiser
  local ca = ffi.new("complex[?]", 101)
  for i=1,100 do
    ca[i] = cx(i) -- handled as init single
  end
  local x,y = 0,0
  for i=1,100 do
    x = x + ca[i].re
    y = y + ca[i].im
  end
  assert(x == 5050)
  assert(y == 0)
end

do --- two-arg initialiser
  local ca = ffi.new("complex[?]", 101)
  for i=1,100 do
    ca[i] = cx(i, -i)
  end
  local x,y = 0,0
  for i=1,100 do
    x = x + ca[i].re
    y = y + ca[i].im
  end
  assert(x == 5050)
  assert(y == -5050)
end

do --- float<>double conversions
  local ca = ffi.new("complex[?]", 101)
  local caf = ffi.new("complex float[?]", 101)
  for i=1,100 do
    ca[i] = cxf(i, -i)
    caf[i] = cx(i, -i)
  end
  local x,y = 0,0
  for i=1,100 do
    x = x + caf[i].re + ca[i].re
    y = y + caf[i].im + ca[i].im
  end
  assert(x == 2*5050)
  assert(y == -2*5050)
end

do --- Complex struct field
  local s = ffi.new("struct { complex x;}")
  for i=1,100 do
    s.x = 12.5i
  end
  assert(s.x.re == 0)
  assert(s.x.im == 12.5)
end

do --- Index overflow for complex is ignored
  local c = cx(1, 2)
  local x
  for i=1e7,1e7+100 do x = c[i] end
end
