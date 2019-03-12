local ffi = require("ffi")

ffi.cdef[[
typedef struct { int a, b, c; } jit_struct_foo_t;
typedef struct { int a, b, c; } jit_struct_foo2_t;
typedef struct { int a[10]; int b[10]; } jit_struct_sarr_t;
typedef struct jit_struct_chain_t {
  struct jit_struct_chain_t *next;
  int v;
} jit_struct_chain_t;
]]

do --- iteration variable as field name
  local s = ffi.new("jit_struct_foo_t")
  for j,k in ipairs{ "a", "b", "c" } do
    for i=1,100 do s[k] = s[k] + j end
  end
  assert(s.a == 100)
  assert(s.b == 200)
  assert(s.c == 300)
end

do --- constant field names
  local s = ffi.new("jit_struct_foo_t")
  for i=1,100 do
    s.a = s.a + 1
    s.b = s.b + 2
    s.c = s.c + 3
  end
  assert(s.a == 100)
  assert(s.b == 200)
  assert(s.c == 300)
end

do --- constants from structure
  local s = ffi.new("jit_struct_foo_t")
  local s2 = ffi.new("jit_struct_foo2_t", 1, 2, 3)
  for i=1,100 do
    s.a = s.a + s2.a
    s.b = s.b + s2.b
    s.c = s.c + s2.c
  end
  assert(s.a == 100)
  assert(s.b == 200)
  assert(s.c == 300)
end

do --- adding to array elements
  local s = ffi.new("jit_struct_sarr_t")
  for i=1,100 do
    s.a[5] = s.a[5] + 1
    s.b[5] = s.b[5] + 2
  end
  assert(s.a[5] == 100)
  assert(s.b[5] == 200)
end

do --- double indexing
  local s = ffi.new([[
	struct {
	  struct {
	    int x;
	    int b[10];
	  } a[100];
	}]])
  s.a[10].b[4] = 10
  s.a[95].b[4] = 95
  local x = 0
  for i=1,100 do
    x = x + s.a[i-1].b[4] -- reassociate offsets for base and index
  end
  assert(x == 105)
end

do --- structurally identical
  local s1 = ffi.new("struct { int a; }")
  local s2 = ffi.new("struct { int a; }")
  local x = 0
  for j=1,2 do
    for i=1,100 do
      s2.a = i
      s1.a = 1
      x = x + s2.a -- cannot forward across aliasing store
    end
    if j == 1 then
      assert(x == 5050)
      s2 = s1
      x = 0
    else
      assert(x == 100)
    end
  end
end

do --- structurally different
  local s1 = ffi.new("struct { int a; }")
  local s2 = ffi.new("struct { char a; }")
  local x = 0
  for j=1,2 do
    for i=1,100 do
      s2.a = i
      s1.a = 1
      x = x + s2.a -- can forward across aliasing store
    end
    if j == 1 then
      assert(x == 5050)
      s2 = s1 -- this will cause a side trace
      x = 0
    else
      assert(x == 100)
    end
  end
end

do --- union
  local s = ffi.new("union { uint8_t a; int8_t b; }")
  local x = 0
  for i=1,200 do
    s.a = i
    x = x + s.b -- same offset, but must not alias (except if sign-extended)
  end
  assert(x == 1412)
end

do --- circular chain
  local s1 = ffi.new("jit_struct_chain_t")
  local s2 = ffi.new("jit_struct_chain_t")
  local s3 = ffi.new("jit_struct_chain_t")
  s1.next = s2
  s2.next = s3
  s3.next = s1
  local p = s1
  for i=1,99 do
    p.v = i
    p = p.next
  end
  assert(s1.v == 97)
  assert(s2.v == 98)
  assert(s3.v == 99)
end

do --- int struct initialiser
  local ct = ffi.typeof("struct { int a,b,c; }")
  local x,y,z = 0,0,0
  for i=1,100 do
    local s = ct(i, i+1)
    x = x + s.a
    y = y + s.b
    z = z + s.c
  end
  assert(x == 5050)
  assert(y == 5150)
  assert(z == 0)
end

do --- double struct initialiser
  local ct = ffi.typeof("struct { double a,b,c; }")
  local x,y,z = 0,0,0
  for i=1,100 do
    local s = ct(i, i+1)
    x = x + s.a
    y = y + s.b
    z = z + s.c
  end
  assert(x == 5050)
  assert(y == 5150)
  assert(z == 0)
end

do --- pointer / int struct initialiser
  local s1 = ffi.new("jit_struct_chain_t")
  local s
  for i=1,100 do
    s = ffi.new("jit_struct_chain_t", s1, i)
  end
  assert(tonumber(ffi.cast("int", s.next)) ==
	 tonumber(ffi.cast("int", ffi.cast("jit_struct_chain_t *", s1))))
  assert(s.v == 100)
end

do --- unstable pointer/int type struct initialiser
  local ct = ffi.typeof("struct { int *p; int y; }")
  local s
  for i=1,200 do
    if i == 100 then ct = ffi.typeof("jit_struct_chain_t") end
    s = ct(nil, 10)
  end
  assert(s.v == 10)
end

do --- upvalued int box
  local s = ffi.new("struct { int x; }", 42)
  local function f()
    for i=1,100 do
      s.x = i
      assert(s.x == i)
    end
  end
  f()
end

