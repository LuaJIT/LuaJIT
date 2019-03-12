local ffi = require("ffi")

do --- errno
  ffi.errno(42)
  local x = 0
  for i=1,100 do x = x + ffi.errno() end
  assert(x == 4200)
  ffi.errno(0)
end

do --- string
  local a = ffi.new("uint8_t[?]", 101)
  for i=0,99 do a[i] = i end
  local s
  for i=1,90 do s = ffi.string(a+i, 10) end
  assert(s == "Z[\\]^_`abc")
  for i=1,90 do s = ffi.string(a+i) end
  assert(s == "Z[\\]^_`abc")
end

do --- fill
  local a = ffi.new("uint8_t[?]", 100)
  local x = 0
  for i=0,90 do x = x + a[i]; ffi.fill(a+i, 10, i); x = x + a[i] end
  assert(x == 8100)
  for i=1,100 do ffi.fill(a, 15, 0x1234) end
  assert(a[0] == 0x34 and a[14] == 0x34 and a[15] == 15)
  local b = ffi.new("uint32_t[?]", 104)
  for i=0,100 do ffi.fill(b+i, 15, 0x1234) end
  assert(b[0] == 0x34343434)
  assert(b[103] == (ffi.abi("le") and 0x343434 or 0x34343400))
end

do --- copy array elements
  local a = ffi.new("uint8_t[?]", 100)
  local b = ffi.new("uint8_t[?]", 100)
  for i=0,99 do b[i] = i end
  local x = 0
  for i=0,90 do x = x + a[i]; ffi.copy(a+i, b+i, 1); x = x + a[i] end
  assert(x == 4095)
  local x = 0
  for i=0,90 do ffi.copy(b+i, a+90-i, 10); x = x + b[i] end
  assert(x == 4095)
end

do --- copy from string
  local a = ffi.new("uint8_t[?]", 100, 42)
  for i=0,90 do ffi.copy(a+i, "abc") end
  local x = 0
  for i=0,99 do x = x + a[i] end
  assert(x == 9276)
end

do --- copy structures
  local tp = ffi.typeof("struct { int x, y; }")
  local a = tp(1, 2)
  local b = tp(3, 4)
  local x = 0
  for i=1,100 do a.y = i; ffi.copy(b, a, 8); x = x + b.y end
  assert(x == 5050)
  local x = 0
  for i=1,100 do a.y = i; local t = tp(a); x = x + t.y end
  assert(x == 5050)
end

do --- init struct from first field, complex
  local tp = ffi.typeof("struct { complex x, y; }")
  local cx = ffi.typeof("complex")
  local a = tp(cx(1, 2), cx(3, 4))
  local x = 0
  for i=1,100 do a.y = i; local t = tp(a); x = x + t.y.re end
  assert(x == 5050)
end

do --- int array as parameterised type
  local tp = ffi.typeof("int[10]")
  local a = tp(42)
  local b = ffi.new(ffi.typeof("struct { $ x; }", tp))
  for i=1,100 do b.x = a end
  assert(b.x[0] == 42 and b.x[9] == 42)
end

do --- double array as parameterised type
  local tp = ffi.typeof("double[5]")
  local a = tp(42)
  local b = ffi.new(ffi.typeof("struct { $ x; }", tp))
  for i=1,100 do b.x = a end
  assert(b.x[0] == 42 and b.x[4] == 42)
  b.x[0] = 0
  for i=1,100 do ffi.copy(b.x, a, ffi.sizeof(a)) end
  assert(b.x[0] == 42 and b.x[4] == 42)
end

do --- abi
  local x, y
  for i=1,100 do x = ffi.abi("32bit"); y = ffi.abi("64bit") end
  assert(x == ffi.abi("32bit"))
  assert(y == ffi.abi("64bit"))
  for _,s in ipairs{"64bit", "32bit", "fpu", "softfp", "hardfp", "eabi", "win", "le", "be"} do
    for i=1,100 do x = ffi.abi(s) end
    assert(x == ffi.abi(s))
  end
end

do --- typeof constructed typeof
  local ct = ffi.typeof("struct { int x; }")
  local cd = ct()
  for i=1,100 do assert(ffi.typeof(cd) == ct) end
end
