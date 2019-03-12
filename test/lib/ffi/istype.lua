local ffi = require("ffi")

do --- 1
  local void_t = ffi.typeof("void")
  assert(ffi.istype(void_t, void_t))
  assert(ffi.istype("const void", void_t))

  assert(ffi.istype("void", "void") == false) -- 2nd arg is a string.
  assert(ffi.istype("double", 1.5) == false) -- 2nd arg is a number.
end

do --- 2
  local i8_t = ffi.typeof("int8_t")
  local u8_t = ffi.typeof("uint8_t")
  local i32_t = ffi.typeof("int32_t")
  assert(ffi.istype(i32_t, i32_t) == true)
  assert(ffi.istype("const int32_t", i32_t) == true)

  assert(ffi.istype("bool", u8_t) == false)
  assert(ffi.istype(i8_t, u8_t) == false)
  assert(ffi.istype(i32_t, u8_t) == false)
  assert(ffi.istype(u8_t, i32_t) == false)
  assert(ffi.istype("double", i32_t) == false)

  assert(ffi.istype("int64_t", ffi.typeof("long long")))
  assert(ffi.istype("long long", ffi.typeof("int64_t")))
end

do --- 3
  local ptr_t = ffi.typeof("int *")
  local p = ptr_t()
  assert(ffi.istype(ptr_t, ptr_t) == true)
  assert(ffi.istype(ptr_t, p) == true)
  assert(ffi.istype(p, ptr_t) == true)
  assert(ffi.istype("const int *", ptr_t) == true)
  assert(ffi.istype("const int * const", ptr_t) == true)
  assert(ffi.istype("unsigned int *", ptr_t) == true)

  assert(ffi.istype("char *", ptr_t) == false)
  assert(ffi.istype("void *", ptr_t) == false)
end

do --- 4
  ffi.cdef[[
typedef int istype_arr_t[10];
typedef const istype_arr_t istype_carr_t;
typedef struct { int x; } istype_struct_t;
]]

  local arr_t = ffi.typeof("istype_arr_t")
  local carr_t = ffi.typeof("istype_carr_t")
  assert(ffi.istype(arr_t, arr_t) == true)
  assert(ffi.istype("int[10]", arr_t) == true)

  assert(ffi.istype("int[11]", arr_t) == false)
  assert(ffi.istype("int[]", arr_t) == false)
  assert(ffi.istype("int *", arr_t) == false)

  assert(ffi.istype("const int[10]", arr_t) == true)
  assert(ffi.istype("volatile int[10]", arr_t) == true)
  assert(ffi.istype(carr_t, arr_t) == true)

  local struct_t = ffi.typeof("istype_struct_t")
  local structp_t = ffi.typeof("istype_struct_t *")
  assert(ffi.istype(struct_t, struct_t) == true)
  assert(ffi.istype("const istype_struct_t", struct_t) == true)
  assert(ffi.istype("struct { int x; }", struct_t) == false)
  assert(ffi.istype(struct_t, structp_t) == true) -- struct ptr is ok for struct.
  assert(ffi.istype(structp_t, struct_t) == false)
end

do --- 5
  local int_t = ffi.typeof("int")
  local t = {}
  for i=1,200 do t[i] = int_t() end
  t[100] = ffi.new("uint8_t")
  local x = 0
  for i=1,200 do if not ffi.istype("int", t[i]) then x = x + i end end
  assert(x == 100)
  x = 0
  for i=1,200 do if not ffi.istype(int_t, t[i]) then x = x + i end end
  assert(x == 100)
  for i=1,200 do t[i] = int_t end
  t[100] = ffi.typeof("uint8_t")
  x = 0
  for i=1,200 do if not ffi.istype(t[i], int_t) then x = x + i end end
  assert(x == 100)
end
