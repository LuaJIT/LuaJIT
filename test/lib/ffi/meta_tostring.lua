local ffi = require("ffi")

ffi.cdef[[
typedef union meta_tostring_foo_t {
  int64_t i64;
  uint64_t u64;
  complex cd;
  double d[2];
  complex float cf;
  float f[2];
} meta_tostring_foo_t;
]]

do --- tostring/typeof semi-roundtrip
  assert(tostring(ffi.typeof("int (*(*[1][2])[3][4])[5][6]")) ==
   "ctype<int (*(*[1][2])[3][4])[5][6]>")
  assert(tostring(ffi.typeof("int (*const)(void)")) ==
   "ctype<int (*const)()>")
  assert(tostring(ffi.typeof("complex float(*(void))[2]")) ==
   "ctype<complex float (*())[2]>")
  assert(tostring(ffi.typeof("complex*")) == "ctype<complex *>")
end

do --- assorted union fields
  local foo_t = ffi.typeof("meta_tostring_foo_t")
  local x = foo_t()

  assert(tostring(foo_t) == "ctype<union meta_tostring_foo_t>")
  assert(string.find(tostring(x), "^cdata<union meta_tostring_foo_t>: "))

  x.i64 = -1;
  assert(tostring(x.i64) == "-1LL")
  assert(tostring(x.u64) == "18446744073709551615ULL")

  x.d[0] = 12.5
  x.d[1] = -753.125
  assert(tostring(x.cd) == "12.5-753.125i")
  x.d[0] = -12.5
  x.d[1] = 753.125
  assert(tostring(x.cd) == "-12.5+753.125i")
  x.d[0] = 0/-1
  x.d[1] = 0/-1
  assert(tostring(x.cd) == "-0-0i")
  x.d[0] = 1/0
  x.d[1] = -1/0
  assert(tostring(x.cd) == "inf-infI")
  x.d[0] = -1/0
  x.d[1] = 0/0
  assert(tostring(x.cd) == "-inf+nanI")

  x.f[0] = 12.5
  x.f[1] = -753.125
  assert(tostring(x.cf) == "12.5-753.125i")
end

