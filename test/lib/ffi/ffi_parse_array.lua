local ffi = require("ffi")

dofile("../common/ffi_util.inc")

checkfail{
  "int [",
  "int [-1]",
  "int [[1]]",
  "int [10][]",
  "int [10][?]",
  "int [][]",
  "int [][?]",
  "int [?][]",
  "int [?][?]",
  "int [0x10000][0x2000]",
  "int [256][256][256][256]",
  "int [10](void)",
  "int (void)[10]",
  "int &[10]",
  "union { double x; int a[?]; }",
}

ffi.cdef([[
  typedef int foo1_t[10];
  typedef foo1_t foo2_t[5];
]])
assert(ffi.sizeof("foo1_t") == 40)
assert(ffi.sizeof("foo2_t") == 200)

local P = ffi.sizeof("void *")

checktypes{
  10,	1,	"char [10]",
  4*10,	4,	"int [10]",
  4*10,	4,	"int [10]",
  4*10*5, 4,	"int [10][5]",
  4*10*5*3*2*7, 4,	"int [10][5][3][2][7]",
  4*10*5, 4,	"int ([10])[5]",
  P*10,	P,	"int *[10]",
  P,	P,	"int (*)[10]",
  P*5,	P,	"int (*[5])[10]",
  8*10,	4,	"struct { int x; char y; } [10]",
  P*5*10, P,	"volatile int *(* const *[5][10])(void)",
  nil,	4,	"int []",
  4*10,	8,	"int __attribute__((aligned(8))) [10]",
  4*10,	8,	"__attribute__((aligned(8))) int [10]",
  4*10,	8,	"int [10] __attribute__((aligned(8)))",
  97,	1,	"char ['a']",
  83,	1,	"char ['\\123']",
  79,	1,	"char ['\x4F']",
  5,	1,	"char [sizeof(\"aa\" \"bb\")]",
  80,	8,	"double [10]",
}

do
  assert(ffi.sizeof("int [?]", 10) == 4*10)
  local id = ffi.typeof("const short [?]")
  assert(ffi.sizeof(id, 10) == 2*10)
  assert(ffi.sizeof(id, 0) == 0*10)
  fails(ffi.sizeof, id)
  assert(ffi.sizeof(id, -1) == nil)
  assert(ffi.sizeof(id, 0x80000000) == nil)
  assert(ffi.sizeof(id, 0x40000000) == nil)
  assert(ffi.sizeof(id, 0x3fffffff) == 2*0x3fffffff)
end

do
  assert(ffi.sizeof("struct { double x; int a[?]; }", 10) == 8+4*10)
  local id = ffi.typeof("struct { int x; short a[?]; }")
  assert(ffi.sizeof(id, 10) == 4+2*10)
  assert(ffi.sizeof(id, 0) == 4+0*10)
  fails(ffi.sizeof, id)
  assert(ffi.sizeof(id, -1) == nil)
  assert(ffi.sizeof(id, 0x80000000) == nil)
  assert(ffi.sizeof(id, 0x40000000) == nil)
  assert(ffi.sizeof(id, 0x3ffffffd) == 4+2*0x3ffffffd)
end

