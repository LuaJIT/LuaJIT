local ffi = require("ffi")

dofile("../common/ffi_util.inc")

checkfail{
  "struct",
  "struct {",
  "struct xx xx {}",
  "struct { int x  }",
  "struct { int x,  }",
  "struct { int x,y  }",
  "struct { void x; }",
  "struct { int x(void); }",
  "struct recursive1 { struct recursive1 { } x; }",
  "union",
  "union {",
  "union xx xx {}",
  "union { int x  }",
  "union { int x,  }",
  "union { int x,y  }",
  "union { void x; }",
  "union { int x(void); }",
  "union recursive1 { union recursive1 { } x; }",
}

-- NYI: rollback doesn't recover struct state
-- ffi.cdef("struct zzz")
-- fails(ffi.cdef, "struct zzz { int")
-- ffi.cdef("struct zzz { int x; }")

ffi.cdef("struct foo; typedef struct foo foo_t;")
assert(ffi.sizeof("struct foo") == nil)
assert(ffi.sizeof("foo_t") == nil)
ffi.cdef("struct foo { int x,y; };")
assert(ffi.sizeof("struct foo") == 8)
assert(ffi.sizeof("foo_t") == 8)
assert(ffi.sizeof(ffi.typeof("struct foo")) == 8)
assert(ffi.sizeof(ffi.typeof("foo_t")) == 8)
ffi.cdef("struct foo;")
fails(ffi.cdef, "struct foo {};")
fails(ffi.cdef, "union foo;")
fails(ffi.cdef, "union foo {};")
fails(ffi.cdef, "enum foo;")
fails(ffi.cdef, "enum foo { ZZZ1 };")

local P = ffi.sizeof("void *")
local A = (ffi.arch == "x86" and not ffi.abi("win")) and 4 or 8

checktypes{
  0,	1,	"struct {}",
  1,	1,	"struct { char x; }",
  2,	1,	"struct { char x,y; }",
  4,	1,	"struct { char x,y; char a,b; }",
  4,	2,	"struct { char x; short y; }",
  4,	2,	"struct { short x; char y; }",
  8,	4,	"struct { char x; int y; }",
  8,	4,	"struct { int x; char y; }",
  12,	4,	"struct { char x; int y; char z; }",
  P*4,	P,	"struct { char x,*y,**z,a,b,c,d; }",
  64,	4,	"struct { struct { struct { struct { int x,y; } a,b; } a,b; } a,b; }",
  4,	4,	"struct { struct { struct { struct { int x; }; }; }; }",
  8,	4,	"struct { struct foo; }",
  8,	4,	"struct { foo_t; }",
  8,	8,	"struct __attribute__((aligned(sizeof(foo_t)))) { int a; }",
  6,	2,	"struct { char a; char x; short y; char z; char c; }",
  10,	2,	"struct { char a; struct { char x; short y; char z; } b; char c; }",
  8,	A,	"struct { double a; }",
  A+8,	A,	"struct { int a; double b; }",
  8,	A,	"struct { long long a; }",
  A+8,	A,	"struct { int a; long long b; }",
  16,	A,	"struct { _Complex a; }",
  A+16,	A,	"struct { int a; _Complex b; }",
  8,	8,	"struct { float __attribute__((mode(__V2SF__))) a; }",
  16,	8,	"struct { int a; float __attribute__((mode(__V2SF__))) b; }",
  16,	8,	"struct { float __attribute__((mode(__V2SF__))) a[2]; }",
  24,	8,	"struct { int a; float __attribute__((mode(__V2SF__))) b[2]; }",
  16,	16,	"struct { float __attribute__((vector_size(16))) a; }",
  32,	16,	"struct { int a; float __attribute__((vector_size(16))) b; }",
}

checktypes{
  0,	1,	"union {}",
  1,	1,	"union { char x; }",
  1,	1,	"union { char x,y; }",
  2,	2,	"union { char x; short y; }",
  2,	2,	"union { short x; char y; }",
  4,	4,	"union { char x; int y; }",
  4,	4,	"union { int x; char y; }",
  4,	4,	"union { char x; int y; short z; }",
  P,	P,	"union { char x,*y,**z,a,b,c,d; }",
  4,	4,	"union { union { union { union { int x,y; } a,b; } a,b; } a,b; }",
  4,	4,	"union { union { union { union { int x; }; }; }; }",
  2,	2,	"union { union { short x; }; char y; }",
  2,	2,	"union { struct { short x; }; char y; }",
  4,	2,	"struct { union { short x; }; char y; }",
  2,	1,	"union { struct { char a,b; }; char y; }",
  2,	1,	"struct { union { char a,b; }; char y; }",
  8,	A,	"union { double a; }",
  8,	A,	"union { int a; double b; }",
  8,	A,	"union { long long a; }",
  8,	A,	"union { int a; long long b; }",
  16,	A,	"union { _Complex a; }",
  16,	A,	"union { int a; _Complex b; }",
  8,	8,	"union { float __attribute__((mode(__V2SF__))) a; }",
  8,	8,	"union { int a; float __attribute__((mode(__V2SF__))) b; }",
  16,	16,	"union { float __attribute__((vector_size(16))) a; }",
  16,	16,	"union { int a; float __attribute__((vector_size(16))) b; }",
}

do
  local ct
  ct = ffi.typeof("struct { int a; char b; short c; int d; }")
  assert(ffi.offsetof(ct, "a") == 0)
  assert(ffi.offsetof(ct, "b") == 4)
  assert(ffi.offsetof(ct, "c") == 6)
  assert(ffi.offsetof(ct, "d") == 8)
  ct = ffi.typeof("struct { char a; struct { char x; short y; char z; }; char c; }")
  assert(ffi.offsetof(ct, "a") == 0)
  assert(ffi.offsetof(ct, "x") == 2)
  assert(ffi.offsetof(ct, "y") == 4)
  assert(ffi.offsetof(ct, "z") == 6)
  assert(ffi.offsetof(ct, "c") == 8)
  ct = ffi.typeof("struct { char a; struct { short b; struct { int c; }; }; }")
  assert(ffi.offsetof(ct, "a") == 0)
  assert(ffi.offsetof(ct, "b") == 4)
  assert(ffi.offsetof(ct, "c") == 8)
  ct = ffi.typeof("struct { int a; double b; }")
  assert(ffi.offsetof(ct, "a") == 0)
  assert(ffi.offsetof(ct, "b") == A)
end

checkfail{
  "struct { int :; }",
  "struct { int a:; }",
  "struct { int a:bad; }",
  "struct { int a:0; }",
  "struct { int a:33; }",
  "struct { int a:-1; }",
  "struct { _Bool a:2; }",
  "struct { double a:2; }",
  "struct { complex a:2; }",
  "struct { int __attribute__((mode(__TI__))) a:2; }",
  "struct { int __attribute__((vector_size(16))) a:2; }",
  "struct { int a[2]:2; }",
  "struct { void a:2; }",
}

checktypes{
  4,	4,	"struct { unsigned a:1; }",
  4,	4,	"struct { unsigned a:1, b:1, c:1; }",
  1,	1,	"struct { _Bool a:1, b:1, c:1; }",
  8,	4,	"struct { unsigned a:16, b:16, c:16; }",
  8,	4,	"struct { unsigned a:17, b:16, c:16; }",
  12,	4,	"struct { unsigned a:17, b:16, c:17; }",
  12,	4,	"struct { unsigned a:16, b:17, c:16; }",
  8,	4,	"struct { unsigned a:16, :16, c:16; }",
  8,	4,	"struct { unsigned a:17, :16, c:16; }",
  12,	4,	"struct { unsigned a:17, :16, c:17; }",
  12,	4,	"struct { unsigned a:16, :17, c:16; }",
  8,	4,	"struct { unsigned a:16, :0, c:16; }",
  4,	4,	"struct { unsigned a:16, b:16, :0, :0; }",
  8,	4,	"struct { unsigned a:16, :0, :0, :0, c:16; }",
  1,	1,	"struct { char a:1; _Bool b:1; }",
  1,	1,	"struct { char a:1; signed char b:1; unsigned char c:1; }",
}

-- NYI: bit fields > 32 bit
-- local L = ffi.alignof("struct { long long a; }")
-- checktypes{
--   L,	L,	"struct { long long a:1; }",
-- }

-- Bit field packing.
checktypes{
  1,	1,	"struct { _Bool a:1, b:1, c:1; }",
  4,	4,	"struct { short a:9; int b:9; char c; }",
  4,	4,	"struct { char a; int b:7; }",
  4,	4,	"struct { short a; char b; int c:7; }",
  4,	4,	"struct { char a:7; int b:7; int c:7; int d:10; }",
  4,	1,	"struct { char a:7; char b:7; char c:7; char d:7; }",
  4,	4,	"struct { char a:7; int  b:7, c:7, d:7; int  e:4; }",
  4,	4,	"struct { char a:7; int  b:7, c:7, d:7; char e:4; }",
  5,	1,	"struct { char a:7; char b:7, c:7, d:7; char e:4; }",
  4,	1,	"struct __attribute__((packed)) { char a:7; char b:7, c:7, d:7; char e:4; }",
  4,	4,	"struct { char a:7; int b:7; int  c:7; int d:10; }",
  8,	4,	"struct { char a:7; int b:7; char c:7; int d:10; }",
  4,	1,	"struct __attribute__((packed)) { char a:7; int b:7; char c:7; int d:10; }",
  4,	1,	"struct { char a:7; int b:7; char c:7; int d:10; } __attribute__((packed))",
  2,	1,	"struct __attribute__((packed)) { char a:4; char b:8; }",
  2,	1,	"struct __attribute__((packed)) { char a:4; char :0; char b:4; }",
  1,	1,	"struct __attribute__((packed)) { _Bool a:1; _Bool b:1; }",
  2,	1,	"struct __attribute__((packed)) { _Bool a:1; _Bool b:1 __attribute((aligned(1))); }",
  4,	2,	"struct __attribute__((packed)) { _Bool a:1; _Bool b:1 __attribute((aligned(2))); }",
  8,	4,	"struct { _Bool a:1; int b __attribute((aligned(2))); }",
  16,	8,	"struct { _Bool a:1; int b __attribute((aligned(8))); }",
  6,	2,	"struct { _Bool a:1; int b __attribute((aligned(2))) __attribute((packed)); }",
  6,	2,	"struct __attribute__((packed)) { _Bool a:1; int b __attribute((aligned(2))); }",
  6,	2,	"struct __attribute__((packed)) { _Bool a:1; int b __attribute((aligned(2))) __attribute((packed)); }",
}

do
  ffi.cdef[[
    struct foo_packorig { char a; int b; short c; };
    #pragma pack(1)
    struct foo_pack1 { char a; int b; short c; };
    #pragma pack(2)
    struct foo_pack2 { char a; int b; short c; };
    #pragma pack(4)
    struct foo_pack4 { char a; int b; short c; };
    #pragma pack(8)
    struct foo_pack8 { char a; int b; short c; };
    #pragma pack()
    struct foo_packdef { char a; int b; short c; };
    #pragma pack(push)
    struct foo_packpush { char a; int b; short c; };
    #pragma pack(1)
    struct foo_packpush1 { char a; int b; short c; };
    #pragma pack(pop)
    struct foo_packpop { char a; int b; short c; };
    #pragma pack(push,2)
    struct foo_packpush2 { char a; int b; short c; };
    #pragma pack(pop)
    struct foo_packpop2 { char a; int b; short c; };
  ]]

  assert(ffi.sizeof("struct foo_packorig") == 12)
  assert(ffi.sizeof("struct foo_pack1") == 7)
  assert(ffi.sizeof("struct foo_pack2") == 8)
  assert(ffi.sizeof("struct foo_pack4") == 12)
  assert(ffi.sizeof("struct foo_pack8") == 12)
  assert(ffi.sizeof("struct foo_packdef") == 12)
  assert(ffi.sizeof("struct foo_packpush") == 12)
  assert(ffi.sizeof("struct foo_packpush1") == 7)
  assert(ffi.sizeof("struct foo_packpop") == 12)
  assert(ffi.sizeof("struct foo_packpush2") == 8)
  assert(ffi.sizeof("struct foo_packpop2") == 12)
end

do
  ffi.cdef[[
    #pragma pack(2)
    struct foo_packalign8 {
      char a; int y __attribute((aligned(8)));
    };
    typedef int __attribute((aligned(8))) int_align8;
    struct foo_packintalign8 {
      char a; int_align8 y;
    };
    typedef int __attribute((aligned(1))) int_align1;
    struct foo_packintalign1 {
      char a; int_align1 y;
    };
  ]]

  assert(ffi.sizeof("struct foo_packalign8") == 6)
  assert(ffi.sizeof("struct foo_packintalign8") == 6)
  assert(ffi.sizeof("struct foo_packintalign1") == 5)
end

