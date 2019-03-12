local ffi = require("ffi")

local ctest = require("ctest")

dofile("../common/ffi_util.inc")

local tonumber = tonumber

ffi.cdef[[
typedef struct bar_t {
  int v, w;
} bar_t;
// Same structure, but treated as different struct.
typedef struct barx_t {
  int v, w;
} barx_t;

typedef struct nest_t {
  int a,b;
  struct { int c,d; };
  struct { int e1,e2; } e;
  int f[2];
} nest_t;

typedef union uni_t {
  int8_t a;
  int16_t b;
  int32_t c;
} uni_t;

typedef struct arrinc_t {
  int a[];
} arrinc_t;

typedef enum uenum_t {
  UE0, UE71 = 71, UE72
} uenum_t;

typedef enum ienum_t {
  IE0, IEM12 = -12, IEM11
} ienum_t;

typedef struct foo_t {
  bool b;
  int8_t i8;
  uint8_t u8;
  int16_t i16;
  uint16_t u16;
  int32_t i32;
  uint32_t u32;
  int64_t i64;
  uint64_t u64;
  float f;
  double d;
  complex cf;
  complex cd;
  uint8_t __attribute__((mode(__V16QI__))) v16qi;
  int __attribute__((mode(__V4SI__))) v4si;
  double __attribute__((mode(__V2DF__))) v2df;
  int *pi;
  int *__ptr32 p32i;
  const int *pci;
  volatile int *pvi;
  int **ppi;
  const int **ppci;
  void **ppv;
  char *(*ppf)(char *, const char *);
  int ai[10];
  int ai_guard;
  int ai2[10];
  char ac[10];
  char ac_guard;
  bar_t s;
  bar_t s2;
  bar_t *ps;
  const bar_t *pcs;
  barx_t sx;
  struct { int a,b,c; } si;
  int si_guard;
  nest_t sn;
  uni_t ui;
  uenum_t ue;
  ienum_t ie;
} foo_t;

char *strcpy(char *dest, const char *src);
typedef struct FILE FILE;
int fileno(FILE *stream);
int _fileno(FILE *stream);
]]

do
  local foo_t = ffi.typeof("foo_t")
  local sz = ffi.sizeof(foo_t)
  local x = foo_t()
  local y = foo_t()
  ffi.fill(x, sz, 0xff)
  ffi.fill(y, sz, 0xee)

  -- unknown member
  fails(function(x) local a = x.bad end, x)
  fails(function(x) x.bad = 1 end, x)
  -- too many initializers
  fails(function(x) x.d = ffi.new("double", 1,2) end, x)

  -- conversions to bool
  x.b = false
  assert(x.b == false)
  x.b = true
  assert(x.b == true)
  x.b = 0
  assert(x.b == false)
  x.b = 10
  assert(x.b == true)
  y.b = false
  x.b = y.b
  assert(x.b == false)
  x.b = ffi.new("bool", true)
  assert(x.b == true)
  x.b = ffi.cast("bool", false)
  assert(x.b == false)
  x.b = ffi.new("int32_t", 17)
  assert(x.b == true)
  x.b = ffi.new("int32_t", 0)
  assert(x.b == false)

  -- conversions from bool
  x.i32 = true
  assert(x.i32 == 1)
  x.i32 = false
  assert(x.i32 == 0)
  x.i8 = ffi.new("bool", true)
  assert(x.i8 == 1)
  x.i8 = ffi.new("bool", false)
  assert(x.i8 == 0)
  x.d = true
  assert(x.d == 1)
  x.d = ffi.new("bool", false)
  assert(x.d == 0)
  -- assignment of bool to other types is not allowed
  fails(function(x) x.cd = true end, x)
  fails(function(x) x.v4si = true end, x)
  fails(function(x) x.ai = true end, x)
  fails(function(x) x.s = true end, x)

  -- int to int conversions
  x.i8 = 99
  assert(x.i8 == 99)
  x.i8 = -99
  assert(x.i8 == -99)
  x.i8 = 128
  assert(x.i8 == -128)
  x.i8 = 0xfffe
  assert(x.i8 == -2)
  y.i8 = 91
  x.i8 = y.i8
  assert(x.i8 == 91)
  x.i8 = ffi.new("uint8_t", 0xb7)
  assert(x.i8 == -73)
  x.i8 = ffi.new("int16_t", 0x7fa0)
  assert(x.i8 == -96)
  x.i8 = ffi.new("int32_t", 0xff91)
  assert(x.i8 == -111)
  x.i8 = ffi.new("int64_t", 0xff81)
  assert(x.i8 == -127)

  x.u8 = 99
  assert(x.u8 == 99)
  x.u8 = -99
  assert(x.u8 == 256-99)
  x.u8 = 128
  assert(x.u8 == 128)
  x.u8 = 0xfffe
  assert(x.u8 == 0xfe)
  x.u8 = ffi.new("int8_t", -73)
  assert(x.u8 == 0xb7)
  x.u8 = ffi.new("int16_t", 0x7fa0)
  assert(x.u8 == 0xa0)
  x.u8 = ffi.new("int32_t", 0xff91)
  assert(x.u8 == 0x91)
  x.u8 = ffi.new("int64_t", 0xff81)
  assert(x.u8 == 0x81)

  x.i16 = 99
  assert(x.i16 == 99)
  x.i16 = -99
  assert(x.i16 == -99)
  x.i16 = 32768
  assert(x.i16 == -32768)
  x.i16 = 0xffffffe
  assert(x.i16 == -2)
  x.i16 = ffi.new("int8_t", -10)
  assert(x.i16 == -10)
  x.i16 = ffi.new("uint8_t", 254)
  assert(x.i16 == 254)
  x.i16 = ffi.new("uint16_t", 0xefa0)
  assert(x.i16 == 0xefa0-65536)
  x.i16 = ffi.new("int32_t", 0xffe291)
  assert(x.i16 == 0xe291-65536)
  x.i16 = ffi.new("int64_t", 0xffd481)
  assert(x.i16 == 0xd481-65536)

  x.u16 = 99
  assert(x.u16 == 99)
  x.u16 = -99
  assert(x.u16 == 65536-99)
  x.u16 = 32768
  assert(x.u16 == 32768)
  x.u16 = 0xffffffe
  assert(x.u16 == 65534)
  x.u16 = ffi.new("int8_t", -10)
  assert(x.u16 == 65536-10)
  x.u16 = ffi.new("uint8_t", 254)
  assert(x.u16 == 254)
  x.u16 = ffi.new("int16_t", 0xefa0-65536)
  assert(x.u16 == 0xefa0)
  x.u16 = ffi.new("int32_t", 0xffe291)
  assert(x.u16 == 0xe291)
  x.u16 = ffi.new("int64_t", 0xffd481)
  assert(x.u16 == 0xd481)

  x.i32 = 99
  assert(x.i32 == 99)
  x.i32 = -99
  assert(x.i32 == -99)
  -- double to int conversion for values >= 0x80000000 is undefined
  x.i32 = ffi.new("int8_t", -10)
  assert(x.i32 == -10)
  x.i32 = ffi.new("uint8_t", 254)
  assert(x.i32 == 254)
  x.i32 = ffi.new("int16_t", -517)
  assert(x.i32 == -517)
  x.i32 = ffi.new("uint16_t", 35876)
  assert(x.i32 == 35876)
  x.i32 = ffi.new("uint32_t", 0xffffe291)
  assert(x.i32 == 0xffffe291-2^32)
  x.i32 = ffi.new("int64_t", 15*2^32-317)
  assert(x.i32 == -317)

  x.u32 = 99
  assert(x.u32 == 99)
  -- x.u32 = -99 -- this is undefined on some architectures
  -- assert(x.u32 == 2^32-99)
  x.u32 = 0x87654321
  assert(x.u32 == 0x87654321)
  x.u32 = ffi.new("int8_t", -10)
  assert(x.u32 == 2^32-10)
  x.u32 = ffi.new("uint8_t", 254)
  assert(x.u32 == 254)
  x.u32 = ffi.new("int16_t", -517)
  assert(x.u32 == 2^32-517)
  x.u32 = ffi.new("uint16_t", 35876)
  assert(x.u32 == 35876)
  x.u32 = ffi.new("int32_t", 0xffffe291-2^32)
  assert(x.u32 == 0xffffe291)
  x.u32 = ffi.new("int64_t", 15*2^32-317)
  assert(x.u32 == 2^32-317)

  x.i64 = 99
  assert(tonumber(x.i64) == 99)
  x.i64 = -99
  assert(tonumber(x.i64) == -99)
  x.i64 = 0x1234*2^32+0x87654321
  assert(tonumber(x.i64) == 0x1234*2^32+0x87654321)
  -- double to int64 conversion for values >= 2^63-1 is undefined
  x.i64 = ffi.new("int8_t", -10)
  assert(tonumber(x.i64) == -10)
  x.i64 = ffi.new("uint8_t", 254)
  assert(tonumber(x.i64) == 254)
  x.i64 = ffi.new("int16_t", -517)
  assert(tonumber(x.i64) == -517)
  x.i64 = ffi.new("uint16_t", 35876)
  assert(tonumber(x.i64) == 35876)
  x.i64 = ffi.new("int32_t", -12345678)
  assert(tonumber(x.i64) == -12345678)
  x.i64 = ffi.new("uint32_t", 0xffeeddcc)
  assert(tonumber(x.i64) == 0xffeeddcc)
  x.i64 = ffi.new("uint64_t", 0xffeeddcc*2^32)
  assert(tonumber(x.i64) == 0xffeeddcc*2^32-2^64)

  x.u64 = 99
  assert(tonumber(x.u64) == 99)
  -- x.u64 = -99 -- this is undefined on some architectures
  -- assert(tonumber(x.u64) == 2^64-99)
  x.u64 = 0x1234*2^32+0x87654321
  assert(tonumber(x.u64) == 0x1234*2^32+0x87654321)
  -- double to int64 conversion for values >= 2^63-1 is undefined
  x.u64 = ffi.new("int8_t", -10)
  assert(tonumber(x.u64) == 2^64-10)
  x.u64 = ffi.new("uint8_t", 254)
  assert(tonumber(x.u64) == 254)
  x.u64 = ffi.new("int16_t", -517)
  assert(tonumber(x.u64) == 2^64-517)
  x.u64 = ffi.new("uint16_t", 35876)
  assert(tonumber(x.u64) == 35876)
  x.u64 = ffi.new("int32_t", -12345678)
  assert(tonumber(x.u64) == 2^64-12345678)
  x.u64 = ffi.new("uint32_t", 0xffeeddcc)
  assert(tonumber(x.u64) == 0xffeeddcc)
  x.u64 = ffi.new("int64_t", -0x7feeddcc*2^32)
  assert(tonumber(x.u64) == 2^64-0x7feeddcc*2^32)

  -- FP to int conversions, test for truncation
  x.i32 = 1.9
  assert(x.i32 == 1)
  x.i32 = 2.9
  assert(x.i32 == 2)
  x.i32 = -1.9
  assert(x.i32 == -1)
  x.i32 = -2.9
  assert(x.i32 == -2)
  x.i8 = 1.9
  assert(x.i8 == 1)
  x.u8 = 1.9
  assert(x.u8 == 1)
  x.i16 = 1.9
  assert(x.i16 == 1)
  x.u16 = 1.9
  assert(x.u16 == 1)
  x.u32 = 1.9
  assert(x.u32 == 1)
  x.u64 = 1.9
  assert(tonumber(x.u64) == 1)

  -- int to FP conversions (most tested above)
  x.f = ffi.new("int32_t", -17)
  assert(x.f == -17)
  x.d = ffi.new("int32_t", -17)
  assert(x.d == -17)
  -- test for rounding due to precision loss
  x.f = -1717986919
  assert(x.f == -1717986944)
  x.f = ffi.new("int32_t", 0x77777777)
  assert(x.f == 0x77777780)
  x.d = ffi.new("union { uint32_t u32[2]; uint64_t u64; }",
		{{ 0x77777777, 0x77777777}}).u64
  assert(x.d == 0x77777777*2^32 + 0x77777800)

  -- complex initialization
  x.cd = ffi.new("complex", 9.125, -78.5)
  assert(x.cd.re == 9.125 and x.cd.im == -78.5)
  x.cd = ffi.new("complex", {9.125, -78.5})
  assert(x.cd.re == 9.125 and x.cd.im == -78.5)
  -- too many initializers
  fails(function(x) x.cd = ffi.new("complex", 1,2,3) end, x)

  -- conversions between FP and complex
  x.cf = -17.25
  assert(x.cf.re == -17.25 and x.cf.im == 0)
  x.cf = ffi.new("complex float", -57.5) -- missing initializer
  assert(x.cf.re == -57.5 and x.cf.im == 0)
  x.cf = ffi.new("complex float", 9.125, -78.5)
  assert(x.cf.re == 9.125 and x.cf.im == -78.5)
  x.cf = ffi.new("complex double", 9.125, -78.5)
  assert(x.cf.re == 9.125 and x.cf.im == -78.5)

  x.cd = -17.25
  assert(x.cd.re == -17.25 and x.cd.im == 0)
  x.cd = ffi.new("complex double", -57.5) -- missing initializer
  assert(x.cd.re == -57.5 and x.cd.im == 0)
  x.cd = ffi.new("complex float", 9.125, -78.5)
  assert(x.cd.re == 9.125 and x.cd.im == -78.5)
  x.cd = ffi.new("complex double", 9.125, -78.5)
  assert(x.cd.re == 9.125 and x.cd.im == -78.5)

  x.f = ffi.new("complex float", 9.125, -78.5)
  assert(x.f == 9.125)
  x.f = ffi.new("complex double", 9.125, -78.5)
  assert(x.f == 9.125)

  x.d = ffi.new("complex float", 9.125, -78.5)
  assert(x.d == 9.125)
  x.d = ffi.new("complex double", 9.125, -78.5)
  assert(x.d == 9.125)

  -- conversions between int and complex
  x.cd = ffi.new("int32_t", -138)
  assert(x.cd.re == -138 and x.cd.im == 0)
  x.i32 = ffi.new("complex", 9.125, -78.5)
  assert(x.i32 == 9)

  -- vector initialization
  x.v4si = ffi.new("int __attribute__((mode(__V4SI__)))", 1, 2, 3, 4)
  assert(x.v4si[0] == 1 and x.v4si[1] == 2 and
	 x.v4si[2] == 3 and x.v4si[3] == 4)
  x.v2df = ffi.new("double __attribute__((mode(__V2DF__)))", {3.5, -6.75})
  assert(x.v2df[0] == 3.5 and x.v2df[1] == -6.75)
  -- too many initializers
  fails(function(x)
    x.v4si = ffi.new("int __attribute__((mode(__V4SI__)))", 1,2,3,4,5)
  end, x)

  -- conversions to vectors
  x.v4si = -17
  assert(x.v4si[0] == -17 and x.v4si[1] == -17 and
	 x.v4si[2] == -17 and x.v4si[3] == -17)
  x.v4si = ffi.new("int32_t", 712)
  assert(x.v4si[0] == 712 and x.v4si[1] == 712 and
	 x.v4si[2] == 712 and x.v4si[3] == 712)
  x.v2df = 12.5
  assert(x.v2df[0] == 12.5 and x.v2df[1] == 12.5)
  x.v2df = ffi.new("complex", 9.125, -78.5)
  assert(x.v2df[0] == 9.125 and x.v2df[1] == 9.125)

  -- assignment of same-sized but differently-typed vectors
  x.v16qi = 99
  x.v4si = 0x33333333
  x.v16qi = x.v4si
  assert(x.v16qi[0] == 0x33 and x.v16qi[15] == 0x33)

  -- string converted to enum
  -- x.ue = -1 -- this is undefined on some architectures
  -- assert(x.ue == 0xffffffff)
  x.ue = "UE0"
  assert(x.ue == 0)
  x.ue = "UE72"
  assert(x.ue == 72)
  x.ie = -1
  assert(x.ie == -1)
  x.ie = "IE0"
  assert(x.ie == 0)
  x.ie = "IEM11"
  assert(x.ie == -11)

  x.pi = x.pi
  -- assignment to pointer with higher qualifiers is ok
  x.pci = x.pi
  x.pvi = x.pi
  -- assignment to pointer with lower qualifiers is not ok
  fails(function(x) x.pi = x.pci end, x)
  fails(function(x) x.pi = x.pvi end, x)
  fails(function(x) x.pci = x.pvi end, x)
  fails(function(x) x.pvi = x.pci end, x)
  -- assignment of pointers with incompatible child types is not ok
  fails(function(x) x.ppi = x.ai end, x)
  fails(function(x) x.ppi = x.pi end, x)
  fails(function(x) x.ppv = x.ppi end, x)
  -- qualifiers of child types must match, higher qualifiers not ok
  fails(function(x) x.ppci = x.ppi end, x)
  fails(function(x) x.ppi = x.ppci end, x)

  -- pointer/int conversions are not allowed by default
  fails(function(x) x.pi = 1 end, x)
  fails(function(x) x.i32 = x.pi end, x)
  assert(tonumber(x.pi) == nil)
  assert(tonumber(x.ai) == nil)
  assert(tonumber(x.si) == nil)

  -- but pointer/int casts are allowed
  x.pi = ffi.cast("int *", ffi.new("int32_t", 0x12345678))
  x.i32 = ffi.cast("int32_t", x.pi)
  assert(x.i32 == 0x12345678)
  x.pi = ffi.cast("int *", 1234560.3)
  x.i32 = ffi.cast("int32_t", x.pi)
  assert(x.i32 == 1234560)
  -- bad cast from non-TValue double to pointer
  fails(function(x)
    ffi.cast("int *", ffi.new("double", 1.5))
  end, x)

  -- nil sets a pointer to NULL
  x.pi = nil
  assert(tonumber(ffi.cast("uintptr_t", x.pi)) == 0)

  -- userdata and lightuserdata are treated as void *
  do
    local u = newproxy()
    local uaddr = _G.tonumber(string.match(tostring(u), "(0x.*)"))
    x.pi = u
    assert(tonumber(ffi.cast("uintptr_t", x.pi)) == uaddr)
    x.pi = ctest.lightud(12345678)
    assert(tonumber(ffi.cast("uintptr_t", x.pi)) == 12345678)
  end

  -- io.* file converts to file handle (as a void *)
  if ffi.abi("win") then
    assert(ffi.C._fileno(io.stdout) == 1)
    assert(ffi.C._fileno(io.stderr) == 2)
    local x
    for i=1,100 do x = ffi.C._fileno(io.stderr) end
    assert(x == 2)
  else
    assert(ffi.C.fileno(io.stdout) == 1)
    assert(ffi.C.fileno(io.stderr) == 2)
    local x
    for i=1,100 do x = ffi.C.fileno(io.stderr) end
    assert(x == 2)
  end

  -- truncation/extension of __ptr32
  if ffi.abi("64bit") then
    x.pi = ffi.cast("int *", 15*2^32+0x12345678)
    assert(tonumber(ffi.cast("uintptr_t", x.pi)) == 15*2^32+0x12345678)
    x.p32i = x.pi
    assert(tonumber(ffi.cast("uintptr_t", x.p32i)) == 0x12345678)
    x.pi = ffi.cast("int *", 0x1234*2^32+0x56780000)
    x.pi = x.p32i
    assert(tonumber(ffi.cast("uintptr_t", x.pi)) == 0x12345678)
  end

  -- reference initialization
  do
    x.ai[0] = 712
    local ri = ffi.new("int &", x.ai)
    assert(tonumber(ri) == 712)
    local ra = ffi.new("int (&)[10]", ffi.cast("int (*)[10]", x.ai))
    assert(ra[0] == 712)
  end

  -- ffi.sizeof follows references
  assert(ffi.sizeof(x.ai) == 4*10)
  -- ffi.offsetof follows references
  assert(ffi.offsetof(x.s, "v") == 0)
  assert(ffi.offsetof(x.s, "w") == 4)

  -- ffi.fill writes the right amount
  ffi.fill(x.ai2, ffi.sizeof(x.ai2), 0x72)
  ffi.fill(x.ai, ffi.sizeof(x.ai), 0x13)
  assert(x.ai[0] == 0x13131313)
  assert(x.ai[9] == 0x13131313)
  assert(x.ai2[0] == 0x72727272)
  assert(x.ai2[9] == 0x72727272)

  -- array cannot be assigned a pointer
  fails(function(x) x.ai = x.pi end, x)
  -- but pointer can be assigned the address of an array
  x.pi = x.ai2
  assert(x.pi[0] == 0x72727272)
  assert(x.pi[9] == 0x72727272)
  x.pi = x.ai
  assert(x.pi[0] == 0x13131313)
  assert(x.pi[9] == 0x13131313)
  x.ai = x.ai2 -- array copy
  assert(x.ai[0] == 0x72727272)
  assert(x.ai[9] == 0x72727272)
  -- reflected via pointer, too
  assert(x.pi[0] == 0x72727272)
  assert(x.pi[9] == 0x72727272)
  -- mismatched type or size in array copy
  fails(function(x) x.ai = x.ac end, x)
  fails(function(x) x.ai = ffi.new("int[20]") end, x)
  fails(function(x) x.ai = ffi.new("arrinc_t").a end, x)
  fails(function(x) ffi.new("arrinc_t").a = x.ai end, x)

  ffi.fill(x.s2, ffi.sizeof(x.s2), 0x59)
  x.s.v = 0x12345678
  x.s.w = 0x789abcde
  assert(x.s.v == 0x12345678)
  assert(x.s.w == 0x789abcde)

  -- struct cannot be assigned a pointer
  fails(function(x) x.s = x.ps end, x)
  -- but pointer can be assigned the address of a struct
  x.ps = x.s
  assert(x.ps.v == 0x12345678)
  assert(x.ps.w == 0x789abcde)
  x.pcs = x.s
  assert(x.pcs.v == 0x12345678)
  assert(x.pcs.w == 0x789abcde)
  x.s = x.s2 -- struct copy
  assert(x.s.v == 0x59595959)
  assert(x.s.w == 0x59595959)
  -- reflected via pointer, too
  assert(x.ps.v == 0x59595959)
  assert(x.ps.w == 0x59595959)

  -- structs must be identical, structural equivalence is not enough
  fails(function(x) x.ps = x.sx end, x)
  fails(function(x) x.s = x.sx end, x)

  -- string copy to arrays
  x.ac_guard = 99
  ffi.fill(x.ac, 10, 0x37)
  x.ac = "ABCD"
  assert(x.ac[0] == 65+0)
  assert(x.ac[3] == 65+3)
  assert(x.ac[4] == 0)
  assert(x.ac[5] == 0x37)
  x.ac = "ABCDEFGHI"
  assert(x.ac[8] == 65+8)
  assert(x.ac[9] == 0)
  x.ac = "ABCDEFGHIJ" -- reduced size
  assert(x.ac[8] == 65+8)
  assert(x.ac[9] == 65+9)
  x.ac = "ABCDEFGHIJKLM"
  assert(x.ac[8] == 65+8)
  assert(x.ac[9] == 65+9)
  do -- copy to a[?]
    local vx = ffi.new("struct { char ac[?]; }", 20)
    ffi.fill(vx.ac, 20, 0x37)
    vx.ac = "ABCDEFGHI"
    assert(vx.ac[8] == 65+8)
    assert(vx.ac[9] == 0)
  end
  do -- copy to a[0]
    local vx = ffi.new("union { char ac[0]; char c[20]; }")
    ffi.fill(vx.ac, 20, 0x37)
    vx.ac = "ABCDEFGHI"
    assert(vx.ac[8] == 65+8)
    assert(vx.ac[9] == 0)
  end
  -- mismatched type or size in string copy
  fails(function(x) x.i32 = "ABCD" end, x)
  fails(function(x) x.ai = "ABCD" end, x)
  assert(x.ac_guard == 99) -- Check guard

  -- array initialization
  x.ai = ffi.new("int[10]") -- zero fill
  for i=0,9 do assert(x.ai[i] == 0) end
  x.ai = ffi.new("int[10]", -67) -- replicate first element
  for i=0,9 do assert(x.ai[i] == -67) end
  x.ai = ffi.new("int[10]", 42, -27) -- remainder filled with zero
  assert(x.ai[0] == 42)
  assert(x.ai[1] == -27)
  for i=2,9 do assert(x.ai[i] == 0) end
  x.ai = ffi.new("int[10]", 1,2,3,4,5,6,7,8,9,10)
  for i=0,9 do assert(x.ai[i] == i+1) end
  x.ai = ffi.new("int[10]", {1,2,3,4,5,6,7,8,9,10})
  for i=0,9 do assert(x.ai[i] == i+1) end
  -- VLA initialization
  do
    local v = ffi.new("int[?]", 4)
    for i=0,3 do assert(v[i] == 0) end
    local v = ffi.new("int[?]", 4, 833)
    for i=0,3 do assert(v[i] == 833) end
    local v = ffi.new("int[?]", 4, 12, -9)
    assert(v[0] == 12 and v[1] == -9 and v[2] == 0 and v[3] == 0)
    local v = ffi.new("int[?]", 4, 1,2,3,4)
    assert(v[0] == 1 and v[1] == 2 and v[2] == 3 and v[3] == 4)
  end
  -- too many initializers
  fails(function(x) x.ai = {1,2,3,4,5,6,7,8,9,10,11} end, x)
  for i=0,9 do assert(x.ai[i] == i+1) end -- but it's partially executed
  fails(function(x)
    local v = ffi.new("int[?]", 4, 1,2,3,4,5)
  end, x)

  -- struct initialization
  x.sn = ffi.new("nest_t") -- zero fill
  assert(x.sn.e.e2 == 0)
  x.sn = ffi.new("nest_t", 1,2) -- remainder filled with zero
  assert(x.sn.a == 1 and x.sn.b == 2 and x.sn.c == 0 and x.sn.d == 0)
  assert(x.sn.e.e1 == 0 and x.sn.e.e2 == 0)
  assert(x.sn.f[0] == 0 and x.sn.f[1] == 0)
  x.sn = ffi.new("nest_t", 1,2,3,4,{5,6},{7,8}) -- multi-value init
  assert(x.sn.a == 1 and x.sn.b == 2 and x.sn.c == 3 and x.sn.d == 4)
  assert(x.sn.e.e1 == 5 and x.sn.e.e2 == 6)
  assert(x.sn.f[0] == 7 and x.sn.f[1] == 8)
  x.sn = ffi.new("nest_t", {1,2,3,4,{5,6},{7,8}}) -- single-value init
  assert(x.sn.a == 1 and x.sn.b == 2 and x.sn.c == 3 and x.sn.d == 4)
  assert(x.sn.e.e1 == 5 and x.sn.e.e2 == 6)
  assert(x.sn.f[0] == 7 and x.sn.f[1] == 8)
  -- VLS initialization
  do
    local v = ffi.new("struct { int x; int a[?]; }", 4)
    assert(v.x == 0)
    for i=0,3 do assert(v.a[i] == 0) end
    local v = ffi.new("struct { int x; int a[?]; }", 4, 9, {833})
    assert(v.x == 9)
    -- NYI: fill up VLA in VLS. currently seen as indefinite length
    -- for i=0,3 do assert(v.a[i] == 833) end
    assert(v.a[0] == 833 and v.a[1] == 0 and v.a[2] == 0 and v.a[3] == 0)
  end
  -- no multi-value init beyond first level
  fails(function(x)
    x.sn = ffi.new("nest_t", 1,2,3,4,5,6,7,8)
  end, x)
  -- too many initializers
  fails(function(x)
    x.sn = ffi.new("nest_t", 1,2,3,4,{5,6},{7,8}, 9)
  end, x)

  -- union initialization
  x.ui = ffi.new("uni_t") -- zero fill
  assert(x.ui.a == 0 and x.ui.b == 0 and x.ui.c == 0)
  x.ui = ffi.new("uni_t", 255) -- initialize first field, remainder is zero
  if ffi.abi("le") then
    assert(x.ui.a == -1 and x.ui.b == 255 and x.ui.c == 255)
  else
    assert(x.ui.a == -1 and x.ui.b == -256 and x.ui.c == -16777216)
  end
  -- too many initializers
  fails(function(x)
    x.sn = ffi.new("uni_t", 1,2)
  end, x)
  fails(function()
    ffi.new("union { struct { int x; }; int y; }", 1,2)
  end)

  -- table converted to array
  ffi.fill(x.ai, ffi.sizeof(x.ai), 0x13)
  x.ai_guard = 99
  x.ai = {} -- zero fill
  for i=0,9 do assert(x.ai[i] == 0) end
  x.ai = {42} -- replicate
  for i=0,9 do assert(x.ai[i] == 42) end
  x.ai = {[0] = -67} -- replicate from index 0
  for i=0,9 do assert(x.ai[i] == -67) end
  x.ai = {42, -27} -- remainder filled with zero
  assert(x.ai[0] == 42)
  assert(x.ai[1] == -27)
  for i=2,9 do assert(x.ai[i] == 0) end
  assert(x.ai_guard == 99) -- Check guard

  -- table converted to struct
  ffi.fill(x.si, ffi.sizeof(x.si), 0x74)
  x.si_guard = 97
  -- convert from array part
  x.si = {} -- zero fill
  assert(x.si.a == 0 and x.si.b == 0 and x.si.c == 0)
  x.si = {42, 18} -- fill fields in order
  assert(x.si.a == 42 and x.si.b == 18 and x.si.c == 0)
  x.si = {[0] = -67, 12} -- fill fields in order from index 0
  assert(x.si.a == -67 and x.si.b == 12 and x.si.c == 0)
  x.si = {42, -27, 19, 8} -- too many initializers ignored
  assert(x.si.a == 42 and x.si.b == -27 and x.si.c == 19)
  -- convert from hash part
  x.si = {b = 12}
  assert(x.si.a == 0 and x.si.b == 12 and x.si.c == 0)
  x.si = {b = 12, c = 85, a = 35}
  assert(x.si.a == 35 and x.si.b == 12 and x.si.c == 85)
  x.si = {b = 19, foo = 1, bar = 2} -- unknown initializers ignored
  assert(x.si.a == 0 and x.si.b == 19 and x.si.c == 0)
  x.si = {b = 12, 5, 6, 7} -- hash part ignored if array part exists
  assert(x.si.a == 5 and x.si.b == 6 and x.si.c == 7)
  assert(x.si_guard == 97) -- Check guard

  -- table converted to struct with transparent/nested structs and arrays
  ffi.fill(x.sn, ffi.sizeof(x.sn), 0x74)
  x.sn = {} -- zero fill
  assert(x.sn.e.e2 == 0)
  x.sn = {1,2,3,4,{5,6},{7,8}}
  assert(x.sn.a == 1 and x.sn.b == 2 and x.sn.c == 3 and x.sn.d == 4)
  assert(x.sn.e.e1 == 5 and x.sn.e.e2 == 6)
  assert(x.sn.f[0] == 7 and x.sn.f[1] == 8)
  x.sn = {c = 10, e = {11,12}, f = {13,14}}
  assert(x.sn.a == 0 and x.sn.b == 0 and x.sn.c == 10 and x.sn.d == 0)
  assert(x.sn.e.e1 == 11 and x.sn.e.e2 == 12)
  assert(x.sn.f[0] == 13 and x.sn.f[1] == 14)

  -- table converted to union
  ffi.fill(x.ui, ffi.sizeof(x.ui), 0x58)
  x.ui = {} -- zero fill
  assert(x.ui.a == 0 and x.ui.b == 0 and x.ui.c == 0)
  x.ui = {255, -1, -1} -- only first initializer used
  if ffi.abi("le") then
    assert(x.ui.a == -1 and x.ui.b == 255 and x.ui.c == 255)
  else
    assert(x.ui.a == -1 and x.ui.b == -256 and x.ui.c == -16777216)
  end
  x.ui = {b = -1} -- initialize a specific element of the union
  if ffi.abi("le") then
    assert(x.ui.a == -1 and x.ui.b == -1 and x.ui.c == 65535)
  else
    assert(x.ui.a == -1 and x.ui.b == -1 and x.ui.c == -65536)
  end

  -- copy constructor
  do
    x.s.v = 1; x.s.w = 2
    local s = ffi.new("bar_t", x.s)
    assert(s.v == 1 and s.w == 2)
    for i=0,9 do x.ai[i] = i end
    local a = ffi.new("int[10]", x.ai)
    for i=0,9 do assert(a[i] == i) end
  end

  -- assignment to function pointer
  x.ppf = ffi.C.strcpy
end

do
  collectgarbage()
  local oc = collectgarbage("count")
  local cd = ffi.new"struct { struct { int a; } x;}"
  local function f(cd)
    local x
    for i=1,1e5 do x = cd.x end
  end
  for i=1,2 do
    f(cd)
    local nc = collectgarbage("count")
    assert(nc < oc + 200, "GC step missing for cdata __index")
    jit.off(f)
  end
end

