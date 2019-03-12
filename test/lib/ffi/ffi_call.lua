
local ffi = require("ffi")

dofile("../common/ffi_util.inc")

local tonumber = tonumber

ffi.cdef[[
typedef struct s_ii { int x, y; } s_ii;
typedef struct s_jj { int64_t x, y; } s_jj;
typedef struct s_ff { float x, y; } s_ff;
typedef struct s_dd { double x, y; } s_dd;
typedef struct s_8i { int a,b,c,d,e,f,g,h; } s_8i;

int call_i(int a);
int call_ii(int a, int b);
int call_10i(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j);

typedef enum { XYZ } e_u;

e_u call_ie(e_u a) asm("call_i");

int64_t call_ji(int64_t a, int b);
int64_t call_ij(int a, int64_t b);
int64_t call_jj(int64_t a, int64_t b);

double call_dd(double a, double b);
double call_10d(double a, double b, double c, double d, double e, double f, double g, double h, double i, double j);

float call_ff(float a, float b);
float call_10f(float a, float b, float c, float d, float e, float f, float g, float h, float i, float j);

double call_idifjd(int a, double b, int c, float d, int64_t e, double f);

int call_p_i(int *a);
int *call_p_p(int *a);
int call_pp_i(int *a, int *b);

double call_ividi(int a, ...);

complex call_dd_cd(double a, double b);
complex call_cd(complex a);
complex call_cdcd(complex a, complex b);

complex float call_ff_cf(float a, float b);
complex float call_cf(complex float a);
complex float call_cfcf(complex float a, complex float b);

s_ii call_sii(s_ii a);
s_jj call_sjj(s_jj a);
s_ff call_sff(s_ff a);
s_dd call_sdd(s_dd a);
s_8i call_s8i(s_8i a);
s_ii call_siisii(s_ii a, s_ii b);
s_ff call_sffsff(s_ff a, s_ff b);
s_dd call_sddsdd(s_dd a, s_dd b);
s_8i call_s8is8i(s_8i a, s_8i b);
s_8i call_is8ii(int a, s_8i b, int c);

int __fastcall fastcall_void(void);
int __fastcall fastcall_i(int a);
int __fastcall fastcall_ii(int a, int b);
int __fastcall fastcall_iii(int a, int b, int c);
int64_t __fastcall fastcall_ji(int64_t a, int b);
double __fastcall fastcall_dd(double a, double b);
int __fastcall fastcall_pp_i(int *a, int *b);
s_ii __fastcall fastcall_siisii(s_ii a, s_ii b);
s_dd __fastcall fastcall_sddsdd(s_dd a, s_dd b);

int __stdcall stdcall_i(int a);
int __stdcall stdcall_ii(int a, int b);
double __stdcall stdcall_dd(double a, double b);
float __stdcall stdcall_ff(float a, float b);
]]

local C = ffi.load("../clib/ctest")

assert(C.call_i(-42) == -41)
assert(C.call_ii(-42, 17) == -42+17)
assert(C.call_10i(-42, 17, 12345, 9987, -100, 11, 51, 0x12345678, 338, -78901234) == -42+17+12345+9987-100+11+51+0x12345678+338-78901234)

assert(C.call_ie(123) == 124)

assert(tonumber(C.call_ji(0x123456789LL, -17)) == tonumber(0x123456789LL-17))
assert(tonumber(C.call_ij(-17, 0x123456789LL)) == tonumber(0x123456789LL-17))
assert(tonumber(C.call_jj(-42, 17)) == -42+17)
assert(tonumber(C.call_jj(0x123456789abcdef0LL, -0x789abcde99887766LL)) == tonumber(0x123456789abcdef0LL-0x789abcde99887766LL))

assert(C.call_dd(12.5, -3.25) == 12.5-3.25)
assert(C.call_10d(-42.5, 17.125, 12345.5, 9987, -100.625, 11, 51, 0x12345678, 338, -78901234.75) == -42.5+17.125+12345.5+9987-100.625+11+51+0x12345678+338-78901234.75)

assert(C.call_ff(12.5, -3.25) == 12.5-3.25)
assert(C.call_10f(-42.5, 17.125, 12345.5, 9987, -100.625, 11, 51, 0x123456, 338, -789012.75) == -42.5+17.125+12345.5+9987-100.625+11+51+0x123456+338-789012.75)

assert(C.call_idifjd(-42, 17.125, 0x12345, -100.625, 12345678901234, -789012.75) == -42+17.125+0x12345-100.625+12345678901234-789012.75)

do
  local a = ffi.new("int[10]", -42)
  assert(C.call_p_i(a) == -42+1)
  assert(tonumber(ffi.cast("intptr_t", C.call_p_p(a+3))) == tonumber(ffi.cast("intptr_t", a+4)))
  assert(C.call_pp_i(a+8, a+5) == 3)
end

-- vararg
assert(C.call_ividi(-42, ffi.new("int", 17), 12.5, ffi.new("int", 131)) == -42+17+12.5+131)

-- complex
if pcall(function() return C.call_dd_cd end) then
  do
    local c = C.call_dd_cd(12.5, -3.25)
    assert(c.re == 12.5 and c.im == -3.25*2)
  end
  do
    local c1 = ffi.new("complex", 12.5, -3.25)
    local cz = C.call_cd(c1)
    assert(cz.re == 12.5+1 and cz.im == -3.25-2)
  end
  do
    local c1 = ffi.new("complex", 12.5, -3.25)
    local c2 = ffi.new("complex", -17.125, 100.625)
    local cz = C.call_cdcd(c1, c2)
    assert(cz.re == 12.5-17.125 and cz.im == -3.25+100.625)
  end

  do
    local c = C.call_ff_cf(12.5, -3.25)
    assert(c.re == 12.5 and c.im == -3.25*2)
  end
  do
    local c1 = ffi.new("complex float", 12.5, -3.25)
    local cz = C.call_cf(c1)
    assert(cz.re == 12.5+1 and cz.im == -3.25-2)
  end
  do
    local c1 = ffi.new("complex float", 12.5, -3.25)
    local c2 = ffi.new("complex float", -17.125, 100.625)
    local cz = C.call_cfcf(c1, c2)
    assert(cz.re == 12.5-17.125 and cz.im == -3.25+100.625)
  end
end

-- structs
do
  local s1 = ffi.new("s_ii", -42, 17)
  local sz = C.call_sii(s1)
  assert(s1.x == -42 and s1.y == 17)
  assert(sz.x == -42 and sz.y == 17)
end

do
  local s1 = ffi.new("s_jj", 0x123456789abcdef0LL, -0x789abcde99887766LL)
  local sz = C.call_sjj(s1)
  assert(s1.x == 0x123456789abcdef0LL)
  assert(s1.y == -0x789abcde99887766LL)
  assert(sz.x == 0x123456789abcdef0LL)
  assert(sz.y == -0x789abcde99887766LL)
end

do
  local s1 = ffi.new("s_ff", 12.5, -3.25)
  local sz = C.call_sff(s1)
  assert(s1.x == 12.5 and s1.y == -3.25)
  assert(sz.x == 12.5 and sz.y == -3.25)
end

do
  local s1 = ffi.new("s_dd", 12.5, -3.25)
  local sz = C.call_sdd(s1)
  assert(s1.x == 12.5 and s1.y == -3.25)
  assert(sz.x == 12.5 and sz.y == -3.25)
end

do
  local s1 = ffi.new("s_8i", -42, 17, 12345, 9987, -100, 11, 51, 0x12345678)
  local sz = C.call_s8i(s1)
  assert(s1.a+s1.b+s1.c+s1.d+s1.e+s1.f+s1.g+s1.h == -42+17+12345+9987-100+11+51+0x12345678)
  assert(sz.a+sz.b+sz.c+sz.d+sz.e+sz.f+sz.g+sz.h == -42+17+12345+9987-100+11+51+0x12345678)
end

do
  local s1 = ffi.new("s_ii", -42, 17)
  local s2 = ffi.new("s_ii", 0x12345, -98765)
  local sz = C.call_siisii(s1, s2)
  assert(s1.x == -42 and s1.y == 17)
  assert(s2.x == 0x12345 and s2.y == -98765)
  assert(sz.x == -42+0x12345 and sz.y == 17-98765)
end

do
  local s1 = ffi.new("s_ff", 12.5, -3.25)
  local s2 = ffi.new("s_ff", -17.125, 100.625)
  local sz = C.call_sffsff(s1, s2)
  assert(s1.x == 12.5 and s1.y == -3.25)
  assert(s2.x == -17.125 and s2.y == 100.625)
  assert(sz.x == 12.5-17.125 and sz.y == -3.25+100.625)
end

do
  local s1 = ffi.new("s_dd", 12.5, -3.25)
  local s2 = ffi.new("s_dd", -17.125, 100.625)
  local sz = C.call_sddsdd(s1, s2)
  assert(s1.x == 12.5 and s1.y == -3.25)
  assert(s2.x == -17.125 and s2.y == 100.625)
  assert(sz.x == 12.5-17.125 and sz.y == -3.25+100.625)
end

do
  local s1 = ffi.new("s_8i", -42, 17, 12345, 9987, -100, 11, 51, 0x12345678)
  local s2 = ffi.new("s_8i", 99, 311, 98765, -51, 312, 97, 17, 0x44332211)
  local sz = C.call_s8is8i(s1, s2)
  assert(s1.a+s1.b+s1.c+s1.d+s1.e+s1.f+s1.g+s1.h == -42+17+12345+9987-100+11+51+0x12345678)
  assert(s2.a+s2.b+s2.c+s2.d+s2.e+s2.f+s2.g+s2.h == 99+311+98765-51+312+97+17+0x44332211)
  assert(sz.a+sz.b+sz.c+sz.d+sz.e+sz.f+sz.g+sz.h == -42+17+12345+9987-100+11+51+0x12345678 + 99+311+98765-51+312+97+17+0x44332211)
  assert(sz.a == -42+99)
  assert(sz.h == 0x12345678+0x44332211)
end

do
  local s1 = ffi.new("s_8i", -42, 17, 12345, 9987, -100, 11, 51, 0x12345678)
  local sz = C.call_is8ii(19, s1, -51)
  assert(s1.a+s1.b+s1.c+s1.d+s1.e+s1.f+s1.g+s1.h == -42+17+12345+9987-100+11+51+0x12345678)
  assert(sz.a+sz.b+sz.c+sz.d+sz.e+sz.f+sz.g+sz.h == -42+17+12345+9987-100+11+51+0x12345678 + 19-51)
  assert(sz.a == -42+19)
  assert(sz.c == 12345-51)
end

-- target-specific
if jit.arch == "x86" then
  assert(C.fastcall_void() == 1)
  assert(C.fastcall_i(-42) == -41)
  assert(C.fastcall_ii(-42, 17) == -42+17)
  assert(C.fastcall_iii(-42, 17, 139) == -42+17+139)
  assert(tonumber(C.fastcall_ji(0x123456789LL, -17)) == tonumber(0x123456789LL-17))
  assert(C.fastcall_dd(12.5, -3.25) == 12.5-3.25)

  do
    local a = ffi.new("int[10]", -42)
    assert(C.fastcall_pp_i(a+8, a+5) == 3)
  end

  do
    local s1 = ffi.new("s_ii", -42, 17)
    local s2 = ffi.new("s_ii", 0x12345, -98765)
    local sz = C.fastcall_siisii(s1, s2)
    assert(s1.x == -42 and s1.y == 17)
    assert(s2.x == 0x12345 and s2.y == -98765)
    assert(sz.x == -42+0x12345 and sz.y == 17-98765)
  end

  do
    local s1 = ffi.new("s_dd", 12.5, -3.25)
    local s2 = ffi.new("s_dd", -17.125, 100.625)
    local sz = C.fastcall_sddsdd(s1, s2)
    assert(s1.x == 12.5 and s1.y == -3.25)
    assert(s2.x == -17.125 and s2.y == 100.625)
    assert(sz.x == 12.5-17.125 and sz.y == -3.25+100.625)
  end

  if jit.os == "Windows" then
    assert(C.stdcall_i(-42) == -41)
    assert(C.stdcall_ii(-42, 17) == -42+17)
    assert(C.stdcall_dd(12.5, -3.25) == 12.5-3.25)
    assert(C.stdcall_ff(12.5, -3.25) == 12.5-3.25)
  end
end

