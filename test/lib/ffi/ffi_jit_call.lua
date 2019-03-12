
local ffi = require("ffi")

ffi.cdef[[
int call_10i(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j);
double call_10d(double a, double b, double c, double d, double e, double f, double g, double h, double i, double j);
float call_10f(float a, float b, float c, float d, float e, float f, float g, float h, float i, float j);
int64_t call_ij(int a, int64_t b);
bool call_b(int a) asm("call_i");

int64_t call_max(double,double,double,double,double,double,double,double,double,double,double,double,double,double,double,double,double) asm("call_10d");

int64_t call_10j_p(int a, int b, int c, int d, int e, int f, int g, int h, int i, const char *p) asm("call_10j");

int8_t call_i_i8(int a) asm("call_i");
uint8_t call_i_u8(int a) asm("call_i");
int16_t call_i_i16(int a) asm("call_i");
uint16_t call_i_u16(int a) asm("call_i");
int call_i8_i(int8_t a) asm("call_i");
int call_u8_i(uint8_t a) asm("call_i");
int call_i16_i(int16_t a) asm("call_i");
int call_u16_i(uint16_t a) asm("call_i");

int __fastcall fastcall_void(void);
int __fastcall fastcall_i(int a);
int __fastcall fastcall_ii(int a, int b);
int __fastcall fastcall_iii(int a, int b, int c);
int64_t __fastcall fastcall_ji(int64_t a, int b);
double __fastcall fastcall_dd(double a, double b);
int __fastcall fastcall_pp_i(int *a, int *b);

int __stdcall stdcall_i(int a);
int __stdcall stdcall_ii(int a, int b);
double __stdcall stdcall_dd(double a, double b);
float __stdcall stdcall_ff(float a, float b);
]]

local lib = ffi.load("../clib/ctest")

do
  local x
  for i=1,100 do
    x = lib.call_10i(-42, 17, 12345, 9987, -100, 11, 51, 0x12345678, 338, -78901234)
  end
  assert(x == -42+17+12345+9987-100+11+51+0x12345678+338-78901234)
end

do
  for i=1,100 do
    pcall(lib.call_max, i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i,i)
  end
end

if ffi.abi("64bit") then
  local y = ffi.cast("void *", 0x123456789abcdefLL)
  local x
  for i=1,100 do
    lib.call_10j_p(0,0,0,0,0,0,0,0,0, y)
    x = lib.call_10j_p(0,0,0,0,0,0,0,0,0, nil)
  end
  assert(x == 0)
end

do
  local x = 0
  for i=1,100 do
    x = x + lib.call_ij(100+i, i*0x300000002LL)
  end
  assert(x == 0x3b2e0000623eLL)
end

do
  local x
  for i=1,100 do
    x = lib.call_10d(-42.5, 17.125, 12345.5, 9987, -100.625, 11, 51, 0x12345678, 338, -78901234.75)
  end
  assert(x == -42.5+17.125+12345.5+9987-100.625+11+51+0x12345678+338-78901234.75)
end

do
  local x
  for i=1,100 do
    x = lib.call_10f(-42.5, 17.125, 12345.5, 9987, -100.625, 11, 51, 0x123456, 338, -789012.75)
  end
  assert(x == -42.5+17.125+12345.5+9987-100.625+11+51+0x123456+338-789012.75)
end

do
  local x
  for i=-100,100 do
    if not lib.call_b(i) then x = i end
  end
  assert(x == -1)
  local t = {}
  for i=1,100 do t[i] = -1 end
  t[90] = 0
  for i=1,100 do
    if lib.call_b(t[i]) then x = i end
  end
  assert(x == 90)
end

do
  local function tail(x)
    return lib.call_b(x)
  end
  for i=1,100 do local a,b,c = tail(1), tail(1), tail(1) end
end

do
  local x = 0
  for i=0x01010080,0x010100ff do x = x + lib.call_i_i8(i) end
  assert(x == -8128)
  x = 0
  for i=0x01010080,0x010100ff do x = x + lib.call_i_u8(i) end
  assert(x == 24384)
  x = 0
  for i=0x0101ff80,0x0101ffff do x = x + lib.call_i_i16(i) end
  assert(x == -8128)
  x = 0
  for i=0x0101ff80,0x0101ffff do x = x + lib.call_i_u16(i) end
  assert(x == 8314944)
  x = 0
  for i=0x01010080,0x010100ff do x = x + lib.call_i8_i(i) end
  assert(x == -8128)
  x = 0
  for i=0x01010080,0x010100ff do x = x + lib.call_u8_i(i) end
  assert(x == 24640)
  x = 0
  for i=0x0101ff80,0x0101ffff do x = x + lib.call_i16_i(i) end
  assert(x == -8128)
  x = 0
  for i=0x0101ff80,0x0101ffff do x = x + lib.call_u16_i(i) end
  assert(x == 8380480)
end

-- target-specific
if jit.arch == "x86" then
  for i=1,100 do assert(lib.fastcall_i(-42) == -41) end
  for i=1,100 do assert(lib.fastcall_ii(-42, 17) == -42+17) end
  for i=1,100 do assert(lib.fastcall_iii(-42, 17, 139) == -42+17+139) end
  for i=1,100 do assert(lib.fastcall_ji(0x123456789LL, -17) == 0x123456789LL-17) end
  for i=1,100 do assert(lib.fastcall_dd(12.5, -3.25) == 12.5-3.25) end
  local x = lib.fastcall_ji
  for i=1,100 do assert(x(0x123456789LL, -17) == 0x123456789LL-17) end

  if jit.os == "Windows" then
    for i=1,100 do assert(lib.stdcall_i(-42) == -41) end
    for i=1,100 do assert(lib.stdcall_ii(-42, 17) == -42+17) end
    for i=1,100 do assert(lib.stdcall_dd(12.5, -3.25) == 12.5-3.25) end
    for i=1,100 do assert(lib.stdcall_ff(12.5, -3.25) == 12.5-3.25) end
  end
end

