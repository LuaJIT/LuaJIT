local ffi = require("ffi")
local jit = require("jit")

ffi.cdef[[
typedef float float4 __attribute__((__vector_size__(16)));
typedef float float8 __attribute__((__vector_size__(32)));
typedef int int4 __attribute__((__vector_size__(16)));
typedef uint8_t byte16 __attribute__((__vector_size__(16)));
]]

local float4 = ffi.new("float[4]")
local float4_2 = ffi.new("float[4]", {2, 2, 2, 2})
local float8 = ffi.new("float[8]", 0)
local byte16 = ffi.new("uint8_t[16]", 1, 0xff, 0)
local int4 = ffi.new("int32_t[5]", 0)
local float4ptr = float4+0

local union64 = ffi.new([[
union __attribute__((packed, aligned(4))){
  int64_t i64;
  struct{
    int32_t low;
    int32_t high;
  };
}]])


describe("intrinsic tests", function()

context("nop inout", function()

  it("fpr", function()
    assert_cdef([[void fpr_nop1(double xmm0) __mcode("90_E") __reglist(out, double xmm0)]], "fpr_nop1")
    local fpr1 = ffi.C.fpr_nop1

    assert_error(function() fpr1() end)
    assert_error(function() fpr1(nil) end)
    assert_error(function() fpr1(1, 2) end)
  
    assert_jit(123.075, function(num) return (fpr1(num)) end, 123.075)
    assert_noexit(-123567.075, function(num) return (fpr1(num)) end, -123567.075)
    
    assert_cdef([[void fpr_all(double xmm0, double xmm1, double xmm2, double xmm3, double xmm4, double xmm5, double xmm6, double xmm7) __mcode("?E") 
                                __reglist(out,double xmm0, double xmm1, double xmm2, double xmm3, double xmm4, double xmm5, double xmm6, double xmm7)]])
    local fpr_all = ffi.intrinsic("fpr_all", "\x90", 1)
                                       
    local function testfpr_all(i, r1, r2, r3, r4, r5, r6, r7, r8)
      local spilled = r1*2*i
      local ro1, ro2, ro3, ro4, ro5, ro6, ro7, ro8 = fpr_all(r1, r2, r3, r4, r5, r6, r7, r8)
      return ro1+i, ro2+i, ro3+i, ro4+i, ro5+i, ro6+i, ro7+i, (ro8*ro3)+i, ro2+spilled
    end

    local function checker(i, ro1, ro2, ro3, ro4, ro5, ro6, ro7, ro8, spilled)
      assert(ro1 == 1.5+i)
      assert(ro2 == 2.5+i)
      assert(ro3 == 3.5+i)
      assert(ro4 == 4.5+i)
      assert(ro5 == 5.5+i)
      assert(ro6 == 60000.525+i)
      assert(ro7 == i+-7.5)
      assert(ro8 == (-100*3.5)+i)
      assert(spilled == 2.5+(1.5*2*i))
    end
    
    assert_jitchecker(checker, testfpr_all, 1.5, 2.5, 3.5, 4.5, 5.5, 60000.525, -7.5, -100)
  end)
  
  it("gpr", function()
    assert_cdef([[void gpr_nop1(int32_t eax) __mcode("90_E") __reglist(out, int32_t eax)]], "gpr_nop1")

    local function testgpr1(num) 
      return (ffi.C.gpr_nop1(num)) 
    end

    assert_jit(1235678, testgpr1, 1235678)
    assert_noexit(-1, testgpr1, -1)

    assert_cdef([[void gpr_scatch(int32_t eax, int32_t ecx, int32_t edx) __mcode("90_E") 
                    __reglist(out, int32_t eax, int32_t ecx, int32_t edx)]], "gpr_scatch")

    
    local function testgpr_scratch(i, r1, r2, r3) 
      local ro1, ro2, ro3 = ffi.C.gpr_scatch(r1, r2, r3)
      return ro1+i, ro2+i, ro3+i
    end

    local function checker(i, ro1, ro2, ro3)
      assert(ro1 == 0+i)
      assert(ro2 == 1+i)
      assert(ro3 == 30000+i)
    end

    assert_jitchecker(checker, testgpr_scratch, 0, 1, 30000)  

    assert_cdef([[void gpr_all(int32_t ebp, int32_t esi, int32_t edi, int32_t eax, int32_t ebx, int32_t ecx, int32_t edx) __mcode("?E") 
                    __reglist(out, int32_t ebp, int32_t esi, int32_t edi, int32_t eax, int32_t ebx, int32_t ecx, int32_t edx)]])
                    
    local gpr_all = ffi.intrinsic("gpr_all", "\x90", 1)
    
    local function testgpr_all(i, r1, r2, r3, r4, r5, r6, r7)
      local spilled = r1+(10000*i)
      local ro1, ro2, ro3, ro4, ro5, ro6, ro7 = gpr_all(r1, r2, r3, r4, 100, r6, r7)
      return spilled+(ro1+ro2+ro3+ro4+ro5+ro6+ro7), ro3+i, ro2+i, ro1+i, ro4+i, ro5, ro6+i, ro7+i
    end

    local function checker(i, spilled, ro3, ro2, ro1, ro4, ro5, ro6, ro7)
      assert(ro1 == 1+i)
      assert(ro2 == 2+i)
      assert(ro3 == 3+i)
      assert(ro4 == 4+i)
      assert(ro5 == 100)
      assert(ro6 == 6+i)
      assert(ro7 == 7+i)
      assert(spilled == 124+(10000*i))
    end

    assert_jitchecker(checker, testgpr_all, 1, 2, 3, 4, 5, 6, 7)
  end)

if ffi.arch == "x64" then
  it("gpr64", function()
    assert_cdef([[void gpr64_1(int64_t rdx) __mcode("90_E") __reglist(out, int64_t rdx)]], "gpr64_1")

    local function testgpr1(num) 
      return (ffi.C.gpr64_1(num)) 
    end

    assert_jit(1235678ull, testgpr1, 1235678)
    assert_noexit(-1LL, testgpr1, -1)

    assert_cdef([[void gpr64(int64_t rbp, int64_t rsi, int64_t rdi, int64_t rax, int64_t rbx, int64_t rcx, int64_t rdx) __mcode("?E") 
                    __reglist(out, int64_t rbp, int64_t rsi, int64_t rdi, int64_t rax, int64_t rbx, int64_t rcx, int64_t rdx)]])
                    
    local gpr7 = ffi.intrinsic("gpr64", "\x90", 1)
        
    local function testgpr_all(i, r1, r2, r3, r4, r5, r6, r7)
      local spilled = r1+(10000*i)
      local ro1, ro2, ro3, ro4, ro5, ro6, ro7 = gpr7(r1, r2, r3, 68719476735ll, r5, r6, r7)
      return spilled, ro3+i, ro2+i, ro1+i, ro4+i, ro5+i, ro6+i, ro7+i
    end
    
    local function checker(i, spilled, ro3, ro2, ro1, ro4, ro5, ro6, ro7)
      local sp = (10000*i)+1
      assert(ro1 == 1+i)
      assert(ro2 == 2+i)
      assert(ro3 == 3+i)
      assert(type(ro4) == "cdata" and ro4 == 68719476735ll+i)
      assert(type(ro5) == "cdata" and ro5 == 5ll+i)
      assert(type(ro6) == "cdata" and ro6 == 68719476721ll+i)
      assert(type(ro7) == "cdata" and ro7 == (-7ll)+i)
      assert(spilled == sp)
    end
    
    assert_jitchecker(checker, testgpr_all, 1, 2, 3, 4, 5ll, 68719476721ll, -7ll)
  end)
  
  it("rex fpr", function()
    assert_cdef([[void fpr_reg(double xmm9, double xmm0) __mcode("90_E") __reglist(out, double xmm0, double xmm9)]], "fpr_reg")
    local fpr = ffi.C.fpr_reg
  
    local function testrex(n1, n2)
      local o1, o2 = fpr(n1, n2)
      return o1+o2
    end
    
    assert_jit(444.575, testrex, 123.075, 321.5)
  end)
 
  it("fpr_vexrex(ymm)", function()
    local array = ffi.new("float8", 0, 1, 2, 3, 4, 5, 6, 7)
    --force a Vex.B base register
    
    assert_cdef([[void fpr_vexrex(float8 ymm14, int32_t eax, int32_t ebx, int32_t ecx, int32_t edx, int32_t esi, int32_t edi, int32_t ebp) __mcode("?E") 
                                  __reglist(out, float8 ymm14, int32_t eax, int32_t ebx, int32_t ecx, int32_t edx, int32_t esi, int32_t edi, int32_t ebp) 
                                  __reglist(mod, ymm1, ymm7)]])
                    
    local ymmtest = ffi.intrinsic("fpr_vexrex", "\x90", 1)

    local ymmout = ymmtest(array, 1, 2, 3, 4, 5, 6, 7)
    
    for i=0,7 do
      assert_equal(ymmout[i], i)
    end
  end)
end
  
  it("fpr_vec", function()
    assert_cdef([[void fpr_vec(float4 xmm7v) __mcode("90_E") __reglist(out, float4 xmm7v)]], "fpr_vec")
  
    local v1 = ffi.new("float[4]", 1, 2, 3, 4)
    local xmmout = ffi.C.fpr_vec(v1)  
    assert_v4eq(xmmout, 1, 2, 3, 4)    
  end)
  
  it("check extra register spill", function()
    local array = ffi.new("float4", 1, 2, 3, 4)
    
    -- Use up all gpr scatch registers before loading the vec causing wrapper builder to restart with an extra spill
    assert_cdef([[void spillrestart(float4 xmm0v, int32_t eax, int32_t ecx, int32_t edx, int32_t esi, int32_t edi, int32_t ebx) __mcode("?E") 
                                  __reglist(out, int32_t eax, int32_t ecx, int32_t edx, int32_t esi, int32_t edi, int32_t ebx, float4 xmm0v) 
                 ]])
    
    local xmmtest = ffi.intrinsic("spillrestart", "\x90", 1)
    local eax, ecx, edx, esi, edi, ebx, xmmout = xmmtest(array, 1, 2, 3, 4, 5, 6)
    
    assert_equal(eax, 1)
    assert_equal(ecx, 2)
    assert_equal(edx, 3)
    assert_equal(esi, 4)
    assert_equal(edi, 5)
    assert_equal(ebx, 6)

    for i=1,4 do
      assert_equal(xmmout[i-1], i)
    end
  end) 

  it("fpr_vec(ymm)", function()
    assert_cdef([[void fpr_ymmvec(float8 ymm7) __mcode("90_E") __reglist(out, float8 ymm7)]], "fpr_ymmvec")
    --test using plain array in place of a vector 
    local v1 = ffi.new("float[8]", 0, 1, 2, 3, 4, 5, 6, 7)
    local ymmout = ffi.C.fpr_ymmvec(v1)
    
    for i=0,7 do
      assert_equal(ymmout[i], i)
    end
  
    assert_cdef([[void fpr_ymmvec2(float8 ymm0, void* ymm7) __mcode("90_E") __reglist(out, float8 ymm7, float8 ymm0)]], "fpr_ymmvec2")
    
    local v2 = ffi.new("float[8]", 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5) 
    local ymmtest2 = ffi.C.fpr_ymmvec2
    local ymm7, ymm0 = ymmtest2(v1, v2)
    
    for i=0,7 do
      assert_equal(ymm0[i], i)
    end    
    for i=0,7 do
      assert_equal(ymm7[i], i+0.5)
    end
    
    --test using a cdata vector
    v2 = ffi.new("float8", 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5) 
    ymm7, ymm0 = ymmtest2(v1, v2)
    
    for i=0,7 do
      assert_equal(ymm0[i], i)
    end 
    for i=0,7 do
      assert_equal(ymm7[i], i+0.5)
    end
  end)
  
  it("idiv", function()
    assert_cdef([[void idiv(int32_t eax, int32_t ecx) __mcode("99F7F9_E") __reglist(out, int32_t eax, int32_t edx)]], "idiv")

    local function checker(i, result, remainder)
      local rem = i%3
    
      if rem ~= remainder then
        return rem, remainder
      end
    
      local expected = (i-rem)/3
      
      if expected ~= result then
        return expected, result
      end
    end
    
    local function test_idiv(value, divisor)
      local result, remainder = ffi.C.idiv(value, divisor)
      return result, remainder
    end
  
    assert_jitchecker(checker, test_idiv, 3)
    
    --test with jited with a constant arg
    local function test_idivK(value)
      local result, remainder = ffi.C.idiv(value, 3)
      return result, remainder
    end
    
    assert_jitchecker(checker, test_idivK, 3)
  end)
end)


context("__mcode", function()
  
  it("incomplete mcode def", function()
    assert_cdeferr([[int test1() __mcode]])
    assert_cdeferr([[int test2() __mcode(]])
    assert_cdeferr([[int test3() __mcode()]])
    assert_cdeferr([[int test3() __mcode(,)]])
    assert_cdeferr([[int test4() __mcode("ff"]])
    assert_cdeferr([[int test5() __mcode("ff",,)]])
    assert_cdeferr([[int test6() __mcode("ff" 1)]])
    assert_cdeferr([[int test7() __mcode("ff", )]])
    assert_cdeferr([[int test8() __mcode("ff", 1]])  
    assert_cdeferr([[int test9() __mcode("ff", 1, 1]])
    assert_cdeferr([[int test10() __mcode("ff", 1, 1, ]])
    
    assert_cdeferr([[__mcode("90")]])
    assert_cdeferr([[int __mcode("90")]])
  end)

  it("bad mcoddef", function() 
    assert_cdeferr([[void test1(float a) __mcode(0);]])
    assert_cdeferr([[void test2(float a) __mcode("");]])
    assert_cdeferr([[void test3(float a) __mcode("0");]])
    assert_cdeferr([[void test4(float a) __mcode("rff");]])
    assert_cdeferr([[struct c{float a __mcode("90");};]])
    --Max 2 literals after the opcode string
    assert_cdeferr([[int test11() __mcode("ff", 1, 1, 2)]])
    
    assert_cdeferr([[struct b{float a; __mcode("90");};]])
  end)
  
  it("invalid registers", function()
    assert_cdef([[void validreg_gpr(int eax) __mcode("90_E");]], "validreg_gpr")
    
    assert_cdeferr([[void badreg_1(int e) __mcode("90_E");]], "invalid")
    assert_cdeferr([[void badreg_1(int r20d) __mcode("90_E");]], "invalid") 
    assert_cdeferr([[void badreg_gpr1() __mcode("90_E") __reglist(out, int e);]], "invalid")
    assert_cdeferr([[void badreg_gpr2() __mcode("90_E") __reglist(mod, e);]], "invalid")
    
    assert_cdef([[void validreg_fpr(float xmm0) __mcode("90_E");]], "validreg_fpr")
    
    assert_cdeferr([[void badreg_fpr1(float x) __mcode("90_E");]], "invalid")
    assert_cdeferr([[void badreg_fpr1(float xm) __mcode("90_E");]], "invalid")
    assert_cdeferr([[void badreg_fpr1(float xm0) __mcode("90_E");]], "invalid")
    assert_cdeferr([[void badreg_fpr1(float xmmm0) __mcode("90_E");]], "invalid")
    assert_cdeferr([[void badreg_fpr2(float xmm0vf) __mcode("90_E");]], "invalid")
    --xmm register number too large
    assert_cdeferr([[void badreg_fpr1(float xmm20) __mcode("90_E");]], "invalid")
  end)

  it("invalid commutative mode registers", function()
    assert_cdef([[int4 valid_comm(int4 v1, int4 v2) __mcode("90rMc");]], "valid_comm")
    --must have 1+ input argument
    assert_cdeferr([[int4 invalid_comm1(int4 v1) __mcode("90rMc");]])
    -- input register types must match
    assert_cdeferr([[void invalid_comm2(int32_t i, int4 v1) __mcode("90rMc");]])
    assert_cdeferr([[void invalid_comm3(int4 v1, int32_t i) __mcode("90rMc");]])
  end)
  
  it("multidef rollback", function()
  
    --check ctype rollback after parsing a valid intrinsic the line before
    assert_cdeferr([[
      void multi1() __mcode("90");
      void multi2() __mcode("0");
    ]])
    
    assert_error(function() ffi.C.multi1() end)
    assert_error(function() ffi.C.multi2() end)
    
    assert_not_error(function() ffi.cdef[[
      void multi1(int32_t eax) __mcode("90_E") __reglist(out, int32_t eax);
    ]] end)

    assert_equal(ffi.C.multi1(1.1), 1)
  end)
  
  it("bad dynamic registers", function()
    --No modrm specifed for the implicit output register decleared having a non void return type
    assert_cdeferr([[int32_t dynerr1() __mcode("90");]])
    assert_cdeferr([[void dynerr2(int32_t a) __mcode("90");]])
    assert_cdeferr([[int32_t dynerr3(int32_t a) __mcode("90");]])
    -- no dynamic registers listed
    assert_cdeferr([[void dynerr4() __mcode("90m");]]) 
    assert_cdeferr([[void dynerr5() __mcode("90rM");]])
    assert_cdeferr([[void dynerr6() __mcode("90Mr");]])
    --need 2 in or 1 in and a return type
    assert_cdeferr([[void dynerr7(int32_t a) __mcode("90rM");]])
    --too many dynamic registers
    assert_cdeferr([[void dynerr8(int a, int b, int c) __mcode("90rR");]]) 
  end)

  it("bad ffi types mcode", function()
    assert_cdeferr([[void testffi1(float a2, ...) __mcode("90");]])
    assert_cdeferr([[void testffi2(complex a2) __mcode("90");]])
    
    --NYI non 16/32 byte vectors
    assert_cdeferr([[
      typedef float float2 __attribute__((__vector_size__(8)));
      void testffi2(float2 a2) __mcode("90")
    ]])
  end)

  it("bad args", function()
    assert_cdef([[void idiv2(int32_t eax, int32_t ecx) __mcode("99F7F9_E") __reglist(out, int32_t eax, int32_t edx)]], "idiv2")
    
    local idiv = ffi.C.idiv2
    
    assert_equal(idiv(6, 2), 3)
    --too few arguments
    assert_error(function() idiv() end)
    assert_error(function() idiv(nil) end)
    assert_error(function() idiv(1) end)
    assert_error(function() idiv(1, nil) end)
    
    --too many arguments
    assert_error(function() idiv(1, 2, nil) end)
    assert_error(function() idiv(1, 2, 3) end) 
    assert_error(function() idiv(1, 2, 3, 4) end)
  end) 
  
  it("output pointers", function() 
    assert_cdef([[const char* addptr(const char* nptr, int32_t n) __mcode("03rM");]], "addptr")
    local s = "0123456789abcdefghijklmnopqrstvwxyz"
    
    local ptr = ffi.C.addptr(s, 0)
    assert_equal(ptr, ffi.cast("const char*", s))
    assert_equal(ptr[0], string.byte(s))
    
    local function checker(i, sptr)
      assert(tostring(sptr), tostring(ptr+i))
      assert(sptr == ptr+i)
    end
    
    assert_jitchecker(checker, function(i)
      return (ffi.C.addptr(s, i))
    end)
  end)
  
  it("signed/unsigned numbers", function() 
    assert_cdef([[int32_t sub_signed(int32_t n, int32_t i) __mcode("2brM");]], "sub_signed")
    assert_cdef([[uint32_t sub_unsigned(uint32_t n, uint32_t i) __mcode("2brM");]], "sub_unsigned")
    assert_cdef([[uint32_t sub_signedun(int32_t n, int32_t i) __mcode("2brM");]], "sub_signedun")
    
    assert_equal(tonumber(ffi.C.sub_unsigned(3, 1)), 2)
    
    local function unsignedtest(n1, n2)
      return (tonumber(ffi.C.sub_unsigned(n1, n2)))
    end

    assert_jit(2, unsignedtest, 3, 1)
    assert_jit(2999999999, unsignedtest, 3000000000, 1)
    --wrap around
    assert_jit(4294967295, unsignedtest, 300, 301)
    
    local function unsignedtest_boxed(n1, n2)
      return (ffi.C.sub_unsigned(n1, n2))
    end
    
    assert_jit(ffi.new("uint32_t", 2), unsignedtest_boxed, 3, 1)
    assert_jit(ffi.new("uint32_t", 2999999999), unsignedtest_boxed, 3000000000, 1)
    --wrap around
    assert_jit(ffi.new("uint32_t", 4294967295), unsignedtest_boxed, 300, 301)
    
    local function signedtest(n1, n2)
      return (ffi.C.sub_signed(n1, n2))
    end
    
    assert_jit(-2, signedtest, -1, 1)
    assert_noexit(3, signedtest, -1, -4)
  end)
  
  it("op encode", function()
    assert_cdef([[int32_t not32(int32_t n) __mcode("F72m");]], "not32")

    local function test_not(i)
      return (ffi.C.not32(i))
    end

    assert_jit(-1, test_not, 0)    
    assert_noexit(0, test_not, -1)    
    
    assert_cdef([[int32_t add_imm3(int32_t n) __mcode("830mU", 3);]], "add_imm3")  
    
    local function checker(i, n) 
      return i+3, n
    end
    assert_jitchecker(checker, function(i)
      return (ffi.C.add_imm3(i))
    end)
  end)
  
  it("prefix byte", function() 
    assert_cdef([[void atomicadd(int32_t* nptr, int32_t n) __mcode("01mRIPS", 0xF0);]], "atomicadd")
    
    local sum = 0   
    local function checker(i, jsum)
      sum = sum+i
      if(jsum ~= sum) then 
       return jsum, sum
      end
    end
    
    local numptr = ffi.new("int32_t[1]", 0)
  
    assert_jitchecker(checker, function(i)
      ffi.C.atomicadd(numptr, i)
      return numptr[0]
    end)
  end)
  
  if ffi.arch == "x64" then
    it("prefix64", function()
      assert_cdef([[void atomicadd64(int64_t* nptr, int64_t n) __mcode("01mRIPS", 0xF0);]], "atomicadd64")
      
      local sum = 0
      local function checker(i, jsum)
        sum = sum+i
        assert(jsum == sum)
      end
      
      local numptr = ffi.new("int64_t[1]", 0)
    
      assert_jitchecker(checker, function(i)
        ffi.C.atomicadd64(numptr, i)
        return numptr[0]
      end)
    end)
  end

  it("prefix and imm byte", function() 
    assert_cdef([[void atomicadd1(int32_t* nptr) __mcode("830mIUPS", 0xF0, 0x01);]], "atomicadd1")
    
    local function checker(i, jsum)
      if(jsum ~= i) then 
       return i, jsum
      end
    end
    
    local numptr = ffi.new("int32_t[1]", 0)
  
    assert_jitchecker(checker, function(i)
      ffi.C.atomicadd1(numptr)
      return numptr[0]
    end)
  end)
  it("idiv(template)", function()
    assert_cdef([[void idivT(int32_t eax, int32_t ecx) __mcode("?E") __reglist(out, int32_t eax, int32_t edx)]])
    --trying to create template intrinsic through C library should always fail
    assert_error(function() return ffi.C.idivT end)
    
    local idiv = ffi.intrinsic("idivT", "\x99\xF7\xF9", 3)

    local function checker(i, result, remainder)
      local rem = i%2
    
      if rem ~= remainder then
        return rem, remainder
      end
    
      local expected = (i-rem)/2
      
      if expected ~= result then
        return expected, result
      end
    end
    
    local function test_idiv(value, divisor)
      local result, remainder = idiv(value, divisor)
      return result, remainder
    end
  
    assert_jitchecker(checker, test_idiv, 2)
    
    -- create a second instance and check guard for wrapper pointer fails
    idiv = ffi.intrinsic("idivT", "\x90", 1)
    
    assert_exit(10, test_idiv, 10, 5)
  end)
  
  it("side effects(mode)", function()
    assert_cdef([[void add1_noside(int32_t* nptr) __mcode("830mIU", 0x01);]], "add1_noside") 
    assert_cdef([[void add1_side(int32_t* nptr) __mcode("830mIUs", 0x01);]], "add1_side")
    
    local numptr = ffi.new("int32_t[2]", 0)

    local function checker(i, n)
      assert(n == i)
      assert(numptr[0] >= numptr[1])
    end
  
    local function test_sideff(i)
      ffi.C.add1_side(numptr)
      ffi.C.add1_noside(numptr+1)
      return numptr[0]
    end
  
    assert_jitchecker(checker, test_sideff)   
    assert_greater_than(numptr[0], numptr[1])
    
    numptr[0] = 0
    numptr[1] = 0
    --test directly as JIT'ed 
    test_sideff()
    assert_equal(numptr[0], 1)
    assert_equal(numptr[1], 0)
  end)
  
  it("prefetch", function()
    assert_cdef([[void prefetch0(void* mem) __mcode("0F181mIs")]], "prefetch0")
    assert_cdef([[void prefetch1(void* mem) __mcode("0F182mIs")]], "prefetch1")
    assert_cdef([[void prefetch2(void* mem) __mcode("0F183mIs")]], "prefetch2")
    assert_cdef([[void prefetchnta(void* mem) __mcode("0F180mIs")]], "prefetchnta")

    local asm = ffi.C
    local kmem = ffi.new("int[4]")
    local mem = 1
    mem = mem and ffi.new("int[8]", 1, 2, 3, 4, 5, 6, 7, 8)

    local function testprefetch(a, b, c)
      local n = a+b
      local ptr = mem+c

      asm.prefetch2(ptr)
      asm.prefetch1(kmem)
      asm.prefetch0(mem+a)
      asm.prefetchnta(mem)
      
      asm.prefetch0(kmem+a)
      asm.prefetch1(kmem+b)
      return (ptr) ~= 0 and ptr[0] + ptr[3] 
    end

    assert_jit(11, testprefetch, 1, 2, 3)
  end)
  
  it("cmpxchg", function()
    assert_cdef([[void cmpxchg(int32_t* gpr32, int32_t gpr32, int32_t eax) __mcode("0FB1mRPEI", 0xF0) __reglist(out, int32_t eax);]], "cmpxchg")
    
    local kptr32 = ffi.new("int32_t[1]", 0)
    int4[0] = 0
  
    local function checker(i, n, eax)
      assert(n == i)
      assert(kptr32[0] == i)
      assert(eax == i-1)
    end
  
    local function test_cmpxchg(i)
      local eax = ffi.C.cmpxchg(kptr32, i, i-1)
      return kptr32[0], eax
    end
  
    assert_jitchecker(checker, test_cmpxchg)
    --test not equal non swapping
    local num, eax = test_cmpxchg(0)
    assert_equal(eax, kptr32[0])
    
    num, eax = test_cmpxchg(kptr32[0]+1)
    assert_equal(eax, kptr32[0]-1)
  end)

if ffi.arch == "x64" then  
  it("cmpxchg64", function()
    assert_cdef([[void cmpxchg64(int64_t* gpr64, int64_t gpr64, int64_t rax) __mcode("0FB1mRPEIX", 0xF0) __reglist(out, int64_t rax);]], "cmpxchg64")
    
    local kptr64 = ffi.new("int64_t[1]", 0)
    
    local function test_cmpxchg64(i)
      local rax = ffi.C.cmpxchg64(kptr64, -i, -(i-1))
      return kptr64[0], rax
    end
    
    local function checker(i, newval, rax)
      assert(newval == -i)
      assert(kptr64[0] == -i)
      assert(rax == -(i-1))
    end
    
    assert_jitchecker(checker, test_cmpxchg64, 2)
    
    --test not equal non swapping
    local num, rax = test_cmpxchg64(0, 1)
    assert_equal(rax, kptr64[0])
  end)
end

  it("cmpxchg8b", function()
  
    ffi.cdef([[typedef struct int32pair {
      int32_t i1;
      int32_t i2;
    } __attribute__((aligned(8))) int32pair;]])
  
    assert_cdef([[void cmpxchg8b(void* gpr32, int32_t eax, int32_t edx, int32_t ebx, int32_t ecx) __mcode("0FC71mPEI", 0xf0) 
                  __reglist(out, int32_t eax, int32_t edx);]], "cmpxchg8b")
    
    local int32pair = ffi.new("int32pair") 
    int32pair.i1 = 1
    int32pair.i2 = -1
    
    local function test_cmpxchg8b(i)
      local eax,edx = ffi.C.cmpxchg8b(int32pair, i, -i, i+1, -(i+1))
      return int32pair.i1, int32pair.i2, eax, edx
    end
    
    local function checker(i, n1, n2, eax, edx)
      assert(n1 == i+1)
      assert(n2 == -(i+1))
      assert(int32pair.i1 == i+1)
      assert(int32pair.i2 == -(i+1))
      
      assert(eax == i)
      assert(edx == -i)
    end
    
    assert_jitchecker(checker, test_cmpxchg8b)
  end)

  it("cpuid_brand", function()
    assert_cdef([[void cpuid(int32_t eax, int32_t ecx) __mcode("0FA2_E") __reglist(out, int32_t eax, int32_t ebx, int32_t ecx, int32_t edx);]], "cpuid")

    local cpuid = ffi.C.cpuid
    
    local function getcpuidstr(eax)
      int4[0] = 0; int4[1] = 0; int4[2] = 0; int4[3] = 0
      int4[0], int4[1], int4[2], int4[3] = cpuid(eax, 0)
      return (ffi.string(ffi.cast("char*", int4+0)))
    end
  
    local brand = getcpuidstr(-2147483646)..getcpuidstr(-2147483645)..getcpuidstr(-2147483644)
    print("Processor brand: "..brand)
  
    local function testcpuid_brand()    
      local s = ""
      
      int4[0] = 0
      int4[1] = 0 
      int4[2] = 0 
      int4[3] = 0
      
      int4[0], int4[1], int4[2], int4[3] = cpuid(-2147483646, 0)
      s = s..ffi.string(ffi.cast("char*", int4+0))
      
      int4[0], int4[1], int4[2], int4[3] = cpuid(-2147483645, 0)
      s = s..ffi.string(ffi.cast("char*", int4+0))
      
      int4[0], int4[1], int4[2], int4[3] = cpuid(-2147483644, 0)
      s = s..ffi.string(ffi.cast("char*", int4+0))
    
      return s
    end
    
    assert_jit(brand, testcpuid_brand)
  end)
  
  it("no dse between intrinsic", function() 
    assert_cdef([[int32_t volatileload(int32_t* iptr) __mcode("8brMI");]], "volatileload")
    
    local numptr = ffi.new("int32_t[1]", 0)
    
    local volatileload = ffi.C.volatileload
    
    local function testdse()
      assert(volatileload)
      local sum = 0
      numptr[0] = 0
      numptr[0] = 1
      sum = volatileload(numptr)
      numptr[0] = 2
      sum = sum + volatileload(numptr)
      return sum
    end
    
    assert_jit(3, testdse)
  end)
end)

context("__reglist", function()

  it("incomplete reglist", function()
    assert_cdeferr([[int test1() __mcode("90") __reglist]])
    assert_cdeferr([[int test2() __mcode("90") __reglist(]])
    assert_cdeferr([[int test3() __mcode("90") __reglist();]])
    assert_cdeferr([[int test4() __mcode("90") __reglist(,);]])
    assert_cdeferr([[int test5() __mcode("90") __reglist(in, eax);]])
    assert_cdeferr([[int test6() __mcode("90") __reglist(out, ]])
    assert_cdeferr([[int test6() __mcode("90") __reglist(mod, ]])
  
    assert_cdeferr([[int test7() __mcode("90") __reglist(mod, eax, ]])
    assert_cdeferr([[int test8() __mcode("90") __reglist("out, ]])
    assert_cdeferr([[int test9() __mcode("90") __reglist(o]])
    assert_cdeferr([[int test10() __mcode("90") __reglist(ou]])
    assert_cdeferr([[int invalid_reglist4() __mcode("90") __reglist(out, int)]])
    assert_cdeferr([[int invalid_reglist4() __mcode("90") __reglist(out, int eax,)]])
  end)

  it("invalid reglist", function()
    assert_cdeferr([[int invalid_reglist1() __mcode("90") __reglist(inn, int eax)]])
    assert_cdeferr([[int invalid_reglist2() __mcode("90") __reglist(o, int eax)]])
    assert_cdeferr([[int invalid_reglist3() __mcode("90") __reglist(oout, int eax)]])
    assert_cdeferr([[int invalid_reglist4() __mcode("90") __reglist(out, int reax)]])
    
    --exceeded max register list size
    assert_cdeferr([[int invalid_reglist5() __mcode("90") __reglist(out, int eax, int ebx, 
                      int ecx, int edx, int esi, int edi, float xmm0, float xmm1, float xmm2)]])
  end)
  
  it("stack pointer blacklist", function()
  
    assert_cdeferr([[void blacklist_in(int esp) __mcode("90_E")]], "blacklist")
    assert_cdeferr([[void blacklist_out(int eax) __mcode("90_E") __reglist(out, int esp)]], "blacklist")
    --FIXME
    --assert_cdeferr([[void blacklist_mod(int eax) __mcode("90_E") __reglist(mod, esp)]], "blacklist")
  
    if ffi.arch == "x64" then
      assert_cdeferr([[void blacklist_64(int rsp) __mcode("90_E")]], "blacklist")
    end
  end)
  
  it("duplicate regs", function()
    assert_cdeferr([[void duplicate_in(int eax, int eax) __mcode("90_E")]], "duplicate")
    assert_cdeferr([[void duplicate_inxmm(float4 xmm0, float4 xmm0) __mcode("90_E")]], "duplicate")
    assert_cdeferr([[void duplicate_out(int eax) __mcode("90_E") __reglist(out, int eax, int eax)]], "duplicate")
    --FIXME assert_cdeferr([[void duplicate_mod(int eax) __mcode("90_E") __reglist(mod, eax, eax)]], "duplicate")
  end)
  
  it("rdtsc", function()
    assert_cdef([[void rdtsc() __mcode("0f31") __reglist(out, int32_t eax, int32_t edx);]], "rdtsc")

    local rdtsc = ffi.C.rdtsc
    
    local function getticks()
      union64.low, union64.high = rdtsc()
      return union64.i64
    end
    
    local prev = 0ll
    
    local function checker(i, result)
      --print(tonumber(result-prev))
      assert(result > prev)

      prev = result
    end
    
    assert_jitchecker(checker, getticks)
  end)
end) 

it("popcnt", function()
  assert_cdef([[int32_t popcnt(int32_t n) __mcode("f30fb8rM");]], "popcnt")

  local popcnt = ffi.C.popcnt

  assert_equal(popcnt(7),    3)
  assert_equal(popcnt(1024), 1)
  assert_equal(popcnt(1023), 10)

  local function testpopcnt(num)
    return (popcnt(num))
  end
  
  assert_jit(10, testpopcnt, 1023)
  assert_noexit(32, testpopcnt, -1)
  assert_noexit(0, testpopcnt, 0)
  assert_noexit(1, testpopcnt, 1)
  
  ffi.cdef([[int32_t popcntuf(int32_t n) __mcode("f30fb8rR");]])
  --check unfused
  popcnt = ffi.C.popcntuf
  
  assert_equal(popcnt(7),    3)
  assert_equal(popcnt(1024), 1)
end)

it("addsd", function()
  assert_cdef([[double addsd(double n1, double n2) __mcode("F20F58rM");]], "addsd")
  local addsd = ffi.C.addsd
  
  function test_addsd(n1, n2)
    return (addsd(n1, n2))
  end
   
  assert_equal(3, addsd(1, 2))
  assert_equal(0, addsd(0, 0))
  
  assert_jit(-3, test_addsd, -4.5, 1.5)
  assert_noexit(3, test_addsd, 4.5, -1.5)
  
  --check dual num exit
  assert_equal(5, test_addsd(3 , 2))
  
  --test same ref input
  function test_addsd2(n)
    return (addsd(n, n))
  end
  
  assert_jit(3, test_addsd2, 1.5)
  assert_noexit(-3, test_addsd2, -1.5)
  
  --check dual num exit
  assert_equal(6, test_addsd2(3))
  
  --check unfused
  ffi.cdef([[double addsduf(double n1, double n2) __mcode("F20F58rR");]])
  addsd = ffi.C.addsduf
  
  assert_equal(3, addsd(1, 2))
  assert_equal(0, addsd(0, 0))
end)

it("addss", function()
  assert_cdef([[float addss(float n1, float n2) __mcode("F30F58rM");]], "addss")
  local addsd = ffi.C.addss
   
  function test_addsd(n1, n2)
    return (addsd(n1, n2))
  end
  
  assert_equal(3, addsd(1, 2))
  assert_equal(0, addsd(0, 0))
  
  assert_jit(-3, test_addsd, -4.5, 1.5)
  assert_noexit(3, test_addsd, 4.5, -1.5)
  --check dual num exit
  assert_equal(5, test_addsd(3, 2))
  
  --test same ref input
  function test_addss2(n)
    return (addsd(n, n))
  end  
  
  assert_jit(-9, test_addss2, -4.5)
  assert_noexit(3, test_addss2, 1.5)
  
  --check unfused
  ffi.cdef[[float addssuf(float n1, float n2) __mcode("F30F58rR");]]
  addsd = ffi.C.addssuf
  
  assert_equal(3, addsd(1, 2))
  assert_equal(0, addsd(0, 0))
end)

it("shufps", function()
  assert_cdef([[float4 shufps(float4 v1, float4 v2) __mcode("0FC6rMU", 0);]], "shufps")
  
  local shufps = ffi.C.shufps
   
  local v = ffi.new("float4", 1.5, 2.25, 3.125, 4.0625)
  local vzero = ffi.new("float4", 1)
   
  function test_shufps(v1, v2)
    return (shufps(v1, v2))
  end
  
  local vout = shufps(v, v)
  assert_equal(vout[0], 1.5)
  assert_equal(vout[1], 1.5)
  assert_equal(vout[2], 1.5)
  assert_equal(vout[3], 1.5)
  
  assert_cdef([[float4 shufpsrev(float4 v1, float4 v2) __mcode("0FC6rMU", 0x1b);]], "shufpsrev")
  
  local vout = ffi.C.shufpsrev(v, v)

  assert_equal(vout[0], 4.0625)
  assert_equal(vout[1], 3.125)
  assert_equal(vout[2], 2.25)
  assert_equal(vout[3], 1.5)
end)

it("phaddd 4byte opcode", function()

  ffi.cdef([[int4 phaddd(int4 v1, int4 v2) __mcode("660F3802rM");]])

  local phaddd = ffi.C.phaddd

  function hsum(v)
    local result = phaddd(v, v)
    result = phaddd(result, result)
    return result[0]
  end

  local v = ffi.new("int4", 1, 2, 3, 4)
  local vzero = ffi.new("int4", 0)

  assert_equal(hsum(v), 10)
  assert_equal(hsum(vzero), 0)
end)

context("mixed register type opcodes", function()

  it("pcmpstr", function()
    ffi.cdef([[void pcmpistri(byte16 string, byte16 mask) __mcode("660F3A63rMU", 0x2) __reglist(out, int32_t ecx)]])
    
    local charlist = ffi.new("byte16", 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    local string = ffi.new("byte16",   2, 2, 3, 2, 1, 1, 2, 3, 4, 5, 6, 7, 7, 7, 2, 2)

    local ecx = ffi.C.pcmpistri(charlist, string)
    assert_equal(ecx, 4)
    
    ffi.cdef([[
      void pcmpistrm(byte16 string, byte16 mask) __mcode("660F3A62rMU", 0x40) __reglist(out, byte16 xmm0v);
      int32_t pmovmskb(byte16 mask) __mcode("660FD7rM");
    ]])
    
    local mask = ffi.C.pcmpistrm(charlist, string)
    mask = ffi.C.pmovmskb(mask)
    
    assert_equal(mask, 48)
  end)

  it("cvttsd2s", function()  
    assert_cdef([[int cvttsd2s(double n) __mcode("F20F2CrM");]], "cvttsd2s")
    local cvttsd2s = ffi.C.cvttsd2s
    
    function test_cvttsd2s(n)
      return (cvttsd2s(n))
    end
    
    assert_equal(0, cvttsd2s(-0))
    assert_equal(1, cvttsd2s(1))
    assert_equal(1, cvttsd2s(1.2))
    
    assert_jit(3, test_cvttsd2s, 3.3)
    assert_noexit(-1, test_cvttsd2s, -1.5)
    --check dual num exit
    assert_equal(5, test_cvttsd2s(5))
    
    --check unfused
    ffi.cdef([[int cvttsd2suf(double n) __mcode("F20F2CrR");]])
    cvttsd2s = ffi.C.cvttsd2suf
    
    assert_equal(0, cvttsd2s(-0))
    assert_equal(1, cvttsd2s(1))
    assert_equal(1, cvttsd2s(1.2))
  end)
  
  it("cvtsi2sd", function()
    assert_cdef([[double cvtsi2sd(int32_t n) __mcode("F20F2ArM");]], "cvtsi2sd")
    local cvtsi2sd = ffi.C.cvtsi2sd
    
    function test_cvtsi2sd(n1, n2)
      return (cvtsi2sd(n1)+n2)
    end
    
    assert_equal(0.5, test_cvtsi2sd(0, 0.5))
    assert_equal(1.25, test_cvtsi2sd(1.0, 0.25))
    assert_equal(-1.5, test_cvtsi2sd(-2, 0.5))
    
    assert_jit(3.25, test_cvtsi2sd, 3, 0.25)
    assert_noexit(-1.5, test_cvtsi2sd, -2, 0.5)
    
    --check dual num exit
    assert_equal(11, test_cvtsi2sd(5, 6))
    
    --check unfused
    ffi.cdef([[double cvtsi2sduf(int32_t n) __mcode("F20F2ArR");]])
    cvtsi2sd = ffi.C.cvtsi2sduf
    assert_equal(0.5, test_cvtsi2sd(0, 0.5))
    assert_equal(1.25, test_cvtsi2sd(1.0, 0.25))
    assert_equal(-1.5, test_cvtsi2sd(-2, 0.5))
  end)
  
  it("pextrw", function()
    local v = ffi.new("byte16", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
    
    assert_cdef([[int32_t pextrw_0(byte16 v) __mcode("660FC5mRU", 0);]], "pextrw_0")
    assert_equal(0x0201, ffi.C.pextrw_0(v))
    
    assert_cdef([[int32_t pextrw_7(byte16 v) __mcode("660FC5mRU", 7);]], "pextrw_7")
    assert_equal(0x100f, ffi.C.pextrw_7(v))
  end)
  
  it("pinsrw", function()
    assert_cdef([[int4 pinsrw_0(byte16 v, int32_t word) __mcode("660FC4rMU", 0);]], "pinsrw_0")
    
    local v = ffi.new("byte16", 0)
    local vout = ffi.C.pinsrw_0(v, 0xf0f1)   
    assert_equal(0xf0f1, vout[0])
    
    assert_cdef([[int4 pinsrw_7(byte16 v, int32_t word) __mcode("660FC4rMU", 7);]], "pinsrw_7")
    vout = ffi.C.pinsrw_0(v, 0xf0f1)  
    assert_equal(0xf0f1, vout[0])
  end)
end)

end)


