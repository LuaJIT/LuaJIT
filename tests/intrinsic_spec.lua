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
end
  
  it("fpr_vec", function()
    assert_cdef([[void fpr_vec(void* xmm7v) __mcode("90_E") __reglist(out, float4 xmm7v)]], "fpr_vec")
  
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


end)


