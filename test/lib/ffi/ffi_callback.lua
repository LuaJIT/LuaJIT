
local ffi = require("ffi")

ffi.cdef[[
void qsort(void *base, size_t nmemb, size_t size,
	   int (*compar)(const uint8_t *, const uint8_t *));
]]

do
  local cb = ffi.cast("int (*)(int, int, int)", function(a, b, c)
    return a+b+c
  end)

  assert(cb(10, 99, 13) == 122)

  -- Don't compile call to blacklisted function.
  for i=1,200 do
    if i > 60 then assert(cb(10, 99, 13) == 122) end
  end
end

do
  assert(ffi.cast("int64_t (*)(int64_t, int64_t, int64_t)", function(a, b, c)
      return a+b+c
    end)(12345678901234567LL, 70000000000000001LL, 10000000909090904LL) ==
    12345678901234567LL+70000000000000001LL+10000000909090904LL)

  assert(ffi.cast("double (*)(double, float, double)", function(a, b, c)
      return a+b+c
    end)(7.125, -123.25, 9999.33) == 7.125-123.25+9999.33)

  assert(ffi.cast("double (*)(int, double)", function(a, b)
      return a+b
    end)(12345, 7.125) == 12345 + 7.125)

  assert(ffi.cast("float (*)(double, float, double)", function(a, b, c)
      return a+b+c
    end)(7.125, -123.25, 9999.33) == 9883.205078125)

  assert(ffi.cast("int (*)(int, int, int, int, int, int, int, int, int, int)",
    function(a, b, c, d, e, f, g, h, i, j)
      return a+b+c+d+e+f+g+h+i+j
    end)(-42, 17, 12345, 9987, -100, 11, 51, 0x12345678, 338, -78901234) ==
    -42+17+12345+9987-100+11+51+0x12345678+338-78901234)

  assert(ffi.cast("double (*)(double, double, double, double, double, double, double, double, double, double)",
    function(a, b, c, d, e, f, g, h, i, j)
      return a+b+c+d+e+f+g+h+i+j
    end)(-42.5, 17.125, 12345.5, 9987, -100.625, 11, 51, 0x12345678, 338, -78901234.75) ==
    -42.5+17.125+12345.5+9987-100.625+11+51+0x12345678+338-78901234.75)
end

-- Target-specific tests.
if jit.arch == "x86" then
  assert(ffi.cast("__fastcall int (*)(int, int, int)", function(a, b, c)
      return a+b+c
    end)(10, 99, 13) == 122)

  assert(ffi.cast("__stdcall int (*)(int, int, int)", function(a, b, c)
      return a+b+c
    end)(10, 99, 13) == 122)

  -- Test reordering.
  assert(ffi.cast("int64_t __fastcall (*)(int64_t, int, int)", function(a, b, c)
      return a+b+c
    end)(12345678901234567LL, 12345, 989797123) ==
    12345678901234567LL+12345+989797123)
end

-- Error handling.
do
  local function f()
    return
  end -- Error for result conversion triggered here.
  local ok, err = pcall(ffi.cast("int (*)(void)", f))
  assert(ok == false)
  assert(string.match(err, ":"..debug.getinfo(f, "S").lastlinedefined..":"))

  assert(pcall(ffi.cast("int (*)(void)", function() end)) == false)
  assert(pcall(ffi.cast("int (*)(void)", function() error("test") end)) == false)
  assert(pcall(ffi.cast("int (*)(void)", function(a) return a+1 end)) == false)

  assert(pcall(ffi.cast("int (*)(int,int,int,int, int,int,int,int, int)", function() error("test") end), 1,1,1,1, 1,1,1,1, 1) == false)
  assert(pcall(ffi.cast("int (*)(int,int,int,int, int,int,int,int, int)", function() error("test") end), 1,1,1,1, 1,1,1,1, 1) == false)
end

do
  local function cmp(pa, pb)
    local a, b = pa[0], pb[0]
    if a < b then
      return -1
    elseif a > b then
      return 1
    else
      return 0
    end
  end

  local arr = ffi.new("uint8_t[?]", 256)
  for i=0,255 do arr[i] = math.random(0, 255) end
  ffi.C.qsort(arr, 256, 1, cmp)
  for i=0,254 do assert(arr[i] <= arr[i+1]) end
end

if ffi.abi"win" then
  ffi.cdef[[
  typedef int (__stdcall *WNDENUMPROC)(void *hwnd, intptr_t l);
  int EnumWindows(WNDENUMPROC func, intptr_t l);
  int SendMessageA(void *hwnd, uint32_t msg, int w, intptr_t l);
  enum { WM_GETTEXT = 13 };
  ]]

  local C = ffi.C
  local buf = ffi.new("char[?]", 256)
  local lbuf = ffi.cast("intptr_t", buf)
  local count = 0
  C.EnumWindows(function(hwnd, l)
    if C.SendMessageA(hwnd, C.WM_GETTEXT, 255, lbuf) ~= 0 then
      count = count + 1
    end
    return true
  end, 0)
  assert(count > 10)
end

do
  local cb = ffi.cast("int(*)(void)", function() return 1 end)
  assert(cb() == 1)
  cb:free()
  assert(pcall(cb) == false)
  assert(pcall(cb.free, cb) == false)
  assert(pcall(cb.set, cb, function() end) == false)
  cb = ffi.cast("int(*)(void)", function() return 2 end)
  assert(cb() == 2)
  cb:set(function() return 3 end)
  assert(cb() == 3)
end

do
  local ft = ffi.typeof("void(*)(void)")
  local function f() end
  local t = {}
  for i=1,4 do
    for i=1,400 do t[i] = ft(f) end
    for i=1,400 do t[i]:free() end
  end
end

do
  assert(ffi.cast("int (*)()", function() return string.byte"A" end)() == 65)
end

do
  local f = ffi.cast("void (*)(void)", function() debug.traceback() end)
  debug.sethook(function() debug.sethook(nil, "", 0); f() end, "", 1)
  local x
end

