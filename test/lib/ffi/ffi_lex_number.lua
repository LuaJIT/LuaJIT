local ffi = require("ffi")

dofile("../common/ffi_util.inc")

local function checklex(t)
  for i=1,1e9,2 do
    local s = t[i+1]
    if not s then break end
    local s2 = assert(loadstring("return tostring("..s..")"))()
    if s2 ~= t[i] then
      print(s2)
      error("lexer failed for '"..s.."'", 2)
    end
  end
end

checklex{
  "0LL",			"0ll",
  "0LL",			"0LL",
  "0ULL",			"0ull",
  "0ULL",			"0ULl",
  "18446744073709551615ULL",	"18446744073709551615llu",
  "9223372036854775807LL",	"0x7fffffffffffffffll",
  "9223372036854775808ULL",	"0x8000000000000000ull",
  "1311768467463790320LL",	"0x123456789abcdef0ll",
  "-1LL",			"-1ll",
  "18446744073709551615ULL",	"-1ull",
  "-9223372036854775807LL",	"-0x7fffffffffffffffll",
  "9223372036854775808ULL",	"-0x8000000000000000ull",
  "0+0i",			"0i",
  "0+0i",			"0I",
  "0+12.5i",			"12.5i",
  "0+4660i",			"0x1234i",
  "0+infI",			"1e400i",
  "0-infI",			"-1e400i",
  "0-12.5i",			"-12.5i",
  "0-0i",			"-0i",
}

checkfail({
  "0l",
  "0lll",
  "0u",
  "0ul",
  "0ulll",
  "0wll",
  "0xll",
  ".0ll",
  "0ii",
}, function(s) assert(loadstring("return "..s)) end)

