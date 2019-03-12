local ffi = require("ffi")

dofile("../common/ffi_util.inc")

ffi.cdef[[
typedef struct s_t {
  int v, w;
} s_t;

typedef const s_t cs_t;

typedef enum en_t { EE } en_t;

typedef struct pcs_t {
  int v;
  const int w;
} pcs_t;

typedef struct foo_t {
  static const int cc = 17;
  enum { CC = -37 };
  int i;
  const int ci;
  int bi:8;
  const int cbi:8;
  en_t e;
  const en_t ce;
  int a[10];
  const int ca[10];
  const char cac[10];
  s_t s;
  cs_t cs;
  pcs_t pcs1, pcs2;
  const struct {
    int ni;
  };
  complex cx;
  const complex ccx;
  complex *cp;
  const complex *ccp;
} foo_t;
]]

do
  local foo_t = ffi.typeof("foo_t")
  local x = foo_t()

  -- constval
  assert(x.cc == 17)
  fails(function(x) x.cc = 1 end, x)
  assert(x.CC == -37)
  fails(function(x) x.CC = 1 end, x)

  -- fields
  x.i = 1
  fails(function(x) x.ci = 1 end, x)
  x.e = 1
  fails(function(x) x.ce = 1 end, x)

  -- bitfields
  x.bi = 1
  fails(function(x) x.cbi = 1 end, x)

  -- arrays
  do
    local a = ffi.new("int[10]")
    a[0] = 1
    local ca = ffi.new("const int[10]")
    fails(function(ca) ca[0] = 1 end, ca)
  end
  x.a[0] = 1
  fails(function(x) x.ca[0] = 1 end, x)
  fails(function(x) x.a = x.ca end, x) -- incompatible type
  fails(function(x) x.ca = x.a end, x)
  fails(function(x) x.ca = {} end, x)
  fails(function(x) x.cac = "abc" end, x)

  -- structs
  do
    local s = ffi.new("s_t")
    s.v = 1
    local cs = ffi.new("cs_t")
    fails(function(cs) cs.v = 1 end, cs)
  end
  x.s.v = 1
  fails(function(x) x.cs.v = 1 end, x)
  x.s = x.cs
  fails(function(x) x.cs = x.s end, x)
  fails(function(x) x.cs = {} end, x)

  -- pseudo-const structs
  x.pcs1.v = 1
  fails(function(x) x.pcs1.w = 1 end, x)
  fails(function(x) x.pcs1 = x.pcs2 end, x)
  fails(function(x) x.pcs1 = {} end, x)

  -- transparent structs
  local y = x.ni
  fails(function(x) x.ni = 1 end, x)

  -- complex subtype is implicitly const and doesn't inherit const attribute
  x.cx = 1
  fails(function(x) x.ccx = 1 end, x)
  do
    local cxa = ffi.new("complex[1]")
    local ccxa = ffi.new("const complex[1]")
    x.cp = cxa
    x.ccp = cxa
    fails(function(x) x.cp = ccxa end, x)
    x.ccp = ccxa
  end
end

