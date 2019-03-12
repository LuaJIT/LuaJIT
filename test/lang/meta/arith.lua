local function create(arith, v1, v2)
  local meta = {
    __add=function(a,b) return arith("add", a, b) end,
    __sub=function(a,b) return arith("sub", a, b) end,
    __mul=function(a,b) return arith("mul", a, b) end,
    __div=function(a,b) return arith("div", a, b) end,
    __mod=function(a,b) return arith("mod", a, b) end,
    __pow=function(a,b) return arith("pow", a, b) end,
    __unm=function(a,b) return arith("unm", a, b) end,
  }
  return setmetatable({v1}, meta), setmetatable({v2}, meta)
end

do --- op
  local a, b = create(function(op,a,b) return op end)
  assert(a+b == "add")
  assert(a-b == "sub")
  assert(a*b == "mul")
  assert(a/b == "div")
  assert(a%b == "mod")
  assert(a^b == "pow")
  assert(-a == "unm")
end

do --- lhs
  local a, b = create(function(op,a,b) return a[1] end, "foo", 42)
  assert(a+b == "foo")
  assert(a-b == "foo")
  assert(a*b == "foo")
  assert(a/b == "foo")
  assert(a%b == "foo")
  assert(a^b == "foo")
  assert(-a == "foo")
end

do --- rhs
  local a, b = create(function(op,a,b) return b[1] end, 42, "foo")
  assert(a+b == "foo")
  assert(a-b == "foo")
  assert(a*b == "foo")
  assert(a/b == "foo")
  assert(a%b == "foo")
  assert(a^b == "foo")
  assert(-a == 42)
end

do --- meta only lhs
  local a, b = create(function(op,a,b) return a[1]+b end, 39), 3
  assert(a+b == 42)
  assert(a-b == 42)
  assert(a*b == 42)
  assert(a/b == 42)
  assert(a%b == 42)
  assert(a^b == 42)
end

do --- meta only rhs
  local a, b = 39, create(function(op,a,b) return a+b[1] end, 3)
  assert(a+b == 42)
  assert(a-b == 42)
  assert(a*b == 42)
  assert(a/b == 42)
  assert(a%b == 42)
  assert(a^b == 42)
end

do --- defaults string, int
  local a, b = "39", 3
  assert(a+b == 42)
  assert(a-b == 36)
  assert(a*b == 117)
  assert(a/b == 13)
  assert(a%b == 0)
  assert(a^b == 59319)
  assert(-a == -39)
end

do --- defaults int, string
  local a, b = 39, "3"
  assert(a+b == 42)
  assert(a-b == 36)
  assert(a*b == 117)
  assert(a/b == 13)
  assert(a%b == 0)
  assert(a^b == 59319)
  assert(-a == -39)
end

do --- defaults string, string
  local a, b = "39", "3"
  assert(a+b == 42)
  assert(a-b == 36)
  assert(a*b == 117)
  assert(a/b == 13)
  assert(a%b == 0)
  assert(a^b == 59319)
  assert(-a == -39)
end

do --- defaults string, kint
  local a = "39"
  assert(a+3 == 42)
  assert(a-3 == 36)
  assert(a*3 == 117)
  assert(a/3 == 13)
  assert(a%3 == 0)
  assert(a^3 == 59319)
end

do --- defaults kint, string
  local b = "3"
  assert(39+b == 42)
  assert(39-b == 36)
  assert(39*b == 117)
  assert(39/b == 13)
  assert(39%b == 0)
  assert(39^b == 59319)
end
