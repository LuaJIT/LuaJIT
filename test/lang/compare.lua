local function lt(x, y)
  if x < y then return true else return false end
end

local function le(x, y)
  if x <= y then return true else return false end
end

local function gt(x, y)
  if x > y then return true else return false end
end

local function ge(x, y)
  if x >= y then return true else return false end
end

local function eq(x, y)
  if x == y then return true else return false end
end

local function ne(x, y)
  if x ~= y then return true else return false end
end


local function ltx1(x)
  if x < 1 then return true else return false end
end

local function lex1(x)
  if x <= 1 then return true else return false end
end

local function gtx1(x)
  if x > 1 then return true else return false end
end

local function gex1(x)
  if x >= 1 then return true else return false end
end

local function eqx1(x)
  if x == 1 then return true else return false end
end

local function nex1(x)
  if x ~= 1 then return true else return false end
end


local function lt1x(x)
  if 1 < x then return true else return false end
end

local function le1x(x)
  if 1 <= x then return true else return false end
end

local function gt1x(x)
  if 1 > x then return true else return false end
end

local function ge1x(x)
  if 1 >= x then return true else return false end
end

local function eq1x(x)
  if 1 == x then return true else return false end
end

local function ne1x(x)
  if 1 ~= x then return true else return false end
end

local function check(a, b)
  if a ~= b then
    error("check failed with "..tostring(a).." ~= "..tostring(b), 2)
  end
end

do --- 1,2
  local x,y = 1,2

  check(x<y,	true)
  check(x<=y,	true)
  check(x>y,	false)
  check(x>=y,	false)
  check(x==y,	false)
  check(x~=y,	true)

  check(1<y,	true)
  check(1<=y,	true)
  check(1>y,	false)
  check(1>=y,	false)
  check(1==y,	false)
  check(1~=y,	true)

  check(x<2,	true)
  check(x<=2,	true)
  check(x>2,	false)
  check(x>=2,	false)
  check(x==2,	false)
  check(x~=2,	true)

  check(lt(x,y),	true)
  check(le(x,y),	true)
  check(gt(x,y),	false)
  check(ge(x,y),	false)
  check(eq(y,x),	false)
  check(ne(y,x),	true)
end

do --- 2,1
  local x,y = 2,1

  check(x<y,	false)
  check(x<=y,	false)
  check(x>y,	true)
  check(x>=y,	true)
  check(x==y,	false)
  check(x~=y,	true)

  check(2<y,	false)
  check(2<=y,	false)
  check(2>y,	true)
  check(2>=y,	true)
  check(2==y,	false)
  check(2~=y,	true)

  check(x<1,	false)
  check(x<=1,	false)
  check(x>1,	true)
  check(x>=1,	true)
  check(x==1,	false)
  check(x~=1,	true)

  check(lt(x,y),	false)
  check(le(x,y),	false)
  check(gt(x,y),	true)
  check(ge(x,y),	true)
  check(eq(y,x),	false)
  check(ne(y,x),	true)
end

do --- 1,1
  local x,y = 1,1

  check(x<y,	false)
  check(x<=y,	true)
  check(x>y,	false)
  check(x>=y,	true)
  check(x==y,	true)
  check(x~=y,	false)

  check(1<y,	false)
  check(1<=y,	true)
  check(1>y,	false)
  check(1>=y,	true)
  check(1==y,	true)
  check(1~=y,	false)

  check(x<1,	false)
  check(x<=1,	true)
  check(x>1,	false)
  check(x>=1,	true)
  check(x==1,	true)
  check(x~=1,	false)

  check(lt(x,y),	false)
  check(le(x,y),	true)
  check(gt(x,y),	false)
  check(ge(x,y),	true)
  check(eq(y,x),	true)
  check(ne(y,x),	false)
end

do --- 2
  check(lt1x(2),	true)
  check(le1x(2),	true)
  check(gt1x(2),	false)
  check(ge1x(2),	false)
  check(eq1x(2),	false)
  check(ne1x(2),	true)

  check(ltx1(2),	false)
  check(lex1(2),	false)
  check(gtx1(2),	true)
  check(gex1(2),	true)
  check(eqx1(2),	false)
  check(nex1(2),	true)
end

do --- 1
  check(lt1x(1),	false)
  check(le1x(1),	true)
  check(gt1x(1),	false)
  check(ge1x(1),	true)
  check(eq1x(1),	true)
  check(ne1x(1),	false)

  check(ltx1(1),	false)
  check(lex1(1),	true)
  check(gtx1(1),	false)
  check(gex1(1),	true)
  check(eqx1(1),	true)
  check(nex1(1),	false)
end

do --- 0
  check(lt1x(0),	false)
  check(le1x(0),	false)
  check(gt1x(0),	true)
  check(ge1x(0),	true)
  check(eq1x(0),	false)
  check(ne1x(0),	true)

  check(ltx1(0),	true)
  check(lex1(0),	true)
  check(gtx1(0),	false)
  check(gex1(0),	false)
  check(eqx1(0),	false)
  check(nex1(0),	true)
end

do --- pcall
  assert(not pcall(function()
    local a, b = 10.5, nil
    return a < b
  end))
end

do --- bit +bit
  for i=1,100 do
    assert(bit.tobit(i+0x7fffffff) < 0)
  end
  for i=1,100 do
    assert(bit.tobit(i+0x7fffffff) <= 0)
  end
end

do --- string 1 255
  local a = "\255\255\255\255"
  local b = "\1\1\1\1"

  assert(a > b)
  assert(a > b)
  assert(a >= b)
  assert(b <= a)
end

do --- String comparisons:
  local function str_cmp(a, b, lt, gt, le, ge)
    assert(a<b == lt)
    assert(a>b == gt)
    assert(a<=b == le)
    assert(a>=b == ge)
    assert((not (a<b)) == (not lt))
    assert((not (a>b)) == (not gt))
    assert((not (a<=b)) == (not le))
    assert((not (a>=b)) == (not ge))
  end

  local function str_lo(a, b)
    str_cmp(a, b, true, false, true, false)
  end

  local function str_eq(a, b)
    str_cmp(a, b, false, false, true, true)
  end

  local function str_hi(a, b)
    str_cmp(a, b, false, true, false, true)
  end

  str_lo("a", "b")
  str_eq("a", "a")
  str_hi("b", "a")

  str_lo("a", "aa")
  str_hi("aa", "a")

  str_lo("a", "a\0")
  str_hi("a\0", "a")
end

do --- obj_eq/ne
  local function obj_eq(a, b)
    assert(a==b == true)
    assert(a~=b == false)
  end

  local function obj_ne(a, b)
    assert(a==b == false)
    assert(a~=b == true)
  end

  obj_eq(nil, nil)
  obj_ne(nil, false)
  obj_ne(nil, true)

  obj_ne(false, nil)
  obj_eq(false, false)
  obj_ne(false, true)

  obj_ne(true, nil)
  obj_ne(true, false)
  obj_eq(true, true)

  obj_eq(1, 1)
  obj_ne(1, 2)
  obj_ne(2, 1)

  obj_eq("a", "a")
  obj_ne("a", "b")
  obj_ne("a", 1)
  obj_ne(1, "a")

  local t, t2 = {}, {}
  obj_eq(t, t)
  obj_ne(t, t2)
  obj_ne(t, 1)
  obj_ne(t, "")
end
