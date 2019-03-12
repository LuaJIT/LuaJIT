
local function check(a, b)
  if a ~= b then
    error("check failed with "..tostring(a).." ~= "..tostring(b), 2)
  end
end

local nan, one = 0/0, 1

do --- nan nan
  check(nan<nan,	false)
  check(nan<=nan,	false)
  check(nan>nan,	false)
  check(nan>=nan,	false)
  check(nan==nan,	false)
  check(nan~=nan,	true)
end

do --- nan one
  check(nan<one,	false)
  check(nan<=one,	false)
  check(nan>one,	false)
  check(nan>=one,	false)
  check(nan==one,	false)
  check(nan~=one,	true)
end

do --- one nan
  check(one<nan,	false)
  check(one<=nan,	false)
  check(one>nan,	false)
  check(one>=nan,	false)
  check(one==nan,	false)
  check(one~=nan,	true)
end

do --- nan 1
  check(nan<1,	false)
  check(nan<=1,	false)
  check(nan>1,	false)
  check(nan>=1,	false)
  check(nan==1,	false)
  check(nan~=1,	true)
end

do --- 1 nan
  check(1<nan,	false)
  check(1<=nan,	false)
  check(1>nan,	false)
  check(1>=nan,	false)
  check(1==nan,	false)
  check(1~=nan,	true)
end

do --- not nan nan
  check(not (nan<nan),	true)
  check(not (nan<=nan),	true)
  check(not (nan>nan),	true)
  check(not (nan>=nan),	true)
  check(not (nan==nan),	true)
  check(not (nan~=nan),	false)
end

do --- not nan one
  check(not (nan<one),	true)
  check(not (nan<=one),	true)
  check(not (nan>one),	true)
  check(not (nan>=one),	true)
  check(not (nan==one),	true)
  check(not (nan~=one),	false)
end

do --- not one nan
  check(not (one<nan),	true)
  check(not (one<=nan),	true)
  check(not (one>nan),	true)
  check(not (one>=nan),	true)
  check(not (one==nan),	true)
  check(not (one~=nan),	false)
end

do --- not nan 1
  check(not (nan<1),	true)
  check(not (nan<=1),	true)
  check(not (nan>1),	true)
  check(not (nan>=1),	true)
  check(not (nan==1),	true)
  check(not (nan~=1),	false)
end

do --- not 1 nan
  check(not (1<nan),	true)
  check(not (1<=nan),	true)
  check(not (1>nan),	true)
  check(not (1>=nan),	true)
  check(not (1==nan),	true)
  check(not (1~=nan),	false)
end

