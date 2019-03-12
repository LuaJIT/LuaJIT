
local what

local function mm(a, b)
  local dbg = debug.getinfo(1)
  what = dbg.namewhat == "metamethod" and dbg.name or
					  dbg.namewhat.." "..(dbg.name or "?")
end

local function ck(s)
  assert(what == s, "bad debug info for metamethod "..s)
end

local mt = {
  __index = mm,
  __newindex = mm,
  __eq = mm,
  __add = mm,
  __sub = mm,
  __mul = mm,
  __div = mm,
  __mod = mm,
  __pow = mm,
  __unm = mm,
  __len = mm,
  __lt = mm,
  __le = mm,
  __concat = mm,
  __call = mm,
}

do --- table metamethods +goto
  local t = setmetatable({}, mt)
  local t2 = setmetatable({}, mt)

  local x = t.x;		ck("__index")
  t.x = 1;		ck("__newindex")
  local x = t + t;	ck("__add")
  local x = t - t;	ck("__sub")
  local x = t * t;	ck("__mul")
  local x = t / t;	ck("__div")
  local x = t % t;	ck("__mod")
  local x = t ^ t;	ck("__pow")
  local x = -t;		ck("__unm")
  local x = t..t;		ck("__concat")
  local x = t();		ck("local t")

  local x = t == t2;	ck("__eq")
  local x = t ~= t2;	ck("__eq")
  local x = t < t2;	ck("__lt")
  local x = t > t2;	ck("__lt")
  local x = t <= t2;	ck("__le")
  local x = t >= t2;	ck("__le")
end

do --- userdata metamethods +luajit
  local u = newproxy()
  local u2 = newproxy()
  debug.setmetatable(u, mt)
  debug.setmetatable(u2, mt)

  local x = u.x;		ck("__index")
  u.x = 1;		ck("__newindex")
  local x = u + u;	ck("__add")
  local x = u - u;	ck("__sub")
  local x = u * u;	ck("__mul")
  local x = u / u;	ck("__div")
  local x = u % u;	ck("__mod")
  local x = u ^ u;	ck("__pow")
  local x = -u;		ck("__unm")
  local x = #u;		ck("__len")
  local x = u..u;		ck("__concat")
  local x = u();		ck("local u")

  local x = u == u2;	ck("__eq")
  local x = u ~= u2;	ck("__eq")
  local x = u < u2;	ck("__lt")
  local x = u > u2;	ck("__lt")
  local x = u <= u2;	ck("__le")
  local x = u >= u2;	ck("__le")
end
