
local r = 0
local function g()
  r = r + 1
  for i=1,100 do end
end

local function f()
  for j=1,20 do
    if j > 19 then
      return g() -- Tailcall at base.
      -- Let this link to the already compiled loop in g().
    end
  end
end

g() -- Compile this loop first.
for i=1,50 do f() end
assert(r == 51)

