
local sub = string.sub
iubc = setmetatable({
  A="T", C="G", B="V", D="H", K="M", R="Y",
  a="T", c="G", b="V", d="H", k="M", r="Y",
  T="A", G="C", V="B", H="D", M="K", Y="R", U="A",
  t="A", g="C", v="B", h="D", m="K", y="R", u="A",
  N="N", S="S", W="W", n="N", s="S", w="W",
}, { __index = function(t, s)
  local r = t[sub(s, 2)]..t[sub(s, 1, 1)]; t[s] = r; return r end })

local wcode = [=[
return function(t, n)
  if n == 1 then return end
  local iubc, sub, write = iubc, string.sub, io.write
  local s = table.concat(t, "", 1, n-1)
  for i=#s-59,1,-60 do
    write(]=]
for i=59,3,-4 do wcode = wcode.."iubc[sub(s, i+"..(i-3)..", i+"..i..")], " end
wcode = wcode..[=["\n")
  end
  local r = #s % 60
  if r ~= 0 then
    for i=r,1,-4 do write(iubc[sub(s, i-3 < 1 and 1 or i-3, i)]) end
    write("\n")
  end
end
]=]
local writerev = loadstring(wcode)()

if not arg then arg = {...} end

local t, n = {}, 1
for line in io.lines(arg[1]) do
  local c = sub(line, 1, 1)
  if c == ">" then writerev(t, n); io.write(line, "\n"); n = 1
  elseif c ~= ";" then t[n] = line; n = n + 1 end
end
writerev(t, n)
