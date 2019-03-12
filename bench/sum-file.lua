
if not arg then arg = {...} end

local sum = 0
for line in io.lines(arg[1]) do
  sum = sum + line
end
io.write(sum, "\n")
