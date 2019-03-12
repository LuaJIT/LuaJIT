
if not jit or not jit.status or not jit.status() then return end

for i=1,100 do
  if i==50 then jit.flush(2) end
  for j=1,100 do end
  for j=1,100 do end
end

jit.flush()

local function f() for i=1,100 do end end
for i=1,100 do local x = gcinfo(); f() end

jit.flush()

local function fib(n)
  if n < 2 then return 1 end
  return fib(n-2) + fib(n-1)
end

fib(11)

jit.flush()

local names = {}
for i=1,100 do names[i] = i end

function f()
  for k,v in ipairs(names) do end
end

f()

for i=1,2 do
  f()
  f()
  jit.flush()
end

jit.flush()

jit.flush(1) -- ignored
jit.flush(2) -- ignored
for i=1,1e7 do end -- causes trace #1

jit.flush(2) -- ignored
jit.flush(1) -- ok
jit.flush(1) -- crashes

