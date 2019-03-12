local function Ack(m, n)
  if m == 0 then return n+1 end
  if n == 0 then return Ack(m-1, 1) end
  return Ack(m-1, (Ack(m, n-1))) -- The parentheses are deliberate.
end

local N = tonumber(arg and arg[1]) or 10
io.write("Ack(3,", N ,"): ", Ack(3,N), "\n")
