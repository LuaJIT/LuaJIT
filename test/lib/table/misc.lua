-- TODO: Organise

-- ABC elim
-- +opt +abc
do
  local s, t = {}, {}
  for i=1,100 do t[i] = 1 end
  for i=1,100 do s[i] = t end
  s[90] = {}
  local n = 100
  for i=1,n do s[i][i] = i end
end

--- TSETM
-- Initialize table with multiple return values
do
  local function f(a,b,c)
    return a,b,c
  end

  local t

  t = {(f(1,2,3))}
  assert(t[1] == 1 and t[2] == nil and t[3] == nil)

  t = {f(1,2,3)}
  assert(t[1] == 1 and t[2] == 2 and t[3] == 3 and t[4] == nil)
  t = {f(1,2,3),}
  assert(t[1] == 1 and t[2] == 2 and t[3] == 3 and t[4] == nil)

  t = {f(1,2,3), f(4,5,6)}
  assert(t[1] == 1 and t[2] == 4 and t[3] == 5 and t[4] == 6 and t[5] == nil)

  t = {
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  f(2,3,4)}
  assert(t[255] == 1 and t[256] == 2 and t[257] == 3 and t[258] == 4 and t[259] == nil)
end

--- TSETM 2
-- Initialize table with function returning 2 constant return values
do
  local function f() return 9, 10 end
  local t
  for i=1,100 do t = { 1, 2, 3, f() } end
  assert(t[1] == 1 and t[2] == 2 and t[3] == 3 and t[4] == 9 and t[5] == 10 and
	 t[6] == nil)
end



