
local nan = 0/0
local t = {}
for i=1,100 do t[i] = i+0.5 end
for i=101,200 do t[i] = nan end

do
  local z = 0
  for i=1,200 do if t[i] > 1000 then z=i end end
  assert(z == 0)
end

do
  local z = 0
  for i=1,200 do if not (t[i] < 1000) then z=i end end
  assert(z == 200)
end

do
  local z = 0
  for i=1,200 do if t[i] <= 1000 then z=i end end
  assert(z == 100)
end

do
  local z = 0
  for i=1,200 do if not (t[i] >= 1000) then z=i end end
  assert(z == 200)
end

do
  local z = 0
  for i=1,200 do if t[i] > 0 then z=i end end
  assert(z == 100)
end

do
  local z = 0
  for i=1,200 do if not (t[i] < 0) then z=i end end
  assert(z == 200)
end

do
  local z = 0
  for i=1,200 do if t[i] <= 0 then z=i end end
  assert(z == 0)
end

do
  local z = 0
  for i=1,200 do if not (t[i] >= 0) then z=i end end
  assert(z == 200)
end

do local z; for i=1,100 do z = 0/0 end; assert(z ~= z) end

do local z; for i=1,100 do z = nan == nan end; assert(z == false) end
do local z; for i=1,100 do z = nan == 1 end; assert(z == false) end
do local z; for i=1,100 do z = 1 == nan end; assert(z == false) end

do local z; for i=1,100 do z = nan ~= nan end; assert(z == true) end
do local z; for i=1,100 do z = nan ~= 1 end; assert(z == true) end
do local z; for i=1,100 do z = 1 ~= nan end; assert(z == true) end

do local z; for i=1,100 do z = nan < nan end; assert(z == false) end
do local z; for i=1,100 do z = nan < 1 end; assert(z == false) end
do local z; for i=1,100 do z = 1 < nan end; assert(z == false) end

do local z; for i=1,100 do z = not (nan < nan) end; assert(z == true) end
do local z; for i=1,100 do z = not (nan < 1) end; assert(z == true) end
do local z; for i=1,100 do z = not (1 < nan) end; assert(z == true) end

do local z; for i=1,100 do z = nan > nan end; assert(z == false) end
do local z; for i=1,100 do z = nan > 1 end; assert(z == false) end
do local z; for i=1,100 do z = 1 > nan end; assert(z == false) end

do local z; for i=1,100 do z = not (nan > nan) end; assert(z == true) end
do local z; for i=1,100 do z = not (nan > 1) end; assert(z == true) end
do local z; for i=1,100 do z = not (1 > nan) end; assert(z == true) end

do local z; for i=1,100 do z = nan <= nan end; assert(z == false) end
do local z; for i=1,100 do z = nan <= 1 end; assert(z == false) end
do local z; for i=1,100 do z = 1 <= nan end; assert(z == false) end

do local z; for i=1,100 do z = not (nan <= nan) end; assert(z == true) end
do local z; for i=1,100 do z = not (nan <= 1) end; assert(z == true) end
do local z; for i=1,100 do z = not (1 <= nan) end; assert(z == true) end

do local z; for i=1,100 do z = nan >= nan end; assert(z == false) end
do local z; for i=1,100 do z = nan >= 1 end; assert(z == false) end
do local z; for i=1,100 do z = 1 >= nan end; assert(z == false) end

do local z; for i=1,100 do z = not (nan >= nan) end; assert(z == true) end
do local z; for i=1,100 do z = not (nan >= 1) end; assert(z == true) end
do local z; for i=1,100 do z = not (1 >= nan) end; assert(z == true) end

