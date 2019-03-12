local sqrt = math.sqrt
local huge = math.huge

local delta = 1
while delta * delta + 1 ~= 1 do
  delta = delta * 0.5
end

local function length(x, y, z)  return sqrt(x*x + y*y + z*z) end
local function vlen(v)          return length(v[1], v[2], v[3]) end
local function mul(c, x, y, z)  return c*x, c*y, c*z end
local function unitise(x, y, z) return mul(1/length(x, y, z), x, y, z) end
local function dot(x1, y1, z1, x2, y2, z2)
  return x1*x2 + y1*y2 + z1*z2
end

local function vsub(a, b)        return a[1] - b[1], a[2] - b[2], a[3] - b[3] end
local function vdot(a, b)        return dot(a[1], a[2], a[3], b[1], b[2], b[3]) end


local sphere = {}
function sphere:new(centre, radius)
  self.__index = self
  return setmetatable({centre=centre, radius=radius}, self)
end

local function sphere_distance(self, origin, dir)
  local vx, vy, vz = vsub(self.centre, origin)
  local b = dot(vx, vy, vz, dir[1], dir[2], dir[3])
  local r = self.radius
  local disc = r*r + b*b - vx*vx-vy*vy-vz*vz
  if disc < 0 then return huge end
  local d = sqrt(disc)
  local t2 = b + d
  if t2 < 0 then return huge end
  local t1 = b - d
  return t1 > 0 and t1 or t2
end

function sphere:intersect(origin, dir, best)
  local lambda = sphere_distance(self, origin, dir)
  if lambda < best[1] then
    local c = self.centre
    best[1] = lambda
    local b2 = best[2]
    b2[1], b2[2], b2[3] =
      unitise(
        origin[1] - c[1] + lambda * dir[1],
        origin[2] - c[2] + lambda * dir[2],
        origin[3] - c[3] + lambda * dir[3])
  end
end

local group = {}
function group:new(bound)
  self.__index = self
  return setmetatable({bound=bound, children={}}, self)
end

function group:add(s)
  self.children[#self.children+1] = s
end

function group:intersect(origin, dir, best)
  local lambda = sphere_distance(self.bound, origin, dir)
  if lambda < best[1] then
    for _, c in ipairs(self.children) do
      c:intersect(origin, dir, best)
    end
  end
end

local hit = { 0, 0, 0 }
local ilight
local best = { huge, { 0, 0, 0 } }

local function ray_trace(light, camera, dir, scene)
  best[1] = huge
  scene:intersect(camera, dir, best)
  local b1 = best[1]
  if b1 == huge then return 0 end
  local b2 = best[2]
  local g = vdot(b2, light)
  if g >= 0 then return 0 end
  hit[1] = camera[1] + b1*dir[1] + delta*b2[1]
  hit[2] = camera[2] + b1*dir[2] + delta*b2[2]
  hit[3] = camera[3] + b1*dir[3] + delta*b2[3]
  best[1] = huge
  scene:intersect(hit, ilight, best)
  if best[1] == huge then
    return -g
  else
    return 0
  end
end

local function create(level, centre, radius)
  local s = sphere:new(centre, radius)
  if level == 1 then return s end
  local gr = group:new(sphere:new(centre, 3*radius))
  gr:add(s)
  local rn = 3*radius/sqrt(12)
  for dz = -1,1,2 do
    for dx = -1,1,2 do
      gr:add(create(level-1, { centre[1] + rn*dx, centre[2] + rn, centre[3] + rn*dz }, radius*0.5))
    end
  end
  return gr
end


local level, n, ss = tonumber(arg and arg[1]) or 9, tonumber(arg and arg[2]) or 256, 4
local iss = 1/ss
local gf = 255/(ss*ss)

io.write(("P5\n%d %d\n255\n"):format(n, n))
local light = { unitise(-1, -3, 2) }
ilight = { -light[1], -light[2], -light[3] }
local camera = { 0, 0, -4 }
local dir = { 0, 0, 0 }

local scene = create(level, {0, -1, 0}, 1)

for y = n/2-1, -n/2, -1 do
  for x = -n/2, n/2-1 do
    local g = 0
    for d = y, y+.99, iss do
      for e = x, x+.99, iss do
        dir[1], dir[2], dir[3] = unitise(e, d, n)
        g = g + ray_trace(light, camera, dir, scene) 
      end
    end
    io.write(string.char(math.floor(0.5 + g*gf)))
  end
end
