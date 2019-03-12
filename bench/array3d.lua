
local function array_set(self, x, y, z, p)
  assert(x >= 0 and x < self.nx, "x outside PA")
  assert(y >= 0 and y < self.ny, "y outside PA")
  assert(z >= 0 and z < self.nz, "z outside PA")
  local pos = (z*self.ny + y)*self.nx + x
  local image = self.image
  if self.packed then
    local maxv = self.max_voltage
    if p > maxv then self.max_voltage = p*2.0 end
    local oldp = image[pos] or 0.0 -- Works with uninitialized table, too
    if oldp > maxv then p = p + maxv*2.0 end
    image[pos] = p
  else
    image[pos] = p
  end
  self.changed = true
  self.changed_recently = true
end

local function array_points(self)
  local y, z = 0, 0
  return function(self, x)
    x = x + 1
    if x >= self.nx then
      x = 0
      y = y + 1
      if y >= self.ny then
	y = 0
	z = z + 1
	if z >= self.nz then
	  return nil, nil, nil
	end
      end
    end
    return x, y, z
  end, self, 0
end

local function array_new(nx, ny, nz, packed)
  return {
    nx = nx, ny = ny, nz = nz,
    packed = packed, max_voltage = 0.0,
    changed = false, changed_recently = false,
    image = {}, -- Preferably use a fixed-type, pre-sized array here.
    set = array_set,
    points = array_points,
  }
end

local dim = tonumber(arg and arg[1]) or 30 -- Array dimension dim^3
local packed = arg and arg[2] == "packed"   -- Packed image or flat
local arr = array_new(dim, dim, dim, packed)

for x,y,z in arr:points() do
  arr:set(x, y, z, x*x)
end
assert(arr.image[dim^3-1] == (dim-1)^2)

