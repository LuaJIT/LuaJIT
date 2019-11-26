local tointeger = math.tointeger

do --- smoke
  assert(tointeger(-12) == -12)
  assert(tointeger(-12.0) == -12)
  assert(tointeger(-12.34) == nil)
  assert(tointeger('-12') == nil)
  assert(tointeger('-12.0') == nil)
  assert(tointeger('-12.34') == nil)
  assert(tointeger('bad') == nil)
  assert(tointeger(true) == nil)
  assert(tointeger({}) == nil)
end
