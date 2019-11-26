local ult = math.ult

do --- smoke
  assert(ult(2, 3) == true)
  assert(ult(2, 2) == false)
  assert(ult(2, 1) == false)
end
