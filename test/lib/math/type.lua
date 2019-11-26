local type = math.type

do --- smoke
  assert(type(3) == 'integer')
  assert(type(3.14) == 'float')
  assert(type('3.14') == nil)
end
