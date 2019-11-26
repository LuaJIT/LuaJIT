do --- pi
  assert(math.pi == 3.141592653589793)
end

do --- huge
  assert(math.huge > 0)
  assert(1/math.huge == 0)
end

do --- maxinteger
  assert(math.maxinteger == 9007199254740992)
end

do --- mininteger
  assert(math.mininteger == -9007199254740992)
end
