local unpack = string.unpack

do --- smoke
  assert(unpack('c3', 'foobar') == 'foo')
end
