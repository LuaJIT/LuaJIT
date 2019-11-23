local pack = string.pack

do --- smoke
  assert(pack('b', 0x31) == '\x31')
  assert(pack('c3', 'foo') == 'foo')
end
