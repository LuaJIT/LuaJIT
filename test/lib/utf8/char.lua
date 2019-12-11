local char = utf8.char

do --- smoke
  assert(char(65, 66, 67) == 'ABC')
  assert(char(0x20AC) == '\u{20AC}')
  assert(char() == '')

  assert(char(0):len() == 1)
  assert(char(0x7F):len() == 1)
  assert(char(0x80):len() == 2)
  assert(char(0x7FF):len() == 2)
  assert(char(0x800):len() == 3)
  assert(char(0xFFFF):len() == 3)
  assert(char(0x10000):len() == 4)
  assert(char(0x10FFFF):len() == 4)
end
