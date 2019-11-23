local packsize = string.packsize

do --- smoke
  assert(packsize('b') == 1)
end
