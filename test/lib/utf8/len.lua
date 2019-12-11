local len = utf8.len

do --- smoke
  assert(len('A') == 1)
  assert(len('') == 0)
  assert(len("\u{41}\u{42}\u{43}") == 3)
  assert(len("A\u{20AC}3") == 3)

  assert(len('A', 1) == 1)
  assert(len('A', 2) == 0)
  assert(len('ABC', 1, 1) == 1)
  assert(len('ABC', 2, 2) == 1)
  assert(len('ABC', -1) == 1)
  assert(len('ABC', -2) == 2)
end
