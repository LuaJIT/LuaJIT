local codepoint = utf8.codepoint

do --- smoke
  assert(codepoint("A\u{20AC}3") == 0x41)
  assert(codepoint("A\u{20AC}3", 2) == 0x20AC)
  assert(codepoint("A\u{20AC}3", -1) == 0x33)
  assert(codepoint("A\u{20AC}3", 5) == 0x33)
end
