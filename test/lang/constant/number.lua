do --- exp
  assert(1e5 == 100000)
  assert(1e+5 == 100000)
  assert(1e-5 == 0.00001)
end

do --- hex exp +hexfloat !lex
  assert(0xe+9 == 23)
  assert(0xep9 == 7168)
  assert(0xep+9 == 7168)
  assert(0xep-9 == 0.02734375)
end
