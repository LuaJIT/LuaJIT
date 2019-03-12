local abs = math.abs
local expect_error = require"common.expect_error"

do --- smoke
  assert(abs(-1.5) == 1.5)
  assert(abs("-1.5") == 1.5)
end

do --- argcheck
  expect_error(function() abs() end,
      "bad argument #1 to 'abs' (number expected, got no value)")
  expect_error(function() abs(false) end,
      "bad argument #1 to 'abs' (number expected, got boolean)")
  expect_error(function() abs("a") end,
      "bad argument #1 to 'abs' (number expected, got string)")
end
