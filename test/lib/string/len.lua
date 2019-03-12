local len = string.len
local expect_error = require"common.expect_error"

do --- smoke
  assert(len("abc") == 3)
  assert(len(123) == 3)
end

do --- argcheck
  expect_error(function() len() end,
      "bad argument #1 to 'len' (string expected, got nil)")
  expect_error(function() len(false) end,
      "bad argument #1 to 'len' (string expected, got boolean)")
end
