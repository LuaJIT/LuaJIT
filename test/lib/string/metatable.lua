do --- __index metamethod is string library
  assert(debug.getmetatable("").__index == string)
end
