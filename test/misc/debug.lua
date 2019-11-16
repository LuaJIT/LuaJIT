do --- debug.* smoke tests
  assert(not pcall(debug.getinfo, 1, '>S'))
  assert(not pcall(debug.getinfo, 1, '>f', 'boo'))
end
