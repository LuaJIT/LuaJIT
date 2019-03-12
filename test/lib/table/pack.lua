do --- empty
  local t = table.pack()
  assert(type(t) == "table")
  assert(t.n == 0)
  assert(t[0] == nil)
  assert(t[1] == nil)
end
