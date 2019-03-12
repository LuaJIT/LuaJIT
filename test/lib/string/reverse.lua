local reverse = string.reverse

do --- misc
  local y
  for i=1,100 do y = reverse("abc") end
  assert(y == "cba")
  local x = "abcd"
  for i=1,100 do y = reverse(x) end
  assert(y == "dcba")
  x = 1234
  for i=1,100 do y = reverse(x) end
  assert(y == "4321")
end
