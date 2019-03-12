do --- string_op
  local t, y = {}, {}
  for i=1,100 do t[i] = string.char(i, 16+i, 32+i) end
  for i=1,100 do t[i] = string.reverse(t[i]) end
  assert(t[100] == "\132\116\100")
  for i=1,100 do t[i] = string.reverse(t[i]) end
  for i=1,100 do assert(t[i] == string.char(i, 16+i, 32+i)) end
  for i=1,100 do y[i] = string.upper(t[i]) end
  assert(y[65] == "AQA")
  assert(y[97] == "AQ\129")
  assert(y[100] == "DT\132")
  for i=1,100 do y[i] = string.lower(t[i]) end
  assert(y[65] == "aqa")
  assert(y[97] == "aq\129")
  assert(y[100] == "dt\132")
end
