
do --- float
  local t = { "local x\n" }
  for i=2,65537 do t[i] = "x="..i..".5\n" end
  assert(loadstring(table.concat(t)) ~= nil)
  t[65538] = "x=65538.5"
  assert(loadstring(table.concat(t)) == nil)
end

do --- int
  local t = { "local x\n" }
  for i=2,65537 do t[i] = "x='"..i.."'\n" end
  assert(loadstring(table.concat(t)) ~= nil)
  t[65538] = "x='65538'"
  assert(loadstring(table.concat(t)) == nil)
end
