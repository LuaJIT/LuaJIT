local tnew = require"table.new"

do --- table.new
  local x, y
  for i=1,100 do
    x = tnew(100, 30)
    assert(type(x) == "table")
    if i == 90 then y = x end
  end
  assert(x ~= y)
end
