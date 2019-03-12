do --- table 1
  local t=setmetatable({}, {__newindex=function(t,k,v)
    rawset(t, k, 100-v)
  end})

  for i=1,100 do t[i] = i end
  for i=1,100 do assert(t[i] == 100-i) end

  for i=1,100 do t[i] = i end
  for i=1,100 do assert(t[i] == i) end

  for i=1,100 do t[i] = nil end
  for i=1,100 do t[i] = i end
  for i=1,100 do assert(t[i] == 100-i) end
end

do --- jit gaining href
  local count = 0
  local t = setmetatable({ foo = nil },
    { __newindex=function() count = count + 1 end })
  for j=1,2 do
    for i=1,100 do t.foo = 1 end
    rawset(t, "foo", 1)
  end
  assert(count == 100)
end

do --- jit gaining aref
  local count = 0
  local t = setmetatable({ nil },
    { __newindex=function() count = count + 1 end })
  for j=1,2 do
    for i=1,100 do t[1] = 1 end
    rawset(t, 1, 1)
  end
  assert(count == 100)
end

do --- resize
  local grandparent = {}
  grandparent.__newindex = function(s,_,_) tostring(s) end

  local parent = {}
  parent.__newindex = parent
  parent.bar = 1
  setmetatable(parent, grandparent)

  local child = setmetatable({}, parent)
  child.foo = _
end

do --- str
  local t=setmetatable({}, {__newindex=function(t,k,v)
    assert(v == "foo"..k)
    rawset(t, k, "bar"..k)
  end})

  for i=1,100 do t[i]="foo"..i end
  for i=1,100 do assert(t[i] == "bar"..i) end

  for i=1,100 do t[i]="baz"..i end
  for i=1,100 do assert(t[i] == "baz"..i) end

  local t=setmetatable({foo=1,bar=1,baz=1},{})
  t.baz=nil
  t.baz=2
  t.baz=nil
  t.baz=2
end
