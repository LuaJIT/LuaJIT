return function(f, msg)
  local ok, err = pcall(f)
  if ok then error("error check unexpectedly succeeded", 2) end
  if msg then
    if type(err) ~= "string" then
      error("error check failed with "..tostring(err), 2)
    end
    local line, err2 = string.match(err, ":(%d*): (.*)")
    if err2 ~= msg then
      if err2:gsub(" got no value", " got nil") == msg then
        return
      end
      error("error check failed with "..err, 2)
    end
  end
end
