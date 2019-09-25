do --- find relative +jit
  jit.opt.start("hotloop=1")
  for _ = 1, 20 do
    local value = "abc"
    local pos_c = string.find(value, "c", 1, true)
    local value2 = string.sub(value, 1, pos_c - 1)
    local pos_b = string.find(value2, "b", 2, true)
    assert(pos_b == 2, "FAIL: position of 'b' is " .. pos_b)
  end
end
