do --- rechain
  local k

  collectgarbage()

  local t = {}
  t.ac = 1

  t.nn = 1
  t.mm = 1
  t.nn = nil
  t.mm = nil

  k = "a".."i"
  t[k] = 2

  t.ad = 3

  t[k] = nil
  k = nil

  collectgarbage()

  k = "a".."f"
  t[k] = 4

  t.ak = 5

  assert(t[k] == 4)
end

do --- TSETM gc
  local function f()
    collectgarbage()
    return "a", "b"
  end
  for i = 1, 10 do
    local t = {f()}
    assert(t[1] == "a")
    assert(t[2] == "b")
  end
end
