
do
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

