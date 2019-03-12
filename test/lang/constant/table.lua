
do --- tnew
  local a = nil
  local b = {}
  local t = {[true] = a, [false] = b or 1}
  assert(t[true] == nil)
  assert(t[false] == b)
end

do --- tdup
  local b = {}
  local t = {[true] = nil, [false] = b or 1}
  assert(t[true] == nil)
  assert(t[false] == b)
end
