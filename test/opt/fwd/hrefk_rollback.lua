do --- https://github.com/LuaJIT/LuaJIT/issues/124
  local function foo(a, b, f)
    return f and (a.f0 < b.f1 and
                  b.f0 < a.f1 and
                  a.f2 < b.f3 and
                  b.f2 < a.f3)
  end

  local function bar(f0, f1, f2, f3, X, f)
    for _, v in ipairs(X) do
      local b = {}
      b.f0 = 0
      b.f2 = v
      b.f1 = b.f0 + 1
      b.f3 = b.f2 + 1

      if foo({f0 = f0, f1 = f1, f2 = f2, f3 = f3}, b, f) then
        return false
      end
    end

    return true
  end

  local X = { 0, 1, 0, 0 }

  for i = 1, 20 do
    assert(bar(0, 1, 2, 3, X, true))
  end

  assert(not bar(0, 1, 1, 2, X, true))
end
