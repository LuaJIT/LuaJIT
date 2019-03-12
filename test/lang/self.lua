do --- trivial setget
  local t = {}

  function t:set(x)
    self.a=x
  end

  function t:get()
    return self.a
  end

  t:set("foo")
  assert(t:get() == "foo")
  assert(t.a == "foo")

  t:set(42)
  assert(t:get() == 42)
  assert(t.a == 42)
end
