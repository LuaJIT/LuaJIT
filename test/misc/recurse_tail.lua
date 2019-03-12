
do
  local tr1
  function tr1(n)
    if n <= 0 then return 0 end
    return tr1(n-1)
  end
  assert(tr1(200) == 0)
end

do
  local tr1, tr2
  function tr1(n)
    if n <= 0 then return 0 end
    return tr2(n-1)
  end
  function tr2(n)
    return tr1(n)
  end
  assert(tr2(200) == 0)
end

