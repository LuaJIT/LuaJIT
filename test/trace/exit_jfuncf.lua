do --- everything
  local assert = assert

  local function rec(a, b, c, d, e, f)
    assert(f == a+1)
    if b == 0 then return 7 end
    do local x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21, x22, x23, x24, x25, x26, x27, x28, x29, x30, x31, x32, x33, x34, x35, x36, x37, x38, x39, x40, x41, x42, x43, x44, x45, x46, x47, x48, x49, x50, x51, x52, x53, x54, x55, x56, x57, x58, x59, x60, x61, x62, x63, x64, x65, x66, x67, x68, x69, x70, x71, x72, x73, x74, x75, x76, x77, x78, x79, x80, x81, x82, x83, x84, x85, x86, x87, x88, x89, x90, x91, x92, x93, x94, x95, x96, x97, x98, x99, x100 end
    return rec(a, b-1, c, d, e, f)+1
  end

  -- Compile recursive function.
  assert(rec(42, 200, 1, 2, 3, 43) == 207)

  local function trec()
    return rec(42, 0, 1, 2, 3, 43)
  end

  -- Compile function jumping to JFUNCF.
  for i=1,200 do
    gcinfo()
    assert(trec() == 7)
  end

  -- Shrink stack.
  for j=1,10 do collectgarbage() end

  -- Cause an exit due to stack growth with PC pointing to JFUNCF.
  -- Needs to load RD with nres+1 and not with the bytecode RD.
  assert(trec() == 7)
end
