do --- 1. Open upvalue above base slot, aliasing an SSA value.
  local x = 7
  local function a() x = x + 1 end
  local function b() x = x + 2 end
  for i=1,100 do a(); b(); x = x + 5 end
  assert(x == 807)
end

do --- 2. Open upvalue below base slot. UREFO CSE for a.x + b.x, but not x in loop.
    --    ULOAD not disambiguated. 2x ULOAD + 2x USTORE (+ 1x DSE USTORE).
  local x = 7
  (function()
    local function a() x = x + 1 end
    local function b() x = x + 2 end
    for i=1,100 do a(); b(); x = x + 5 end
  end)()
  assert(x == 807)
end

do --- 3. Closed upvalue. UREFC CSE for a.x + b.x, but not x in loop.
    --    ULOAD not disambiguated. 2x ULOAD + 2x USTORE (+ 1x DSE for USTORE).
  local xx = (function()
    local x = 7
    local function a() x = x + 1 end
    local function b() x = x + 2 end
    return function() for i=1,100 do a(); b(); x = x + 5 end; return x end
  end)()()
  assert(xx == 807)
end

do --- 4. Open upvalue below base slot. Forwarded. 1x USTORE (+ 1x DSE USTORE).
  local x = 7
  (function()
    local function a() x = x + 1 end
    for i=1,100 do a(); a() end
  end)()
  assert(x == 207)
end

do --- 5. Closed upvalue. Forwarded. 1x USTORE (+ 1x DSE USTORE).
  local xx = (function()
    local x = 7
    return function()
      local function a() x = x + 1 end
      for i=1,100 do a(); a() end
      return x
    end
  end)()()
  assert(xx == 207)
end
