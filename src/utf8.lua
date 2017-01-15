----------------------------------------------------------------------------
-- utf8 library.
--
-- Copyright (C) 2019 moonjit developers. https://github.com/moonjit/moonjit
--
----------------------------------------------------------------------------

local error, tostring, type, unpack = error, tostring, type, unpack
local byte, char = string.byte, string.char
local getinfo = debug.getinfo
local band, bor, lshift, rshift = bit.band, bit.bor, bit.lshift, bit.rshift

local function argerror(narg, extramsg, level)
    level = level or 1
    local caller = getinfo(level + 1, 'n').name
    error("bad argument #" .. tostring(narg) .. " to '"
          .. caller .. "' (" .. extramsg .. ")", level + 2)
end

local function typeerror (narg, arg, tname, level)
    level = level or 1
    local got = (arg == nil) and 'no value' or type(arg)
    argerror(narg, tname .. " expected, got " .. got, level + 1)
end

local function posrelat(pos, len)
  pos = (pos < 0) and (pos + len + 1) or pos
  if pos < 0 then
    pos = 1
  end
  return pos
end

local function utf8_decode(s, pos)
  local c = byte(s, pos)
  pos = pos + 1
  if c < 0x80 then
    return pos, c
  else
    local res = 0
    local count = 0
    while band(c, 0x40) ~= 0 do
      count = count + 1
      local cc = byte(s, pos)
      pos = pos + 1
      if (cc == nil) or (band(cc, 0xC0) ~= 0x80) then
        return
      end
      res = bor(lshift(res, 6), band(cc, 0x3F))
      c = lshift(c, 1)
    end
    res = bor(res, lshift(band(c, 0x7F), count * 5))
    if (count > 3) or (res > 0x10FFFF) then
      return
    end
    return pos, res
  end
end


-- len(s [, i [, j]]) --> number of characters that start in the
-- range [i,j], or nil + current position if 's' is not well formed in
-- that interval
local function utf8_len(s, i, j)
  if type(s) ~= 'string' then
    typeerror(1, s, 'string')
  end
  local len = #s
  i = i or 1
  if type(i) ~= 'number' then
    typeerror(2, i, 'number')
  end
  local posi = posrelat(i, len)
  j = j or -1
  if type(j) ~= 'number' then
    typeerror(3, j, 'number')
  end
  local posj = posrelat(j, len)
  if (1 > posi) or (posi > (len + 1)) then
    argerror(2, "initial position out of string")
  end
  if posj > (len + 1) then
    argerror(3, "final position out of string")
  end
  local n = 0
  while posi <= posj do
    local pos = utf8_decode(s, posi)
    if not pos then
      return nil, posi
    end
    posi = pos
    n = n + 1
  end
  return n
end


-- codepoint(s, [i, [j]])  -> returns codepoints for all characters
-- that start in the range [i,j]
local function utf8_codepoint(s, i, j)
  if type(s) ~= 'string' then
    typeerror(1, s, 'string')
  end
  local len = #s
  i = i or 1
  if type(i) ~= 'number' then
    typeerror(2, i, 'number')
  end
  local posi = posrelat(i, len)
  j = j or posi
  if type(j) ~= 'number' then
    typeerror(3, j, 'number')
  end
  local pose = posrelat(j, len)
  if 1 > posi then
    argerror(2, "out of range")
  end
  if pose > len then
    argerror(3, "out of range")
  end
  local t = {}
  while posi <= pose do
    local res
    posi, res = utf8_decode(s, posi)
    if not posi then
      error("invalid UTF-8 code", 2)
    end
    t[#t+1] = res
  end
  return unpack(t)
end


-- char(n1, n2, ...)  -> char(n1)..char(n2)...
local function utf8_char(...)
  local arg = {...}
  local t = {}
  for i = 1, #arg do
    local v = arg[i]
    if type(v) ~= 'number' then
      typeerror(i, v, 'number')
    end
    if (v < 0) or (v > 0x10FFFF) then
      argerror(i, "value out of range")
    end
    if v < 0x80 then
      t[#t+1] = v
    else
      if v < 0x800 then
        t[#t+1] = bor(0xC0, rshift(v, 6))
      else
        if v < 0x10000 then
          t[#t+1] = bor(0xE0, rshift(v, 12))
        else
          t[#t+1] = bor(0xF0, rshift(v, 18))
          t[#t+1] = bor(0x80, band(rshift(v, 12), 0x3f))
        end
        t[#t+1] = bor(0x80, band(rshift(v, 6), 0x3f))
      end
      t[#t+1] = bor(0x80, band(v, 0x3F))
    end
  end
  return char(unpack(t))
end


-- offset(s, n, [i])  -> index where n-th character *after*
--   position 'i' starts; 0 means character at 'i'.
local function utf8_offset(s, n, i)
  local function iscont(pos)
    return band(byte(s, pos) or 0, 0xC0) == 0x80
  end

  if type(s) ~= 'string' then
    typeerror(1, s, 'string')
  end
  local len = #s
  if type(n) ~= 'number' then
    typeerror(2, n, 'number')
  end
  i = i or ((n >= 0) and 1 or (len + 1))
  if type(i) ~= 'number' then
    typeerror(3, i, 'number')
  end
  local posi = posrelat(i, len)
  if (1 > posi) or (posi > (len + 1)) then
    argerror(3, "position out of range")
  end
  if n == 0 then
    while (posi > 0) and iscont(posi) do
      posi = posi - 1
    end
  else
    if iscont(posi) then
      error("initial position is a continuation byte", 2)
    end
    if n < 0 then
      while (n < 0) and (posi > 1) do
        repeat
          posi = posi - 1
        until (posi <= 0) or not iscont(posi)
        n = n + 1
      end
    else
      n = n - 1
      while (n > 0) and (posi <= len) do
        repeat
          posi = posi + 1
        until not iscont(posi)
        n = n - 1
      end
    end
  end
  if n == 0 then
    return posi
  end
end


local function utf8_codes(ss)
  local function iter_codes(s, n)
    local function iscont(pos)
      return band(byte(s, pos) or 0, 0xC0) == 0x80
    end

    if type(s) ~= 'string' then
      typeerror(1, s, 'string')
    end
    local len = #s
    if type(n) ~= 'number' then
      typeerror(2, n, 'number')
    end
    if n < 0 then
      n = 0
    elseif n <= len then
      n = n + 1
      while iscont(n) do
        n = n + 1
      end
    end
    if n > len then
      return
    else
      local pos, code = utf8_decode(s, n)
      if (pos == nil) or iscont(pos) then
        error("invalid UTF-8 code", 2)
      end
      return n, code
    end
  end

  if type(ss) ~= 'string' then
    typeerror(1, ss, 'string')
  end
  return iter_codes, ss, 0
end

return {
  charpattern = "[\0-\x7F\xC2-\xF4][\x80-\xBF]*",
  len = utf8_len,
  codepoint = utf8_codepoint,
  char = utf8_char,
  offset = utf8_offset,
  codes = utf8_codes,
}

