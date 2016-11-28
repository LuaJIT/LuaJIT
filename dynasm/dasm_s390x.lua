------------------------------------------------------------------------------
-- DynASM s390x module.
--
-- Copyright (C) 2005-2016 Mike Pall. All rights reserved.
-- See dynasm.lua for full copyright notice.
------------------------------------------------------------------------------

-- Module information:
local _info = {
  arch =	"s390x",
  description =	"DynASM s390x module",
  version =	"1.4.0",
  vernum =	 10400,
  release =	"2015-10-18",
  author =	"Mike Pall",
  license =	"MIT",
}

-- Exported glue functions for the arch-specific module.
local _M = { _info = _info }

-- Cache library functions.
local type, tonumber, pairs, ipairs = type, tonumber, pairs, ipairs
local assert, setmetatable, rawget = assert, setmetatable, rawget
local _s = string
local sub, format, byte, char = _s.sub, _s.format, _s.byte, _s.char
local match, gmatch, gsub = _s.match, _s.gmatch, _s.gsub
local concat, sort, insert = table.concat, table.sort, table.insert
local bit = bit or require("bit")
local band, shl, shr, sar = bit.band, bit.lshift, bit.rshift, bit.arshift
local ror, tohex = bit.ror, bit.tohex

-- Inherited tables and callbacks.
local g_opt, g_arch
local wline, werror, wfatal, wwarn

-- Action name list.
-- CHECK: Keep this in sync with the C code!
local action_names = {
  "STOP", "SECTION", "ESC", "REL_EXT",
  "ALIGN", "REL_LG", "LABEL_LG",
  "REL_PC", "LABEL_PC", "IMM", "IMM6", "IMM12", "IMM13W", "IMM13X", "IMML",
}

-- Maximum number of section buffer positions for dasm_put().
-- CHECK: Keep this in sync with the C code!
local maxsecpos = 25 -- Keep this low, to avoid excessively long C lines.

-- Action name -> action number.
local map_action = {}
for n,name in ipairs(action_names) do
  map_action[name] = n-1
end

-- Action list buffer.
local actlist = {}

-- Argument list for next dasm_put(). Start with offset 0 into action list.
local actargs = { 0 }

-- Current number of section buffer positions for dasm_put().
local secpos = 1

------------------------------------------------------------------------------

-- Dump action names and numbers.
local function dumpactions(out)
  out:write("DynASM encoding engine action codes:\n")
  for n,name in ipairs(action_names) do
    local num = map_action[name]
    out:write(format("  %-10s %02X  %d\n", name, num, num))
  end
  out:write("\n")
end

-- Write action list buffer as a huge static C array.
local function writeactions(out, name)
  local nn = #actlist
  if nn == 0 then nn = 1; actlist[0] = map_action.STOP end
  out:write("static const unsigned int ", name, "[", nn, "] = {\n")
  for i = 1,nn-1 do
    assert(out:write("0x", tohex(actlist[i]), ",\n"))
  end
  assert(out:write("0x", tohex(actlist[nn]), "\n};\n\n"))
end

------------------------------------------------------------------------------

-- Add word to action list.
local function wputxw(n)
  assert(n >= 0 and n <= 0xffffffff and n % 1 == 0, "word out of range")
  actlist[#actlist+1] = n
end

-- Add action to list with optional arg. Advance buffer pos, too.
local function waction(action, val, a, num)
  local w = assert(map_action[action], "bad action name `"..action.."'")
  wputxw(w * 0x10000 + (val or 0))
  if a then actargs[#actargs+1] = a end
  if a or num then secpos = secpos + (num or 1) end
end

-- Flush action list (intervening C code or buffer pos overflow).
local function wflush(term)
  if #actlist == actargs[1] then return end -- Nothing to flush.
  if not term then waction("STOP") end -- Terminate action list.
  wline(format("dasm_put(Dst, %s);", concat(actargs, ", ")), true)
  actargs = { #actlist } -- Actionlist offset is 1st arg to next dasm_put().
  secpos = 1 -- The actionlist offset occupies a buffer position, too.
end

-- Put escaped word.
local function wputw(n)
  if n <= 0x000fffff then waction("ESC") end
  wputxw(n)
end

-- Reserve position for word.
local function wpos()
  local pos = #actlist+1
  actlist[pos] = ""
  return pos
end

-- Store word to reserved position.
local function wputpos(pos, n)
  assert(n >= 0 and n <= 0xffffffff and n % 1 == 0, "word out of range")
  if n <= 0x000fffff then
    insert(actlist, pos+1, n)
    n = map_action.ESC * 0x10000
  end
  actlist[pos] = n
end

------------------------------------------------------------------------------

-- Global label name -> global label number. With auto assignment on 1st use.
local next_global = 20
local map_global = setmetatable({}, { __index = function(t, name)
  if not match(name, "^[%a_][%w_]*$") then werror("bad global label") end
  local n = next_global
  if n > 2047 then werror("too many global labels") end
  next_global = n + 1
  t[name] = n
  return n
end})

-- Dump global labels.
local function dumpglobals(out, lvl)
  local t = {}
  for name, n in pairs(map_global) do t[n] = name end
  out:write("Global labels:\n")
  for i=20,next_global-1 do
    out:write(format("  %s\n", t[i]))
  end
  out:write("\n")
end

-- Write global label enum.
local function writeglobals(out, prefix)
  local t = {}
  for name, n in pairs(map_global) do t[n] = name end
  out:write("enum {\n")
  for i=20,next_global-1 do
    out:write("  ", prefix, t[i], ",\n")
  end
  out:write("  ", prefix, "_MAX\n};\n")
end

-- Write global label names.
local function writeglobalnames(out, name)
  local t = {}
  for name, n in pairs(map_global) do t[n] = name end
  out:write("static const char *const ", name, "[] = {\n")
  for i=20,next_global-1 do
    out:write("  \"", t[i], "\",\n")
  end
  out:write("  (const char *)0\n};\n")
end

------------------------------------------------------------------------------

-- Extern label name -> extern label number. With auto assignment on 1st use.
local next_extern = 0
local map_extern_ = {}
local map_extern = setmetatable({}, { __index = function(t, name)
  -- No restrictions on the name for now.
  local n = next_extern
  if n > 2047 then werror("too many extern labels") end
  next_extern = n + 1
  t[name] = n
  map_extern_[n] = name
  return n
end})

-- Dump extern labels.
local function dumpexterns(out, lvl)
  out:write("Extern labels:\n")
  for i=0,next_extern-1 do
    out:write(format("  %s\n", map_extern_[i]))
  end
  out:write("\n")
end

-- Write extern label names.
local function writeexternnames(out, name)
  out:write("static const char *const ", name, "[] = {\n")
  for i=0,next_extern-1 do
    out:write("  \"", map_extern_[i], "\",\n")
  end
  out:write("  (const char *)0\n};\n")
end

------------------------------------------------------------------------------

-- Arch-specific maps.
-- TODO: add s390x related register names
-- Ext. register name -> int. name.
--local map_archdef = { xzr = "@x31", wzr = "@w31", lr = "x30", }
local map_archdef = {}

-- Int. register name -> ext. name.
-- local map_reg_rev = { ["@x31"] = "xzr", ["@w31"] = "wzr", x30 = "lr", }
local map_reg_rev = {}
	
local map_type = {}		-- Type name -> { ctype, reg }
local ctypenum = 0		-- Type number (for Dt... macros).

-- Reverse defines for registers.
function _M.revdef(s)
  return map_reg_rev[s] or s
end
-- not sure of these
local map_shift = { lsl = 0, lsr = 1, asr = 2, }

local map_extend = {
  uxtb = 0, uxth = 1, uxtw = 2, uxtx = 3,
  sxtb = 4, sxth = 5, sxtw = 6, sxtx = 7,
}

local map_cond = {
  o = 1, h = 2, hle = 3, l = 4,
  nhe = 5, lh = 6, ne = 7, e = 8,
  nlh = 9, he = 10, nl = 11, le = 12,
  nh = 13, no = 14, [""] = 15,
}

------------------------------------------------------------------------------

local parse_reg_type


local function parse_gpr(expr)
 -- assuming we get r0-r31  for now
  local r = match(expr, "^r([1-3]?[0-9])$")
  if r then
    r = tonumber(r)
    if r <= 31 then return r, tp end
  end
  werror("bad register name `"..expr.."'")
end

local function parse_fpr(expr)
  local r = match(expr, "^f([1-3]?[0-9])$")
  if r then
    r = tonumber(r)
    if r <= 31 then return r end
  end
  werror("bad register name `"..expr.."'")
end





local function parse_reg_base(expr)
  if expr == "sp" then return 0x3e0 end
  local base, tp = parse_reg(expr)
  if parse_reg_type ~= "x" then werror("bad register type") end
  parse_reg_type = false
  return shl(base, 5), tp
end

local parse_ctx = {}

local loadenv = setfenv and function(s)
  local code = loadstring(s, "")
  if code then setfenv(code, parse_ctx) end
  return code
end or function(s)
  return load(s, "", nil, parse_ctx)
end

-- Try to parse simple arithmetic, too, since some basic ops are aliases.
local function parse_number(n)
  local x = tonumber(n)
  if x then return x end
  local code = loadenv("return "..n)
  if code then
    local ok, y = pcall(code)
    if ok then return y end
  end
  return nil
end

local function parse_imm(imm, bits, shift, scale, signed)
  imm = match(imm, "^#(.*)$")
  if not imm then werror("expected immediate operand") end
  local n = parse_number(imm)
  if n then
    local m = sar(n, scale)
    if shl(m, scale) == n then
      if signed then
	local s = sar(m, bits-1)
	if s == 0 then return shl(m, shift)
	elseif s == -1 then return shl(m + shl(1, bits), shift) end
      else
	if sar(m, bits) == 0 then return shl(m, shift) end
      end
    end
    werror("out of range immediate `"..imm.."'")
  else
    waction("IMM", (signed and 32768 or 0)+scale*1024+bits*32+shift, imm)
    return 0
  end
end

local function parse_imm12(imm)
  imm = match(imm, "^#(.*)$")
  if not imm then werror("expected immediate operand") end
  local n = parse_number(imm)
  if n then
    if shr(n, 12) == 0 then
      return shl(n, 10)
    elseif band(n, 0xff000fff) == 0 then
      return shr(n, 2) + 0x00400000
    end
    werror("out of range immediate `"..imm.."'")
  else
    waction("IMM12", 0, imm)
    return 0
  end
end

local function parse_imm13(imm)
  imm = match(imm, "^#(.*)$")
  if not imm then werror("expected immediate operand") end
  local n = parse_number(imm)
  local r64 = parse_reg_type == "x"
  if n and n % 1 == 0 and n >= 0 and n <= 0xffffffff then
    local inv = false
    if band(n, 1) == 1 then n = bit.bnot(n); inv = true end
    local t = {}
    for i=1,32 do t[i] = band(n, 1); n = shr(n, 1) end
    local b = table.concat(t)
    b = b..(r64 and (inv and "1" or "0"):rep(32) or b)
    local p0, p1, p0a, p1a = b:match("^(0+)(1+)(0*)(1*)")
    if p0 then
      local w = p1a == "" and (r64 and 64 or 32) or #p1+#p0a
      if band(w, w-1) == 0 and b == b:sub(1, w):rep(64/w) then
	local s = band(-2*w, 0x3f) - 1
	if w == 64 then s = s + 0x1000 end
	if inv then
	  return shl(w-#p1-#p0, 16) + shl(s+w-#p1, 10)
	else
	  return shl(w-#p0, 16) + shl(s+#p1, 10)
	end
      end
    end
    werror("out of range immediate `"..imm.."'")
  elseif r64 then
    waction("IMM13X", 0, format("(unsigned int)(%s)", imm))
    actargs[#actargs+1] = format("(unsigned int)((unsigned long long)(%s)>>32)", imm)
    return 0
  else
    waction("IMM13W", 0, imm)
    return 0
  end
end

local function parse_imm6(imm)
  imm = match(imm, "^#(.*)$")
  if not imm then werror("expected immediate operand") end
  local n = parse_number(imm)
  if n then
    if n >= 0 and n <= 63 then
      return shl(band(n, 0x1f), 19) + (n >= 32 and 0x80000000 or 0)
    end
    werror("out of range immediate `"..imm.."'")
  else
    waction("IMM6", 0, imm)
    return 0
  end
end

local function parse_imm_load(imm, scale)
  local n = parse_number(imm)
  if n then
    local m = sar(n, scale)
    if shl(m, scale) == n and m >= 0 and m < 0x1000 then
      return shl(m, 10) + 0x01000000 -- Scaled, unsigned 12 bit offset.
    elseif n >= -256 and n < 256 then
      return shl(band(n, 511), 12) -- Unscaled, signed 9 bit offset.
    end
    werror("out of range immediate `"..imm.."'")
  else
    waction("IMML", 0, imm)
    return 0
  end
end

local function parse_fpimm(imm)
  imm = match(imm, "^#(.*)$")
  if not imm then werror("expected immediate operand") end
  local n = parse_number(imm)
  if n then
    local m, e = math.frexp(n)
    local s, e2 = 0, band(e-2, 7)
    if m < 0 then m = -m; s = 0x00100000 end
    m = m*32-16
    if m % 1 == 0 and m >= 0 and m <= 15 and sar(shl(e2, 29), 29)+2 == e then
      return s + shl(e2, 17) + shl(m, 13)
    end
    werror("out of range immediate `"..imm.."'")
  else
    werror("NYI fpimm action")
  end
end

local function parse_shift(expr)
  local s, s2 = match(expr, "^(%S+)%s*(.*)$")
  s = map_shift[s]
  if not s then werror("expected shift operand") end
  return parse_imm(s2, 6, 10, 0, false) + shl(s, 22)
end

local function parse_lslx16(expr)
  local n = match(expr, "^lsl%s*#(%d+)$")
  n = tonumber(n)
  if not n then werror("expected shift operand") end
  if band(n, parse_reg_type == "x" and 0xffffffcf or 0xffffffef) ~= 0 then
    werror("bad shift amount")
  end
  return shl(n, 17)
end

local function parse_extend(expr)
  local s, s2 = match(expr, "^(%S+)%s*(.*)$")
  if s == "lsl" then
    s = parse_reg_type == "x" and 3 or 2
  else
    s = map_extend[s]
  end
  if not s then werror("expected extend operand") end
  return (s2 == "" and 0 or parse_imm(s2, 3, 10, 0, false)) + shl(s, 13)
end

local function parse_cond(expr, inv)
  local c = map_cond[expr]
  if not c then werror("expected condition operand") end
  return shl(bit.bxor(c, inv), 12)
end

local function parse_load(params, nparams, n, op)
  if params[n+2] then werror("too many operands") end
  local pn, p2 = params[n], params[n+1]
  local p1, wb = match(pn, "^%[%s*(.-)%s*%](!?)$")
  if not p1 then
    if not p2 then
      local reg, tailr = match(pn, "^([%w_:]+)%s*(.*)$")
      if reg and tailr ~= "" then
	local base, tp = parse_reg_base(reg)
	if tp then
	  waction("IMML", 0, format(tp.ctypefmt, tailr))
	  return op + base
	end
      end
    end
    werror("expected address operand")
  end
  local scale = shr(op, 30)
  if p2 then
    if wb == "!" then werror("bad use of '!'") end
    op = op + parse_reg_base(p1) + parse_imm(p2, 9, 12, 0, true) + 0x400
  elseif wb == "!" then
    local p1a, p2a = match(p1, "^([^,%s]*)%s*,%s*(.*)$")
    if not p1a then werror("bad use of '!'") end
    op = op + parse_reg_base(p1a) + parse_imm(p2a, 9, 12, 0, true) + 0xc00
  else
    local p1a, p2a = match(p1, "^([^,%s]*)%s*(.*)$")
    op = op + parse_reg_base(p1a)
    if p2a ~= "" then
      local imm = match(p2a, "^,%s*#(.*)$")
      if imm then
	op = op + parse_imm_load(imm, scale)
      else
	local p2b, p3b, p3s = match(p2a, "^,%s*([^,%s]*)%s*,?%s*(%S*)%s*(.*)$")
	op = op + shl(parse_reg(p2b), 16) + 0x00200800
	if parse_reg_type ~= "x" and parse_reg_type ~= "w" then
	  werror("bad index register type")
	end
	if p3b == "" then
	  if parse_reg_type ~= "x" then werror("bad index register type") end
	  op = op + 0x6000
	else
	  if p3s == "" or p3s == "#0" then
	  elseif p3s == "#"..scale then
	    op = op + 0x1000
	  else
	    werror("bad scale")
	  end
	  if parse_reg_type == "x" then
	    if p3b == "lsl" and p3s ~= "" then op = op + 0x6000
	    elseif p3b == "sxtx" then op = op + 0xe000
	    else
	      werror("bad extend/shift specifier")
	    end
	  else
	    if p3b == "uxtw" then op = op + 0x4000
	    elseif p3b == "sxtw" then op = op + 0xc000
	    else
	      werror("bad extend/shift specifier")
	    end
	  end
	end
      end
    else
      if wb == "!" then werror("bad use of '!'") end
      op = op + 0x01000000
    end
  end
  return op
end

local function parse_load_pair(params, nparams, n, op)
  if params[n+2] then werror("too many operands") end
  local pn, p2 = params[n], params[n+1]
  local scale = shr(op, 30) == 0 and 2 or 3
  local p1, wb = match(pn, "^%[%s*(.-)%s*%](!?)$")
  if not p1 then
    if not p2 then
      local reg, tailr = match(pn, "^([%w_:]+)%s*(.*)$")
      if reg and tailr ~= "" then
	local base, tp = parse_reg_base(reg)
	if tp then
	  waction("IMM", 32768+7*32+15+scale*1024, format(tp.ctypefmt, tailr))
	  return op + base + 0x01000000
	end
      end
    end
    werror("expected address operand")
  end
  if p2 then
    if wb == "!" then werror("bad use of '!'") end
    op = op + 0x00800000
  else
    local p1a, p2a = match(p1, "^([^,%s]*)%s*,%s*(.*)$")
    if p1a then p1, p2 = p1a, p2a else p2 = "#0" end
    op = op + (wb == "!" and 0x01800000 or 0x01000000)
  end
  return op + parse_reg_base(p1) + parse_imm(p2, 7, 15, scale, true)
end

local function parse_label(label, def)
  local prefix = sub(label, 1, 2)
  -- =>label (pc label reference)
  if prefix == "=>" then
    return "PC", 0, sub(label, 3)
  end
  -- ->name (global label reference)
  if prefix == "->" then
    return "LG", map_global[sub(label, 3)]
  end
  if def then
    -- [1-9] (local label definition)
    if match(label, "^[1-9]$") then
      return "LG", 10+tonumber(label)
    end
  else
    -- [<>][1-9] (local label reference)
    local dir, lnum = match(label, "^([<>])([1-9])$")
    if dir then -- Fwd: 1-9, Bkwd: 11-19.
      return "LG", lnum + (dir == ">" and 0 or 10)
    end
    -- extern label (extern label reference)
    local extname = match(label, "^extern%s+(%S+)$")
    if extname then
      return "EXT", map_extern[extname]
    end
  end
  werror("bad label `"..label.."'")
end

local function branch_type(op)
  if band(op, 0x7c000000) == 0x14000000 then return 0 -- B, BL
  elseif shr(op, 24) == 0x54 or band(op, 0x7e000000) == 0x34000000 or
	 band(op, 0x3b000000) == 0x18000000 then
    return 0x800 -- B.cond, CBZ, CBNZ, LDR* literal
  elseif band(op, 0x7e000000) == 0x36000000 then return 0x1000 -- TBZ, TBNZ
  elseif band(op, 0x9f000000) == 0x10000000 then return 0x2000 -- ADR
  elseif band(op, 0x9f000000) == band(0x90000000) then return 0x3000 -- ADRP
  else
    assert(false, "unknown branch type")
  end
end

------------------------------------------------------------------------------

local map_op, op_template

local function op_alias(opname, f)
  return function(params, nparams)
    if not params then return "-> "..opname:sub(1, -3) end
    f(params, nparams)
    op_template(params, map_op[opname], nparams)
  end
end

local function alias_bfx(p)
  p[4] = "#("..p[3]:sub(2)..")+("..p[4]:sub(2)..")-1"
end

local function alias_bfiz(p)
  parse_reg(p[1])
  if parse_reg_type == "w" then
    p[3] = "#-("..p[3]:sub(2)..")%32"
    p[4] = "#("..p[4]:sub(2)..")-1"
  else
    p[3] = "#-("..p[3]:sub(2)..")%64"
    p[4] = "#("..p[4]:sub(2)..")-1"
  end
end

local alias_lslimm = op_alias("ubfm_4", function(p)
  parse_reg(p[1])
  local sh = p[3]:sub(2)
  if parse_reg_type == "w" then
    p[3] = "#-("..sh..")%32"
    p[4] = "#31-("..sh..")"
  else
    p[3] = "#-("..sh..")%64"
    p[4] = "#63-("..sh..")"
  end
end)

-- Template strings for s390x instructions.
map_op = {
  a =             "000000005a000000j",
ar =            "0000000000001a00g",
ay =            "0000e3000000005ak",
ag =            "0000e30000000008k",
agr =           "00000000b9080000h",
agf =           "0000e30000000018k",
agfr =          "00000000b9180000h",
axbr =          "00000000b34a0000h",
adbr =          "00000000b31a0000h",
aebr =          "00000000b30a0000h",
ah =            "000000004a000000j",
ahy =           "0000e3000000007ak",
afi =           "0000c20900000000l",
agfi =          "0000c20800000000l",
aih =           "0000cc0800000000l",
al =            "000000005e000000j",
alr =           "0000000000001e00g",
aly =           "0000e3000000005ek",
alg =           "0000e3000000000ak",
algr =          "00000000b90a0000h",
algf =          "0000e3000000001ak",
algfr =                 "00000000b91a0000h",
alfi =          "0000c20b00000000l",
algfi =                 "0000c20a00000000l",
alc =           "0000e30000000098k",
alcr =          "00000000b9980000h",
alcg =          "0000e30000000088k",
alcgr =                 "00000000b9880000h",
alsih =                 "0000cc0a00000000l",
alsihn =                "0000cc0b00000000l",
axr =           "0000000000003600g",
ad =            "000000006a000000j",
adr =           "0000000000002a00g",
ae =            "000000007a000000j",
aer =           "0000000000003a00g",
aw =            "000000006e000000j",
awr =           "0000000000002e00g",
au =            "000000007e000000j",
aur =           "0000000000003e00g",
n =             "0000000054000000j",
nr =            "0000000000001400g",
ny =            "0000e30000000054k",
ng =            "0000e30000000080k",
ngr =           "00000000b9800000h",
nihf =          "0000c00a00000000l",
nilf =          "0000c00b00000000l",
bal =           "0000000045000000j",
balr =          "000000000000500g",
bas =           "000000004d000000j",
basr =          "0000000000000d00g",
bassm =                 "0000000000000c00g",
bsa =           "00000000b25a0000h",
bsm =           "0000000000000b00g",
bakr =          "00000000b2400000h",
bsg =           "00000000b2580000h",
bc =            "0000000047000000j",
bcr =           "000000000000700g",
bct =           "0000000046000000j",
bctr =          "000000000000600g",
bctg =          "0000e30000000046k",
bctgr =                 "00000000b9460000h",
bxh =           "0000000086000000m",
bxhg =          "0000eb0000000044n",
bxle =          "0000000087000000m",
bxleg =                 "0000eb0000000045n",
brasl =                 "0000c00500000000l",
brcl =          "0000c00400000000l",
brcth =                 "0000cc0600000000l",
cksm =          "00000000b2410000h",
km =            "00000000b92e0000h",
kmf =           "00000000b92a0000h",
kmc =           "00000000b92f0000h",
kmo =           "00000000b92b0000h",
c =             "0000000059000000j",
cr =            "0000000000001900g",
cy =            "0000e30000000059k",
cg =            "0000e30000000020k",
cgr =           "00000000b9200000h",
cgf =           "0000e30000000030k",
cgfr =          "00000000b9300000h",
cxbr =          "00000000b3490000h",
cxtr =          "00000000b3ec0000h",
cxr =           "00000000b3690000h",
cdbr =          "00000000b3190000h",
cdtr =          "00000000b3e40000h",
cd =            "0000000069000000j",
cdr =           "0000000000002900g",
cebr =          "00000000b3090000h",
ce =            "0000000079000000j",
cer =           "0000000000003900g",
kxbr =          "00000000b3480000h",
kxtr =          "00000000b3e80000h",
kdbr =          "00000000b3180000h",
kdtr =          "00000000b3e00000h",
kebr =          "00000000b3080000h",
cs =            "00000000ba000000m",
csy =           "0000eb0000000014n",
csg =           "0000eb0000000030n",
csp =           "00000000b2500000h",
cspg =          "00000000b98a0000h",
cextr =                 "00000000b3fc0000h",
cedtr =                 "00000000b3f40000h",
cds =           "00000000bb000000m",
cdsy =          "0000eb0000000031n",
cdsg =          "0000eb000000003en",
ch =            "0000000049000000j",
chy =           "0000e30000000079k",
cgh =           "0000e30000000034k",
chrl =          "0000c60500000000l",
cghrl =                 "0000c60400000000l",
chf =           "0000e300000000cdk",
chhr =          "00000000b9cd0000h",
chlr =          "00000000b9dd0000h",
cfi =           "0000c20d00000000l",
cgfi =          "0000c20c00000000l",
cih =           "0000cc0d00000000l",
cl =            "0000000055000000j",
clr =           "0000000000001500g",
cly =           "0000e30000000055k",
clg =           "0000e30000000021k",
clgr =          "00000000b9210000h",
clgf =          "0000e30000000031k",
clgfr =                 "00000000b9310000h",
clmh =          "0000eb0000000020n",
clm =           "00000000bd000000m",
clmy =          "0000eb0000000021n",
clhf =          "0000e300000000cfk",
clhhr =                 "00000000b9cf0000h",
clhlr =                 "00000000b9df0000h",
clfi =          "0000c20f00000000l",
clgfi =                 "0000c20e00000000l",
clih =          "0000cc0f00000000l",
clcl =          "0000000000000f00g",
clcle =                 "00000000a9000000m",
clclu =                 "0000eb000000008fn",
clrl =          "0000c60f00000000l",
clhrl =                 "0000c60700000000l",
clgrl =                 "0000c60a00000000l",
clghrl =                "0000c60600000000l",
clgfrl =                "0000c60e00000000l",
clst =          "00000000b25d0000h",
crl =           "0000c60d00000000l",
cgrl =          "0000c60800000000l",
cgfrl =                 "0000c60c00000000l",
	cuse =          "00000000b2570000h",
cmpsc =                 "00000000b2630000h",
kimd =          "00000000b93e0000h",
klmd =          "00000000b93f0000h",
kmac =          "00000000b91e0000h",
thdr =          "00000000b3590000h",
thder =                 "00000000b3580000h",
cxfbr =                 "00000000b3960000h",
cxftr =                 "00000000b9590000h",
cxfr =          "00000000b3b60000h",
cdfbr =                 "00000000b3950000h",
cdftr =                 "00000000b9510000h",
cdfr =          "00000000b3b50000h",
cefbr =                 "00000000b3940000h",
cefr =          "00000000b3b40000h",
cxgbr =                 "00000000b3a60000h",
cxgtr =                 "00000000b3f90000h",
cxgr =          "00000000b3c60000h",
cdgbr =                 "00000000b3a50000h",
cdgtr =                 "00000000b3f10000h",
cdgr =          "00000000b3c50000h",
cegbr =                 "00000000b3a40000h",
cegr =          "00000000b3c40000h",
cxstr =                 "00000000b3fb0000h",
cdstr =                 "00000000b3f30000h",
cxutr =                 "00000000b3fa0000h",
cdutr =                 "00000000b3f20000h",
cvb =           "000000004f000000j",
cvby =          "0000e30000000006k",
cvbg =          "0000e3000000000ek",
cvd =           "000000004e000000j",
cvdy =          "0000e30000000026k",
cvdg =          "0000e3000000002ek",
cuxtr =                 "00000000b3ea0000h",
cudtr =                 "00000000b3e20000h",
cu42 =          "00000000b9b30000h",
cu41 =          "00000000b9b20000h",
cpya =          "00000000b24d0000h",
d =             "000000005d000000j",
dr =            "0000000000001d00g",
dxbr =          "00000000b34d0000h",
dxr =           "00000000b22d0000h",
ddbr =          "00000000b31d0000h",
dd =            "000000006d000000j",
ddr =           "0000000000002d00g",
debr =          "00000000b30d0000h",
de =            "000000007d000000j",
der =           "0000000000003d00g",
dl =            "0000e30000000097k",
dlr =           "00000000b9970000h",
dlg =           "0000e30000000087k",
dlgr =          "00000000b9870000h",
dsg =           "0000e3000000000dk",
dsgr =          "00000000b90d0000h",
dsgf =          "0000e3000000001dk",
dsgfr =                 "00000000b91d0000h",
x =             "0000000057000000j",
xr =            "0000000000001700g",
xy =            "0000e30000000057k",
xg =            "0000e30000000082k",
xgr =           "00000000b9820000h",
xihf =          "0000c00600000000l",
xilf =          "0000c00700000000l",
ex =            "0000000044000000j",
exrl =          "0000c60000000000l",
ear =           "00000000b24f0000h",
esea =          "00000000b99d0000h",
eextr =          "00000000b3ed0000h",
eedtr =          "00000000b3e50000h",
ecag =          "0000eb000000004cn",
efpc =          "00000000b38c0000h",
epar =          "00000000b2260000h",
epair =                 "00000000b99a0000h",
epsw =          "00000000b98d0000h",
esar =          "00000000b2270000h",
esair =                 "00000000b99b0000h",
esxtr =                 "00000000b3ef0000h",
esdtr =                 "00000000b3e70000h",
ereg =          "00000000b2490000h",
eregg =                 "00000000b90e0000h",
esta =          "00000000b24a0000h",
flogr =                 "00000000b9830000h",
hdr =           "0000000000002400g",
her =           "0000000000003400g",
iac =           "00000000b2240000h",
ic =            "0000000043000000j",
icy =           "0000e30000000073k",
icmh =          "0000eb0000000080n",
icm =           "00000000bf000000m",
icmy =          "0000eb0000000081n",
iihf =          "0000c00800000000l",
iilf =          "0000c00900000000l",
ipm =           "00000000b2220000h",
iske =          "00000000b2290000h",
ivsk =          "00000000b2230000h",
l =             "0000000058000000j",
lr =            "0000000000001800g",
ly =            "0000e30000000058k",
lg =            "0000e30000000004k",
lgr =           "00000000b9040000h",
lgf =           "0000e30000000014k",
lgfr =          "00000000b9140000h",
lxr =           "00000000b3650000h",
ld =            "0000000068000000j",
ldr =           "0000000000002800g",
ldy =           "0000ed0000000065k",
le =            "0000000078000000j",
ler =           "0000000000003800g",
	ley =           "0000ed0000000064k",
lam =           "000000009a000000m",
lamy =          "0000eb000000009an",
la =            "0000000041000000j",
lay =           "0000e30000000071k",
lae =           "0000000051000000j",
laey =          "0000e30000000075k",
larl =          "0000c00000000000l",
laa =           "0000eb00000000f8n",
laag =          "0000eb00000000e8n",
laal =          "0000eb00000000fan",
laalg =                 "0000eb00000000ean",
lan =           "0000eb00000000f4n",
lang =          "0000eb00000000e4n",
lax =           "0000eb00000000f7n",
laxg =          "0000eb00000000e7n",
lao =           "0000eb00000000f6n",
laog =          "0000eb00000000e6n",
lt =            "0000e30000000012k",
ltr =           "0000000000001200g",
ltg =           "0000e30000000002k",
ltgr =          "00000000b9020000h",
ltgf =          "0000e30000000032k",
ltgfr =                 "00000000b9120000h",
ltxbr =                 "00000000b3420000h",
ltxtr =                 "00000000b3de0000h",
ltxr =          "00000000b3620000h",
ltdbr =                 "00000000b3120000h",
ltdtr =                 "00000000b3d60000h",
ltdr =          "0000000000002200g",
ltebr =                 "00000000b3020000h",
lter =          "0000000000003200g",
lb =            "0000e30000000076k",
lbr =           "00000000b9260000h",
lgb =           "0000e30000000077k",
lgbr =          "00000000b9060000h",
	lbh =           "0000e300000000c0k",
lcr =           "0000000000001300g",
lcgr =          "00000000b9030000h",
lcgfr =                 "00000000b9130000h",
lcxbr =                 "00000000b3430000h",
lcxr =          "00000000b3630000h",
lcdbr =                 "00000000b3130000h",
lcdr =          "0000000000002300g",
lcdfr =                 "00000000b3730000h",
lcebr =                 "00000000b3030000h",
lcer =          "0000000000003300g",
lctl =          "00000000b7000000m",
lctlg =                 "0000eb000000002fn",
fixr =          "00000000b3670000h",
fidr =          "00000000b37f0000h",
fier =          "00000000b3770000h",
ldgr =          "00000000b3c10000h",
lgdr =          "00000000b3cd0000h",
lh =            "0000000048000000j",
lhr =           "00000000b9270000h",
lhy =           "0000e30000000078k",
lgh =           "0000e30000000015k",
lghr =          "00000000b9070000h",
lhh =           "0000e300000000c4k",
lhrl =          "0000c40500000000l",
lghrl =                 "0000c40400000000l",
lfh =           "0000e300000000cak",
lgfi =          "0000c00100000000l",
lxdbr =                 "00000000b3050000h",
lxdr =          "00000000b3250000h",
lxebr =                 "00000000b3060000h",
lxer =          "00000000b3260000h",
ldebr =                 "00000000b3040000h",
lder =          "00000000b3240000h",
llgf =          "0000e30000000016k",
llgfr =                 "00000000b9160000h",
llc =           "0000e30000000094k",
llcr =          "00000000b9940000h",
llgc =          "0000e30000000090k",
llgcr =                 "00000000b9840000h",
llch =          "0000e300000000c2k",
llh =           "0000e30000000095k",
llhr =          "00000000b9950000h",
llgh =          "0000e30000000091k",
llghr =                 "00000000b9850000h",
llhh =          "0000e300000000c6k",
llhrl =                 "0000c40200000000l",
llghrl =                "0000c40600000000l",
llihf =                 "0000c00e00000000l",
llilf =                 "0000c00f00000000l",
llgfrl =                "0000c40e00000000l",
llgt =          "0000e30000000017k",
llgtr =                 "00000000b9170000h",
lm =            "0000000098000000m",
lmy =           "0000eb0000000098n",
lmg =           "0000eb0000000004n",
lmh =           "0000eb0000000096n",
lnr =           "0000000000001100g",
lngr =          "00000000b9010000h",
lngfr =                 "00000000b9110000h",
lnxbr =                 "00000000b3410000h",
lnxr =          "00000000b3610000h",
lndbr =                 "00000000b3110000h",
lndr =          "0000000000002100g",
lndfr =                 "00000000b3710000h",
lnebr =                 "00000000b3010000h",
lner =          "0000000000003100g",
loc =           "0000eb00000000f2n",
locg =          "0000eb00000000e2n",
lpq =           "0000e3000000008fk",
lpr =           "0000000000001000g",
lpgr =          "00000000b9000000h",
lpgfr =                 "00000000b9100000h",
lpxbr =                 "00000000b3400000h",
lpxr =          "00000000b3600000h",
lpdbr =                 "00000000b3100000h",
lpdr =          "0000000000002000g",
lpdfr =                 "00000000b3700000h",
lpebr =                 "00000000b3000000h",
lper =          "0000000000003000g",
lra =           "00000000b1000000j",
lray =          "0000e30000000013k",
lrag =          "0000e30000000003k",
lrl =           "0000c40d00000000l",
lgrl =          "0000c40800000000l",
lgfrl =                 "0000c40c00000000l",
lrvh =          "0000e3000000001fk",
lrv =           "0000e3000000001ek",
lrvr =          "00000000b91f0000h",
lrvg =          "0000e3000000000fk",
lrvgr =                 "00000000b90f0000h",
ldxbr =                 "00000000b3450000h",
ldxr =          "0000000000002500g",
lrdr =          "0000000000002500g",
lexbr =                 "00000000b3460000h",
lexr =          "00000000b3660000h",
ledbr =                 "00000000b3440000h",
ledr =          "0000000000003500g",
lrer =          "0000000000003500g",
lura =          "00000000b24b0000h",
lurag =                 "00000000b9050000h",
lzxr =          "00000000b3760000h",
lzdr =          "00000000b3750000h",
lzer =          "00000000b3740000h",
msta =          "00000000b2470000h",
mvcl =          "0000000000000e00g",
mvcle =                 "00000000a8000000m",
mvclu =                 "0000eb000000008en",
mvpg =          "00000000b2540000h",
mvst =          "00000000b2550000h",
m =             "000000005c000000j",
mfy =           "0000e3000000005ck",
mr =            "0000000000001c00g",
mxbr =          "00000000b34c0000h",
mxr =           "0000000000002600g",
mdbr =          "00000000b31c0000h",
md =            "000000006c000000j",
mdr =           "0000000000002c00g",
mxdbr =                 "00000000b3070000h",
mxd =           "0000000067000000j",
mxdr =          "0000000000002700g",
meebr =                 "00000000b3170000h",
meer =          "00000000b3370000h",
mdebr =                 "00000000b30c0000h",
mde =           "000000007c000000j",
mder =          "0000000000003c00g",
me =            "000000007c000000j",
mer =           "0000000000003c00g",
mh =            "000000004c000000j",
mhy =           "0000e3000000007ck",
mlg =           "0000e30000000086k",
mlgr =          "00000000b9860000h",
ml =            "0000e30000000096k",
mlr =           "00000000b9960000h",
ms =            "0000000071000000j",
msr =           "00000000b2520000h",
msy =           "0000e30000000051k",
msg =           "0000e3000000000ck",
msgr =          "00000000b90c0000h",
msgf =          "0000e3000000001ck",
msgfr =                 "00000000b91c0000h",
msfi =          "0000c20100000000l",
msgfi =                 "0000c20000000000l",
o =             "0000000056000000j",
["or"] =            "0000000000001600g",
oy =            "0000e30000000056k",
og =            "0000e30000000081k",
ogr =           "00000000b9810000h",
oihf =          "0000c00c00000000l",
oilf =          "0000c00d00000000l",
pgin =          "00000000b22e0000h",
pgout =                 "00000000b22f0000h",
pcc =           "00000000b92c0000h",
pckmo =                 "00000000b9280000h",
pfmf =          "00000000b9af0000h",
ptf =           "00000000b9a20000h",
popcnt =                "00000000b9e10000h",
pfd =           "0000e30000000036k",
pfdrl =                 "0000c60200000000l",
pt =            "00000000b2280000h",
pti =           "00000000b99e0000h",
palb =          "00000000b2480000h",
rrbe =          "00000000b22a0000h",
rrbm =          "00000000b9ae0000h",
rll =           "0000eb000000001dn",
rllg =          "0000eb000000001cn",
srst =          "00000000b25e0000h",
srstu =                 "00000000b9be0000h",
sar =           "00000000b24e0000h",
sfpc =          "00000000b3840000h",
sfasr =                 "00000000b3850000h",
spm =           "000000000000400g",
ssar =          "00000000b2250000h",
ssair =                 "00000000b99f0000h",
slda =          "000000008f000000m",
sldl =          "000000008d000000m",
sla =           "000000008b000000m",
slak =          "0000eb00000000ddn",
slag =          "0000eb000000000bn",
sll =           "0000000089000000m",
sllk =          "0000eb00000000dfn",
sllg =          "0000eb000000000dn",
srda =          "000000008e000000m",
srdl =          "000000008c000000m",
sra =           "000000008a000000m",
srak =          "0000eb00000000dcn",
srag =          "0000eb000000000an",
srl =           "0000000088000000m",
srlk =          "0000eb00000000den",
srlg =          "0000eb000000000cn",
sqxbr =                 "00000000b3160000h",
sqxr =          "00000000b3360000h",
sqdbr =                 "00000000b3150000h",
sqdr =          "00000000b2440000h",
sqebr =                 "00000000b3140000h",
sqer =          "00000000b2450000h",
st =            "0000000050000000j",
sty =           "0000e30000000050k",
stg =           "0000e30000000024k",
std =           "0000000060000000j",
stdy =          "0000ed0000000067k",
ste =           "0000000070000000j",
stey =          "0000ed0000000066k",
stam =          "000000009b000000m",
stamy =                 "0000eb000000009bn",
stc =           "0000000042000000j",
stcy =          "0000e30000000072k",
stch =          "0000e300000000c3k",
stcmh =                 "0000eb000000002cn",
stcm =          "00000000be000000m",
stcmy =                 "0000eb000000002dn",
stctl =                 "00000000b6000000m",
stctg =                 "0000eb0000000025n",
sth =           "0000000040000000j",
sthy =          "0000e30000000070k",
sthh =          "0000e300000000c7k",
sthrl =                 "0000c40700000000l",
stfh =          "0000e300000000cbk",
stm =           "0000000090000000m",
stmy =          "0000eb0000000090n",
stmg =          "0000eb0000000024n",
stmh =          "0000eb0000000026n",
stoc =          "0000eb00000000f3n",
stocg =                 "0000eb00000000e3n",
stpq =          "0000e3000000008ek",
strl =          "0000c40f00000000l",
stgrl =                 "0000c40b00000000l",
strvh =                 "0000e3000000003fk",
strv =          "0000e3000000003ek",
strvg =                 "0000e3000000002fk",
stura =                 "00000000b2460000h",
sturg =                 "00000000b9250000h",
s =             "000000005b000000j",
sr =            "0000000000001b00g",
sy =            "0000e3000000005bk",
sg =            "0000e30000000009k",
sgr =           "00000000b9090000h",
sgf =           "0000e30000000019k",
sgfr =          "00000000b9190000h",
sxbr =          "00000000b34b0000h",
sdbr =          "00000000b31b0000h",
sebr =          "00000000b30b0000h",
sh =            "000000004b000000j",
shy =           "0000e3000000007bk",
sl =            "000000005f000000j",
slr =           "0000000000001f00g",
sly =           "0000e3000000005fk",
slg =           "0000e3000000000bk",
slgr =          "00000000b90b0000h",
slgf =          "0000e3000000001bk",
slgfr =                 "00000000b91b0000h",
slfi =          "0000c20500000000l",
slgfi =                 "0000c20400000000l",
slb =           "0000e30000000099k",
slbr =          "00000000b9990000h",
slbg =          "0000e30000000089k",
slbgr =                 "00000000b9890000h",
sxr =           "0000000000003700g",
sd =            "000000006b000000j",
sdr =           "0000000000002b00g",
se =            "000000007b000000j",
ser =           "0000000000003b00g",
su =            "000000007f000000j",
sur =           "0000000000003f00g",
sw =            "000000006f000000j",
swr =           "0000000000002f00g",
tar =           "00000000b24c0000h",
tb =            "00000000b22c0000h",
trace =                 "0000000099000000m",
tracg =                 "0000eb000000000fn",
tre =           "00000000b2a50000h",
}
for cond,c in pairs(map_cond) do
  -- Extended mnemonics for branches.
  -- TODO: replace 'B' with correct encoding.
  -- brc
  map_op["j"..cond.."_1"] = "00000000"..tohex(0xa7040000+shl(c, 20)).."B"
  -- brcl
  map_op["jg"..cond.."_1"] = tohex(0xc004+shl(c, 4)).."00000000".."B"
  -- bc
  map_op["b"..cond.."_1"] = "00000000"..tohex(0x47000000+shl(c, 20)).."B"
  -- bcr
  map_op["b"..cond.."r_1"] = "00000000"..tohex(0x0700+shl(c, 4)).."B"
end
------------------------------------------------------------------------------
-- Handle opcodes defined with template strings.
local function parse_template(params, template, nparams, pos)
  local op = tonumber(sub(template, 1, 16), 16)  -- 
						 -- 00000000005a0000  converts to 90
  local n,rs = 1,26
  
  parse_reg_type = false
  -- Process each character. (if its RX-a==> 1st iteration gets R, 2nd==X and so on)
  for p in gmatch(sub(template, 17), ".") do
  local pr1,pr2,pr3
    if p == "g" then
	pr1,pr2=param[n],param[n+1]			
      op = op + parse_reg(pr1)+parse_reg(pr2); n = n + 1  -- not sure if we will require n later, so keeping it as it is now
    elseif p == "h" then
      
    elseif p == "j" then
      
    elseif p == "k" then
      
    elseif p == "l" then
      
    elseif p == "m" then
      
    elseif p == "n" then
	
      end
  end
  wputpos(pos, op)
end
function op_template(params, template, nparams)
  if not params then return template:gsub("%x%x%x%x%x%x%x%x", "") end
  -- Limit number of section buffer positions used by a single dasm_put().
  -- A single opcode needs a maximum of 3 positions.
  if secpos+3 > maxsecpos then wflush() end
  local pos = wpos()
  local lpos, apos, spos = #actlist, #actargs, secpos
  local ok, err
  for t in gmatch(template, "[^|]+") do
    ok, err = pcall(parse_template, params, t, nparams, pos)
    if ok then return end
    secpos = spos
    actlist[lpos+1] = nil
    actlist[lpos+2] = nil
    actlist[lpos+3] = nil
    actargs[apos+1] = nil
    actargs[apos+2] = nil
    actargs[apos+3] = nil
  end
  error(err, 0)
end
map_op[".template__"] = op_template
------------------------------------------------------------------------------
-- Pseudo-opcode to mark the position where the action list is to be emitted.
map_op[".actionlist_1"] = function(params)
  if not params then return "cvar" end
  local name = params[1] -- No syntax check. You get to keep the pieces.
  wline(function(out) writeactions(out, name) end)
end
-- Pseudo-opcode to mark the position where the global enum is to be emitted.
map_op[".globals_1"] = function(params)
  if not params then return "prefix" end
  local prefix = params[1] -- No syntax check. You get to keep the pieces.
  wline(function(out) writeglobals(out, prefix) end)
end
-- Pseudo-opcode to mark the position where the global names are to be emitted.
map_op[".globalnames_1"] = function(params)
  if not params then return "cvar" end
  local name = params[1] -- No syntax check. You get to keep the pieces.
  wline(function(out) writeglobalnames(out, name) end)
end
-- Pseudo-opcode to mark the position where the extern names are to be emitted.
map_op[".externnames_1"] = function(params)
  if not params then return "cvar" end
  local name = params[1] -- No syntax check. You get to keep the pieces.
  wline(function(out) writeexternnames(out, name) end)
end
------------------------------------------------------------------------------
-- Label pseudo-opcode (converted from trailing colon form).
map_op[".label_1"] = function(params)
  if not params then return "[1-9] | ->global | =>pcexpr" end
  if secpos+1 > maxsecpos then wflush() end
  local mode, n, s = parse_label(params[1], true)
  if mode == "EXT" then werror("bad label definition") end
  waction("LABEL_"..mode, n, s, 1)
end
------------------------------------------------------------------------------
-- Pseudo-opcodes for data storage.
map_op[".long_*"] = function(params)
  if not params then return "imm..." end
  for _,p in ipairs(params) do
    local n = tonumber(p)
    if not n then werror("bad immediate `"..p.."'") end
    if n < 0 then n = n + 2^32 end
    wputw(n)
    if secpos+2 > maxsecpos then wflush() end
  end
end
-- Alignment pseudo-opcode.
map_op[".align_1"] = function(params)
  if not params then return "numpow2" end
  if secpos+1 > maxsecpos then wflush() end
  local align = tonumber(params[1])
  if align then
    local x = align
    -- Must be a power of 2 in the range (2 ... 256).
    for i=1,8 do
      x = x / 2
      if x == 1 then
	waction("ALIGN", align-1, nil, 1) -- Action byte is 2**n-1.
	return
      end
    end
  end
  werror("bad alignment")
end
------------------------------------------------------------------------------
-- Pseudo-opcode for (primitive) type definitions (map to C types).
map_op[".type_3"] = function(params, nparams)
  if not params then
    return nparams == 2 and "name, ctype" or "name, ctype, reg"
  end
  local name, ctype, reg = params[1], params[2], params[3]
  if not match(name, "^[%a_][%w_]*$") then
    werror("bad type name `"..name.."'")
  end
  local tp = map_type[name]
  if tp then
    werror("duplicate type `"..name.."'")
  end
  -- Add #type to defines. A bit unclean to put it in map_archdef.
  map_archdef["#"..name] = "sizeof("..ctype..")"
  -- Add new type and emit shortcut define.
  local num = ctypenum + 1
  map_type[name] = {
    ctype = ctype,
    ctypefmt = format("Dt%X(%%s)", num),
    reg = reg,
  }
  wline(format("#define Dt%X(_V) (int)(ptrdiff_t)&(((%s *)0)_V)", num, ctype))
  ctypenum = num
end
map_op[".type_2"] = map_op[".type_3"]
-- Dump type definitions.
local function dumptypes(out, lvl)
  local t = {}
  for name in pairs(map_type) do t[#t+1] = name end
  sort(t)
  out:write("Type definitions:\n")
  for _,name in ipairs(t) do
    local tp = map_type[name]
    local reg = tp.reg or ""
    out:write(format("  %-20s %-20s %s\n", name, tp.ctype, reg))
  end
  out:write("\n")
end
------------------------------------------------------------------------------
-- Set the current section.
function _M.section(num)
  waction("SECTION", num)
  wflush(true) -- SECTION is a terminal action.
end
------------------------------------------------------------------------------
-- Dump architecture description.
function _M.dumparch(out)
  out:write(format("DynASM %s version %s, released %s\n\n",
    _info.arch, _info.version, _info.release))
  dumpactions(out)
end
-- Dump all user defined elements.
function _M.dumpdef(out, lvl)
  dumptypes(out, lvl)
  dumpglobals(out, lvl)
  dumpexterns(out, lvl)
end
------------------------------------------------------------------------------
-- Pass callbacks from/to the DynASM core.
function _M.passcb(wl, we, wf, ww)
  wline, werror, wfatal, wwarn = wl, we, wf, ww
  return wflush
end
-- Setup the arch-specific module.
function _M.setup(arch, opt)
  g_arch, g_opt = arch, opt
end
-- Merge the core maps and the arch-specific maps.
function _M.mergemaps(map_coreop, map_def)
  setmetatable(map_op, { __index = map_coreop })
  setmetatable(map_def, { __index = map_archdef })
  return map_op, map_def
end
return _M
------------------------------------------------------------------------------
