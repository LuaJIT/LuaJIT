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
  "REL_PC", "LABEL_PC", "DISP12", "DISP20", "IMM8", "IMM16", "IMM32", "LEN8R","LEN4HR","LEN4LR",
}

-- Maximum number of section buffer positions for dasm_put().
-- CHECK: Keep this in sync with the C code!
local maxsecpos = 25 -- Keep this low, to avoid excessively long C lines.

-- Action name -> action number.
local map_action = {}
local max_action = 0
for n,name in ipairs(action_names) do
  map_action[name] = n-1
  max_action = n
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

local function havearg(a)
  return a == "ESC" or
         a == "SECTION" or
         a == "REL_LG" or
         a == "LABEL_LG" or
         a == "REL_EXT"
end

-- Write action list buffer as a huge static C array.
local function writeactions(out, name)
  local nn = #actlist
  if nn == 0 then nn = 1; actlist[0] = map_action.STOP end
  out:write("static const unsigned short ", name, "[", nn, "] = {")
  local esc = false -- also need to escape for action arguments
  for i = 1,nn do
    assert(out:write("\n  0x", sub(tohex(actlist[i]), 5, 8)))
    if i ~= nn then assert(out:write(",")) end
    local name = action_names[actlist[i]+1]
    if not esc and name then
      assert(out:write(" /* ", name, " */"))
      esc = havearg(name)
    else
      esc = false
    end
  end
  assert(out:write("\n};\n\n"))
end

------------------------------------------------------------------------------

-- Add halfword to action list.
local function wputxhw(n)
  assert(n >= 0 and n <= 0xffff, "halfword out of range")
  actlist[#actlist+1] = n
end

-- Add action to list with optional arg. Advance buffer pos, too.
local function waction(action, val, a, num)
  local w = assert(map_action[action], "bad action name `"..action.."'")
  wputxhw(w)
  if val then wputxhw(val) end -- Not sure about this, do we always have one arg?
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

-- Put escaped halfword.
local function wputhw(n)
  if n <= max_action then waction("ESC") end
  wputxhw(n)
end

-- Reserve position for halfword.
local function wpos()
  local pos = #actlist+1
  actlist[pos] = ""
  return pos
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
-- Ext. register name -> int. name.
local map_archdef = { sp = "r15" }

-- Int. register name -> ext. name.
local map_reg_rev = { r15 = "sp" }

local map_type = {}		-- Type name -> { ctype, reg }
local ctypenum = 0		-- Type number (for Dt... macros).

-- Reverse defines for registers.
function _M.revdef(s)
  return map_reg_rev[s] or s
end

local map_cond = {
  o = 1, h = 2, hle = 3, l = 4,
  nhe = 5, lh = 6, ne = 7, e = 8,
  nlh = 9, he = 10, nl = 11, le = 12,
  nh = 13, no = 14, [""] = 15,
}

------------------------------------------------------------------------------

local function parse_reg(expr)
  if not expr then werror("expected register name") end
  local tname, ovreg = match(expr, "^([%w_]+):(r1?%d)$")
  local tp = map_type[tname or expr]
  if tp then
    local reg = ovreg or tp.reg
    if not reg then
      werror("type `"..(tname or expr).."' needs a register override")
    end
    expr = reg
  end
  local r = match(expr, "^[rf](1?%d)$")
  if r then
    r = tonumber(r)
    if r <= 15 then return r, tp end
  end
  werror("bad register name `"..expr.."'")
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

local function is_uint12(num)
  return 0 <= num and num < 4096
end

local function is_int20(num)
  return -shl(1, 19) <= num and num < shl(1, 19)
end

local function is_int32(num)
  return -2147483648 <= num and num < 2147483648
end

local function is_uint16(num)
  return 0 <= num and num < 0xffff
end

local function is_int16(num)
  return -32768 <= num and num < 32768
end

local function is_int8(num)
  return -128 <= num and num < 128
end

-- Split a memory operand of the form d(b) or d(x,b) into d, x and b.
-- If x is not specified then it is 0.
local function split_memop(arg)
  local reg = "[%w_:]+"
  local d, x, b = match(arg, "^(.*)%(%s*("..reg..")%s*,%s*("..reg..")%s*%)$")
  if d then
    return d, parse_reg(x), parse_reg(b)
  end
  local d, b = match(arg, "^(.*)%(%s*("..reg..")%s*%)$")
  if d then
    return d, 0, parse_reg(b)
  end
  local reg, tailr = match(arg, "^([%w_:]+)%s*(.*)$")
  if reg then
    local r, tp = parse_reg(reg)
    if tp then
      return format(tp.ctypefmt, tailr), 0, r
    end
  end
  -- TODO: handle values without registers?
  -- TODO: handle registers without a displacement?
  werror("bad memory operand: "..arg)
  return nil
end

-- Parse memory operand of the form d(x, b) where 0 <= d < 4096 and b and x
-- are GPRs.
-- If the fourth return value is not-nil then it needs to be called to
-- insert an action.
-- Encoded as: xbddd
local function parse_mem_bx(arg)
  local d, x, b = split_memop(arg)
  local dval = tonumber(d)
  if dval then
    if not is_uint12(dval) then
      werror("displacement out of range: ", dval)
    end
    return dval, x, b, nil
  end
  if match(d, "^[rf]1?[0-9]?") then
    werror("expected immediate operand, got register")
  end
  return 0, x, b, function() waction("DISP12", nil, d) end
end

-- Parse memory operand of the form d(b) where 0 <= d < 4096 and b is a GPR.
-- Encoded as: bddd
local function parse_mem_b(arg)
  local d, x, b, a = parse_mem_bx(arg)
  if x ~= 0 then
    werror("unexpected index register")
  end
  return d, b, a
end

-- Parse memory operand of the form d(x, b) where -(2^20)/2 <= d < (2^20)/2
-- and b and x are GPRs.
-- Encoded as: xblllhh (ls are the low-bits of d, and hs are the high bits).
local function parse_mem_bxy(arg)
  local d, x, b = split_memop(arg)
  local dval = tonumber(d)
  if dval then
    if not is_int20(dval) then
      werror("displacement out of range: ", dval)
    end
    return dval, x, b, nil
  end
  if match(d, "^[rf]1?[0-9]?") then
    werror("expected immediate operand, got register")
  end
  return 0, x, b, function() waction("DISP20", nil, d) end
end

-- Parse memory operand of the form d(b) where -(2^20)/2 <= d < (2^20)/2 and
-- b is a GPR.
-- Encoded as: blllhh (ls are the low-bits of d, and hs are the high bits).
local function parse_mem_by(arg)
  local d, x, b, a = parse_mem_bxy(arg)
  if x ~= 0 then
    werror("unexpected index register")
  end
  return d, b, a
end

-- Parse memory operand of the form d(l, b) where 0 <= d < 4096, 1 <= l <= 256,
-- and b is a GPR.
local function parse_mem_lb(arg)
  local reg = "r1?[0-9]"
  local d, l, b = match(arg, "^(.*)%s*%(%s*(.*)%s*,%s*("..reg..")%s*%)$")
  if not d then
    -- TODO: handle values without registers?
    -- TODO: handle registers without a displacement?
    werror("bad memory operand: "..arg)
    return nil
  end
  local dval = tonumber(d)
  local dact = nil
  if dval then
    if not is_uint12(dval) then
      werror("displacement out of range: ", dval)
    end
  else
    dval = 0
    dact = function() waction("DISP12", nil, d) end
  end
  local lval = tonumber(l)
  local lact = nil
  if lval then
    if lval < 1 or lval > 256 then
      werror("length out of range: ", dval)
    end
    lval = lval - 1
  else
    lval = 0
    lact = function() waction("LEN8R", nil, l) end
  end
  return dval, lval, parse_reg(b), dact, lact
end

local function parse_mem_l2b(arg,high_l)
  local reg = "r1?[0-9]"
  local d, l, b = match(arg, "^(.*)%s*%(%s*(.*)%s*,%s*("..reg..")%s*%)$")
  if not d then
    -- TODO: handle values without registers?
    -- TODO: handle registers without a displacement?
    werror("bad memory operand: "..arg)
    return nil
  end
  local dval = tonumber(d)
  local dact = nil
  if dval then
    if not is_uint12(dval) then
      werror("displacement out of range: ", dval)
    end
  else
    dval = 0
    dact = function() waction("DISP12", nil, d) end
  end
  local lval = tonumber(l)
  local lact = nil
  if lval then
    if lval < 1 or lval > 128 then
      werror("length out of range: ", dval)
    end
    lval = lval - 1
  else
    lval = 0
    if high_l then
    lact = function() waction("LEN4HR", nil, l) end
    else
    lact = function() waction("LEN4LR",nil,l) end
    end
  end
  return dval, lval, parse_reg(b), dact, lact
end

local function parse_imm32(imm)
  local imm_val = tonumber(imm)
  if imm_val then
    if not is_int32(imm_val) then
      werror("immediate value out of range: ", imm_val)
    end
    wputhw(band(shr(imm_val, 16), 0xffff))
    wputhw(band(imm_val, 0xffff))
  elseif match(imm, "^[rfv]([1-3]?[0-9])$") or
	 match(imm, "^([%w_]+):(r1?[0-9])$") then
    werror("expected immediate operand, got register")
  else
    waction("IMM32", nil, imm) -- if we get label
  end
end

local function parse_imm16(imm)
  local imm_val = tonumber(imm)
  if imm_val then
    if not is_int16(imm_val) and not is_uint16(imm_val) then
      werror("immediate value out of range: ", imm_val)
    end
    wputhw(band(imm_val, 0xffff))
  elseif match(imm, "^[rfv]([1-3]?[0-9])$") or
	 match(imm, "^([%w_]+):(r1?[0-9])$") then
    werror("expected immediate operand, got register")
  else
    waction("IMM16", nil, imm)
  end
end

local function parse_imm8(imm)
  local imm_val = tonumber(imm)
  if imm_val then
    if not is_int8(imm_val) then
      werror("Immediate value out of range: ", imm_val)
    end
  else
    iact = function() waction("IMM8",nil,imm) end
  end
  return imm_val, iact
end

local function parse_mask(mask)
  local m3 = parse_number(mask)
  if m3 then
    if ((m3 == 1) or (m3 == 0) or ( m3 >=3 and m3 <=7)) then
      return m3
    else
      werror("Mask value should be 0,1 or 3-7: ", m3)
    end
  end
end

local function parse_mask2(mask)
  local m4 = parse_number(mask)
  if ( m4 >=0 and m4 <=1) then
    return m4
  else
    werror("Mask value should be 0 or 1: ", m4)
  end
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

------------------------------------------------------------------------------

local map_op, op_template

local function op_alias(opname, f)
  return function(params, nparams)
    if not params then return "-> "..opname:sub(1, -3) end
    f(params, nparams)
    op_template(params, map_op[opname], nparams)
  end
end

-- Template strings for s390x instructions.
map_op = {
  a_2 =		"00005a000000RX-a",
  ar_2 =	"000000001a00RR",
  ay_2 =	"e3000000005aRXY-a",
  ag_2 =	"e30000000008RXY-a",
  agr_2 =	"0000b9080000RRE",
  agf_2 =	"e30000000018RXY-a",
  agfr_2 =	"0000b9180000RRE",
  axbr_2 =	"0000b34a0000RRE",
  adbr_2 =	"0000b31a0000RRE",
  aebr_2 =	"0000b30a0000RRE",
  aghi_2 =	"0000a70b0000RI-a",
  ah_2 =	"00004a000000RX-a",
  ahi_2 =	"0000a70a0000RI-a",
  ahy_2 =	"e3000000007aRXY-a",
  afi_2 =	"c20900000000RIL-a",
  agfi_2 =	"c20800000000RIL-a",
  aih_2 =	"cc0800000000RIL-a",
  al_2 =	"00005e000000RX-a",
  alr_2 =	"000000001e00RR",
  aly_2 =	"e3000000005eRXY-a",
  alg_2 =	"e3000000000aRXY-a",
  algr_2 =	"0000b90a0000RRE",
  algf_2 =	"e3000000001aRXY-a",
  algfr_2 =	"0000b91a0000RRE",
  alfi_2 =	"c20b00000000RIL-a",
  algfi_2 =	"c20a00000000RIL-a",
  alc_2 =	"e30000000098RXY-a",
  alcr_2 =	"0000b9980000RRE",
  alcg_2 =	"e30000000088RXY-a",
  alcgr_2 =	"0000b9880000RRE",
  alsih_2 =	"cc0a00000000RIL-a",
  alsihn_2 =	"cc0b00000000RIL-a",
  axr_2 =	"000000003600RR",
  ad_2 =	"00006a000000RX-a",
  adr_2 =	"000000002a00RR",
  ae_2 =	"00007a000000RX-a",
  aer_2 =	"000000003a00RR",
  aw_2 =	"00006e000000RX-a",
  awr_2 =	"000000002e00RR",
  au_2 =	"00007e000000RX-a",
  aur_2 =	"000000003e00RR",
  n_2 =		"000054000000RX-a",
  nr_2 =	"000000001400RR",
  ny_2 =	"e30000000054RXY-a",
  ng_2 =	"e30000000080RXY-a",
  ngr_2 =	"0000b9800000RRE",
  nihf_2 =	"c00a00000000RIL-a",
  nihh_2 =	"0000a5040000RI-a",
  nihl_2 =	"0000a5050000RI-a",
  nilf_2 =	"c00b00000000RIL-a",
  nilh_2 =	"0000a5060000RI-a",
  nill_2 =	"0000a5070000RI-a",
  bal_2 =	"000045000000RX-a",
  balr_2 =	"000000000500RR",
  bas_2 =	"00004d000000RX-a",
  basr_2 =	"000000000d00RR",
  bassm_2 =	"000000000c00RR",
  bsa_2 =	"0000b25a0000RRE",
  bsm_2 =	"000000000b00RR",
  bakr_2 =	"0000b2400000RRE",
  bsg_2 =	"0000b2580000RRE",
  bc_2 =	"000047000000RX-b",
  bcr_2 =	"000000000700RR",
  bct_2 =	"000046000000RX-a",
  bctr_2 =	"000000000600RR",
  bctg_2 =	"e30000000046RXY-a",
  bctgr_2 =	"0000b9460000RRE",
  bxh_3 =	"000086000000RS-a",
  bxhg_3 =	"eb0000000044RSY-a",
  bxle_3 =	"000087000000RS-a",
  bxleg_3 =	"eb0000000045RSY-a",
  brasl_2 =	"c00500000000RIL-b",
  brcl_2 =	"c00400000000RIL-c",
  brcth_2 =	"cc0600000000RIL-b",
  cksm_2 =	"0000b2410000RRE",
  km_2 =	"0000b92e0000RRE",
  kmf_2 =	"0000b92a0000RRE",
  kmc_2 =	"0000b92f0000RRE",
  kmo_2 =	"0000b92b0000RRE",
  c_2 =		"000059000000RX-a",
  cr_2 =	"000000001900RR",
  cy_2 =	"e30000000059RXY-a",
  cg_2 =	"e30000000020RXY-a",
  cgr_2 =	"0000b9200000RRE",
  cgf_2 =	"e30000000030RXY-a",
  cgfr_2 =	"0000b9300000RRE",
  cxbr_2 =	"0000b3490000RRE",
  cxtr_2 =	"0000b3ec0000RRE",
  cxr_2 =	"0000b3690000RRE",
  cdbr_2 =	"0000b3190000RRE",
  cdtr_2 =	"0000b3e40000RRE",
  cd_2 =	"000069000000RX-a",
  cdr_2 =	"000000002900RR",
  cebr_2 =	"0000b3090000RRE",
  ce_2 =	"000079000000RX-a",
  cer_2 =	"000000003900RR",
  kxbr_2 =	"0000b3480000RRE",
  kxtr_2 =	"0000b3e80000RRE",
  kdbr_2 =	"0000b3180000RRE",
  kdtr_2 =	"0000b3e00000RRE",
  kebr_2 =	"0000b3080000RRE",
  cs_3 =	"0000ba000000RS-a",
  csy_3 =	"eb0000000014RSY-a",
  csg_3 =	"eb0000000030RSY-a",
  csp_2 =	"0000b2500000RRE",
  cspg_2 =	"0000b98a0000RRE",
  cextr_2 =	"0000b3fc0000RRE",
  cedtr_2 =	"0000b3f40000RRE",
  cds_3 =	"0000bb000000RS-a",
  cdsy_3 =	"eb0000000031RSY-a",
  cdsg_3 =	"eb000000003eRSY-a",
  ch_2 =	"000049000000RX-a",
  chy_2 =	"e30000000079RXY-a",
  cgh_2 =	"e30000000034RXY-a",
  chrl_2 =	"c60500000000RIL-b",
  cghrl_2 =	"c60400000000RIL-b",
  chf_2 =	"e300000000cdRXY-a",
  chhr_2 =	"0000b9cd0000RRE",
  chlr_2 =	"0000b9dd0000RRE",
  cfi_2 =	"c20d00000000RIL-a",
  cgfi_2 =	"c20c00000000RIL-a",
  cih_2 =	"cc0d00000000RIL-a",
  cl_2 =	"000055000000RX-a",
  clr_2 =	"000000001500RR",
  cly_2 =	"e30000000055RXY-a",
  clg_2 =	"e30000000021RXY-a",
  clgr_2 =	"0000b9210000RRE",
  clgf_2 =	"e30000000031RXY-a",
  clgfr_2 =	"0000b9310000RRE",
  clmh_3 =	"eb0000000020RSY-b",
  clm_3 =	"0000bd000000RS-b",
  clmy_3 =	"eb0000000021RSY-b",
  clhf_2 =	"e300000000cfRXY-a",
  clhhr_2 =	"0000b9cf0000RRE",
  clhlr_2 =	"0000b9df0000RRE",
  clfi_2 =	"c20f00000000RIL-a",
  clgfi_2 =	"c20e00000000RIL-a",
  clih_2 =	"cc0f00000000RIL-a",
  clcl_2 =	"000000000f00RR",
  clcle_3 =	"0000a9000000RS-a",
  clclu_3 =	"eb000000008fRSY-a",
  clrl_2 =	"c60f00000000RIL-b",
  clhrl_2 =	"c60700000000RIL-b",
  clgrl_2 =	"c60a00000000RIL-b",
  clghrl_2 =	"c60600000000RIL-b",
  clgfrl_2 =	"c60e00000000RIL-b",
  clst_2 =	"0000b25d0000RRE",
  crl_2 =	"c60d00000000RIL-b",
  cgrl_2 =	"c60800000000RIL-b",
  cgfrl_2 =	"c60c00000000RIL-b",
  cuse_2 =	"0000b2570000RRE",
  cmpsc_2 =	"0000b2630000RRE",
  kimd_2 =	"0000b93e0000RRE",
  klmd_2 =	"0000b93f0000RRE",
  kmac_2 =	"0000b91e0000RRE",
  thdr_2 =	"0000b3590000RRE",
  thder_2 =	"0000b3580000RRE",
  cxfbr_2 =	"0000b3960000RRE",
  cxftr_2 =	"0000b9590000RRE",
  cxfr_2 =	"0000b3b60000RRE",
  cdfbr_2 =	"0000b3950000RRE",
  cdftr_2 =	"0000b9510000RRE",
  cdfr_2 =	"0000b3b50000RRE",
  cefbr_2 =	"0000b3940000RRE",
  cefr_2 =	"0000b3b40000RRE",
  cxgbr_2 =	"0000b3a60000RRE",
  cxgtr_2 =	"0000b3f90000RRE",
  cxgr_2 =	"0000b3c60000RRE",
  cdgbr_2 =	"0000b3a50000RRE",
  cdgtr_2 =	"0000b3f10000RRE",
  cdgr_2 =	"0000b3c50000RRE",
  cegbr_2 =	"0000b3a40000RRE",
  cegr_2 =	"0000b3c40000RRE",
  cxstr_2 =	"0000b3fb0000RRE",
  cdstr_2 =	"0000b3f30000RRE",
  cxutr_2 =	"0000b3fa0000RRE",
  cdutr_2 =	"0000b3f20000RRE",
  cvb_2 =	"00004f000000RX-a",
  cvby_2 =	"e30000000006RXY-a",
  cvbg_2 =	"e3000000000eRXY-a",
  cvd_2 =	"00004e000000RX-a",
  cvdy_2 =	"e30000000026RXY-a",
  cvdg_2 =	"e3000000002eRXY-a",
  cuxtr_2 =	"0000b3ea0000RRE",
  cudtr_2 =	"0000b3e20000RRE",
  cu42_2 =	"0000b9b30000RRE",
  cu41_2 =	"0000b9b20000RRE",
  cpya_2 =	"0000b24d0000RRE",
  d_2 =		"00005d000000RX-a",
  dr_2 =	"000000001d00RR",
  dxbr_2 =	"0000b34d0000RRE",
  dxr_2 =	"0000b22d0000RRE",
  ddbr_2 =	"0000b31d0000RRE",
  dd_2 =	"00006d000000RX-a",
  ddr_2 =	"000000002d00RR",
  debr_2 =	"0000b30d0000RRE",
  de_2 =	"00007d000000RX-a",
  der_2 =	"000000003d00RR",
  dl_2 =	"e30000000097RXY-a",
  dlr_2 =	"0000b9970000RRE",
  dlg_2 =	"e30000000087RXY-a",
  dlgr_2 =	"0000b9870000RRE",
  dsg_2 =	"e3000000000dRXY-a",
  dsgr_2 =	"0000b90d0000RRE",
  dsgf_2 =	"e3000000001dRXY-a",
  dsgfr_2 =	"0000b91d0000RRE",
  x_2 =		"000057000000RX-a",
  xr_2 =	"000000001700RR",
  xy_2 =	"e30000000057RXY-a",
  xg_2 =	"e30000000082RXY-a",
  xgr_2 =	"0000b9820000RRE",
  xihf_2 =	"c00600000000RIL-a",
  xilf_2 =	"c00700000000RIL-a",
  ex_2 =	"000044000000RX-a",
  exrl_2 =	"c60000000000RIL-b",
  ear_2 =	"0000b24f0000RRE",
  esea_2 =	"0000b99d0000RRE",
  eextr_2 =	"0000b3ed0000RRE",
  eedtr_2 =	"0000b3e50000RRE",
  ecag_3 =	"eb000000004cRSY-a",
  efpc_2 =	"0000b38c0000RRE",
  epar_2 =	"0000b2260000RRE",
  epair_2 =	"0000b99a0000RRE",
  epsw_2 =	"0000b98d0000RRE",
  esar_2 =	"0000b2270000RRE",
  esair_2 =	"0000b99b0000RRE",
  esxtr_2 =	"0000b3ef0000RRE",
  esdtr_2 =	"0000b3e70000RRE",
  ereg_2 =	"0000b2490000RRE",
  eregg_2 =	"0000b90e0000RRE",
  esta_2 =	"0000b24a0000RRE",
  flogr_2 =	"0000b9830000RRE",
  hdr_2 =	"000000002400RR",
  her_2 =	"000000003400RR",
  iac_2 =	"0000b2240000RRE",
  ic_2 =	"000043000000RX-a",
  icy_2 =	"e30000000073RXY-a",
  icmh_3 =	"eb0000000080RSY-b",
  icm_3 =	"0000bf000000RS-b",
  icmy_3 =	"eb0000000081RSY-b",
  iihf_2 =	"c00800000000RIL-a",
  iihh_2 =	"0000a5000000RI-a",
  iihl_2 =	"0000a5010000RI-a",
  iilf_2 =	"c00900000000RIL-a",
  iilh_2 =	"0000a5020000RI-a",
  iill_2 =	"0000a5030000RI-a",
  ipm_2 =	"0000b2220000RRE",
  iske_2 =	"0000b2290000RRE",
  ivsk_2 =	"0000b2230000RRE",
  l_2 =		"000058000000RX-a",
  lr_2 =	"000000001800RR",
  ly_2 =	"e30000000058RXY-a",
  lg_2 =	"e30000000004RXY-a",
  lgr_2 =	"0000b9040000RRE",
  lgf_2 =	"e30000000014RXY-a",
  lgfr_2 =	"0000b9140000RRE",
  lghi_2 =	"0000a7090000RI-a",
  lxr_2 =	"0000b3650000RRE",
  ld_2 =	"000068000000RX-a",
  ldr_2 =	"000000002800RR",
  ldy_2 =	"ed0000000065RXY-a",
  le_2 =	"000078000000RX-a",
  ler_2 =	"000000003800RR",
  ley_2 =	"ed0000000064RXY-a",
  lam_3 =	"00009a000000RS-a",
  lamy_3 =	"eb000000009aRSY-a",
  la_2 =	"000041000000RX-a",
  lay_2 =	"e30000000071RXY-a",
  lae_2 =	"000051000000RX-a",
  laey_2 =	"e30000000075RXY-a",
  larl_2 =	"c00000000000RIL-b",
  laa_3 =	"eb00000000f8RSY-a",
  laag_3 =	"eb00000000e8RSY-a",
  laal_3 =	"eb00000000faRSY-a",
  laalg_3 =	"eb00000000eaRSY-a",
  lan_3 =	"eb00000000f4RSY-a",
  lang_3 =	"eb00000000e4RSY-a",
  lax_3 =	"eb00000000f7RSY-a",
  laxg_3 =	"eb00000000e7RSY-a",
  lao_3 =	"eb00000000f6RSY-a",
  laog_3 =	"eb00000000e6RSY-a",
  lt_2 =	"e30000000012RXY-a",
  ltr_2 =	"000000001200RR",
  ltg_2 =	"e30000000002RXY-a",
  ltgr_2 =	"0000b9020000RRE",
  ltgf_2 =	"e30000000032RXY-a",
  ltgfr_2 =	"0000b9120000RRE",
  ltxbr_2 =	"0000b3420000RRE",
  ltxtr_2 =	"0000b3de0000RRE",
  ltxr_2 =	"0000b3620000RRE",
  ltdbr_2 =	"0000b3120000RRE",
  ltdtr_2 =	"0000b3d60000RRE",
  ltdr_2 =	"000000002200RR",
  ltebr_2 =	"0000b3020000RRE",
  lter_2 =	"000000003200RR",
  lb_2 =	"e30000000076RXY-a",
  lbr_2 =	"0000b9260000RRE",
  lgb_2 =	"e30000000077RXY-a",
  lgbr_2 =	"0000b9060000RRE",
  lbh_2 =	"e300000000c0RXY-a",
  lcr_2 =	"000000001300RR",
  lcgr_2 =	"0000b9030000RRE",
  lcgfr_2 =	"0000b9130000RRE",
  lcxbr_2 =	"0000b3430000RRE",
  lcxr_2 =	"0000b3630000RRE",
  lcdbr_2 =	"0000b3130000RRE",
  lcdr_2 =	"000000002300RR",
  lcdfr_2 =	"0000b3730000RRE",
  lcebr_2 =	"0000b3030000RRE",
  lcer_2 =	"000000003300RR",
  lctl_3 =	"0000b7000000RS-a",
  lctlg_3 =	"eb000000002fRSY-a",
  fixr_2 =	"0000b3670000RRE",
  fidr_2 =	"0000b37f0000RRE",
  fier_2 =	"0000b3770000RRE",
  ldgr_2 =	"0000b3c10000RRE",
  lgdr_2 =	"0000b3cd0000RRE",
  lh_2 =	"000048000000RX-a",
  lhr_2 =	"0000b9270000RRE",
  lhy_2 =	"e30000000078RXY-a",
  lgh_2 =	"e30000000015RXY-a",
  lghr_2 =	"0000b9070000RRE",
  lhh_2 =	"e300000000c4RXY-a",
  lhi_2 =	"0000a7080000RI-a",
  lhrl_2 =	"c40500000000RIL-b",
  lghrl_2 =	"c40400000000RIL-b",
  lfh_2 =	"e300000000caRXY-a",
  lgfi_2 =	"c00100000000RIL-a",
  lxdbr_2 =	"0000b3050000RRE",
  lxdr_2 =	"0000b3250000RRE",
  lxebr_2 =	"0000b3060000RRE",
  lxer_2 =	"0000b3260000RRE",
  ldebr_2 =	"0000b3040000RRE",
  lder_2 =	"0000b3240000RRE",
  llgf_2 =	"e30000000016RXY-a",
  llgfr_2 =	"0000b9160000RRE",
  llc_2 =	"e30000000094RXY-a",
  llcr_2 =	"0000b9940000RRE",
  llgc_2 =	"e30000000090RXY-a",
  llgcr_2 =	"0000b9840000RRE",
  llch_2 =	"e300000000c2RXY-a",
  llh_2 =	"e30000000095RXY-a",
  llhr_2 =	"0000b9950000RRE",
  llgh_2 =	"e30000000091RXY-a",
  llghr_2 =	"0000b9850000RRE",
  llhh_2 =	"e300000000c6RXY-a",
  llhrl_2 =	"c40200000000RIL-b",
  llghrl_2 =	"c40600000000RIL-b",
  llihf_2 =	"c00e00000000RIL-a",
  llihh_2 =	"0000a50c0000RI-a",
  llihl_2 =	"0000a50d0000RI-a",
  llilf_2 =	"c00f00000000RIL-a",
  llilh_2 =	"0000a50e0000RI-a",
  llill_2 =	"0000a50f0000RI-a",
  llgfrl_2 =	"c40e00000000RIL-b",
  llgt_2 =	"e30000000017RXY-a",
  llgtr_2 =	"0000b9170000RRE",
  lm_3 =	"000098000000RS-a",
  lmy_3 =	"eb0000000098RSY-a",
  lmg_3 =	"eb0000000004RSY-a",
  lmh_3 =	"eb0000000096RSY-a",
  lnr_2 =	"000000001100RR",
  lngr_2 =	"0000b9010000RRE",
  lngfr_2 =	"0000b9110000RRE",
  lnxbr_2 =	"0000b3410000RRE",
  lnxr_2 =	"0000b3610000RRE",
  lndbr_2 =	"0000b3110000RRE",
  lndr_2 =	"000000002100RR",
  lndfr_2 =	"0000b3710000RRE",
  lnebr_2 =	"0000b3010000RRE",
  lner_2 =	"000000003100RR",
  loc_3 =	"eb00000000f2RSY-b",
  locg_3 =	"eb00000000e2RSY-b",
  lpq_2 =	"e3000000008fRXY-a",
  lpr_2 =	"000000001000RR",
  lpgr_2 =	"0000b9000000RRE",
  lpgfr_2 =	"0000b9100000RRE",
  lpxbr_2 =	"0000b3400000RRE",
  lpxr_2 =	"0000b3600000RRE",
  lpdbr_2 =	"0000b3100000RRE",
  lpdr_2 =	"000000002000RR",
  lpdfr_2 =	"0000b3700000RRE",
  lpebr_2 =	"0000b3000000RRE",
  lper_2 =	"000000003000RR",
  lra_2 =	"0000b1000000RX-a",
  lray_2 =	"e30000000013RXY-a",
  lrag_2 =	"e30000000003RXY-a",
  lrl_2 =	"c40d00000000RIL-b",
  lgrl_2 =	"c40800000000RIL-b",
  lgfrl_2 =	"c40c00000000RIL-b",
  lrvh_2 =	"e3000000001fRXY-a",
  lrv_2 =	"e3000000001eRXY-a",
  lrvr_2 =	"0000b91f0000RRE",
  lrvg_2 =	"e3000000000fRXY-a",
  lrvgr_2 =	"0000b90f0000RRE",
  ldxbr_2 =	"0000b3450000RRE",
  ldxr_2 =	"000000002500RR",
  lrdr_2 =	"000000002500RR",
  lexbr_2 =	"0000b3460000RRE",
  lexr_2 =	"0000b3660000RRE",
  ledbr_2 =	"0000b3440000RRE",
  ledr_2 =	"000000003500RR",
  lrer_2 =	"000000003500RR",
  lura_2 =	"0000b24b0000RRE",
  lurag_2 =	"0000b9050000RRE",
  lzxr_2 =	"0000b3760000RRE",
  lzdr_2 =	"0000b3750000RRE",
  lzer_2 =	"0000b3740000RRE",
  msta_2 =	"0000b2470000RRE",
  mvcl_2 =	"000000000e00RR",
  mvcle_3 =	"0000a8000000RS-a",
  mvclu_3 =	"eb000000008eRSY-a",
  mvpg_2 =	"0000b2540000RRE",
  mvst_2 =	"0000b2550000RRE",
  m_2 =		"00005c000000RX-a",
  mfy_2 =	"e3000000005cRXY-a",
  mr_2 =	"000000001c00RR",
  mxbr_2 =	"0000b34c0000RRE",
  mxr_2 =	"000000002600RR",
  mdbr_2 =	"0000b31c0000RRE",
  md_2 =	"00006c000000RX-a",
  mdr_2 =	"000000002c00RR",
  mxdbr_2 =	"0000b3070000RRE",
  mxd_2 =	"000067000000RX-a",
  mxdr_2 =	"000000002700RR",
  meebr_2 =	"0000b3170000RRE",
  meer_2 =	"0000b3370000RRE",
  mdebr_2 =	"0000b30c0000RRE",
  mde_2 =	"00007c000000RX-a",
  mder_2 =	"000000003c00RR",
  me_2 =	"00007c000000RX-a",
  mer_2 =	"000000003c00RR",
  mh_2 =	"00004c000000RX-a",
  mhy_2 =	"e3000000007cRXY-a",
  mlg_2 =	"e30000000086RXY-a",
  mlgr_2 =	"0000b9860000RRE",
  ml_2 =	"e30000000096RXY-a",
  mlr_2 =	"0000b9960000RRE",
  ms_2 =	"000071000000RX-a",
  msr_2 =	"0000b2520000RRE",
  msy_2 =	"e30000000051RXY-a",
  msg_2 =	"e3000000000cRXY-a",
  msgr_2 =	"0000b90c0000RRE",
  msgf_2 =	"e3000000001cRXY-a",
  msgfr_2 =	"0000b91c0000RRE",
  msfi_2 =	"c20100000000RIL-a",
  msgfi_2 =	"c20000000000RIL-a",
  maer_3 =	"0000b32e0000RRD",
  mvhhi_2 =	"e54400000000SIL",
  mvhi_2 =	"e54c00000000SIL",
  mvghi_2 =	"e54800000000SIL",
  o_2 =		"000056000000RX-a",
  or_2 =	"000000001600RR",
  oy_2 =	"e30000000056RXY-a",
  og_2 =	"e30000000081RXY-a",
  ogr_2 =	"0000b9810000RRE",
  oihf_2 =	"c00c00000000RIL-a",
  oihh_2 =	"0000a5080000RI-a",
  oihl_2 =	"0000a5090000RI-a",
  oilf_2 =	"c00d00000000RIL-a",
  oilh_2 =	"0000a50a0000RI-a",
  oill_2 =	"0000a50b0000RI-a",
  pgin_2 =	"0000b22e0000RRE",
  pgout_2 =	"0000b22f0000RRE",
  pcc_2 =	"0000b92c0000RRE",
  pckmo_2 =	"0000b9280000RRE",
  pfmf_2 =	"0000b9af0000RRE",
  ptf_2 =	"0000b9a20000RRE",
  popcnt_2 =	"0000b9e10000RRE",
  pfd_2 =	"e30000000036m",
  pfdrl_2 =	"c60200000000RIL-c",
  pt_2 =	"0000b2280000RRE",
  pti_2 =	"0000b99e0000RRE",
  palb_2 =	"0000b2480000RRE",
  rrbe_2 =	"0000b22a0000RRE",
  rrbm_2 =	"0000b9ae0000RRE",
  rll_3 =	"eb000000001dRSY-a",
  rllg_3 =	"eb000000001cRSY-a",
  srst_2 =	"0000b25e0000RRE",
  srstu_2 =	"0000b9be0000RRE",
  sar_2 =	"0000b24e0000RRE",
  sfpc_2 =	"0000b3840000RRE",
  sfasr_2 =	"0000b3850000RRE",
  spm_2 =	"000000000400RR",
  ssar_2 =	"0000b2250000RRE",
  ssair_2 =	"0000b99f0000RRE",
  slda_3 =	"00008f000000RS-a",
  sldl_3 =	"00008d000000RS-a",
  sla_3 =	"00008b000000RS-a",
  slak_3 =	"eb00000000ddRSY-a",
  slag_3 =	"eb000000000bRSY-a",
  sll_3 =	"000089000000RS-a",
  sllk_3 =	"eb00000000dfRSY-a",
  sllg_3 =	"eb000000000dRSY-a",
  srda_3 =	"00008e000000RS-a",
  srdl_3 =	"00008c000000RS-a",
  sra_3 =	"00008a000000RS-a",
  srak_3 =	"eb00000000dcRSY-a",
  srag_3 =	"eb000000000aRSY-a",
  srl_3 =	"000088000000RS-a",
  srlk_3 =	"eb00000000deRSY-a",
  srlg_3 =	"eb000000000cRSY-a",
  sqxbr_2 =	"0000b3160000RRE",
  sqxr_2 =	"0000b3360000RRE",
  sqdbr_2 =	"0000b3150000RRE",
  sqdr_2 =	"0000b2440000RRE",
  sqebr_2 =	"0000b3140000RRE",
  sqer_2 =	"0000b2450000RRE",
  st_2 =	"000050000000RX-a",
  sty_2 =	"e30000000050RXY-a",
  stg_2 =	"e30000000024RXY-a",
  std_2 =	"000060000000RX-a",
  stdy_2 =	"ed0000000067RXY-a",
  ste_2 =	"000070000000RX-a",
  stey_2 =	"ed0000000066RXY-a",
  stam_3 =	"00009b000000RS-a",
  stamy_3 =	"eb000000009bRSY-a",
  stc_2 =	"000042000000RX-a",
  stcy_2 =	"e30000000072RXY-a",
  stch_2 =	"e300000000c3RXY-a",
  stcmh_3 =	"eb000000002cRSY-b",
  stcm_3 =	"0000be000000RS-b",
  stcmy_3 =	"eb000000002dRSY-b",
  stctl_3 =	"0000b6000000RS-a",
  stctg_3 =	"eb0000000025RSY-a",
  sth_2 =	"000040000000RX-a",
  sthy_2 =	"e30000000070RXY-a",
  sthh_2 =	"e300000000c7RXY-a",
  sthrl_2 =	"c40700000000RIL-b",
  stfh_2 =	"e300000000cbRXY-a",
  stm_3 =	"000090000000RS-a",
  stmy_3 =	"eb0000000090RSY-a",
  stmg_3 =	"eb0000000024RSY-a",
  stmh_3 =	"eb0000000026RSY-a",
  stoc_3 =	"eb00000000f3RSY-b",
  stocg_3 =	"eb00000000e3RSY-b",
  stpq_2 =	"e3000000008eRXY-a",
  strl_2 =	"c40f00000000RIL-b",
  stgrl_2 =	"c40b00000000RIL-b",
  strvh_2 =	"e3000000003fRXY-a",
  strv_2 =	"e3000000003eRXY-a",
  strvg_2 =	"e3000000002fRXY-a",
  stura_2 =	"0000b2460000RRE",
  sturg_2 =	"0000b9250000RRE",
  s_2 =		"00005b000000RX-a",
  sr_2 =	"000000001b00RR",
  sy_2 =	"e3000000005bRXY-a",
  sg_2 =	"e30000000009RXY-a",
  sgr_2 =	"0000b9090000RRE",
  sgf_2 =	"e30000000019RXY-a",
  sgfr_2 =	"0000b9190000RRE",
  sxbr_2 =	"0000b34b0000RRE",
  sdbr_2 =	"0000b31b0000RRE",
  sebr_2 =	"0000b30b0000RRE",
  sh_2 =	"00004b000000RX-a",
  shy_2 =	"e3000000007bRXY-a",
  sl_2 =	"00005f000000RX-a",
  slr_2 =	"000000001f00RR",
  sly_2 =	"e3000000005fRXY-a",
  slg_2 =	"e3000000000bRXY-a",
  slgr_2 =	"0000b90b0000RRE",
  slgf_2 =	"e3000000001bRXY-a",
  slgfr_2 =	"0000b91b0000RRE",
  slfi_2 =	"c20500000000RIL-a",
  slgfi_2 =	"c20400000000RIL-a",
  slb_2 =	"e30000000099RXY-a",
  slbr_2 =	"0000b9990000RRE",
  slbg_2 =	"e30000000089RXY-a",
  slbgr_2 =	"0000b9890000RRE",
  sxr_2 =	"000000003700RR",
  sd_2 =	"00006b000000RX-a",
  sdr_2 =	"000000002b00RR",
  se_2 =	"00007b000000RX-a",
  ser_2 =	"000000003b00RR",
  su_2 =	"00007f000000RX-a",
  sur_2 =	"000000003f00RR",
  sw_2 =	"00006f000000RX-a",
  swr_2 =	"000000002f00RR",
  tar_2 =	"0000b24c0000RRE",
  tb_2 =	"0000b22c0000RRE",
  tmhh_2 =	"0000a7020000RI-a",
  tmhl_2 =	"0000a7030000RI-a",
  tmlh_2 =	"0000a7000000RI-a",
  tmll_2 =	"0000a7010000RI-a",
  trace_3 =	"000099000000RS-a",
  tracg_3 =	"eb000000000fRSY-a",
  tre_2 =	"0000b2a50000RRE",


  -- SS-a instructions
  clc_2 =	"d50000000000SS-a",
  ed_2 =	"de0000000000SS-a",
  edmk_2 =	"df0000000000SS-a",
  mvc_2 =	"d20000000000SS-a",
  mvcin_2 =	"e80000000000SS-a",
  mvn_2 =	"d10000000000SS-a",
  mvz_2 =	"d30000000000SS-a",
  nc_2 =	"d40000000000SS-a",
  oc_2 =	"d60000000000SS-a",
  tr_2 =	"dc0000000000SS-a",
  trt_2 =	"dd0000000000SS-a",
  trtr_2 =	"d00000000000SS-a",
  unpka_2 =	"ea0000000000SS-a",
  unpku_2 =	"e20000000000SS-a",
  xc_2 =	"d70000000000SS-a",
  ap_2 =	"fa0000000000SS-b",
  -- RRF-e instructions
  cfebr_3 =	"0000b3980000RRF-e",
  cfebra_4 =	"0000b3980000RRF-e",
  -- RXE instructions
  adb_2 =	"ed000000001aRXE",
  aeb_2 =	"ed000000000aRXE",
  cdb_2 =	"ed0000000019RXE",
  ceb_2 =	"ed0000000009RXE",
  ddb_2 =	"ed000000001dRXE",
  deb_2 =	"ed000000000dRXE",
  mdb_2 =	"ed000000001cRXE",
  mdeb_2 =	"ed000000000cRXE",
  meeb_2 =	"ed0000000017RXE",
  mxdb_2 =	"ed0000000007RXE",
  sqdb_2 =	"ed0000000015RXE",
  sqeb_2 =	"ed0000000014RXE",
  sdb_2 =	"ed000000001bRXE",
  seb_2 =	"ed000000000bRXE",
  -- RRF-b instructions
  didbr_4 =	"0000b3580000RRF-b",
  -- S mode instructions
  stfl_1 =	"0000b2b10000sS",
  -- I- mode instructions
  svc_1 =	"000000000a00iI",
  -- RI-a mode instructions
  -- TODO: change "i" to "RI-a"
  chi_2 =	"0000a70e0000i",
  cghi_2 =	"0000a70f0000i",
  mhi_2 =	"0000a70c0000i",
  mghi_2 =	"0000a70d0000i",
  -- RI-b mode instructions
  bras_2 =	"0000a7050000RI-b",
  -- RI-c mode instructions
  brc_2 =	"0000a7040000RI-c",
  -- RIL-c
  brcl_2 =	"c00400000000RIL-c",
  -- RX-b mode instructions
  bc_2 =	"000047000000RX-b",
  -- RSI
  brxh_3 =	"000084000000RSI",
  -- RIE-e
  brxhg_3 =	"ec0000000044RIE-e",
  -- SI
  ni_2 =	"000094000000SI",
  -- RXF
  madb_3 =	"ed000000001eRXF",
  -- RRD
  maebr_3 =	"0000b30e0000RRD",
  -- RS-b
  clm_3 =	"0000bd000000RS-b"
}
for cond,c in pairs(map_cond) do
  -- Extended mnemonics for branches.
  -- TODO: replace 'B' with correct encoding.
  -- brc
  map_op["j"..cond.."_1"] = "0000"..tohex(0xa7040000+shl(c, 20)).."w"
  -- brcl
  map_op["jg"..cond.."_1"] = tohex(0xc0040000+shl(c, 20)).."0000".."x"
  -- bc
  map_op["b"..cond.."_1"] = "0000"..tohex(0x47000000+shl(c, 20)).."y"
  -- bcr
  map_op["b"..cond.."r_1"] = "0000"..tohex(0x0700+shl(c, 4)).."z"
end
------------------------------------------------------------------------------
-- Handle opcodes defined with template strings.
local function parse_template(params, template, nparams, pos)
  -- Read the template in 16-bit chunks.
  -- Leading halfword zeroes should not be written out.
  local op0 = tonumber(sub(template, 1, 4), 16)
  local op1 = tonumber(sub(template, 5, 8), 16)
  local op2 = tonumber(sub(template, 9, 12), 16)

  -- Process each character.
  local p = sub(template, 13)
  if p == "RR" then
    op2 = op2 + shl(parse_reg(params[1]),4) + parse_reg(params[2])
    wputhw(op2)
  elseif p == "RRE" then
    op2 = op2 + shl(parse_reg(params[1]),4) + parse_reg(params[2])
    wputhw(op1); wputhw(op2)
  elseif p == "RI-a" then
    op1 = op1 + shl(parse_reg(params[1]),4)
    wputhw(op1);
    parse_imm16(params[2])
  elseif p == "RX-a" then
    local d, x, b, a = parse_mem_bx(params[2])
    op1 = op1 + shl(parse_reg(params[1]), 4) + x
    op2 = op2 + shl(b, 12) + d
    wputhw(op1); wputhw(op2);
    if a then a() end
  elseif p == "RX-b" then
  elseif p == "RXY-a" then
    local d, x, b, a = parse_mem_bxy(params[2])
    op0 = op0 + shl(parse_reg(params[1]), 4) + x
    op1 = op1 + shl(b, 12) + band(d, 0xfff)
    op2 = op2 + band(shr(d, 4), 0xff00)
    wputhw(op0); wputhw(op1); wputhw(op2)
    if a then a() end
  elseif p == "m" then

  elseif p == "RIL-a" then
    op0 = op0 + shl(parse_reg(params[1]), 4)
    wputhw(op0);
    parse_imm32(params[2])
  elseif p == "RIL-b" then
    op0 = op0 + shl(parse_reg(params[1]), 4)
    wputhw(op0);
    local mode, n, s = parse_label(params[2])
    waction("REL_"..mode, n, s)
  elseif p == "RS-a" then
    local d, b, a = parse_mem_b(params[3])
    op1 = op1 + shl(parse_reg(params[1]), 4) + parse_reg(params[2])
    op2 = op2 + shl(b, 12) + d
    wputhw(op1); wputhw(op2)
    if a then a() end -- a() emits action.
  elseif p == "RSY-a" then
    local d, b, a = parse_mem_by(params[3])
    op0 = op0 + shl(parse_reg(params[1]), 4) + parse_reg(params[2])
    op1 = op1 + shl(b, 12) + band(d, 0xfff)
    op2 = op2 + band(shr(d, 4), 0xff00)
    wputhw(op0); wputhw(op1); wputhw(op2)
    if a then a() end -- a() emits action.
  elseif p == "SS-a" then
    local d1, l1, b1, d1a, l1a = parse_mem_lb(params[1])
    local d2, b2, d2a = parse_mem_b(params[2])
    op0 = op0 + l1
    op1 = op1 + shl(b1, 12) + d1
    op2 = op2 + shl(b2, 12) + d2
    wputhw(op0)
    if l1a then l1a() end
    wputhw(op1)
    if d1a then d1a() end
    wputhw(op2)
    if d2a then d2a() end
  elseif p == "SS-b" then
    local high_l=true;
    local d1, l1, b1, d1a, l1a = parse_mem_l2b(params[1],high_l)
    high_l=false;
    local d2, l2, b2, d2a, l2a = parse_mem_l2b(params[2],high_l)
    op0 = op0 + shl(l1,4) + l2
    op1 = op1 + shl(b1, 12) + d1
    op2 = op2 + shl(b2, 12) + d2
    wputhw(op0)
    if l1a then l1a() end
    if l2a then l2a() end
    wputhw(op1)
    if d1a then d1a() end
    wputhw(op2)
    if d2a then d2a() end
  elseif p == "SIL" then
    wputhw(op0)
    local d, b, a = parse_mem_b(params[1])
    op1 = op1 + shl(b, 12) + d
    wputhw(op1)
    if a then a() end
    parse_imm16(params[2])
  elseif p == "RRF-e" then
    wputhw(op1)
    op2 = op2 + shl(parse_reg(params[1]),4) + shl(parse_mask(params[2]),12) + parse_reg(params[3])
    if params[4] then
      op2 = op2 + shl(parse_mask2(params[4]),8)
    end
    wputhw(op2)
  elseif p == "RXE" then
    local d, x, b, a = parse_mem_bx(params[2])
    op0 = op0 + shl(parse_reg(params[1]), 4) + x
    op1 = op1 + shl(b, 12) + d
    -- m3 is not present, so assumed its not part of the instruction since its not passed as a prameter
    wputhw(op0);
    wputhw(op1);
    if a then a() end
    wputhw(op2);
  elseif p == "RRF-b" then
    wputhw(op1);
    op2 = op2 + shl(parse_reg(params[1]),4) + shl(parse_reg(params[2]),12) + parse_reg(params[3]) + parse_mask(params[4])
    wputhw(op2)
  elseif p =="S" then
    wputhw(op1);
    local d, b, a = parse_mem_b(params[1])
    op2 = op2 + shl(b,12) + d;
    wputhw(op2)
    if a then a() end
  elseif p =="I" then
    local imm_val, a = parse_imm8(params[1])
    op2 = op2 + imm_val;
    wputhw(op2);
    if a then a() end
  elseif p == "RI-b" then
    op1 = op1 + shl(parse_reg(params[1]),4)
    wputhw(op1)
    local mode, n, s = parse_label(params[2])
    waction("REL_"..mode, n, s)
  elseif p == "RI-c" then
    op1 = op1 + shl(parse_num(params[1]),4)
    wputhw(op1)
    local mode, n, s = parse_label(params[2])
    waction("REL_"..mode, n, s)
  elseif p == "RIL-c" then
    op0 = op0 + shl(parse_num(params[1]),4)
    wputhhw(op0)
    local mode, n, s = parse_label(params[2])
    waction("REL_"..mode, n, s)
  elseif p == "RX-b" then
    local d, x, b, a = parse_mem_bx(params[2])
    op1 = op1 + shl(parse_num(params[1]), 4) + x
    op2 = op2 + shl(b, 12) + d
    wputhw(op1);wputhw(op2);
    if a then a() end
  elseif p == "RSI" then
    op1 = op1 + shl(parse_reg(params[1]),4) + parse_reg(params[2])
    wputhw(op1)
    local mode, n, s = parse_label(params[3])
    waction("REL_"..mode, n, s)
  elseif p == "RIE-e" then
    op0 = op0 + shl(parse_reg(params[1]),4) + parse_reg(params[2])
    wputhw1(op0)
    local mode, n, s = parse_label(params[3])
    waction("REL_"..mode, n, s)
    wputhw(op2)
  elseif p == "SI" then
    local imm_val, a = parse_imm8(params[2])
    op1 = op1 + imm_val
    wputhw(op1)
    if a then a() end
    local d, b, a = parse_mem_b(params[1])
    op2 = op2 + shl(b,12) + d
    wputhw(op2)
    if a then a() end
  elseif p == "RXF" then
    local d, x, b, a = parse_mem_bx(params[3])
    op0 = op0 + shl(parse_reg(params[2]),4) + x
    op1 = op1 + shl(b, 12) + d
    wputhw(op0); wputhw(op1);
    if a then a() end
    op2 = op2 + shl(parse_reg(params[1]),12)
    wputhw(op2)
  elseif p == "RRD" then
    wputhw(op1)
    op2 = op2 + shl(parse_reg(params[1]),12) + shl(parse_reg(params[2]),4) + parse_reg(params[3])
    wputhw(op2)
  elseif p == "w" then
    local mode, n, s = parse_label(params[1])
    wputhw(op1)
    waction("REL_"..mode, n, s)
  elseif p == "x" then
    local mode, n, s = parse_label(params[1])
    wputhw(op0)
    waction("REL_"..mode, n, s)
  elseif p == "y" then
    local d, x, b, a = parse_mem_bx(params[1])
    op1 = op1 + x
    op2 = op2 + shl(b, 12) + d
    wputhw(op1); wputhw(op2);
    if a then a() end -- a() emits action.
  elseif p == "z" then
    op2 = op2 + parse_reg(params[1])
    wputhw(op2)
  elseif p == "RS-b" then
    local m = parse_mask(params[2])
    local d, b, a = parse_mem_b(params[3])
    op1 = op1 + shl(parse_reg(params[1]), 4) + m
    op2 = op2 + shl(b, 12) + d
    wputhw(op1); wputhw(op2)
    if a then a() end
  else
    werror("unrecognized encoding")
  end
end

function op_template(params, template, nparams)
  if not params then return template:gsub("%x%x%x%x%x%x%x%x%x%x%x%x", "") end
  -- Limit number of section buffer positions used by a single dasm_put().
  -- A single opcode needs a maximum of 5 positions.
  if secpos+5 > maxsecpos then wflush() end
  local lpos, apos, spos = #actlist, #actargs, secpos
  local ok, err
  for t in gmatch(template, "[^|]+") do
    ok, err = pcall(parse_template, params, t, nparams)
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
	waction("ALIGN", align-1, nil, 1) -- Action halfword is 2**n-1.
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
