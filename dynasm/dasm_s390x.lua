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
  "REL_PC", "LABEL_PC", "DISP12", "DISP20", "IMM16", "IMM32", "LEN8R","LEN4HR","LEN4LR",
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
         a == "LABEL_LG"
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
  a_2 =		"00005a000000j",
  ar_2 =	"000000001a00g",
  ay_2 =	"e3000000005al",
  ag_2 =	"e30000000008l",
  agr_2 =	"0000b9080000h",
  agf_2 =	"e30000000018l",
  agfr_2 =	"0000b9180000h",
  axbr_2 =	"0000b34a0000h",
  adbr_2 =	"0000b31a0000h",
  aebr_2 =	"0000b30a0000h",
  aghi_2 =	"0000a70b0000i",
  ah_2 =	"00004a000000j",
  ahi_2 =	"0000a70a0000i",
  ahy_2 =	"e3000000007al",
  afi_2 =	"c20900000000n",
  agfi_2 =	"c20800000000n",
  aih_2 =	"cc0800000000n",
  al_2 =	"00005e000000j",
  alr_2 =	"000000001e00g",
  aly_2 =	"e3000000005el",
  alg_2 =	"e3000000000al",
  algr_2 =	"0000b90a0000h",
  algf_2 =	"e3000000001al",
  algfr_2 =	"0000b91a0000h",
  alfi_2 =	"c20b00000000n",
  algfi_2 =	"c20a00000000n",
  alc_2 =	"e30000000098l",
  alcr_2 =	"0000b9980000h",
  alcg_2 =	"e30000000088l",
  alcgr_2 =	"0000b9880000h",
  alsih_2 =	"cc0a00000000n",
  alsihn_2 =	"cc0b00000000n",
  axr_2 =	"000000003600g",
  ad_2 =	"00006a000000j",
  adr_2 =	"000000002a00g",
  ae_2 =	"00007a000000j",
  aer_2 =	"000000003a00g",
  aw_2 =	"00006e000000j",
  awr_2 =	"000000002e00g",
  au_2 =	"00007e000000j",
  aur_2 =	"000000003e00g",
  n_2 =		"000054000000j",
  nr_2 =	"000000001400g",
  ny_2 =	"e30000000054l",
  ng_2 =	"e30000000080l",
  ngr_2 =	"0000b9800000h",
  nihf_2 =	"c00a00000000n",
  nihh_2 =	"0000a5040000i",
  nihl_2 =	"0000a5050000i",
  nilf_2 =	"c00b00000000n",
  nilh_2 =	"0000a5060000i",
  nill_2 =	"0000a5070000i",
  bal_2 =	"000045000000j",
  balr_2 =	"000000000500g",
  bas_2 =	"00004d000000j",
  basr_2 =	"000000000d00g",
  bassm_2 =	"000000000c00g",
  bsa_2 =	"0000b25a0000h",
  bsm_2 =	"000000000b00g",
  bakr_2 =	"0000b2400000h",
  bsg_2 =	"0000b2580000h",
  bc_2 =	"000047000000k",
  bcr_2 =	"000000000700g",
  bct_2 =	"000046000000j",
  bctr_2 =	"000000000600g",
  bctg_2 =	"e30000000046l",
  bctgr_2 =	"0000b9460000h",
  bxh_3 =	"000086000000q",
  bxhg_3 =	"eb0000000044s",
  bxle_3 =	"000087000000q",
  bxleg_3 =	"eb0000000045s",
  brasl_2 =	"c00500000000o",
  brcl_2 =	"c00400000000p",
  brcth_2 =	"cc0600000000o",
  cksm_2 =	"0000b2410000h",
  km_2 =	"0000b92e0000h",
  kmf_2 =	"0000b92a0000h",
  kmc_2 =	"0000b92f0000h",
  kmo_2 =	"0000b92b0000h",
  c_2 =		"000059000000j",
  cr_2 =	"000000001900g",
  cy_2 =	"e30000000059l",
  cg_2 =	"e30000000020l",
  cgr_2 =	"0000b9200000h",
  cgf_2 =	"e30000000030l",
  cgfr_2 =	"0000b9300000h",
  cxbr_2 =	"0000b3490000h",
  cxtr_2 =	"0000b3ec0000h",
  cxr_2 =	"0000b3690000h",
  cdbr_2 =	"0000b3190000h",
  cdtr_2 =	"0000b3e40000h",
  cd_2 =	"000069000000j",
  cdr_2 =	"000000002900g",
  cebr_2 =	"0000b3090000h",
  ce_2 =	"000079000000j",
  cer_2 =	"000000003900g",
  kxbr_2 =	"0000b3480000h",
  kxtr_2 =	"0000b3e80000h",
  kdbr_2 =	"0000b3180000h",
  kdtr_2 =	"0000b3e00000h",
  kebr_2 =	"0000b3080000h",
  cs_3 =	"0000ba000000q",
  csy_3 =	"eb0000000014s",
  csg_3 =	"eb0000000030s",
  csp_2 =	"0000b2500000h",
  cspg_2 =	"0000b98a0000h",
  cextr_2 =	"0000b3fc0000h",
  cedtr_2 =	"0000b3f40000h",
  cds_3 =	"0000bb000000q",
  cdsy_3 =	"eb0000000031s",
  cdsg_3 =	"eb000000003es",
  ch_2 =	"000049000000j",
  chy_2 =	"e30000000079l",
  cgh_2 =	"e30000000034l",
  chrl_2 =	"c60500000000o",
  cghrl_2 =	"c60400000000o",
  chf_2 =	"e300000000cdl",
  chhr_2 =	"0000b9cd0000h",
  chlr_2 =	"0000b9dd0000h",
  cfi_2 =	"c20d00000000n",
  cgfi_2 =	"c20c00000000n",
  cghi_2 =	"0000a70f0000i",
  cih_2 =	"cc0d00000000n",
  cl_2 =	"000055000000j",
  clr_2 =	"000000001500g",
  cly_2 =	"e30000000055l",
  clg_2 =	"e30000000021l",
  clgr_2 =	"0000b9210000h",
  clgf_2 =	"e30000000031l",
  clgfr_2 =	"0000b9310000h",
  clmh_3 =	"eb0000000020t",
  clm_3 =	"0000bd000000r",
  clmy_3 =	"eb0000000021t",
  clhf_2 =	"e300000000cfl",
  clhhr_2 =	"0000b9cf0000h",
  clhlr_2 =	"0000b9df0000h",
  clfi_2 =	"c20f00000000n",
  clgfi_2 =	"c20e00000000n",
  clih_2 =	"cc0f00000000n",
  clcl_2 =	"000000000f00g",
  clcle_3 =	"0000a9000000q",
  clclu_3 =	"eb000000008fs",
  clrl_2 =	"c60f00000000o",
  clhrl_2 =	"c60700000000o",
  clgrl_2 =	"c60a00000000o",
  clghrl_2 =	"c60600000000o",
  clgfrl_2 =	"c60e00000000o",
  clst_2 =	"0000b25d0000h",
  crl_2 =	"c60d00000000o",
  cgrl_2 =	"c60800000000o",
  cgfrl_2 =	"c60c00000000o",
  cuse_2 =	"0000b2570000h",
  cmpsc_2 =	"0000b2630000h",
  kimd_2 =	"0000b93e0000h",
  klmd_2 =	"0000b93f0000h",
  kmac_2 =	"0000b91e0000h",
  thdr_2 =	"0000b3590000h",
  thder_2 =	"0000b3580000h",
  cxfbr_2 =	"0000b3960000h",
  cxftr_2 =	"0000b9590000h",
  cxfr_2 =	"0000b3b60000h",
  cdfbr_2 =	"0000b3950000h",
  cdftr_2 =	"0000b9510000h",
  cdfr_2 =	"0000b3b50000h",
  cefbr_2 =	"0000b3940000h",
  cefr_2 =	"0000b3b40000h",
  cxgbr_2 =	"0000b3a60000h",
  cxgtr_2 =	"0000b3f90000h",
  cxgr_2 =	"0000b3c60000h",
  cdgbr_2 =	"0000b3a50000h",
  cdgtr_2 =	"0000b3f10000h",
  cdgr_2 =	"0000b3c50000h",
  cegbr_2 =	"0000b3a40000h",
  cegr_2 =	"0000b3c40000h",
  cxstr_2 =	"0000b3fb0000h",
  cdstr_2 =	"0000b3f30000h",
  cxutr_2 =	"0000b3fa0000h",
  cdutr_2 =	"0000b3f20000h",
  cvb_2 =	"00004f000000j",
  cvby_2 =	"e30000000006l",
  cvbg_2 =	"e3000000000el",
  cvd_2 =	"00004e000000j",
  cvdy_2 =	"e30000000026l",
  cvdg_2 =	"e3000000002el",
  cuxtr_2 =	"0000b3ea0000h",
  cudtr_2 =	"0000b3e20000h",
  cu42_2 =	"0000b9b30000h",
  cu41_2 =	"0000b9b20000h",
  cpya_2 =	"0000b24d0000h",
  d_2 =		"00005d000000j",
  dr_2 =	"000000001d00g",
  dxbr_2 =	"0000b34d0000h",
  dxr_2 =	"0000b22d0000h",
  ddbr_2 =	"0000b31d0000h",
  dd_2 =	"00006d000000j",
  ddr_2 =	"000000002d00g",
  debr_2 =	"0000b30d0000h",
  de_2 =	"00007d000000j",
  der_2 =	"000000003d00g",
  dl_2 =	"e30000000097l",
  dlr_2 =	"0000b9970000h",
  dlg_2 =	"e30000000087l",
  dlgr_2 =	"0000b9870000h",
  dsg_2 =	"e3000000000dl",
  dsgr_2 =	"0000b90d0000h",
  dsgf_2 =	"e3000000001dl",
  dsgfr_2 =	"0000b91d0000h",
  x_2 =		"000057000000j",
  xr_2 =	"000000001700g",
  xy_2 =	"e30000000057l",
  xg_2 =	"e30000000082l",
  xgr_2 =	"0000b9820000h",
  xihf_2 =	"c00600000000n",
  xilf_2 =	"c00700000000n",
  ex_2 =	"000044000000j",
  exrl_2 =	"c60000000000o",
  ear_2 =	"0000b24f0000h",
  esea_2 =	"0000b99d0000h",
  eextr_2 =	"0000b3ed0000h",
  eedtr_2 =	"0000b3e50000h",
  ecag_3 =	"eb000000004cs",
  efpc_2 =	"0000b38c0000h",
  epar_2 =	"0000b2260000h",
  epair_2 =	"0000b99a0000h",
  epsw_2 =	"0000b98d0000h",
  esar_2 =	"0000b2270000h",
  esair_2 =	"0000b99b0000h",
  esxtr_2 =	"0000b3ef0000h",
  esdtr_2 =	"0000b3e70000h",
  ereg_2 =	"0000b2490000h",
  eregg_2 =	"0000b90e0000h",
  esta_2 =	"0000b24a0000h",
  flogr_2 =	"0000b9830000h",
  hdr_2 =	"000000002400g",
  her_2 =	"000000003400g",
  iac_2 =	"0000b2240000h",
  ic_2 =	"000043000000j",
  icy_2 =	"e30000000073l",
  icmh_3 =	"eb0000000080t",
  icm_3 =	"0000bf000000r",
  icmy_3 =	"eb0000000081t",
  iihf_2 =	"c00800000000n",
  iihh_2 =	"0000a5000000i",
  iihl_2 =	"0000a5010000i",
  iilf_2 =	"c00900000000n",
  iilh_2 =	"0000a5020000i",
  iill_2 =	"0000a5030000i",
  ipm_2 =	"0000b2220000h",
  iske_2 =	"0000b2290000h",
  ivsk_2 =	"0000b2230000h",
  l_2 =		"000058000000j",
  lr_2 =	"000000001800g",
  ly_2 =	"e30000000058l",
  lg_2 =	"e30000000004l",
  lgr_2 =	"0000b9040000h",
  lgf_2 =	"e30000000014l",
  lgfr_2 =	"0000b9140000h",
  lghi_2 =	"0000a7090000i",
  lxr_2 =	"0000b3650000h",
  ld_2 =	"000068000000j",
  ldr_2 =	"000000002800g",
  ldy_2 =	"ed0000000065l",
  le_2 =	"000078000000j",
  ler_2 =	"000000003800g",
  ley_2 =	"ed0000000064l",
  lam_3 =	"00009a000000q",
  lamy_3 =	"eb000000009as",
  la_2 =	"000041000000j",
  lay_2 =	"e30000000071l",
  lae_2 =	"000051000000j",
  laey_2 =	"e30000000075l",
  larl_2 =	"c00000000000o",
  laa_3 =	"eb00000000f8s",
  laag_3 =	"eb00000000e8s",
  laal_3 =	"eb00000000fas",
  laalg_3 =	"eb00000000eas",
  lan_3 =	"eb00000000f4s",
  lang_3 =	"eb00000000e4s",
  lax_3 =	"eb00000000f7s",
  laxg_3 =	"eb00000000e7s",
  lao_3 =	"eb00000000f6s",
  laog_3 =	"eb00000000e6s",
  lt_2 =	"e30000000012l",
  ltr_2 =	"000000001200g",
  ltg_2 =	"e30000000002l",
  ltgr_2 =	"0000b9020000h",
  ltgf_2 =	"e30000000032l",
  ltgfr_2 =	"0000b9120000h",
  ltxbr_2 =	"0000b3420000h",
  ltxtr_2 =	"0000b3de0000h",
  ltxr_2 =	"0000b3620000h",
  ltdbr_2 =	"0000b3120000h",
  ltdtr_2 =	"0000b3d60000h",
  ltdr_2 =	"000000002200g",
  ltebr_2 =	"0000b3020000h",
  lter_2 =	"000000003200g",
  lb_2 =	"e30000000076l",
  lbr_2 =	"0000b9260000h",
  lgb_2 =	"e30000000077l",
  lgbr_2 =	"0000b9060000h",
  lbh_2 =	"e300000000c0l",
  lcr_2 =	"000000001300g",
  lcgr_2 =	"0000b9030000h",
  lcgfr_2 =	"0000b9130000h",
  lcxbr_2 =	"0000b3430000h",
  lcxr_2 =	"0000b3630000h",
  lcdbr_2 =	"0000b3130000h",
  lcdr_2 =	"000000002300g",
  lcdfr_2 =	"0000b3730000h",
  lcebr_2 =	"0000b3030000h",
  lcer_2 =	"000000003300g",
  lctl_3 =	"0000b7000000q",
  lctlg_3 =	"eb000000002fs",
  fixr_2 =	"0000b3670000h",
  fidr_2 =	"0000b37f0000h",
  fier_2 =	"0000b3770000h",
  ldgr_2 =	"0000b3c10000h",
  lgdr_2 =	"0000b3cd0000h",
  lh_2 =	"000048000000j",
  lhr_2 =	"0000b9270000h",
  lhy_2 =	"e30000000078l",
  lgh_2 =	"e30000000015l",
  lghr_2 =	"0000b9070000h",
  lhh_2 =	"e300000000c4l",
  lhi_2 =	"0000a7080000i",
  lhrl_2 =	"c40500000000o",
  lghrl_2 =	"c40400000000o",
  lfh_2 =	"e300000000cal",
  lgfi_2 =	"c00100000000n",
  lxdbr_2 =	"0000b3050000h",
  lxdr_2 =	"0000b3250000h",
  lxebr_2 =	"0000b3060000h",
  lxer_2 =	"0000b3260000h",
  ldebr_2 =	"0000b3040000h",
  lder_2 =	"0000b3240000h",
  llgf_2 =	"e30000000016l",
  llgfr_2 =	"0000b9160000h",
  llc_2 =	"e30000000094l",
  llcr_2 =	"0000b9940000h",
  llgc_2 =	"e30000000090l",
  llgcr_2 =	"0000b9840000h",
  llch_2 =	"e300000000c2l",
  llh_2 =	"e30000000095l",
  llhr_2 =	"0000b9950000h",
  llgh_2 =	"e30000000091l",
  llghr_2 =	"0000b9850000h",
  llhh_2 =	"e300000000c6l",
  llhrl_2 =	"c40200000000o",
  llghrl_2 =	"c40600000000o",
  llihf_2 =	"c00e00000000n",
  llihh_2 =	"0000a50c0000i",
  llihl_2 =	"0000a50d0000i",
  llilf_2 =	"c00f00000000n",
  llilh_2 =	"0000a50e0000i",
  llill_2 =	"0000a50f0000i",
  llgfrl_2 =	"c40e00000000o",
  llgt_2 =	"e30000000017l",
  llgtr_2 =	"0000b9170000h",
  lm_3 =	"000098000000q",
  lmy_3 =	"eb0000000098s",
  lmg_3 =	"eb0000000004s",
  lmh_3 =	"eb0000000096s",
  lnr_2 =	"000000001100g",
  lngr_2 =	"0000b9010000h",
  lngfr_2 =	"0000b9110000h",
  lnxbr_2 =	"0000b3410000h",
  lnxr_2 =	"0000b3610000h",
  lndbr_2 =	"0000b3110000h",
  lndr_2 =	"000000002100g",
  lndfr_2 =	"0000b3710000h",
  lnebr_2 =	"0000b3010000h",
  lner_2 =	"000000003100g",
  loc_3 =	"eb00000000f2t",
  locg_3 =	"eb00000000e2t",
  lpq_2 =	"e3000000008fl",
  lpr_2 =	"000000001000g",
  lpgr_2 =	"0000b9000000h",
  lpgfr_2 =	"0000b9100000h",
  lpxbr_2 =	"0000b3400000h",
  lpxr_2 =	"0000b3600000h",
  lpdbr_2 =	"0000b3100000h",
  lpdr_2 =	"000000002000g",
  lpdfr_2 =	"0000b3700000h",
  lpebr_2 =	"0000b3000000h",
  lper_2 =	"000000003000g",
  lra_2 =	"0000b1000000j",
  lray_2 =	"e30000000013l",
  lrag_2 =	"e30000000003l",
  lrl_2 =	"c40d00000000o",
  lgrl_2 =	"c40800000000o",
  lgfrl_2 =	"c40c00000000o",
  lrvh_2 =	"e3000000001fl",
  lrv_2 =	"e3000000001el",
  lrvr_2 =	"0000b91f0000h",
  lrvg_2 =	"e3000000000fl",
  lrvgr_2 =	"0000b90f0000h",
  ldxbr_2 =	"0000b3450000h",
  ldxr_2 =	"000000002500g",
  lrdr_2 =	"000000002500g",
  lexbr_2 =	"0000b3460000h",
  lexr_2 =	"0000b3660000h",
  ledbr_2 =	"0000b3440000h",
  ledr_2 =	"000000003500g",
  lrer_2 =	"000000003500g",
  lura_2 =	"0000b24b0000h",
  lurag_2 =	"0000b9050000h",
  lzxr_2 =	"0000b3760000h",
  lzdr_2 =	"0000b3750000h",
  lzer_2 =	"0000b3740000h",
  msta_2 =	"0000b2470000h",
  mvcl_2 =	"000000000e00g",
  mvcle_3 =	"0000a8000000q",
  mvclu_3 =	"eb000000008es",
  mvpg_2 =	"0000b2540000h",
  mvst_2 =	"0000b2550000h",
  m_2 =		"00005c000000j",
  mfy_2 =	"e3000000005cl",
  mr_2 =	"000000001c00g",
  mxbr_2 =	"0000b34c0000h",
  mxr_2 =	"000000002600g",
  mdbr_2 =	"0000b31c0000h",
  md_2 =	"00006c000000j",
  mdr_2 =	"000000002c00g",
  mxdbr_2 =	"0000b3070000h",
  mxd_2 =	"000067000000j",
  mxdr_2 =	"000000002700g",
  meebr_2 =	"0000b3170000h",
  meer_2 =	"0000b3370000h",
  mdebr_2 =	"0000b30c0000h",
  mde_2 =	"00007c000000j",
  mder_2 =	"000000003c00g",
  me_2 =	"00007c000000j",
  mer_2 =	"000000003c00g",
  mh_2 =	"00004c000000j",
  mhy_2 =	"e3000000007cl",
  mlg_2 =	"e30000000086l",
  mlgr_2 =	"0000b9860000h",
  ml_2 =	"e30000000096l",
  mlr_2 =	"0000b9960000h",
  ms_2 =	"000071000000j",
  msr_2 =	"0000b2520000h",
  msy_2 =	"e30000000051l",
  msg_2 =	"e3000000000cl",
  msgr_2 =	"0000b90c0000h",
  msgf_2 =	"e3000000001cl",
  msgfr_2 =	"0000b91c0000h",
  msfi_2 =	"c20100000000n",
  msgfi_2 =	"c20000000000n",
  maer_3 =	"0000b32e0000r",
  mvhhi_2 =	"e54400000000SIL",
  mvhi_2 =	"e54c00000000SIL",
  mvghi_2 =	"e54800000000SIL",
  o_2 =		"000056000000j",
  or_2 =	"000000001600g",
  oy_2 =	"e30000000056l",
  og_2 =	"e30000000081l",
  ogr_2 =	"0000b9810000h",
  oihf_2 =	"c00c00000000n",
  oihh_2 =	"0000a5080000i",
  oihl_2 =	"0000a5090000i",
  oilf_2 =	"c00d00000000n",
  oilh_2 =	"0000a50a0000i",
  oill_2 =	"0000a50b0000i",
  pgin_2 =	"0000b22e0000h",
  pgout_2 =	"0000b22f0000h",
  pcc_2 =	"0000b92c0000h",
  pckmo_2 =	"0000b9280000h",
  pfmf_2 =	"0000b9af0000h",
  ptf_2 =	"0000b9a20000h",
  popcnt_2 =	"0000b9e10000h",
  pfd_2 =	"e30000000036m",
  pfdrl_2 =	"c60200000000p",
  pt_2 =	"0000b2280000h",
  pti_2 =	"0000b99e0000h",
  palb_2 =	"0000b2480000h",
  rrbe_2 =	"0000b22a0000h",
  rrbm_2 =	"0000b9ae0000h",
  rll_3 =	"eb000000001ds",
  rllg_3 =	"eb000000001cs",
  srst_2 =	"0000b25e0000h",
  srstu_2 =	"0000b9be0000h",
  sar_2 =	"0000b24e0000h",
  sfpc_2 =	"0000b3840000h",
  sfasr_2 =	"0000b3850000h",
  spm_2 =	"000000000400g",
  ssar_2 =	"0000b2250000h",
  ssair_2 =	"0000b99f0000h",
  slda_3 =	"00008f000000q",
  sldl_3 =	"00008d000000q",
  sla_3 =	"00008b000000q",
  slak_3 =	"eb00000000dds",
  slag_3 =	"eb000000000bs",
  sll_3 =	"000089000000q",
  sllk_3 =	"eb00000000dfs",
  sllg_3 =	"eb000000000ds",
  srda_3 =	"00008e000000q",
  srdl_3 =	"00008c000000q",
  sra_3 =	"00008a000000q",
  srak_3 =	"eb00000000dcs",
  srag_3 =	"eb000000000as",
  srl_3 =	"000088000000q",
  srlk_3 =	"eb00000000des",
  srlg_3 =	"eb000000000cs",
  sqxbr_2 =	"0000b3160000h",
  sqxr_2 =	"0000b3360000h",
  sqdbr_2 =	"0000b3150000h",
  sqdr_2 =	"0000b2440000h",
  sqebr_2 =	"0000b3140000h",
  sqer_2 =	"0000b2450000h",
  st_2 =	"000050000000j",
  sty_2 =	"e30000000050l",
  stg_2 =	"e30000000024l",
  std_2 =	"000060000000j",
  stdy_2 =	"ed0000000067l",
  ste_2 =	"000070000000j",
  stey_2 =	"ed0000000066l",
  stam_3 =	"00009b000000q",
  stamy_3 =	"eb000000009bs",
  stc_2 =	"000042000000j",
  stcy_2 =	"e30000000072l",
  stch_2 =	"e300000000c3l",
  stcmh_3 =	"eb000000002ct",
  stcm_3 =	"0000be000000r",
  stcmy_3 =	"eb000000002dt",
  stctl_3 =	"0000b6000000q",
  stctg_3 =	"eb0000000025s",
  sth_2 =	"000040000000j",
  sthy_2 =	"e30000000070l",
  sthh_2 =	"e300000000c7l",
  sthrl_2 =	"c40700000000o",
  stfh_2 =	"e300000000cbl",
  stm_3 =	"000090000000q",
  stmy_3 =	"eb0000000090s",
  stmg_3 =	"eb0000000024s",
  stmh_3 =	"eb0000000026s",
  stoc_3 =	"eb00000000f3t",
  stocg_3 =	"eb00000000e3t",
  stpq_2 =	"e3000000008el",
  strl_2 =	"c40f00000000o",
  stgrl_2 =	"c40b00000000o",
  strvh_2 =	"e3000000003fl",
  strv_2 =	"e3000000003el",
  strvg_2 =	"e3000000002fl",
  stura_2 =	"0000b2460000h",
  sturg_2 =	"0000b9250000h",
  s_2 =		"00005b000000j",
  sr_2 =	"000000001b00g",
  sy_2 =	"e3000000005bl",
  sg_2 =	"e30000000009l",
  sgr_2 =	"0000b9090000h",
  sgf_2 =	"e30000000019l",
  sgfr_2 =	"0000b9190000h",
  sxbr_2 =	"0000b34b0000h",
  sdbr_2 =	"0000b31b0000h",
  sebr_2 =	"0000b30b0000h",
  sh_2 =	"00004b000000j",
  shy_2 =	"e3000000007bl",
  sl_2 =	"00005f000000j",
  slr_2 =	"000000001f00g",
  sly_2 =	"e3000000005fl",
  slg_2 =	"e3000000000bl",
  slgr_2 =	"0000b90b0000h",
  slgf_2 =	"e3000000001bl",
  slgfr_2 =	"0000b91b0000h",
  slfi_2 =	"c20500000000n",
  slgfi_2 =	"c20400000000n",
  slb_2 =	"e30000000099l",
  slbr_2 =	"0000b9990000h",
  slbg_2 =	"e30000000089l",
  slbgr_2 =	"0000b9890000h",
  sxr_2 =	"000000003700g",
  sd_2 =	"00006b000000j",
  sdr_2 =	"000000002b00g",
  se_2 =	"00007b000000j",
  ser_2 =	"000000003b00g",
  su_2 =	"00007f000000j",
  sur_2 =	"000000003f00g",
  sw_2 =	"00006f000000j",
  swr_2 =	"000000002f00g",
  tar_2 =	"0000b24c0000h",
  tb_2 =	"0000b22c0000h",
  tmhh_2 =	"0000a7020000i",
  tmhl_2 =	"0000a7030000i",
  tmlh_2 =	"0000a7000000i",
  tmll_2 =	"0000a7010000i",
  trace_3 =	"000099000000q",
  tracg_3 =	"eb000000000fs",
  tre_2 =	"0000b2a50000h",

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
  if p == "g" then
    op2 = op2 + shl(parse_reg(params[1]),4) + parse_reg(params[2])
    wputhw(op2)
  elseif p == "h" then
    op2 = op2 + shl(parse_reg(params[1]),4) + parse_reg(params[2])
    wputhw(op1); wputhw(op2)
  elseif p == "i" then
    op1 = op1 + shl(parse_reg(params[1]),4)
    wputhw(op1);
    parse_imm16(params[2])
  elseif p == "j" then
    local d, x, b, a = parse_mem_bx(params[2])
    op1 = op1 + shl(parse_reg(params[1]), 4) + x
    op2 = op2 + shl(b, 12) + d
    wputhw(op1); wputhw(op2);
    if a then a() end
  elseif p == "k" then
  elseif p == "l" then
    local d, x, b, a = parse_mem_bxy(params[2])
    op0 = op0 + shl(parse_reg(params[1]), 4) + x
    op1 = op1 + shl(b, 12) + band(d, 0xfff)
    op2 = op2 + band(shr(d, 4), 0xff00)
    wputhw(op0); wputhw(op1); wputhw(op2)
    if a then a() end
  elseif p == "m" then

  elseif p == "n" then
    op0 = op0 + shl(parse_reg(params[1]), 4)
    wputhw(op0);
    parse_imm32(params[2])
  elseif p == "o" then
    op0 = op0 + shl(parse_reg(params[1]), 4)
    wputhw(op0);
    local mode, n, s = parse_label(params[2])
    waction("REL_"..mode, n, s)
  elseif p == "q" then
    local d, b, a = parse_mem_b(params[3])
    op1 = op1 + shl(parse_reg(params[1]), 4) + parse_reg(params[2])
    op2 = op2 + shl(b, 12) + d
    wputhw(op1); wputhw(op2)
    if a then a() end -- a() emits action.
  elseif p == "r" then
    op2 = op2 + shl(parse_reg(params[1]),12) + shl(parse_reg(params[2]),4) + parse_reg(params[3])
    wputhw(op1); wputhw(op2)
  elseif p == "s" then
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
