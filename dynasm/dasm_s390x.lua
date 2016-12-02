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
  "REL_PC", "LABEL_PC", "DISP12", "DISP20", "IMM16", "IMM32",
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
      esc = name == "ESC" or name == "SECTION"
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

local parse_reg_type

local function parse_gpr(expr)
  local r = match(expr, "^r(1?[0-9])$")
  if r then
    r = tonumber(r)
    if r <= 15 then return r, tp end
  end
  werror("bad register name `"..expr.."'")
end

local function parse_fpr(expr)
  local r = match(expr, "^f(1?[0-9])$")
  if r then
    r = tonumber(r)
    if r <= 15 then return r end
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

-- Split a memory operand of the form d(b) or d(x,b) into d, x and b.
-- If x is not specified then it is 0.
local function split_memop(arg)
  local reg = "r1?[0-9]"
  local d, x, b = match(arg, "^(.*)%(("..reg.."), ("..reg..")%)$")
  if d then
    return d, parse_gpr(x), parse_gpr(b)
  end
  local d, b = match(arg, "^(.*)%(("..reg..")%)$")
  if d then
    return d, 0, parse_gpr(b)
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

local function parse_imm(arg)
   local imm_val = tonumber(arg,16)
   if not is_int32(imm_val) then
      werror("Immediate value out of range: ", imm_val)
   end
   return imm_val
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

-- Template strings for s390x instructions.
map_op = {
a_2 =           "000000005a000000j",
ar_2 =          "0000000000001a00g",
ay_2 =          "0000e3000000005al",
ag_2 =          "0000e30000000008l",
agr_2 =                 "00000000b9080000h",
agf_2 =                 "0000e30000000018l",
agfr_2 =                "00000000b9180000h",
axbr_2 =                "00000000b34a0000h",
adbr_2 =                "00000000b31a0000h",
aebr_2 =                "00000000b30a0000h",
ah_2 =          "000000004a000000j",
ahy_2 =                 "0000e3000000007al",
afi_2 =                 "0000c20900000000n",
agfi_2 =                "0000c20800000000n",
aih_2 =                 "0000cc0800000000n",
al_2 =          "000000005e000000j",
alr_2 =                 "0000000000001e00g",
aly_2 =                 "0000e3000000005el",
alg_2 =                 "0000e3000000000al",
algr_2 =                "00000000b90a0000h",
algf_2 =                "0000e3000000001al",
algfr_2 =               "00000000b91a0000h",
alfi_2 =                "0000c20b00000000n",
algfi_2 =               "0000c20a00000000n",
alc_2 =                 "0000e30000000098l",
alcr_2 =                "00000000b9980000h",
alcg_2 =                "0000e30000000088l",
alcgr_2 =               "00000000b9880000h",
alsih_2 =               "0000cc0a00000000n",
alsihn_2 =              "0000cc0b00000000n",
axr_2 =                 "0000000000003600g",
ad_2 =          "000000006a000000j",
adr_2 =                 "0000000000002a00g",
ae_2 =          "000000007a000000j",
aer_2 =                 "0000000000003a00g",
aw_2 =          "000000006e000000j",
awr_2 =                 "0000000000002e00g",
au_2 =          "000000007e000000j",
aur_2 =                 "0000000000003e00g",
n_2 =           "0000000054000000j",
nr_2 =          "0000000000001400g",
ny_2 =          "0000e30000000054l",
ng_2 =          "0000e30000000080l",
ngr_2 =                 "00000000b9800000h",
nihf_2 =                "0000c00a00000000n",
nilf_2 =                "0000c00b00000000n",
bal_2 =                 "0000000045000000j",
balr_2 =                "000000000000500g",
bas_2 =                 "000000004d000000j",
basr_2 =                "0000000000000d00g",
bassm_2 =               "0000000000000c00g",
bsa_2 =                 "00000000b25a0000h",
bsm_2 =                 "0000000000000b00g",
bakr_2 =                "00000000b2400000h",
bsg_2 =                 "00000000b2580000h",
bc_2 =          "0000000047000000k",
bcr_2 =                 "000000000000700g",
bct_2 =                 "0000000046000000j",
bctr_2 =                "000000000000600g",
bctg_2 =                "0000e30000000046l",
bctgr_2 =               "00000000b9460000h",
bxh_3 =                 "0000000086000000q",
bxhg_3 =                "0000eb0000000044s",
bxle_3 =                "0000000087000000q",
bxleg_3 =               "0000eb0000000045s",
brasl_2 =               "0000c00500000000o",
brcl_2 =                "0000c00400000000p",
brcth_2 =               "0000cc0600000000o",
cksm_2 =                "00000000b2410000h",
km_2 =          "00000000b92e0000h",
kmf_2 =                 "00000000b92a0000h",
kmc_2 =                 "00000000b92f0000h",
kmo_2 =                 "00000000b92b0000h",
c_2 =           "0000000059000000j",
cr_2 =          "0000000000001900g",
cy_2 =          "0000e30000000059l",
cg_2 =          "0000e30000000020l",
cgr_2 =                 "00000000b9200000h",
cgf_2 =                 "0000e30000000030l",
cgfr_2 =                "00000000b9300000h",
cxbr_2 =                "00000000b3490000h",
cxtr_2 =                "00000000b3ec0000h",
cxr_2 =                 "00000000b3690000h",
cdbr_2 =                "00000000b3190000h",
cdtr_2 =                "00000000b3e40000h",
cd_2 =          "0000000069000000j",
cdr_2 =                 "0000000000002900g",
cebr_2 =                "00000000b3090000h",
ce_2 =          "0000000079000000j",
cer_2 =                 "0000000000003900g",
kxbr_2 =                "00000000b3480000h",
kxtr_2 =                "00000000b3e80000h",
kdbr_2 =                "00000000b3180000h",
kdtr_2 =                "00000000b3e00000h",
kebr_2 =                "00000000b3080000h",
cs_3 =          "00000000ba000000q",
csy_3 =                 "0000eb0000000014s",
csg_3 =                 "0000eb0000000030s",
csp_2 =                 "00000000b2500000h",
cspg_2 =                "00000000b98a0000h",
cextr_2 =               "00000000b3fc0000h",
cedtr_2 =               "00000000b3f40000h",
cds_3 =                 "00000000bb000000q",
cdsy_3 =                "0000eb0000000031s",
cdsg_3 =                "0000eb000000003es",
ch_2 =          "0000000049000000j",
chy_2 =                 "0000e30000000079l",
cgh_2 =                 "0000e30000000034l",
chrl_2 =                "0000c60500000000o",
cghrl_2 =               "0000c60400000000o",
chf_2 =                 "0000e300000000cdl",
chhr_2 =                "00000000b9cd0000h",
chlr_2 =                "00000000b9dd0000h",
cfi_2 =                 "0000c20d00000000n",
cgfi_2 =                "0000c20c00000000n",
cih_2 =                 "0000cc0d00000000n",
cl_2 =          "0000000055000000j",
clr_2 =                 "0000000000001500g",
cly_2 =                 "0000e30000000055l",
clg_2 =                 "0000e30000000021l",
clgr_2 =                "00000000b9210000h",
clgf_2 =                "0000e30000000031l",
clgfr_2 =               "00000000b9310000h",
clmh_3 =                "0000eb0000000020t",
clm_3 =                 "00000000bd000000r",
clmy_3 =                "0000eb0000000021t",
clhf_2 =                "0000e300000000cfl",
clhhr_2 =               "00000000b9cf0000h",
clhlr_2 =               "00000000b9df0000h",
clfi_2 =                "0000c20f00000000n",
clgfi_2 =               "0000c20e00000000n",
clih_2 =                "0000cc0f00000000n",
clcl_2 =                "0000000000000f00g",
clcle_3 =               "00000000a9000000q",
clclu_3 =               "0000eb000000008fs",
clrl_2 =                "0000c60f00000000o",
clhrl_2 =               "0000c60700000000o",
clgrl_2 =               "0000c60a00000000o",
clghrl_2 =              "0000c60600000000o",
clgfrl_2 =              "0000c60e00000000o",
clst_2 =                "00000000b25d0000h",
crl_2 =                 "0000c60d00000000o",
cgrl_2 =                "0000c60800000000o",
cgfrl_2 =               "0000c60c00000000o",
cuse_2 =                "00000000b2570000h",
cmpsc_2 =               "00000000b2630000h",
kimd_2 =                "00000000b93e0000h",
klmd_2 =                "00000000b93f0000h",
kmac_2 =                "00000000b91e0000h",
thdr_2 =                "00000000b3590000h",
thder_2 =               "00000000b3580000h",
cxfbr_2 =               "00000000b3960000h",
cxftr_2 =               "00000000b9590000h",
cxfr_2 =                "00000000b3b60000h",
cdfbr_2 =               "00000000b3950000h",
cdftr_2 =               "00000000b9510000h",
cdfr_2 =                "00000000b3b50000h",
cefbr_2 =               "00000000b3940000h",
cefr_2 =                "00000000b3b40000h",
cxgbr_2 =               "00000000b3a60000h",
cxgtr_2 =               "00000000b3f90000h",
cxgr_2 =                "00000000b3c60000h",
cdgbr_2 =               "00000000b3a50000h",
cdgtr_2 =               "00000000b3f10000h",
cdgr_2 =                "00000000b3c50000h",
cegbr_2 =               "00000000b3a40000h",
cegr_2 =                "00000000b3c40000h",
cxstr_2 =               "00000000b3fb0000h",
cdstr_2 =               "00000000b3f30000h",
cxutr_2 =               "00000000b3fa0000h",
cdutr_2 =               "00000000b3f20000h",
cvb_2 =                 "000000004f000000j",
cvby_2 =                "0000e30000000006l",
cvbg_2 =                "0000e3000000000el",
cvd_2 =                 "000000004e000000j",
cvdy_2 =                "0000e30000000026l",
cvdg_2 =                "0000e3000000002el",
cuxtr_2 =               "00000000b3ea0000h",
cudtr_2 =               "00000000b3e20000h",
cu42_2 =                "00000000b9b30000h",
cu41_2 =                "00000000b9b20000h",
cpya_2 =                "00000000b24d0000h",
d_2 =           "000000005d000000j",
dr_2 =          "0000000000001d00g",
dxbr_2 =                "00000000b34d0000h",
dxr_2 =                 "00000000b22d0000h",
ddbr_2 =                "00000000b31d0000h",
dd_2 =          "000000006d000000j",
ddr_2 =                 "0000000000002d00g",
debr_2 =                "00000000b30d0000h",
de_2 =          "000000007d000000j",
der_2 =                 "0000000000003d00g",
dl_2 =          "0000e30000000097l",
dlr_2 =                 "00000000b9970000h",
dlg_2 =                 "0000e30000000087l",
dlgr_2 =                "00000000b9870000h",
dsg_2 =                 "0000e3000000000dl",
dsgr_2 =                "00000000b90d0000h",
dsgf_2 =                "0000e3000000001dl",
dsgfr_2 =               "00000000b91d0000h",
x_2 =           "0000000057000000j",
xr_2 =          "0000000000001700g",
xy_2 =          "0000e30000000057l",
xg_2 =          "0000e30000000082l",
xgr_2 =                 "00000000b9820000h",
xihf_2 =                "0000c00600000000n",
xilf_2 =                "0000c00700000000n",
ex_2 =          "0000000044000000j",
exrl_2 =                "0000c60000000000o",
ear_2 =                 "00000000b24f0000h",
esea_2 =                "00000000b99d0000h",
eextr_2 =               "00000000b3ed0000h",
eedtr_2 =               "00000000b3e50000h",
ecag_3 =                "0000eb000000004cs",
efpc_2 =                "00000000b38c0000h",
epar_2 =                "00000000b2260000h",
epair_2 =               "00000000b99a0000h",
epsw_2 =                "00000000b98d0000h",
esar_2 =                "00000000b2270000h",
esair_2 =               "00000000b99b0000h",
esxtr_2 =               "00000000b3ef0000h",
esdtr_2 =               "00000000b3e70000h",
ereg_2 =                "00000000b2490000h",
eregg_2 =               "00000000b90e0000h",
esta_2 =                "00000000b24a0000h",
flogr_2 =               "00000000b9830000h",
hdr_2 =                 "0000000000002400g",
her_2 =                 "0000000000003400g",
iac_2 =                 "00000000b2240000h",
ic_2 =          "0000000043000000j",
icy_2 =                 "0000e30000000073l",
icmh_3 =                "0000eb0000000080t",
icm_3 =                 "00000000bf000000r",
icmy_3 =                "0000eb0000000081t",
iihf_2 =                "0000c00800000000n",
iilf_2 =                "0000c00900000000n",
ipm_2 =                 "00000000b2220000h",
iske_2 =                "00000000b2290000h",
ivsk_2 =                "00000000b2230000h",
l_2 =           "0000000058000000j",
lr_2 =          "0000000000001800g",
ly_2 =          "0000e30000000058l",
lg_2 =          "0000e30000000004l",
lgr_2 =                 "00000000b9040000h",
lgf_2 =                 "0000e30000000014l",
lgfr_2 =                "00000000b9140000h",
lxr_2 =                 "00000000b3650000h",
ld_2 =          "0000000068000000j",
ldr_2 =                 "0000000000002800g",
ldy_2 =                 "0000ed0000000065l",
le_2 =          "0000000078000000j",
ler_2 =                 "0000000000003800g",
ley_2 =                 "0000ed0000000064l",
lam_3 =                 "000000009a000000q",
lamy_3 =                "0000eb000000009as",
la_2 =          "0000000041000000j",
lay_2 =                 "0000e30000000071l",
lae_2 =                 "0000000051000000j",
laey_2 =                "0000e30000000075l",
larl_2 =                "0000c00000000000o",
laa_3 =                 "0000eb00000000f8s",
laag_3 =                "0000eb00000000e8s",
laal_3 =                "0000eb00000000fas",
laalg_3 =               "0000eb00000000eas",
lan_3 =                 "0000eb00000000f4s",
lang_3 =                "0000eb00000000e4s",
lax_3 =                 "0000eb00000000f7s",
laxg_3 =                "0000eb00000000e7s",
lao_3 =                 "0000eb00000000f6s",
laog_3 =                "0000eb00000000e6s",
lt_2 =          "0000e30000000012l",
ltr_2 =                 "0000000000001200g",
ltg_2 =                 "0000e30000000002l",
ltgr_2 =                "00000000b9020000h",
ltgf_2 =                "0000e30000000032l",
ltgfr_2 =               "00000000b9120000h",
ltxbr_2 =               "00000000b3420000h",
ltxtr_2 =               "00000000b3de0000h",
ltxr_2 =                "00000000b3620000h",
ltdbr_2 =               "00000000b3120000h",
ltdtr_2 =               "00000000b3d60000h",
ltdr_2 =                "0000000000002200g",
ltebr_2 =               "00000000b3020000h",
lter_2 =                "0000000000003200g",
lb_2 =          "0000e30000000076l",
lbr_2 =                 "00000000b9260000h",
lgb_2 =                 "0000e30000000077l",
lgbr_2 =                "00000000b9060000h",
lbh_2 =                 "0000e300000000c0l",
lcr_2 =                 "0000000000001300g",
lcgr_2 =                "00000000b9030000h",
lcgfr_2 =               "00000000b9130000h",
lcxbr_2 =               "00000000b3430000h",
lcxr_2 =                "00000000b3630000h",
lcdbr_2 =               "00000000b3130000h",
lcdr_2 =                "0000000000002300g",
lcdfr_2 =               "00000000b3730000h",
lcebr_2 =               "00000000b3030000h",
lcer_2 =                "0000000000003300g",
lctl_3 =                "00000000b7000000q",
lctlg_3 =               "0000eb000000002fs",
fixr_2 =                "00000000b3670000h",
fidr_2 =                "00000000b37f0000h",
fier_2 =                "00000000b3770000h",
ldgr_2 =                "00000000b3c10000h",
lgdr_2 =                "00000000b3cd0000h",
lh_2 =          "0000000048000000j",
lhr_2 =                 "00000000b9270000h",
lhy_2 =                 "0000e30000000078l",
lgh_2 =                 "0000e30000000015l",
lghr_2 =                "00000000b9070000h",
lhh_2 =                 "0000e300000000c4l",
lhrl_2 =                "0000c40500000000o",
lghrl_2 =               "0000c40400000000o",
lfh_2 =                 "0000e300000000cal",
lgfi_2 =                "0000c00100000000n",
lxdbr_2 =               "00000000b3050000h",
lxdr_2 =                "00000000b3250000h",
lxebr_2 =               "00000000b3060000h",
lxer_2 =                "00000000b3260000h",
ldebr_2 =               "00000000b3040000h",
lder_2 =                "00000000b3240000h",
llgf_2 =                "0000e30000000016l",
llgfr_2 =               "00000000b9160000h",
llc_2 =                 "0000e30000000094l",
llcr_2 =                "00000000b9940000h",
llgc_2 =                "0000e30000000090l",
llgcr_2 =               "00000000b9840000h",
llch_2 =                "0000e300000000c2l",
llh_2 =                 "0000e30000000095l",
llhr_2 =                "00000000b9950000h",
llgh_2 =                "0000e30000000091l",
llghr_2 =               "00000000b9850000h",
llhh_2 =                "0000e300000000c6l",
llhrl_2 =               "0000c40200000000o",
llghrl_2 =              "0000c40600000000o",
llihf_2 =               "0000c00e00000000n",
llilf_2 =               "0000c00f00000000n",
llgfrl_2 =              "0000c40e00000000o",
llgt_2 =                "0000e30000000017l",
llgtr_2 =               "00000000b9170000h",
lm_3 =          "0000000098000000q",
lmy_3 =                 "0000eb0000000098s",
lmg_3 =                 "0000eb0000000004s",
lmh_3 =                 "0000eb0000000096s",
lnr_2 =                 "0000000000001100g",
lngr_2 =                "00000000b9010000h",
lngfr_2 =               "00000000b9110000h",
lnxbr_2 =               "00000000b3410000h",
lnxr_2 =                "00000000b3610000h",
lndbr_2 =               "00000000b3110000h",
lndr_2 =                "0000000000002100g",
lndfr_2 =               "00000000b3710000h",
lnebr_2 =               "00000000b3010000h",
lner_2 =                "0000000000003100g",
loc_3 =                 "0000eb00000000f2t",
locg_3 =                "0000eb00000000e2t",
lpq_2 =                 "0000e3000000008fl",
lpr_2 =                 "0000000000001000g",
lpgr_2 =                "00000000b9000000h",
lpgfr_2 =               "00000000b9100000h",
lpxbr_2 =               "00000000b3400000h",
lpxr_2 =                "00000000b3600000h",
lpdbr_2 =               "00000000b3100000h",
lpdr_2 =                "0000000000002000g",
lpdfr_2 =               "00000000b3700000h",
lpebr_2 =               "00000000b3000000h",
lper_2 =                "0000000000003000g",
lra_2 =                 "00000000b1000000j",
lray_2 =                "0000e30000000013l",
lrag_2 =                "0000e30000000003l",
lrl_2 =                 "0000c40d00000000o",
lgrl_2 =                "0000c40800000000o",
lgfrl_2 =               "0000c40c00000000o",
lrvh_2 =                "0000e3000000001fl",
lrv_2 =                 "0000e3000000001el",
lrvr_2 =                "00000000b91f0000h",
lrvg_2 =                "0000e3000000000fl",
lrvgr_2 =               "00000000b90f0000h",
ldxbr_2 =               "00000000b3450000h",
ldxr_2 =                "0000000000002500g",
lrdr_2 =                "0000000000002500g",
lexbr_2 =               "00000000b3460000h",
lexr_2 =                "00000000b3660000h",
ledbr_2 =               "00000000b3440000h",
ledr_2 =                "0000000000003500g",
lrer_2 =                "0000000000003500g",
lura_2 =                "00000000b24b0000h",
lurag_2 =               "00000000b9050000h",
lzxr_2 =                "00000000b3760000h",
lzdr_2 =                "00000000b3750000h",
lzer_2 =                "00000000b3740000h",
msta_2 =                "00000000b2470000h",
mvcl_2 =                "0000000000000e00g",
mvcle_3 =               "00000000a8000000q",
mvclu_3 =               "0000eb000000008es",
mvpg_2 =                "00000000b2540000h",
mvst_2 =                "00000000b2550000h",
m_2 =           "000000005c000000j",
mfy_2 =                 "0000e3000000005cl",
mr_2 =          "0000000000001c00g",
mxbr_2 =                "00000000b34c0000h",
mxr_2 =                 "0000000000002600g",
mdbr_2 =                "00000000b31c0000h",
md_2 =          "000000006c000000j",
mdr_2 =                 "0000000000002c00g",
mxdbr_2 =               "00000000b3070000h",
mxd_2 =                 "0000000067000000j",
mxdr_2 =                "0000000000002700g",
meebr_2 =               "00000000b3170000h",
meer_2 =                "00000000b3370000h",
mdebr_2 =               "00000000b30c0000h",
mde_2 =                 "000000007c000000j",
mder_2 =                "0000000000003c00g",
me_2 =          "000000007c000000j",
mer_2 =                 "0000000000003c00g",
mh_2 =          "000000004c000000j",
mhy_2 =                 "0000e3000000007cl",
mlg_2 =                 "0000e30000000086l",
mlgr_2 =                "00000000b9860000h",
ml_2 =          "0000e30000000096l",
mlr_2 =                 "00000000b9960000h",
ms_2 =          "0000000071000000j",
msr_2 =                 "00000000b2520000h",
msy_2 =                 "0000e30000000051l",
msg_2 =                 "0000e3000000000cl",
msgr_2 =                "00000000b90c0000h",
msgf_2 =                "0000e3000000001cl",
msgfr_2 =               "00000000b91c0000h",
msfi_2 =                "0000c20100000000n",
msgfi_2 =               "0000c20000000000n",
o_2 =           "0000000056000000j",
or_2 =          "0000000000001600g",
oy_2 =          "0000e30000000056l",
og_2 =          "0000e30000000081l",
ogr_2 =                 "00000000b9810000h",
oihf_2 =                "0000c00c00000000n",
oilf_2 =                "0000c00d00000000n",
pgin_2 =                "00000000b22e0000h",
pgout_2 =               "00000000b22f0000h",
pcc_2 =                 "00000000b92c0000h",
pckmo_2 =               "00000000b9280000h",
pfmf_2 =                "00000000b9af0000h",
ptf_2 =                 "00000000b9a20000h",
popcnt_2 =              "00000000b9e10000h",
pfd_2 =                 "0000e30000000036m",
pfdrl_2 =               "0000c60200000000p",
pt_2 =          "00000000b2280000h",
pti_2 =                 "00000000b99e0000h",
palb_2 =                "00000000b2480000h",
rrbe_2 =                "00000000b22a0000h",
rrbm_2 =                "00000000b9ae0000h",
rll_3 =                 "0000eb000000001ds",
rllg_3 =                "0000eb000000001cs",
srst_2 =                "00000000b25e0000h",
srstu_2 =               "00000000b9be0000h",
sar_2 =                 "00000000b24e0000h",
sfpc_2 =                "00000000b3840000h",
sfasr_2 =               "00000000b3850000h",
spm_2 =                 "000000000000400g",
ssar_2 =                "00000000b2250000h",
ssair_2 =               "00000000b99f0000h",
slda_3 =                "000000008f000000q",
sldl_3 =                "000000008d000000q",
sla_3 =                 "000000008b000000q",
slak_3 =                "0000eb00000000dds",
slag_3 =                "0000eb000000000bs",
sll_3 =                 "0000000089000000q",
sllk_3 =                "0000eb00000000dfs",
sllg_3 =                "0000eb000000000ds",
srda_3 =                "000000008e000000q",
srdl_3 =                "000000008c000000q",
sra_3 =                 "000000008a000000q",
srak_3 =                "0000eb00000000dcs",
srag_3 =                "0000eb000000000as",
srl_3 =                 "0000000088000000q",
srlk_3 =                "0000eb00000000des",
srlg_3 =                "0000eb000000000cs",
sqxbr_2 =               "00000000b3160000h",
sqxr_2 =                "00000000b3360000h",
sqdbr_2 =               "00000000b3150000h",
sqdr_2 =                "00000000b2440000h",
sqebr_2 =               "00000000b3140000h",
sqer_2 =                "00000000b2450000h",
st_2 =          "0000000050000000j",
sty_2 =                 "0000e30000000050l",
stg_2 =                 "0000e30000000024l",
std_2 =                 "0000000060000000j",
stdy_2 =                "0000ed0000000067l",
ste_2 =                 "0000000070000000j",
stey_2 =                "0000ed0000000066l",
stam_3 =                "000000009b000000q",
stamy_3 =               "0000eb000000009bs",
stc_2 =                 "0000000042000000j",
stcy_2 =                "0000e30000000072l",
stch_2 =                "0000e300000000c3l",
stcmh_3 =               "0000eb000000002ct",
stcm_3 =                "00000000be000000r",
stcmy_3 =               "0000eb000000002dt",
stctl_3 =               "00000000b6000000q",
stctg_3 =               "0000eb0000000025s",
sth_2 =                 "0000000040000000j",
sthy_2 =                "0000e30000000070l",
sthh_2 =                "0000e300000000c7l",
sthrl_2 =               "0000c40700000000o",
stfh_2 =                "0000e300000000cbl",
stm_3 =                 "0000000090000000q",
stmy_3 =                "0000eb0000000090s",
stmg_3 =                "0000eb0000000024s",
stmh_3 =                "0000eb0000000026s",
stoc_3 =                "0000eb00000000f3t",
stocg_3 =               "0000eb00000000e3t",
stpq_2 =                "0000e3000000008el",
strl_2 =                "0000c40f00000000o",
stgrl_2 =               "0000c40b00000000o",
strvh_2 =               "0000e3000000003fl",
strv_2 =                "0000e3000000003el",
strvg_2 =               "0000e3000000002fl",
stura_2 =               "00000000b2460000h",
sturg_2 =               "00000000b9250000h",
s_2 =           "000000005b000000j",
sr_2 =          "0000000000001b00g",
sy_2 =          "0000e3000000005bl",
sg_2 =          "0000e30000000009l",
sgr_2 =                 "00000000b9090000h",
sgf_2 =                 "0000e30000000019l",
sgfr_2 =                "00000000b9190000h",
sxbr_2 =                "00000000b34b0000h",
sdbr_2 =                "00000000b31b0000h",
sebr_2 =                "00000000b30b0000h",
sh_2 =          "000000004b000000j",
shy_2 =                 "0000e3000000007bl",
sl_2 =          "000000005f000000j",
slr_2 =                 "0000000000001f00g",
sly_2 =                 "0000e3000000005fl",
slg_2 =                 "0000e3000000000bl",
slgr_2 =                "00000000b90b0000h",
slgf_2 =                "0000e3000000001bl",
slgfr_2 =               "00000000b91b0000h",
slfi_2 =                "0000c20500000000n",
slgfi_2 =               "0000c20400000000n",
slb_2 =                 "0000e30000000099l",
slbr_2 =                "00000000b9990000h",
slbg_2 =                "0000e30000000089l",
slbgr_2 =               "00000000b9890000h",
sxr_2 =                 "0000000000003700g",
sd_2 =          "000000006b000000j",
sdr_2 =                 "0000000000002b00g",
se_2 =          "000000007b000000j",
ser_2 =                 "0000000000003b00g",
su_2 =          "000000007f000000j",
sur_2 =                 "0000000000003f00g",
sw_2 =          "000000006f000000j",
swr_2 =                 "0000000000002f00g",
tar_2 =                 "00000000b24c0000h",
tb_2 =          "00000000b22c0000h",
trace_3 =               "0000000099000000q",
tracg_3 =               "0000eb000000000fs",
tre_2 =                 "00000000b2a50000h",
}
for cond,c in pairs(map_cond) do
  -- Extended mnemonics for branches.
  -- TODO: replace 'B' with correct encoding.
  -- brc
  map_op["j"..cond.."_1"] = "00000000"..tohex(0xa7040000+shl(c, 20)).."w"
  -- brcl
  map_op["jg"..cond.."_1"] = tohex(0xc004+shl(c, 4)).."00000000".."x"
  -- bc
  map_op["b"..cond.."_1"] = "00000000"..tohex(0x47000000+shl(c, 20)).."y"
  -- bcr
  map_op["b"..cond.."r_1"] = "00000000"..tohex(0x0700+shl(c, 4)).."z"
end
------------------------------------------------------------------------------
-- Handle opcodes defined with template strings.
local function parse_template(params, template, nparams, pos)
  -- Read the template in 16-bit chunks.
  -- Leading halfword zeroes should not be written out.
  local op0 = tonumber(sub(template, 5, 8), 16)
  local op1 = tonumber(sub(template, 9, 12), 16)
  local op2 = tonumber(sub(template, 13, 16), 16)

  local n,rs = 1,26
  
  parse_reg_type = false
  -- Process each character.
  -- TODO
  -- 12-bit displacements (DISP12) and 16-bit immediates (IMM16) can be put at
  -- one of two locations relative to the end of the instruction.
  -- To make decoding easier we should insert the actions for these immediately
  -- after the halfword they modify.
  -- For example, take the instruction ahik, which is laid out as follows (each
  -- char is 4 bits):
  -- o = op code, r = register, i = immediate
  -- oorr iiii 00oo
  -- This should be emitted as oorr, followed by the immediate action, followed by
  -- 00oo.
  for p in gmatch(sub(template, 17), ".") do
    local pr1,pr2,pr3
    if p == "g" then
      pr1,pr2=params[n],params[n+1]
      op2 = op2 + shl(parse_gpr(pr1),4) + parse_gpr(pr2)
      wputhw(op2)
    elseif p == "h" then
      pr1,pr2=params[n],params[n+1]
      op2 = op2 + shl(parse_gpr(pr1),4) + parse_gpr(pr2)
      wputhw(op1); wputhw(op2)
    elseif p == "j" then
      local d, x, b, a = parse_mem_bx(params[2])
      op1 = op1 + shl(parse_gpr(params[1]), 4) + x
      op2 = op2 + shl(b, 12) + d
      wputhw(op1); wputhw(op2);
      if a then a() end
    elseif p == "k" then

    elseif p == "l" then
      local d, x, b, a = parse_mem_bxy(params[2])
      op0 = op0 + shl(parse_gpr(params[1]), 4) + x
      op1 = op1 + shl(b, 12) + band(d, 0xfff)
      op2 = op2 + band(shr(d, 4), 0xff00)
      wputhw(op0); wputhw(op1); wputhw(op2)
      if a then a() end
    elseif p == "m" then
      
    elseif p == "n" then
      op0 = op0 + shl(parse_gpr(params[1]), 4)	
      local imm = parse_imm(params[2])
      wputhw(op0); waction("IMM32", nil, imm)		
    elseif p == "q" then
      local d, b, a = parse_mem_b(params[3])
      op1 = op1 + shl(parse_gpr(params[1]), 4) + parse_gpr(params[2])
      op2 = op2 + shl(b, 12) + d
      wputhw(op1); wputhw(op2)
      if a then a() end
    elseif p == "s" then
      local d, b, a = parse_mem_by(params[3])
      op0 = op0 + shl(parse_gpr(params[1]), 4) + parse_gpr(params[2])
      op1 = op1 + shl(b, 12) + band(d, 0xfff)
      op2 = op2 + band(shr(d, 4), 0xff00)
      wputhw(op0); wputhw(op1); wputhw(op2)
      if a then a() end
    elseif p == "y" then
      local d, x, b, a = parse_mem_bx(params[1])
      op1 = op1 + x
      op2 = op2 + shl(b, 12) + d
      wputhw(op1); wputhw(op2);
      if a then a() end
    elseif p == "z" then
      op2 = op2 + parse_gpr(params[1])
      wputhw(op2)
    end
  end

end
function op_template(params, template, nparams)
  if not params then return template:gsub("%x%x%x%x%x%x%x%x", "") end
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
