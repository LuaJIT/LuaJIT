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
  local r = match(expr, "^r([1-3]?[0-9])$")
  if r then
    r = tonumber(r)
    if r <= 15 then return r, tp end
  end
  werror("bad register name `"..expr.."'")
end

local function parse_fpr(expr)
  local r = match(expr, "^f([1-3]?[0-9])$")
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

-- Parse memory operand of the form d(b) where 0 <= d < 4096 and b is a GPR.
-- Encoded as: bddd
local function parse_mem_b(arg)
  werror("parse_mem_b: not implemented")
  return nil
end

-- Parse memory operand of the form d(x, b) where 0 <= d < 4096 and b and x
-- are GPRs.
-- Encoded as: xbddd
local function parse_mem_bx(arg)
  werror("parse_mem_bx: not implemented")
  return nil
end

-- Parse memory operand of the form d(b) where -(2^20)/2 <= d < (2^20)/2 and
-- b is a GPR.
-- Encoded as: blllhh (ls are the low-bits of d, and hs are the high bits).
local function parse_mem_by(arg)
  werror("parse_mem_by: not implemented")
  return nil
end

-- Parse memory operand of the form d(x, b) where -(2^20)/2 <= d < (2^20)/2
-- and b and x are GPRs.
-- Encoded as: xblllhh (ls are the low-bits of d, and hs are the high bits).
local function parse_mem_bxy(arg)
  werror("parse_mem_bxy: not implemented")
  return nil
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
a_4 =           "000000005a000000j",
ar_2 =          "0000000000001a00g",
ay_5 =          "0000e3000000005al",
ag_5 =          "0000e30000000008l",
agr_2 =                 "00000000b9080000h",
agf_5 =                 "0000e30000000018l",
agfr_2 =                "00000000b9180000h",
axbr_2 =                "00000000b34a0000h",
adbr_2 =                "00000000b31a0000h",
aebr_2 =                "00000000b30a0000h",
ah_4 =          "000000004a000000j",
ahy_5 =                 "0000e3000000007al",
afi_3 =                 "0000c20900000000n",
agfi_3 =                "0000c20800000000n",
aih_3 =                 "0000cc0800000000n",
al_4 =          "000000005e000000j",
alr_2 =                 "0000000000001e00g",
aly_5 =                 "0000e3000000005el",
alg_5 =                 "0000e3000000000al",
algr_2 =                "00000000b90a0000h",
algf_5 =                "0000e3000000001al",
algfr_2 =               "00000000b91a0000h",
alfi_3 =                "0000c20b00000000n",
algfi_3 =               "0000c20a00000000n",
alc_5 =                 "0000e30000000098l",
alcr_2 =                "00000000b9980000h",
alcg_5 =                "0000e30000000088l",
alcgr_2 =               "00000000b9880000h",
alsih_3 =               "0000cc0a00000000n",
alsihn_3 =              "0000cc0b00000000n",
axr_2 =                 "0000000000003600g",
ad_4 =          "000000006a000000j",
adr_2 =                 "0000000000002a00g",
ae_4 =          "000000007a000000j",
aer_2 =                 "0000000000003a00g",
aw_4 =          "000000006e000000j",
awr_2 =                 "0000000000002e00g",
au_4 =          "000000007e000000j",
aur_2 =                 "0000000000003e00g",
n_4 =           "0000000054000000j",
nr_2 =          "0000000000001400g",
ny_5 =          "0000e30000000054l",
ng_5 =          "0000e30000000080l",
ngr_2 =                 "00000000b9800000h",
nihf_3 =                "0000c00a00000000n",
nilf_3 =                "0000c00b00000000n",
bal_4 =                 "0000000045000000j",
balr_2 =                "000000000000500g",
bas_4 =                 "000000004d000000j",
basr_2 =                "0000000000000d00g",
bassm_2 =               "0000000000000c00g",
bsa_2 =                 "00000000b25a0000h",
bsm_2 =                 "0000000000000b00g",
bakr_2 =                "00000000b2400000h",
bsg_2 =                 "00000000b2580000h",
bc_4 =          "0000000047000000k",
bcr_2 =                 "000000000000700g",
bct_4 =                 "0000000046000000j",
bctr_2 =                "000000000000600g",
bctg_5 =                "0000e30000000046l",
bctgr_2 =               "00000000b9460000h",
bxh_4 =                 "0000000086000000q",
bxhg_5 =                "0000eb0000000044s",
bxle_4 =                "0000000087000000q",
bxleg_5 =               "0000eb0000000045s",
brasl_3 =               "0000c00500000000o",
brcl_3 =                "0000c00400000000p",
brcth_3 =               "0000cc0600000000o",
cksm_2 =                "00000000b2410000h",
km_2 =          "00000000b92e0000h",
kmf_2 =                 "00000000b92a0000h",
kmc_2 =                 "00000000b92f0000h",
kmo_2 =                 "00000000b92b0000h",
c_4 =           "0000000059000000j",
cr_2 =          "0000000000001900g",
cy_5 =          "0000e30000000059l",
cg_5 =          "0000e30000000020l",
cgr_2 =                 "00000000b9200000h",
cgf_5 =                 "0000e30000000030l",
cgfr_2 =                "00000000b9300000h",
cxbr_2 =                "00000000b3490000h",
cxtr_2 =                "00000000b3ec0000h",
cxr_2 =                 "00000000b3690000h",
cdbr_2 =                "00000000b3190000h",
cdtr_2 =                "00000000b3e40000h",
cd_4 =          "0000000069000000j",
cdr_2 =                 "0000000000002900g",
cebr_2 =                "00000000b3090000h",
ce_4 =          "0000000079000000j",
cer_2 =                 "0000000000003900g",
kxbr_2 =                "00000000b3480000h",
kxtr_2 =                "00000000b3e80000h",
kdbr_2 =                "00000000b3180000h",
kdtr_2 =                "00000000b3e00000h",
kebr_2 =                "00000000b3080000h",
cs_4 =          "00000000ba000000q",
csy_5 =                 "0000eb0000000014s",
csg_5 =                 "0000eb0000000030s",
csp_2 =                 "00000000b2500000h",
cspg_2 =                "00000000b98a0000h",
cextr_2 =               "00000000b3fc0000h",
cedtr_2 =               "00000000b3f40000h",
cds_4 =                 "00000000bb000000q",
cdsy_5 =                "0000eb0000000031s",
cdsg_5 =                "0000eb000000003es",
ch_4 =          "0000000049000000j",
chy_5 =                 "0000e30000000079l",
cgh_5 =                 "0000e30000000034l",
chrl_3 =                "0000c60500000000o",
cghrl_3 =               "0000c60400000000o",
chf_5 =                 "0000e300000000cdl",
chhr_2 =                "00000000b9cd0000h",
chlr_2 =                "00000000b9dd0000h",
cfi_3 =                 "0000c20d00000000n",
cgfi_3 =                "0000c20c00000000n",
cih_3 =                 "0000cc0d00000000n",
cl_4 =          "0000000055000000j",
clr_2 =                 "0000000000001500g",
cly_5 =                 "0000e30000000055l",
clg_5 =                 "0000e30000000021l",
clgr_2 =                "00000000b9210000h",
clgf_5 =                "0000e30000000031l",
clgfr_2 =               "00000000b9310000h",
clmh_5 =                "0000eb0000000020t",
clm_4 =                 "00000000bd000000r",
clmy_5 =                "0000eb0000000021t",
clhf_5 =                "0000e300000000cfl",
clhhr_2 =               "00000000b9cf0000h",
clhlr_2 =               "00000000b9df0000h",
clfi_3 =                "0000c20f00000000n",
clgfi_3 =               "0000c20e00000000n",
clih_3 =                "0000cc0f00000000n",
clcl_2 =                "0000000000000f00g",
clcle_4 =               "00000000a9000000q",
clclu_5 =               "0000eb000000008fs",
clrl_3 =                "0000c60f00000000o",
clhrl_3 =               "0000c60700000000o",
clgrl_3 =               "0000c60a00000000o",
clghrl_3 =              "0000c60600000000o",
clgfrl_3 =              "0000c60e00000000o",
clst_2 =                "00000000b25d0000h",
crl_3 =                 "0000c60d00000000o",
cgrl_3 =                "0000c60800000000o",
cgfrl_3 =               "0000c60c00000000o",
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
cvb_4 =                 "000000004f000000j",
cvby_5 =                "0000e30000000006l",
cvbg_5 =                "0000e3000000000el",
cvd_4 =                 "000000004e000000j",
cvdy_5 =                "0000e30000000026l",
cvdg_5 =                "0000e3000000002el",
cuxtr_2 =               "00000000b3ea0000h",
cudtr_2 =               "00000000b3e20000h",
cu42_2 =                "00000000b9b30000h",
cu41_2 =                "00000000b9b20000h",
cpya_2 =                "00000000b24d0000h",
d_4 =           "000000005d000000j",
dr_2 =          "0000000000001d00g",
dxbr_2 =                "00000000b34d0000h",
dxr_2 =                 "00000000b22d0000h",
ddbr_2 =                "00000000b31d0000h",
dd_4 =          "000000006d000000j",
ddr_2 =                 "0000000000002d00g",
debr_2 =                "00000000b30d0000h",
de_4 =          "000000007d000000j",
der_2 =                 "0000000000003d00g",
dl_5 =          "0000e30000000097l",
dlr_2 =                 "00000000b9970000h",
dlg_5 =                 "0000e30000000087l",
dlgr_2 =                "00000000b9870000h",
dsg_5 =                 "0000e3000000000dl",
dsgr_2 =                "00000000b90d0000h",
dsgf_5 =                "0000e3000000001dl",
dsgfr_2 =               "00000000b91d0000h",
x_4 =           "0000000057000000j",
xr_2 =          "0000000000001700g",
xy_5 =          "0000e30000000057l",
xg_5 =          "0000e30000000082l",
xgr_2 =                 "00000000b9820000h",
xihf_3 =                "0000c00600000000n",
xilf_3 =                "0000c00700000000n",
ex_4 =          "0000000044000000j",
exrl_3 =                "0000c60000000000o",
ear_2 =                 "00000000b24f0000h",
esea_2 =                "00000000b99d0000h",
eextr_2 =               "00000000b3ed0000h",
eedtr_2 =               "00000000b3e50000h",
ecag_5 =                "0000eb000000004cs",
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
ic_4 =          "0000000043000000j",
icy_5 =                 "0000e30000000073l",
icmh_5 =                "0000eb0000000080t",
icm_4 =                 "00000000bf000000r",
icmy_5 =                "0000eb0000000081t",
iihf_3 =                "0000c00800000000n",
iilf_3 =                "0000c00900000000n",
ipm_2 =                 "00000000b2220000h",
iske_2 =                "00000000b2290000h",
ivsk_2 =                "00000000b2230000h",
l_4 =           "0000000058000000j",
lr_2 =          "0000000000001800g",
ly_5 =          "0000e30000000058l",
lg_5 =          "0000e30000000004l",
lgr_2 =                 "00000000b9040000h",
lgf_5 =                 "0000e30000000014l",
lgfr_2 =                "00000000b9140000h",
lxr_2 =                 "00000000b3650000h",
ld_4 =          "0000000068000000j",
ldr_2 =                 "0000000000002800g",
ldy_5 =                 "0000ed0000000065l",
le_4 =          "0000000078000000j",
ler_2 =                 "0000000000003800g",
ley_5 =                 "0000ed0000000064l",
lam_4 =                 "000000009a000000q",
lamy_5 =                "0000eb000000009as",
la_4 =          "0000000041000000j",
lay_5 =                 "0000e30000000071l",
lae_4 =                 "0000000051000000j",
laey_5 =                "0000e30000000075l",
larl_3 =                "0000c00000000000o",
laa_5 =                 "0000eb00000000f8s",
laag_5 =                "0000eb00000000e8s",
laal_5 =                "0000eb00000000fas",
laalg_5 =               "0000eb00000000eas",
lan_5 =                 "0000eb00000000f4s",
lang_5 =                "0000eb00000000e4s",
lax_5 =                 "0000eb00000000f7s",
laxg_5 =                "0000eb00000000e7s",
lao_5 =                 "0000eb00000000f6s",
laog_5 =                "0000eb00000000e6s",
lt_5 =          "0000e30000000012l",
ltr_2 =                 "0000000000001200g",
ltg_5 =                 "0000e30000000002l",
ltgr_2 =                "00000000b9020000h",
ltgf_5 =                "0000e30000000032l",
ltgfr_2 =               "00000000b9120000h",
ltxbr_2 =               "00000000b3420000h",
ltxtr_2 =               "00000000b3de0000h",
ltxr_2 =                "00000000b3620000h",
ltdbr_2 =               "00000000b3120000h",
ltdtr_2 =               "00000000b3d60000h",
ltdr_2 =                "0000000000002200g",
ltebr_2 =               "00000000b3020000h",
lter_2 =                "0000000000003200g",
lb_5 =          "0000e30000000076l",
lbr_2 =                 "00000000b9260000h",
lgb_5 =                 "0000e30000000077l",
lgbr_2 =                "00000000b9060000h",
lbh_5 =                 "0000e300000000c0l",
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
lctl_4 =                "00000000b7000000q",
lctlg_5 =               "0000eb000000002fs",
fixr_2 =                "00000000b3670000h",
fidr_2 =                "00000000b37f0000h",
fier_2 =                "00000000b3770000h",
ldgr_2 =                "00000000b3c10000h",
lgdr_2 =                "00000000b3cd0000h",
lh_4 =          "0000000048000000j",
lhr_2 =                 "00000000b9270000h",
lhy_5 =                 "0000e30000000078l",
lgh_5 =                 "0000e30000000015l",
lghr_2 =                "00000000b9070000h",
lhh_5 =                 "0000e300000000c4l",
lhrl_3 =                "0000c40500000000o",
lghrl_3 =               "0000c40400000000o",
lfh_5 =                 "0000e300000000cal",
lgfi_3 =                "0000c00100000000n",
lxdbr_2 =               "00000000b3050000h",
lxdr_2 =                "00000000b3250000h",
lxebr_2 =               "00000000b3060000h",
lxer_2 =                "00000000b3260000h",
ldebr_2 =               "00000000b3040000h",
lder_2 =                "00000000b3240000h",
llgf_5 =                "0000e30000000016l",
llgfr_2 =               "00000000b9160000h",
llc_5 =                 "0000e30000000094l",
llcr_2 =                "00000000b9940000h",
llgc_5 =                "0000e30000000090l",
llgcr_2 =               "00000000b9840000h",
llch_5 =                "0000e300000000c2l",
llh_5 =                 "0000e30000000095l",
llhr_2 =                "00000000b9950000h",
llgh_5 =                "0000e30000000091l",
llghr_2 =               "00000000b9850000h",
llhh_5 =                "0000e300000000c6l",
llhrl_3 =               "0000c40200000000o",
llghrl_3 =              "0000c40600000000o",
llihf_3 =               "0000c00e00000000n",
llilf_3 =               "0000c00f00000000n",
llgfrl_3 =              "0000c40e00000000o",
llgt_5 =                "0000e30000000017l",
llgtr_2 =               "00000000b9170000h",
lm_4 =          "0000000098000000q",
lmy_5 =                 "0000eb0000000098s",
lmg_5 =                 "0000eb0000000004s",
lmh_5 =                 "0000eb0000000096s",
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
loc_5 =                 "0000eb00000000f2t",
locg_5 =                "0000eb00000000e2t",
lpq_5 =                 "0000e3000000008fl",
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
lra_4 =                 "00000000b1000000j",
lray_5 =                "0000e30000000013l",
lrag_5 =                "0000e30000000003l",
lrl_3 =                 "0000c40d00000000o",
lgrl_3 =                "0000c40800000000o",
lgfrl_3 =               "0000c40c00000000o",
lrvh_5 =                "0000e3000000001fl",
lrv_5 =                 "0000e3000000001el",
lrvr_2 =                "00000000b91f0000h",
lrvg_5 =                "0000e3000000000fl",
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
mvcle_4 =               "00000000a8000000q",
mvclu_5 =               "0000eb000000008es",
mvpg_2 =                "00000000b2540000h",
mvst_2 =                "00000000b2550000h",
m_4 =           "000000005c000000j",
mfy_5 =                 "0000e3000000005cl",
mr_2 =          "0000000000001c00g",
mxbr_2 =                "00000000b34c0000h",
mxr_2 =                 "0000000000002600g",
mdbr_2 =                "00000000b31c0000h",
md_4 =          "000000006c000000j",
mdr_2 =                 "0000000000002c00g",
mxdbr_2 =               "00000000b3070000h",
mxd_4 =                 "0000000067000000j",
mxdr_2 =                "0000000000002700g",
meebr_2 =               "00000000b3170000h",
meer_2 =                "00000000b3370000h",
mdebr_2 =               "00000000b30c0000h",
mde_4 =                 "000000007c000000j",
mder_2 =                "0000000000003c00g",
me_4 =          "000000007c000000j",
mer_2 =                 "0000000000003c00g",
mh_4 =          "000000004c000000j",
mhy_5 =                 "0000e3000000007cl",
mlg_5 =                 "0000e30000000086l",
mlgr_2 =                "00000000b9860000h",
ml_5 =          "0000e30000000096l",
mlr_2 =                 "00000000b9960000h",
ms_4 =          "0000000071000000j",
msr_2 =                 "00000000b2520000h",
msy_5 =                 "0000e30000000051l",
msg_5 =                 "0000e3000000000cl",
msgr_2 =                "00000000b90c0000h",
msgf_5 =                "0000e3000000001cl",
msgfr_2 =               "00000000b91c0000h",
msfi_3 =                "0000c20100000000n",
msgfi_3 =               "0000c20000000000n",
o_4 =           "0000000056000000j",
or_2 =	        "0000000000001600g",
oy_5 =          "0000e30000000056l",
og_5 =          "0000e30000000081l",
ogr_2 =                 "00000000b9810000h",
oihf_3 =                "0000c00c00000000n",
oilf_3 =                "0000c00d00000000n",
pgin_2 =                "00000000b22e0000h",
pgout_2 =               "00000000b22f0000h",
pcc_2 =                 "00000000b92c0000h",
pckmo_2 =               "00000000b9280000h",
pfmf_2 =                "00000000b9af0000h",
ptf_2 =                 "00000000b9a20000h",
popcnt_2 =              "00000000b9e10000h",
pfd_5 =                 "0000e30000000036m",
pfdrl_3 =               "0000c60200000000p",
pt_2 =          "00000000b2280000h",
pti_2 =                 "00000000b99e0000h",
palb_2 =                "00000000b2480000h",
rrbe_2 =                "00000000b22a0000h",
rrbm_2 =                "00000000b9ae0000h",
rll_5 =                 "0000eb000000001ds",
rllg_5 =                "0000eb000000001cs",
srst_2 =                "00000000b25e0000h",
srstu_2 =               "00000000b9be0000h",
sar_2 =                 "00000000b24e0000h",
sfpc_2 =                "00000000b3840000h",
sfasr_2 =               "00000000b3850000h",
spm_2 =                 "000000000000400g",
ssar_2 =                "00000000b2250000h",
ssair_2 =               "00000000b99f0000h",
slda_4 =                "000000008f000000q",
sldl_4 =                "000000008d000000q",
sla_4 =                 "000000008b000000q",
slak_5 =                "0000eb00000000dds",
slag_5 =                "0000eb000000000bs",
sll_4 =                 "0000000089000000q",
sllk_5 =                "0000eb00000000dfs",
sllg_5 =                "0000eb000000000ds",
srda_4 =                "000000008e000000q",
srdl_4 =                "000000008c000000q",
sra_4 =                 "000000008a000000q",
srak_5 =                "0000eb00000000dcs",
srag_5 =                "0000eb000000000as",
srl_4 =                 "0000000088000000q",
srlk_5 =                "0000eb00000000des",
srlg_5 =                "0000eb000000000cs",
sqxbr_2 =               "00000000b3160000h",
sqxr_2 =                "00000000b3360000h",
sqdbr_2 =               "00000000b3150000h",
sqdr_2 =                "00000000b2440000h",
sqebr_2 =               "00000000b3140000h",
sqer_2 =                "00000000b2450000h",
st_4 =          "0000000050000000j",
sty_5 =                 "0000e30000000050l",
stg_5 =                 "0000e30000000024l",
std_4 =                 "0000000060000000j",
stdy_5 =                "0000ed0000000067l",
ste_4 =                 "0000000070000000j",
stey_5 =                "0000ed0000000066l",
stam_4 =                "000000009b000000q",
stamy_5 =               "0000eb000000009bs",
stc_4 =                 "0000000042000000j",
stcy_5 =                "0000e30000000072l",
stch_5 =                "0000e300000000c3l",
stcmh_5 =               "0000eb000000002ct",
stcm_4 =                "00000000be000000r",
stcmy_5 =               "0000eb000000002dt",
stctl_4 =               "00000000b6000000q",
stctg_5 =               "0000eb0000000025s",
sth_4 =                 "0000000040000000j",
sthy_5 =                "0000e30000000070l",
sthh_5 =                "0000e300000000c7l",
sthrl_3 =               "0000c40700000000o",
stfh_5 =                "0000e300000000cbl",
stm_4 =                 "0000000090000000q",
stmy_5 =                "0000eb0000000090s",
stmg_5 =                "0000eb0000000024s",
stmh_5 =                "0000eb0000000026s",
stoc_5 =                "0000eb00000000f3t",
stocg_5 =               "0000eb00000000e3t",
stpq_5 =                "0000e3000000008el",
strl_3 =                "0000c40f00000000o",
stgrl_3 =               "0000c40b00000000o",
strvh_5 =               "0000e3000000003fl",
strv_5 =                "0000e3000000003el",
strvg_5 =               "0000e3000000002fl",
stura_2 =               "00000000b2460000h",
sturg_2 =               "00000000b9250000h",
s_4 =           "000000005b000000j",
sr_2 =          "0000000000001b00g",
sy_5 =          "0000e3000000005bl",
sg_5 =          "0000e30000000009l",
sgr_2 =                 "00000000b9090000h",
sgf_5 =                 "0000e30000000019l",
sgfr_2 =                "00000000b9190000h",
sxbr_2 =                "00000000b34b0000h",
sdbr_2 =                "00000000b31b0000h",
sebr_2 =                "00000000b30b0000h",
sh_4 =          "000000004b000000j",
shy_5 =                 "0000e3000000007bl",
sl_4 =          "000000005f000000j",
slr_2 =                 "0000000000001f00g",
sly_5 =                 "0000e3000000005fl",
slg_5 =                 "0000e3000000000bl",
slgr_2 =                "00000000b90b0000h",
slgf_5 =                "0000e3000000001bl",
slgfr_2 =               "00000000b91b0000h",
slfi_3 =                "0000c20500000000n",
slgfi_3 =               "0000c20400000000n",
slb_5 =                 "0000e30000000099l",
slbr_2 =                "00000000b9990000h",
slbg_5 =                "0000e30000000089l",
slbgr_2 =               "00000000b9890000h",
sxr_2 =                 "0000000000003700g",
sd_4 =          "000000006b000000j",
sdr_2 =                 "0000000000002b00g",
se_4 =          "000000007b000000j",
ser_2 =                 "0000000000003b00g",
su_4 =          "000000007f000000j",
sur_2 =                 "0000000000003f00g",
sw_4 =          "000000006f000000j",
swr_2 =                 "0000000000002f00g",
tar_2 =                 "00000000b24c0000h",
tb_2 =          "00000000b22c0000h",
trace_4 =               "0000000099000000q",
tracg_5 =               "0000eb000000000fs",
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
      op1 = op1 + shl(parse_reg(param[1], 8))
      wputhw(op1); wputhw(op2)
      -- TODO: parse param[2] using parse_mem_bx, need to put x into op1, b and d
      -- into op2, emitting an action for the DISP12 afterwards if necessary.
    elseif p == "k" then

    elseif p == "l" then
      
    elseif p == "m" then
      
    elseif p == "n" then

    elseif p == "z" then
       op2 = op2 + parse_gpr(params[1])
       wputhw(op2)
    end
  end

end
function op_template(params, template, nparams)
  if not params then return template:gsub("%x%x%x%x%x%x%x%x", "") end
  -- Limit number of section buffer positions used by a single dasm_put().
  -- A single opcode needs a maximum of 3 positions.
  if secpos+3 > maxsecpos then wflush() end
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
