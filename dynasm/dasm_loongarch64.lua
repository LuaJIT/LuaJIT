------------------------------------------------------------------------------
-- DynASM LoongArch module.
--
-- Copyright (C) 2005-2022 Mike Pall. All rights reserved.
-- See dynasm.lua for full copyright notice.
------------------------------------------------------------------------------

-- Module information:
local _info = {
  arch =	"loongarch64",
  description =	"DynASM LoongArch64 module",
  version =	"1.5.0",
  vernum =	 10500,
  release =	"2021-05-02",
  author =	"Mike Pall",
  license =	"MIT",
}

-- Exported glue functions for the arch-specific module.
local _M = { _info = _info }

-- Cache library functions.
local type, tonumber, pairs, ipairs = type, tonumber, pairs, ipairs
local assert, setmetatable = assert, setmetatable
local _s = string
local sub, format, byte, char = _s.sub, _s.format, _s.byte, _s.char
local match, gmatch = _s.match, _s.gmatch
local concat, sort = table.concat, table.sort
local bit = bit or require("bit")
local band, shl, shr, sar = bit.band, bit.lshift, bit.rshift, bit.arshift
local tohex = bit.tohex

-- Inherited tables and callbacks.
local g_opt, g_arch
local wline, werror, wfatal, wwarn

-- Action name list.
-- CHECK: Keep this in sync with the C code!
local action_names = {
  "STOP", "SECTION", "ESC", "REL_EXT",
  "ALIGN", "REL_LG", "LABEL_LG",
  "REL_PC", "LABEL_PC", "IMM", "IMM2",
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
  wputxw(0xff000000 + w * 0x10000 + (val or 0))
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
  if n >= 0xff000000 then waction("ESC") end
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
local map_archdef = { sp="r3", ra="r1" } -- Ext. register name -> int. name.

local map_type = {}		-- Type name -> { ctype, reg }
local ctypenum = 0		-- Type number (for Dt... macros).

-- Reverse defines for registers.
function _M.revdef(s)
  if s == "r3" then return "sp"
  elseif s == "r1" then return "ra" end
  return s
end

------------------------------------------------------------------------------

-- Template strings for LoongArch instructions.
local map_op = {
  ["clo.w_2"] =		"00001000DJ",
  ["clz.w_2"] =		"00001400DJ",
  ["cto.w_2"] =		"00001800DJ",
  ["ctz.w_2"] =		"00001c00DJ",
  ["clo.d_2"] =		"00002000DJ",
  ["clz.d_2"] =		"00002400DJ",
  ["cto.d_2"] =		"00002800DJ",
  ["ctz.d_2"] =		"00002c00DJ",
  ["revb.2h_2"] =	"00003000DJ",
  ["revb.4h_2"] =	"00003400DJ",
  ["revb.2w_2"] =	"00003800DJ",
  ["revb.d_2"] = 	"00003c00DJ",
  ["revh.2w_2"] =	"00004000DJ",
  ["revh.d_2"] =	"00004400DJ",
  ["bitrev.4b_2"] =	"00004800DJ",
  ["bitrev.8b_2"] =	"00004c00DJ",
  ["bitrev.w_2"] =	"00005000DJ",
  ["bitrev.d_2"] =	"00005400DJ",
  ["ext.w.h_2"] =	"00005800DJ",
  ["ext.w.b_2"] =	"00005c00DJ",

  ["add.w_3"] =		"00100000DJK",
  ["add.d_3"] =		"00108000DJK",
  ["sub.w_3"] =		"00110000DJK",
  ["sub.d_3"] =		"00118000DJK",
  slt_3 = 		"00120000DJK",
  sltu_3 =		"00128000DJK",
  maskeqz_3 = 		"00130000DJK",
  masknez_3 =		"00138000DJK",

  nor_3 =		"00140000DJK",
  and_3 = 		"00148000DJK",
  or_3 = 		"00150000DJK",
  xor_3 = 		"00158000DJK",
  orn_3 =		"00160000DJK",
  andn_3 = 		"00168000DJK",
  ["sll.w_3"] =		"00170000DJK",
  ["srl.w_3"] =		"00178000DJK",
  ["sra.w_3"] = 	"00180000DJK",
  ["sll.d_3"] =		"00188000DJK",
  ["srl.d_3"] =		"00190000DJK",
  ["sra.d_3"] =		"00198000DJK",
  ["rotr.w_3"] =	"001b0000DJK",
  ["rotr.d_3"] =	"001b8000DJK",
  ["mul.w_3"] =		"001c0000DJK",
  ["mulh.w_3"] = 	"001c8000DJK",
  ["mulh.wu_3"] =	"001d0000DJK",
  ["mul.d_3"] =		"001d8000DJK",
  ["mulh.d_3"] =	"001e0000DJK",
  ["mulh.du_3"] =	"001e8000DJK",
  ["mulw.d.w_3"] =	"001f0000DJK",
  ["mulw.d.wu_3"] =	"001f8000DJK",

  ["fabs.h_2"] =	"01140000FG",
  ["fabs.s_2"] = 	"01140400FG",
  ["fabs.d_2"] =	"01140800FG",
  ["fneg.h_2"] =	"01141000FG",
  ["fneg.s_2"] =	"01141400FG",
  ["fneg.d_2"] =	"01141800FG",
  ["flogb.h_2"] =	"01142000FG",
  ["flogb.s_2"] =	"01142400FG",
  ["flogb.d_2"] =	"01142800FG",
  ["fclass.h_2"] =	"01143000FG",
  ["fclass.s_2"] =	"01143400FG",
  ["fclass.d_2"] =	"01143800FG",
  ["fsqrt.h_2"] =	"01144000FG",
  ["fsqrt.s_2"] =	"01144400FG",
  ["fsqrt.d_2"] =	"01144800FG",
  ["frecip.h_2"] = 	"01145000FG",
  ["frecip.s_2"] =	"01145400FG",
  ["frecip.d_2"] =	"01145800FG",
  ["frsqrt.h_2"] =	"01146000FG",
  ["frsqrt.s_2"] =	"01146400FG",
  ["frsqrt.d_2"] =	"01146800FG",
  ["frecipe.h_2"] =	"01147000FG",
  ["frecipe.s_2"] =	"01147400FG",
  ["frecipe.d_2"] =	"01147800FG",
  ["frsqrte.h_2"] =	"01148000FG",
  ["frsqrte.s_2"] =	"01148400FG",
  ["frsqrte.d_2"] =	"01148800FG",

  ["fmov.h_2"] =	"01149000FG",
  ["fmov.s_2"] =	"01149400FG",
  ["fmov.d_2"] =	"01149800FG",
  ["movgr2fr.h_2"] =	"0114a000FJ",
  ["movgr2fr.w_2"] =	"0114a400FJ",
  ["movgr2fr.d_2"] =	"0114a800FJ",
  ["movgr2frh.w_2"] =	"0114ac00FJ",
  ["movfr2gr.h_2"] =	"0114b000DG",
  ["movfr2gr.s_2"] =	"0114b400DG",
  ["movfr2gr.d_2"] =	"0114b800DG",
  ["movfrh2gr.s_2"] =	"0114bc00DG",
  movgr2fcsr_2 =	"0114c000SG",
  movfcsr2gr_2 =	"0114c800FR",
  movfr2cf_2 =		"0114d000EG",
  movcf2fr_2 =		"0114d400FA",
  movgr2cf_2 =		"0114d800EG",
  movcf2gr_2 =		"0114dc00DA",
  ["fcvt.ld.d_2"] =	"0114e000FG",
  ["fcvt.ud.d_2"] =	"0114e400FG",
  ["fcvt.s.d_2"] = 	"01191800FG",
  ["fcvt.d.s_2"] =	"01192400FG",
  ["ftintrm.w.s_2"] =	"011a0400FG",
  ["ftintrm.w.d_2"] =	"011a0800FG",
  ["ftintrm.l.s_2"] =	"011a2400FG",
  ["ftintrm.l.d_2"] =	"011a2800FG",
  ["ftintrp.w.s_2"] =	"011a4400FG",
  ["ftintrp.w.d_2"] =	"011a4800FG",
  ["ftintrp.l.s_2"] =	"011a6400FG",
  ["ftintrp.l.d_2"] =	"011a6800FG",
  ["ftintrz.w.s_2"] =	"011a8400FG",
  ["ftintrz.w.d_2"] =	"011a8800FG",
  ["ftintrz.l.s_2"] =	"011aa400FG",
  ["ftintrz.l.d_2"] =	"011aa800FG",
  ["ftintrne.w.s_2"] =	"011ac400FG",
  ["ftintrne.w.d_2"] =	"011ac800FG",
  ["ftintrne.l.s_2"] =	"011ae400FG",
  ["ftintrne.l.d_2"] =	"011ae800FG",
  ["ftint.w.s_2"] =	"011b0400FG",
  ["ftint.w.d_2"] =	"011b0800FG",
  ["ftint.l.s_2"] =	"011b2400FG",
  ["ftint.l.d_2"] =	"011b2800FG",
  ["ffint.s.w_2"] =	"011d1000FG",
  ["ffint.s.l_2"] =	"011d1800FG",
  ["ffint.d.w_2"] =	"011d2000FG",
  ["ffint.d.l_2"] =	"011d2800FG",
  ["frint.s_2"] =	"011e4400FG",
  ["frint.d_2"] =	"011e4800FG",

  ["fadd.h_3"] =	"01000000FGH",
  ["fadd.s_3"] =	"01008000FGH",
  ["fadd.d_3"] =	"01010000FGH",
  ["fsub.h_3"] =	"01020000FGH",
  ["fsub.s_3"] =	"01028000FGH",
  ["fsub.d_3"] =	"01030000FGH",
  ["fmul.h_3"] =	"01040000FGH",
  ["fmul.s_3"] =	"01048000FGH",
  ["fmul.d_3"] =	"01050000FGH",
  ["fdiv.h_3"] =	"01060000FGH",
  ["fdiv.s_3"] =	"01068000FGH",
  ["fdiv.d_3"] =	"01070000FGH",
  ["fmax.h_3"] =	"01080000FGH",
  ["fmax.s_3"] =	"01088000FGH",
  ["fmax.d_3"] =	"01090000FGH",
  ["fmin.h_3"] = 	"010a0000FGH",
  ["fmin.s_3"] =	"010a8000FGH",
  ["fmin.d_3"] =	"010b0000FGH",
  ["fmaxa.h_3"] =	"010c0000FGH",
  ["fmaxa.s_3"] =	"010c8000FGH",
  ["fmaxa.d_3"] =	"010d0000FGH",
  ["fmina.h_3"] =	"010e0000FGH",
  ["fmina.s_3"] =	"010e8000FGH",
  ["fmina.d_3"] =	"010f0000FGH",
  ["fscaleb.h_3"] =	"01100000FGH",
  ["fscaleb.s_3"] =	"01108000FGH",
  ["fscaleb.d_3"] =	"01110000FGH",
  ["fcopysign.h_3"] =	"01120000FGH",
  ["fcopysign.s_3"] =	"01128000FGH",
  ["fcopysign.d_3"] =	"01130000FGH",

  ["fmadd.s_4"] =	"08100000FGHi",
  ["fmadd.d_4"] =	"08200000FGHi",
  ["fnmadd.d_4"] =	"08a00000FGHi",
  ["fmsub.s_4"] =	"08500000FGHi",
  ["fmsub.d_4"] =	"08600000FGHi",
  ["fnmsub.d_4"] =	"08e00000FGHi",

  ["alsl.w_4"] =	"00040000DJKQ",
  ["alsl.wu_4"] =	"00060000DJKQ",
  ["alsl.d_4"] =	"002c0000DJKQ",
  ["bytepick.w_4"] =	"00080000DJKQ",
  ["bytepick.d_4"] =	"000c0000DJKB",

  ["div.w_3"] = 	"00200000DJK",
  ["mod.w_3"] =		"00208000DJK",
  ["div.wu_3"] =	"00210000DJK",
  ["mod.wu_3"] =	"00218000DJK",
  ["div.d_3"] =		"00220000DJK",
  ["mod.d_3"] =		"00228000DJK",
  ["div.du_3"] =	"00230000DJK",
  ["mod.du_3"] =	"00238000DJK",
  ["crc.w.b.w_3"] =	"00240000DJK",
  ["crc.w.h.w_3"] =	"00248000DJK",
  ["crc.w.w.w_3"] =	"00250000DJK",
  ["crc.w.d.w_3"] =	"00258000DJK",
  ["crcc.w.b.w_3"] =	"00260000DJK",
  ["crcc.w.h.w_3"] =	"00268000DJK",
  ["crcc.w.w.w_3"] =	"00270000DJK",
  ["crcc.w.d.w_3"] =	"00278000DJK",

  break_1 =		"002a0000C",
  syscall_1 =		"002b0000C",

  ["slli.w_3"] =	"00408000DJU",
  ["slli.d_3"] =	"00410000DJV",
  ["srli.w_3"] =	"00448000DJU",
  ["srli.d_3"] =	"00450000DJV",
  ["srai.w_3"] =	"00488000DJU",
  ["srai.d_3"] =	"00490000DJV",
  ["rotri.w_3"] =	"004c8000DJU",
  ["rotri.d_3"] =	"004d0000DJV",

  ["bstrins.w_4"] =	"00600000DJMU",
  ["bstrpick.w_4"] =	"00608000DJMU",
  ["bstrins.d_4"] = 	"00800000DJNV",
  ["bstrpick.d_4"] =	"00c00000DJNV",
  slti_3 =		"02000000DJX",
  sltui_3 =		"02400000DJX",
  ["addi.w_3"] =	"02800000DJX",
  ["addi.d_3"] =	"02c00000DJX",
  ["lu52i.d_3"] =	"03000000DJX",
  andi_3 =		"03400000DJT",
  ori_3 =		"03800000DJT",
  xori_3 = 		"03c00000DJT",
  ["lu12i.w_2"] =	"14000000DZ",
  ["lu32i.d_2"] =	"16000000DZ",
  pcaddi_2 =		"18000000DZ",
  pcalau12i_2 = 	"1a000000DZ",
  pcaddu12i_2 =		"1c000000DZ",
  pcaddu18i_2 = 	"1e000000DZ",

  ["ldx.b_3"] =		"38000000DJK",
  ["ldx.h_3"] =		"38040000DJK",
  ["ldx.w_3"] =		"38080000DJK",
  ["ldx.d_3"] =		"380c0000DJK",
  ["stx.b_3"] =		"38100000DJK",
  ["stx.h_3"] =		"38140000DJK",
  ["stx.w_3"] =		"38180000DJK",
  ["stx.d_3"] =		"381c0000DJK",
  ["ldx.bu_3"] =	"38200000DJK",
  ["ldx.hu_3"] =	"38240000DJK",
  ["ldx.wu_3"] =	"38280000DJK",
  ["fldx.s_3"] =	"38300000FJK",
  ["fldx.d_3"] =	"38340000FJK",
  ["fstx.s_3"] =	"38380000FJK",
  ["fstx.d_3"] =	"383c0000FJK",
  ["fldgt.s_3"] =	"38740000FJK",
  ["fldgt.d_3"] =	"38748000FJK",
  ["fldle.s_3"] =	"38750000FJK",
  ["fldle.d_3"] =	"38758000FJK",
  ["fstgt.s_3"] =	"38760000FJK",
  ["fstgt.d_3"] =	"38768000FJK",
  ["fstle.s_3"] =	"38770000FJK",
  ["fstle.d_3"] =	"38778000FJK",
  ["ldgt.b_3"] =	"38780000DJK",
  ["ldgt.h_3"] =	"38788000DJK",
  ["ldgt.w_3"] =	"38790000DJK",
  ["ldgt.d_3"] =	"38798000DJK",
  ["ldle.b_3"] =	"387a0000DJK",
  ["ldle.h_3"] =	"387a8000DJK",
  ["ldle.w_3"] =	"387b0000DJK",
  ["ldle.d_3"] =	"387b8000DJK",
  ["stgt.b_3"] =	"387c0000DJK",
  ["stgt.h_3"] =	"387c8000DJK",
  ["stgt.w_3"] =	"387d0000DJK",
  ["stgt.d_3"] =	"387d8000DJK",
  ["stle.b_3"] =	"387e0000DJK",
  ["stle.h_3"] =	"387e8000DJK",
  ["stle.w_3"] =	"387f0000DJK",
  ["stle.d_3"] =	"387f8000DJK",

  ["ll.w_3"] =		"20000000DJW",
  ["sc.w_3"] =		"21000000DJW",
  ["ll.d_3"] =		"22000000DJW",
  ["sc.d_3"] =		"23000000DJW",
  ["ldptr.w_3"] =	"24000000DJW",
  ["stptr.w_3"] =	"25000000DJW",
  ["ldptr.d_3"] =	"26000000DJW",
  ["stptr.d_3"] =	"27000000DJW",

  ["ld.b_2"] =		"28000000Do",
  ["ld.h_2"] =		"28400000Do",
  ["ld.w_2"] =		"28800000Do",
  ["ld.d_2"] =		"28c00000Do",
  ["st.b_2"] =		"29000000Do",
  ["st.h_2"] =		"29400000Do",
  ["st.w_2"] =		"29800000Do",
  ["st.d_2"] =		"29c00000Do",
  ["ld.bu_2"] =		"2a000000Do",
  ["ld.hu_2"] =		"2a400000Do",
  ["ld.wu_2"] =		"2a800000Do",
  ["ldx.d_3"] =		"380c0000DJK",
  ["stx.d_3"] =		"381c0000DJK",
  ["fld.s_2"] =		"2b000000Fo",
  ["fst.s_2"] =		"2b400000Fo",
  ["fld.d_2"] =		"2b800000Fo",
  ["fst.d_2"] =		"2bc00000Fo",

  ["fcmp.caf.s_3"] =	"0c100000EGH",
  ["fcmp.saf.s_3"] =	"0c108000EGH",
  ["fcmp.clt.s_3"] =	"0c110000EGH",
  ["fcmp.slt.s_3"] =	"0c118000EGH",
  ["fcmp.ceq.s_3"] =	"0c120000EGH",
  ["fcmp.seq.s_3"] =	"0c128000EGH",
  ["fcmp.cle.s_3"] =	"0c130000EGH",
  ["fcmp.sle.s_3"] =	"0c138000EGH",
  ["fcmp.cun.s_3"] =	"0c140000EGH",
  ["fcmp.sun.s_3"] =	"0c148000EGH",
  ["fcmp.cult.s_3"] =	"0c150000EGH",
  ["fcmp.sult.s_3"] =	"0c158000EGH",
  ["fcmp.cueq.s_3"] =	"0c160000EGH",
  ["fcmp.sueq.s_3"] =	"0c168000EGH",
  ["fcmp.cule.s_3"] =	"0c170000EGH",
  ["fcmp.sule.s_3"] =	"0c178000EGH",
  ["fcmp.cne.s_3"] =	"0c180000EGH",
  ["fcmp.sne.s_3"] =	"0c188000EGH",
  ["fcmp.cor.s_3"] =	"0c1a0000EGH",
  ["fcmp.sor.s_3"] =	"0c1a8000EGH",
  ["fcmp.cune.s_3"] =	"0c1c0000EGH",
  ["fcmp.sune.s_3"] =	"0c1c8000EGH",
  ["fcmp.caf.d_3"] =	"0c200000EGH",
  ["fcmp.saf.d_3"] =	"0c208000EGH",
  ["fcmp.clt.d_3"] =	"0c210000EGH",
  ["fcmp.slt.d_3"] =	"0c218000EGH",
  ["fcmp.ceq.d_3"] =	"0c220000EGH",
  ["fcmp.seq.d_3"] =	"0c228000EGH",
  ["fcmp.cle.d_3"] =	"0c230000EGH",
  ["fcmp.sle.d_3"] =	"0c238000EGH",
  ["fcmp.cun.d_3"] =	"0c240000EGH",
  ["fcmp.sun.d_3"] =	"0c248000EGH",
  ["fcmp.cult.d_3"] =	"0c250000EGH",
  ["fcmp.sult.d_3"] =	"0c258000EGH",
  ["fcmp.cueq.d_3"] =	"0c260000EGH",
  ["fcmp.sueq.d_3"] =	"0c268000EGH",
  ["fcmp.cule.d_3"] =	"0c270000EGH",
  ["fcmp.sule.d_3"] =	"0c278000EGH",
  ["fcmp.cne.d_3"] =	"0c280000EGH",
  ["fcmp.sne.d_3"] =	"0c288000EGH",
  ["fcmp.cor.d_3"] =	"0c2a0000EGH",
  ["fcmp.sor.d_3"] =	"0c2a8000EGH",
  ["fcmp.cune.d_3"] =	"0c2c0000EGH",
  ["fcmp.sune.d_3"] =	"0c2c8000EGH",

  fsel_4 =		"0d000000FGHI",

  ["addu16i.d_3"] = 	"10000000DJY",
  beqz_2 =		"40000000JL",
  bnez_2 = 		"44000000JL",
  bceqz_2 = 		"48000000AL",
  bcnez_2 = 		"48000100AL",
  jirl_3 =		"4c000000DJa",
  b_1 =			"50000000P",
  bl_1 =		"54000000P",
  beq_3 =		"58000000JDO",
  bne_3 = 		"5c000000JDO",
  blt_3 = 		"60000000JDO",
  bge_3 = 		"64000000JDO",
  bltu_3 = 		"68000000JDO",
  bgeu_3 = 		"6c000000JDO",
}

------------------------------------------------------------------------------

local function parse_gpr(expr)
  local tname, ovreg = match(expr, "^([%w_]+):(r[1-3]?[0-9])$")
  local tp = map_type[tname or expr]
  if tp then
    local reg = ovreg or tp.reg
    if not reg then
      werror("type `"..(tname or expr).."' needs a register override")
    end
    expr = reg
  end
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

local function parse_fcsr(expr)
  local r = match(expr, "^fcsr([0-3])$")
  if r then
    r = tonumber(r)
    return r
  end
  werror("bad register name `"..expr.."'")
end

local function parse_fcc(expr)
  local r = match(expr, "^fcc([0-7])$")
  if r then
    r = tonumber(r)
    return r
  end
  werror("bad register name `"..expr.."'")
end

local function parse_imm(imm, bits, shift, scale, signed, action)
  local n = tonumber(imm)
  if n then
    local m = sar(n, scale)
    if shl(m, scale) == n then
      if signed then
	local s = sar(m, bits-1)
	if s == 0 or s == 1 then return shl(m, shift)
	elseif s == -1 then return shl(m + shl(1, bits), shift) end
      else
	if sar(m, bits) == 0 then return shl(m, shift) end
      end
    end
    werror("out of range immediate1 `"..imm.."'")
  elseif match(imm, "^[rf]([1-3]?[0-9])$") or
	 match(imm, "^([%w_]+):([rf][1-3]?[0-9])$") then
    werror("expected immediate operand, got register")
  else
    waction(action or "IMM",
	    (signed and 32768 or 0)+shl(scale, 10)+shl(bits, 5)+shift, imm)
    return 0
  end
end

local function parse_imm21or26(imm, i)
  local n = tonumber(imm)
  if n then
    -- signed
    local m = sar(n, 0)
    if shl(m, 0) == n then
      local s = sar(m, i-1)
      if s == 0 then
        return shl(sub(m, 1, 16), 10) + shl(sub(m, 17, i), 0)
      elseif s == -1 then
        return shl(sub(m, 1, 16), 10) + shl(sub(m, 17, i), 0)
      end
    end
    werror("out of range immediate2 `"..imm.."'")
  else
    waction("IMM2", 0, imm)
    return 0
  end
end

local function parse_disp(disp)
  local imm, reg = match(disp, "^(.*)%(([%w_:]+)%)$")
  if imm then
    local r = shl(parse_gpr(reg), 5)
    local extname = match(imm, "^extern%s+(%S+)$")
    if extname then
      waction("REL_EXT", map_extern[extname], nil, 1)
      return r
    else
      return r + parse_imm(imm, 12, 10, 0, true)
    end
  end
  local reg, tailr = match(disp, "^([%w_:]+)%s*(.*)$")
  if reg and tailr ~= "" then
    local r, tp = parse_gpr(reg)
    if tp then
      waction("IMM", 32768+12*32+10, format(tp.ctypefmt, tailr))
      return shl(r, 5)
    end
  end
  werror("bad displacement `"..disp.."'")
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
  if shr(op, 26) == 0x16 or shr(op, 26) == 0x17 or shr(op, 26) == 0x18 or
     shr(op, 26) == 0x19 or shr(op, 26) == 0x1a or shr(op, 26) == 0x1b then
    return 0 -- BEQ, BNE, BLT, BGE, BLTU, BGEU
  elseif shr(op, 26) == 0x10 or shr(op, 26) == 0x11 or shr(op, 26) == 0x12 then
    return 0x5000 -- BEQZ, BNEZ, BCEQZ, BCNEZ
  elseif band(op, 0xf8000000) == 0x50000000 then return 0xa000 --B, BL
  else
    assert(false, "unknown branch type")
  end
end

------------------------------------------------------------------------------

-- Handle opcodes defined with template strings.
map_op[".template__"] = function(params, template, nparams)
  if not params then return sub(template, 9) end
  local op = tonumber(sub(template, 1, 8), 16)
  local n = 1

  -- Limit number of section buffer positions used by a single dasm_put().
  -- A single opcode needs a maximum of 2 positions (ins/ext).
  if secpos+2 > maxsecpos then wflush() end
  local pos = wpos()

  -- Process each character.
  for p in gmatch(sub(template, 9), ".") do
    if p == "D" then
      op = op + shl(parse_gpr(params[n]), 0); n = n + 1
    elseif p == "J" then
      op = op + shl(parse_gpr(params[n]), 5); n = n + 1
    elseif p == "K" then
      op = op + shl(parse_gpr(params[n]), 10); n = n + 1
    elseif p == "F" then
      op = op + shl(parse_fpr(params[n]), 0); n = n + 1
    elseif p == "G" then
      op = op + shl(parse_fpr(params[n]), 5); n = n + 1
    elseif p == "H" then
      op = op + shl(parse_fpr(params[n]), 10); n = n + 1
    elseif p == "i" then
      op = op + shl(parse_fpr(params[n]), 15); n = n + 1
    elseif p == "I" then
      op = op + shl(parse_fcc(params[n]), 15); n = n + 1
    elseif p == "A" then
      op = op + shl(parse_fcc(params[n]), 5); n = n + 1
    elseif p == "E" then
      op = op + shl(parse_fcc(params[n]), 0); n = n + 1
    elseif op == "S" then
      op = op + shl(parse_fcsr(params[n]), 0); n = n + 1
    elseif op == "R" then
      op = op + shl(parse_fcsr(params[n]), 5); n = n + 1
    elseif p == "U" then
      op = op + parse_imm(params[n], 5, 10, 0, false); n = n + 1
    elseif p == "V" then
      op = op + parse_imm(params[n], 6, 10, 0, false); n = n + 1
    elseif p == "W" then
      op = op + parse_imm(params[n], 14, 10, 0, true); n = n + 1
    elseif p == "X" then
      op = op + parse_imm(params[n], 12, 10, 0, true); n = n + 1
    elseif p == "o" then
      op = op + parse_disp(params[n]); n = n + 1
    elseif p == "Y" then
      op = op + parse_imm(params[n], 16, 10, 0, true); n = n + 1
    elseif p == "Z" then
      op = op + parse_imm(params[n], 20, 5, 0, true); n = n + 1
    elseif p == "T" then
      op = op + parse_imm(params[n], 12, 10, 0, false); n = n + 1
    elseif p == "C" then
      op = op + parse_imm(params[n], 15, 0, 0, false); n = n + 1
    elseif p == "Q" then
      op = op + parse_imm(params[n], 2, 15, 0, false); n = n + 1
    elseif p == "B" then
      op = op + parse_imm(params[n], 3, 15, 0, false); n = n + 1
    elseif p == "M" then
      op = op + parse_imm(params[n], 5, 16, 0, false); n = n + 1
    elseif p == "N" then
      op = op + parse_imm(params[n], 6, 16, 0, false); n = n + 1
    elseif p == "O" or p == "L" or p == "P" then
      local mode, m, s = parse_label(params[n], false)
      local v = branch_type(op)
      waction("REL_"..mode, m+v, s, 1)
      n = n + 1
    elseif p == "a" then
      op = op + parse_imm(params[n], 16, 10, 0, true); n = n + 1
    else
      assert(false)
    end
  end
  wputpos(pos, op)
end

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

