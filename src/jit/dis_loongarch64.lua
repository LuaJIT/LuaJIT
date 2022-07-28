----------------------------------------------------------------------------
-- LuaJIT LoongArch64 disassembler module.
--
-- Copyright (C) 2005-2022 Mike Pall. All rights reserved.
-- Released under the MIT/X license. See Copyright Notice in luajit.h
----------------------------------------------------------------------------
-- This is a helper module used by the LuaJIT machine code dumper module.
--
-- It disassembles most LoongArch instructions.
-- NYI: SIMD instructions.
------------------------------------------------------------------------------

local type = type
local byte, format = string.byte, string.format
local match, gmatch = string.match, string.gmatch
local concat = table.concat
local bit = require("bit")
local band, bor, tohex = bit.band, bit.bor, bit.tohex
local lshift, rshift, arshift = bit.lshift, bit.rshift, bit.arshift

------------------------------------------------------------------------------
-- Opcode maps
------------------------------------------------------------------------------

local map_18_0 = {      -- 18-20:0, 10-17
  shift = 10, mask = 255,
  [4] = "clo.wDJ",
  [5] = "clz.wDJ",
  [6] = "cto.wDJ",
  [7] = "ctz.wDJ",
  [8] = "clo.dDJ",
  [9] = "clz.dDJ",
  [10] = "cto.dDJ",
  [11] = "ctz.dDJ",
  [12] = "revb.2hDJ",
  [13] = "revb.4hDJ",
  [14] = "revb.2wDJ",
  [15] = "revb.dDJ",
  [16] = "revh.2wDJ",
  [17] = "revh.dDJ",
  [18] = "bitrev.4bDJ",
  [19] = "bitrev.8bDJ",
  [20] = "bitrev.wDJ",
  [21] = "bitrev.dDJ",
  [22] = "ext.w.hDJ",
  [23] = "ext.w.bDJ",
}

local map_18_4 = {	-- 18-20:4, 15-17
  shift = 15, mask = 7,
  [0] = "add.wDJK",
  [1] = "add.dDJK",
  [2] = "sub.wDJK",
  [3] = "sub.dDJK",
  [4] = "sltDJK",
  [5] = "sltuDJK",
  [6] = "maskeqzDJK",
  [7] = "masknezDJK",
}

local map_18_5 = {	-- 18-20:5, 15-17
  shift = 15, mask = 7,
  [0] = "norDJK",
  [1] = "andDJK",
  [2] = "orDJK",
  [3] = "xorDJK",
  [4] = "ornDJK",
  [5] = "andnDJK",
  [6] = "sll.wDJK",
  [7] = "srl.wDJK",
}

local map_18_6 = {	-- 18-20:6, 15-17
  shift = 15, mask = 7,
  [0] = "sra.wDJK",
  [1] = "sll.dDJK",
  [2] = "srl.dDJK",
  [3] = "sra.dDJK",
  [6] = "rotr.wDJK",
  [7] = "rotr.dDJK",
}

local map_18_7 = {	-- 18-20:7, 15-17
  shift = 15, mask = 7,
  [0] = "mul.wDJK",
  [1] = "mulh.wDJK",
  [2] = "mulh.wuDJK",
  [3] = "mul.dDJK",
  [4] = "mulh.dDJK",
  [5] = "mulh.duDJK",
  [6] = "mulw.d.wDJK",
  [7] = "mulw.d.wuDJK",
}

local map_farith2 = {
  shift = 10, mask = 31,
  [1] = "fabs.sFG",
  [2] = "fabs.dFG",
  [5] = "fneg.sFG",
  [6] = "fneg.dFG",
  [9] = "flogb.sFG",
  [10] = "flogb.dFG",
  [13] = "fclass.sFG",
  [14] = "fclass.dFG",
  [17] = "fsqrt.sFG",
  [18] = "fsqrt.dFG",
  [21] = "frecip.sFG",
  [22] = "frecip.dFG",
  [25] = "frsqrt.sFG",
  [26] = "frsqrt.dFG",
  [29] = "frecipe.sFG",
  [30] = "frecipe.dFG",
  [33] = "frsqrte.sFG",
  [34] = "frsqrte.dFG",
}

local map_fmov = {
  shift = 10, mask = 31,
  [5] = "fmov.sFG",
  [6] = "fmov.dFG",
  [9] = "movgr2fr.wFJ",
  [10] = "movgr2fr.dFJ",
  [11] = "movgr2frh.wFJ",
  [13] = "movfr2gr.sDG",
  [14] = "movfr2gr.dDG",
  [15] = "movfrh2gr.sDG",
  [16] = "movgr2fcsrSJ",
  [18] = "movfcsr2grDR",
  [20] = { shift = 3, mask = 3, [0] = "movfr2cfEG", },
  [21] = { shift = 8, mask = 3, [0] = "movcf2frFA", },
  [22] = { shift = 3, mask = 3, [0] = "movgr2cfEJ", },
  [23] = { shift = 8, mask = 3, [0] = "movcf2grDA", },
}

local map_fconvert = { -- 15-20: 110010
  shift = 10, mask = 31,
  [6] = "fcvt.s.dFG",	[9] = "fcvt.d.sFG",
}

local map_fconvert1 = { -- 15-20: 110100
  shift = 10, mask = 31,
  [1] = "ftintrm.w.sFG",
  [2] = "ftintrm.w.dFG",
  [9] = "ftintrm.l.sFG",
  [10] = "ftintrm.l.dFG",
  [17] = "ftintrp.w.sFG",
  [18] = "ftintrp.w.dFG",
  [25] = "ftintrp.l.sFG",
  [26] = "ftintrp.l.dFG",
}

local map_fconvert2 = { -- 15-20: 110101
  shift = 10, mask = 31,
  [1] = "ftintrz.w.sFG",
  [2] = "ftintrz.w.dFG",
  [9] = "ftintrz.l.sFG",
  [10] = "ftintrz.l.dFG",
  [17] = "ftintrne.w.sFG",
  [18] = "ftintrne.w.dFG",
  [25] = "ftintrne.l.sFG",
  [26] = "ftintrne.l.dFG",
}

local map_fconvert3 = { -- 15-20: 110110
  shift = 10, mask = 31,
  [1] = "ftint.w.sFG",
  [2] = "ftint.w.dFG",
  [9] = "ftint.l.sFG",
  [10] = "ftint.l.dFG",
}

local map_fconvert4 = { -- 15-20: 111010
  shift = 10, mask = 31,
  [4] = "ffint.s.wFG",
  [6] =  "ffint.s.lFG",
  [8] = "ffint.d.wFG",
  [10] = "ffint.d.lFG",
}

local map_fconvert5 = { -- 15-20: 111100
  shift = 10, mask = 31,
  [17] = "frint.sFG",
  [18] = "frint.dFG",
}

local map_farith = {	-- 22-25:4, 15-21
  shift = 15, mask = 127,
  [1] = "fadd.sFGH",
  [2] = "fadd.dFGH",
  [5] = "fsub.sFGH",
  [6] = "fsub.dFGH",
  [9] = "fmul.sFGH",
  [10] = "fmul.dFGH",
  [13] = "fdiv.sFGH",
  [14] = "fdiv.dFGH",
  [17] = "fmax.sFGH",
  [18] = "fmax.dFGH",
  [21] = "fmin.sFGH",
  [22] = "fmin.dFGH",
  [25] = "fmaxa.sFGH",
  [26] = "fmaxa.dFGH",
  [29] = "fmina.sFGH",
  [30] = "fmina.dFGH",
  [33] = "fscaleb.sFGH",
  [34] = "fscaleb.dFGH",
  [37] = "fcopysign.sFGH",
  [38] = "fcopysign.dFGH",
  [40] = map_farith2, [41] = map_fmov,
  [50] = map_fconvert, [52] = map_fconvert1,
  [53] = map_fconvert2, [54] = map_fconvert3,
  [58] = map_fconvert4, [60] = map_fconvert5,
}

local map_21_0 = {	--21st:0, 18-20
  shift = 18, mask = 7,
  [0] = map_18_0,
  [1] = { shift = 17, mask = 1, [0] = "alsl.wDJKQ", "alsl.wuDJKQ", },
  [2] = {shift = 17, mask = 1, [0] = "bytepick.wDJKQ", },
  [3] = "bytepick.dDJKB",
  [4] = map_18_4,
  [5] = map_18_5,
  [6] = map_18_6,
  [7] = map_18_7,
}

local map_21_1 = {      --21st:1, 22nd:0, 15-20
  shift = 21, mask = 1,
  [1] = {
    shift = 18, mask = 7,
    [0] = {
      shift = 15, mask = 7,
      [0] = "div.wDJK",
      [1] = "mod.wDJK",
      [2] = "div.wuDJK",
      [3] = "mod.wuDJK",
      [4] = "div.dDJK",
      [5] = "mod.dDJK",
      [6] = "div.duDJK",
      [7] = "mod.duDJK",
    },
    [1] = {
      shift = 18, mask = 7,
      [0] = "crc.w.b.wDJK",
      [1] = "crc.w.h.wDJK",
      [2] = "crc.w.w.wDJK",
      [3] = "crc.w.d.wDJK",
      [4] = "crcc.w.b.wDJK",
      [5] = "crcc.w.h.wDJK",
      [6] = "crcc.w.w.wDJK",
      [7] = "crcc.w.d.wDJK",
    },
    [2] = {
      shift = 15, mask = 7,
      [4] = breakC, [6] = syscallC,
    },
    [3] = { shift = 17, mask = 1, [0] = "alsl.dDJKQ", },
  },
}

local map_22_0 = {
  shift = 21, mask = 1,
  [0] = map_21_0,
  [1] = map_21_1,
}

local map_shift = {	-- 22nd:1, 21st:0
  shift = 16, mask = 31,
  [0] = { shift = 15, mask = 1, [1] = "slli.wDJU", },
  [1] = "slli.dDJV",
  [4] = { shift = 15, mask = 1, [1] = "srli.wDJU", },
  [5] = "srli.dDJV",
  [8] = { shift = 15, mask = 1, [1] = "srai.wDJU", },
  [9] = "srai.dDJV",
  [12] = { shift = 15, mask = 1, [1] = "rotri.wDJU", },
  [13] = "rotri.dDJV",
}

local map_22_1 = {        -- 22nd:1
  shift = 21, mask = 1,
  [0] = map_shift,
  [1] = { shift = 15, mask = 1, [0] = "bstrins.wDJMU", [1] = "bstrpick.wDJMU", },
}

local map_26_0 = {
  shift = 22, mask = 15,
  [0] = map_22_0,
  [1] = map_22_1,
  [2] = "bstrins.dDJNV",
  [3] = "bstrpick.dDJNV",
  [4] = map_farith,
  [8] = "sltiDJX",
  [9] = "sltuiDJX",
  [10] = "addi.wDJX",
  [11] = "addi.dDJX",
  [12] = "lu52i.dDJX",
  [13] = "andiDJT",
  [14] = "oriDJT",
  [15] = "xoriDJT",
}

local map_long_i_5 = { -- Long immediate fixed-point arithmetic.
  shift = 25, mask = 1,
  [0] = "lu12i.wDZ",
  [1] = "lu32i.dDZ",
}

local map_long_i_6 = {
  shift = 25, mask = 1,
  [0] = "pcaddiDZ",
  [1] = "pcalau12iDZ",
}

local map_long_i_7 = {
  shift = 25, mask = 1,
  [0] = "pcaddu12iDZ",
  [1] = "pcaddu18iDZ",
}

local map_ldst0_14 = {
  shift = 15, mask = 2047,
  [0] = "ldx.bDJK", [8] = "ldx.hDJK", [16] = "ldx.wDJK",
  [24] = "ldx.dDJK", [32] = "stx.bDJK", [40] = "stx.hDJK",
  [48] = "stx.wDJK", [56] = "stx.dDJK", [64] = "ldx.buDJK",
  [72] = "ldx.huDJK", [80] = "ldx.wuDJK", [96] = "fldx.sFJK",
  [104] = "fldx.dFJK", [112] = "fstx.sFJK", [120] = "fstx.dFJK",
  [232] = "fldgt.sFJK", [233] = "fldgt.dFJK", [234] = "fldle.sFJK",
  [235] = "fldle.dFJK", [236] = "fstgt.sFJK", [237] = "fstgt.dFJK",
  [238] = "fstle.sFJK", [239] = "fstle.dFJK", [240] = "ldgt.bDJK",
  [241] = "ldgt.hDJK", [242] = "ldgt.wDJK", [243] = "ldgt.dDJK",
  [244] = "ldle.bDJK", [245] = "ldle.hDJK", [246] = "ldle.wDJK",
  [247] = "ldle.dDJK", [248] = "stgt.bDJK", [249] = "stgt.hDJK",
  [250] = "stgt.wDJK", [251] = "stgt.dDJK", [252] = "stle.bDJK",
  [253] = "stle.hDJK", [254] = "stle.wDJK", [255] = "stle.dDJK",
}

local map_ldst1_8 = {
  shift = 24, mask = 3,
  [0] = "ll.wDJW",
  [1] = "sc.wDJW",
  [2] = "ll.dDJW",
  [3] = "sc.dDJW",
}

local map_ldst1_9 = {
  shift = 24, mask = 3,
  [0] = "ldptr.wDJW",
  [1] = "stptr.wDJW",
  [2] = "ldptr.dDJW",
  [3] = "stptr.dDJW",
}

local map_ldst1_10 = {
  shift = 22, mask = 15,
  [0] = "ld.bDJX",
  [1] = "ld.hDJX",
  [2] = "ld.wDo",
  [3] = "ld.dDo",
  [4] = "st.bDo",
  [5] = "st.hDo",
  [6] = "st.wDo",
  [7] = "st.dDo",
  [8] = "ld.buDo",
  [9] = "ld.huDo",
  [10] = "ld.wuDJX",
  [12] = "fld.sFo",
  [13] = "fst.sFo",
  [14] = "fld.dFo",
  [15] = "fst.dFo",
}

local map_fcmp0 = {
  shift = 15, mask = 31,
  [0] = "fcmp.caf.sEGH",
  [1] = "fcmp.saf.sEGH",
  [2] = "fcmp.clt.sEGH",
  [3] = "fcmp.slt.sEGH",
  [4] = "fcmp.ceq.sEGH",
  [5] = "fcmp.seq.sEGH",
  [6] = "fcmp.cle.sEGH",
  [7] = "fcmp.sle.sEGH",
  [8] = "fcmp.cun.sEGH",
  [9] = "fcmp.sun.sEGH",
  [10] = "fcmp.cult.sEGH",
  [11] ="fcmp.sult.sEGH",
  [12] = "fcmp.cueq.sEGH",
  [13] = "fcmp.sueq.sEGH",
  [14] = "fcmp.cule.sEGH",
  [15] = "fcmp.sule.sEGH",
  [16] = "fcmp.cne.sEGH",
  [17] = "fcmp.sne.sEGH",
  [20] = "fcmp.cor.sEGH",
  [21] = "fcmp.sor.sEGH",
  [24] = "fcmp.cune.sEGH",
  [25] = "fcmp.sune.sEGH",
}

local map_fcmp1 = {
  shift = 15, mask = 31,
  [0] = "fcmp.caf.dEGH",
  [1] = "fcmp.saf.dEGH",
  [2] = "fcmp.clt.dEGH",
  [3] = "fcmp.slt.dEGH",
  [4] = "fcmp.ceq.dEGH",
  [5] = "fcmp.seq.dEGH",
  [6] = "fcmp.cle.dEGH",
  [7] = "fcmp.sle.dEGH",
  [8] = "fcmp.cun.dEGH",
  [9] = "fcmp.sun.dEGH",
  [10] = "fcmp.cult.dEGH",
  [11] = "fcmp.sult.dEGH",
  [12] = "fcmp.cueq.dEGH",
  [13] = "fcmp.sueq.dEGH",
  [14] = "fcmp.cule.dEGH",
  [15] = "fcmp.sule.dEGH",
  [16] = "fcmp.cne.dEGH",
  [17] = "fcmp.sne.dEGH",
  [20] = "fcmp.cor.dEGH",
  [21] = "fcmp.sor.dEGH",
  [24] = "fcmp.cune.dEGH",
  [25] = "fcmp.sune.dEGH",
}

local map_fcmp = {
  shift = 20, mask = 63,
  [1] = { shift = 3, mask = 3, [0] = map_fcmp0, },
  [2] = { shift = 3, mask = 3, [0] = map_fcmp1, },
  [16] = { shift = 18, mask = 3, [0] = "fselFGHI", },
}

local map_fp = {
  shift = 20, mask = 15,
  [1] = "fmadd.sFGHi",
  [2] = "fmadd.dFGHi",
  [4] = "fmsub.sFGHi",
  [5] = "fmsub.dFGHi",
  [10] = "fnmadd.dFGHi",
  [14] = "fnmsub.dFGHi",
}

local map_init = {
  shift = 26, mask = 63,
  [0] = map_26_0,
  [2] = map_fp,
  [3] = map_fcmp,
  [4] = "addu16i.dDJY",
  [5] = map_long_i_5,
  [6] = map_long_i_6,
  [7] = map_long_i_7,
  [8] = map_ldst1_8,
  [9] = map_ldst1_9,
  [10] = map_ldst1_10,
  [14] = map_ldst0_14,
  [16] = "beqzJL",
  [17] = "bnezJL",
  [18] = { shift = 8, mask = 3, [0] = "bceqzAL", "bcnezAL", },
  [19] = "jirlDJa",
  [20] = "bP",
  [21] = "blP",
  [22] = "beqJDO",
  [23] = "bneJDO",
  [24] = "bltJDO",
  [25] = "bgeJDO",
  [26] = "bltuJDO",
  [27] = "bgeuJDO",
}

------------------------------------------------------------------------------

local map_gpr = {
  [0] = "r0", "ra", "r2", "sp", "r4", "r5", "r6", "r7",
  "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15",
  "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23",
  "r24", "r25", "r26", "r27", "r28", "r29", "r30", "r31",
}

------------------------------------------------------------------------------

-- Output a nicely formatted line with an opcode and operands.
local function putop(ctx, text, operands)
  local pos = ctx.pos
  local extra = ""
  if ctx.rel then
    local sym = ctx.symtab[ctx.rel]
    if sym then extra = "\t->"..sym end
  end
  if ctx.hexdump > 0 then
    ctx.out(format("%08x  %s  %-7s %s%s\n",
	    ctx.addr+pos, tohex(ctx.op), text, concat(operands, ", "), extra))
  else
    ctx.out(format("%08x  %-7s %s%s\n",
	    ctx.addr+pos, text, concat(operands, ", "), extra))
  end
  ctx.pos = pos + 4
end

-- Fallback for unknown opcodes.
local function unknown(ctx)
  return putop(ctx, ".long", { "0x"..tohex(ctx.op) })
end

local function get_le(ctx)
  local pos = ctx.pos
  local b0, b1, b2, b3 = byte(ctx.code, pos+1, pos+4)
  return bor(lshift(b3, 24), lshift(b2, 16), lshift(b1, 8), b0)
end

-- Disassemble a single instruction.
local function disass_ins(ctx)
  local op = ctx:get()
  local operands = {}
  local last = nil
  ctx.op = op
  ctx.rel = nil

  local opat = ctx.map_pri[rshift(op, 26)]
  while type(opat) ~= "string" do
    if not opat then return unknown(ctx) end
    opat = opat[band(rshift(op, opat.shift), opat.mask)]
  end
  local name, pat = match(opat, "^([a-z0-9_.]*)(.*)")
  local altname, pat2 = match(pat, "|([a-z0-9_.|]*)(.*)")
  if altname then pat = pat2 end

  for p in gmatch(pat, ".") do
    local x = nil
    if p == "D" then
      x = map_gpr[band(rshift(op, 0), 31)]
    elseif p == "J" then
      x = map_gpr[band(rshift(op, 5), 31)]
    elseif p == "K" then
      x = map_gpr[band(rshift(op, 10), 31)]
    elseif p == "F" then
      x = "f"..band(rshift(op, 0), 31)
    elseif p == "G" then
      x = "f"..band(rshift(op, 5), 31)
    elseif p == "H" then
      x = "f"..band(rshift(op, 10), 31)
    elseif p == "i" then
      x = "f"..band(rshift(op, 15), 31)
    elseif p == "S" then
      x = "fcsr"..band(rshift(op, 0), 31)
    elseif p == "R" then
      x = "fcsr"..band(rshift(op, 5), 31)
    elseif p == "E" then
      x = "fcc"..band(rshift(op, 0), 7)
    elseif p == "A" then
      x = "fcc"..band(rshift(op, 5), 7)
    elseif p == "I" then
      x = "fcc"..band(rshift(op, 15), 7)
    elseif p == "Q" then	-- sa2
      x = band(rshift(op, 15), 3)
    elseif p == "B" then	-- sa3
      x = band(rshift(op, 15), 7)
    elseif p == "M" then	-- msbw
      x = band(rshift(op, 16), 31)
    elseif p == "N" then	-- msbd
      x = band(rshift(op, 16), 63)
    elseif p == "U" then	-- ui5
      x = band(rshift(op, 10), 31)
    elseif p == "V" then	-- ui6
      x = band(rshift(op, 10), 63)
    elseif p == "T" then	-- ui12
      x = band(rshift(op, 10), 4095)
    elseif p == "W" then	-- si14
      x = band(rshift(op, 10), 16383)
    elseif p == "X" then	-- si12
      x = band(rshift(op, 10), 4095)
    elseif p == "o" then
      local disp = band((rshift(op, 10)), 0xfff)
      operands[#operands] = format("%s, %d", last, disp)
    elseif p == "Y" then	-- si16
      x = band(rshift(op, 10), 65535)
    elseif p == "Z" then	-- si20
      x = band(rshift(op, 10), 1048575)
    elseif p == "C" then	-- code
      x = band(rshift(op, 0), 32767)
    elseif p == "O" then	-- offs[15:0]
      x = band(rshift(op, 10), 65535)
    elseif p == "L" then	-- offs[15:0] + offs[20:16]
      x = lshift(band(op, 31), 16) + band(rshift(op, 10), 65535)
    elseif p == "P" then	-- offs[15:0] + offs[25:16]
      x = lshift(band(op, 1023), 16) + band(rshift(op, 10), 65535)
    elseif p == "a" then
      x = band(rshift(op, 10), 65535)
    else
      assert(false)
    end
    if x then operands[#operands+1] = x; last = x end
  end

  return putop(ctx, name, operands)
end

------------------------------------------------------------------------------

-- Disassemble a block of code.
local function disass_block(ctx, ofs, len)
  if not ofs then ofs = 0 end
  local stop = len and ofs+len or #ctx.code
  stop = stop - stop % 4
  ctx.pos = ofs - ofs % 4
  ctx.rel = nil
  while ctx.pos < stop do disass_ins(ctx) end
end

-- Extended API: create a disassembler context. Then call ctx:disass(ofs, len).
local function create(code, addr, out)
  local ctx = {}
  ctx.code = code
  ctx.addr = addr or 0
  ctx.out = out or io.write
  ctx.symtab = {}
  ctx.disass = disass_block
  ctx.hexdump = 8
  ctx.get = get_le
  ctx.map_pri = map_init
  return ctx
end

-- Simple API: disassemble code (a string) at address and output via out.
local function disass(code, addr, out)
  create(code, addr, out):disass()
end

-- Return register name for RID.
local function regname(r)
  if r < 32 then return map_gpr[r] end
  return "f"..(r-32)
end

-- Public module functions.
return {
  create = create,
  disass = disass,
  regname = regname
}

