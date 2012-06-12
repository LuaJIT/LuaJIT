----------------------------------------------------------------------------
-- LuaJIT module to save/list bytecode.
--
-- Copyright (C) 2005-2012 Mike Pall. All rights reserved.
-- Released under the MIT license. See Copyright Notice in luajit.h
----------------------------------------------------------------------------
--
-- This module saves or lists the bytecode for an input file.
-- It's run by the -b command line option.
--
------------------------------------------------------------------------------

local jit = require("jit")
assert(jit.version_num == 20000, "LuaJIT core/library version mismatch")

-- Symbol name prefix for LuaJIT bytecode.
local LJBC_PREFIX = "luaJIT_BC_"

------------------------------------------------------------------------------

local function usage()
  io.stderr:write[[
Save LuaJIT bytecode: luajit -b[options] input output
  -l        Only list bytecode.
  -s        Strip debug info (default).
  -g        Keep debug info.
  -n name   Set module name (default: auto-detect from input name).
  -t type   Set output file type (default: auto-detect from output name).
  -a arch   Override architecture for object files (default: native).
  -o os     Override OS for object files (default: native).
  -e chunk  Use chunk string as input.
  --        Stop handling options.
  -         Use stdin as input and/or stdout as output.

File types: c h obj o raw (default)
]]
  os.exit(1)
end

local function check(ok, ...)
  if ok then return ok, ... end
  io.stderr:write("luajit: ", ...)
  io.stderr:write("\n")
  os.exit(1)
end

local function readfile(input)
  if type(input) == "function" then return input end
  if input == "-" then input = nil end
  return check(loadfile(input))
end

local function savefile(name, mode)
  if name == "-" then return io.stdout end
  return check(io.open(name, mode))
end

------------------------------------------------------------------------------

local map_type = {
  raw = "raw", c = "c", h = "h", o = "obj", obj = "obj",
}

local map_arch = {
  x86 = true, x64 = true, arm = true, ppc = true, ppcspe = true,
  mips = true, mipsel = true,
}

local map_os = {
  linux = true, windows = true, osx = true, freebsd = true, netbsd = true,
  openbsd = true, solaris = true,
}

local function checkarg(str, map, err)
  str = string.lower(str)
  local s = check(map[str], "unknown ", err)
  return s == true and str or s
end

local function detecttype(str)
  local ext = string.match(string.lower(str), "%.(%a+)$")
  return map_type[ext] or "raw"
end

local function checkmodname(str)
  check(string.match(str, "^[%w_.%-]+$"), "bad module name")
  return string.gsub(str, "[%.%-]", "_")
end

local function detectmodname(str)
  if type(str) == "string" then
    local tail = string.match(str, "[^/\\]+$")
    if tail then str = tail end
    local head = string.match(str, "^(.*)%.[^.]*$")
    if head then str = head end
    str = string.match(str, "^[%w_.%-]+")
  else
    str = nil
  end
  check(str, "cannot derive module name, use -n name")
  return string.gsub(str, "[%.%-]", "_")
end

------------------------------------------------------------------------------

local function bcsave_tail(fp, output, s)
  local ok, err = fp:write(s)
  if ok and output ~= "-" then ok, err = fp:close() end
  check(ok, "cannot write ", output, ": ", err)
end

local function bcsave_raw(output, s)
  local fp = savefile(output, "wb")
  bcsave_tail(fp, output, s)
end

local function bcsave_c(ctx, output, s)
  local fp = savefile(output, "w")
  if ctx.type == "c" then
    fp:write(string.format([[
#ifdef _cplusplus
extern "C"
#endif
#ifdef _WIN32
__declspec(dllexport)
#endif
const char %s%s[] = {
]], LJBC_PREFIX, ctx.modname))
  else
    fp:write(string.format([[
#define %s%s_SIZE %d
static const char %s%s[] = {
]], LJBC_PREFIX, ctx.modname, #s, LJBC_PREFIX, ctx.modname))
  end
  local t, n, m = {}, 0, 0
  for i=1,#s do
    local b = tostring(string.byte(s, i))
    m = m + #b + 1
    if m > 78 then
      fp:write(table.concat(t, ",", 1, n), ",\n")
      n, m = 0, #b + 1
    end
    n = n + 1
    t[n] = b
  end
  bcsave_tail(fp, output, table.concat(t, ",", 1, n).."\n};\n")
end

local function bcsave_elfobj(ctx, output, s, ffi)
  ffi.cdef[[
typedef struct {
  uint8_t emagic[4], eclass, eendian, eversion, eosabi, eabiversion, epad[7];
  uint16_t type, machine;
  uint32_t version;
  uint32_t entry, phofs, shofs;
  uint32_t flags;
  uint16_t ehsize, phentsize, phnum, shentsize, shnum, shstridx;
} ELF32header;
typedef struct {
  uint8_t emagic[4], eclass, eendian, eversion, eosabi, eabiversion, epad[7];
  uint16_t type, machine;
  uint32_t version;
  uint64_t entry, phofs, shofs;
  uint32_t flags;
  uint16_t ehsize, phentsize, phnum, shentsize, shnum, shstridx;
} ELF64header;
typedef struct {
  uint32_t name, type, flags, addr, ofs, size, link, info, align, entsize;
} ELF32sectheader;
typedef struct {
  uint32_t name, type;
  uint64_t flags, addr, ofs, size;
  uint32_t link, info;
  uint64_t align, entsize;
} ELF64sectheader;
typedef struct {
  uint32_t name, value, size;
  uint8_t info, other;
  uint16_t sectidx;
} ELF32symbol;
typedef struct {
  uint32_t name;
  uint8_t info, other;
  uint16_t sectidx;
  uint64_t value, size;
} ELF64symbol;
typedef struct {
  ELF32header hdr;
  ELF32sectheader sect[6];
  ELF32symbol sym[2];
  uint8_t space[4096];
} ELF32obj;
typedef struct {
  ELF64header hdr;
  ELF64sectheader sect[6];
  ELF64symbol sym[2];
  uint8_t space[4096];
} ELF64obj;
]]
  local symname = LJBC_PREFIX..ctx.modname
  local is64, isbe = false, false
  if ctx.arch == "x64" then
    is64 = true
  elseif ctx.arch == "ppc" or ctx.arch == "ppcspe" or ctx.arch == "mips" then
    isbe = true
  end

  -- Handle different host/target endianess.
  local function f32(x) return x end
  local f16, fofs = f32, f32
  if ffi.abi("be") ~= isbe then
    f32 = bit.bswap
    function f16(x) return bit.rshift(bit.bswap(x), 16) end
    if is64 then
      local two32 = ffi.cast("int64_t", 2^32)
      function fofs(x) return bit.bswap(x)*two32 end
    else
      fofs = f32
    end
  end

  -- Create ELF object and fill in header.
  local o = ffi.new(is64 and "ELF64obj" or "ELF32obj")
  local hdr = o.hdr
  if ctx.os == "bsd" or ctx.os == "other" then -- Determine native hdr.eosabi.
    local bf = assert(io.open("/bin/ls", "rb"))
    local bs = bf:read(9)
    bf:close()
    ffi.copy(o, bs, 9)
    check(hdr.emagic[0] == 127, "no support for writing native object files")
  else
    hdr.emagic = "\127ELF"
    hdr.eosabi = ({ freebsd=9, netbsd=2, openbsd=12, solaris=6 })[ctx.os] or 0
  end
  hdr.eclass = is64 and 2 or 1
  hdr.eendian = isbe and 2 or 1
  hdr.eversion = 1
  hdr.type = f16(1)
  hdr.machine = f16(({ x86=3, x64=62, arm=40, ppc=20, ppcspe=20, mips=8, mipsel=8 })[ctx.arch])
  if ctx.arch == "mips" or ctx.arch == "mipsel" then
    hdr.flags = 0x50001006
  end
  hdr.version = f32(1)
  hdr.shofs = fofs(ffi.offsetof(o, "sect"))
  hdr.ehsize = f16(ffi.sizeof(hdr))
  hdr.shentsize = f16(ffi.sizeof(o.sect[0]))
  hdr.shnum = f16(6)
  hdr.shstridx = f16(2)

  -- Fill in sections and symbols.
  local sofs, ofs = ffi.offsetof(o, "space"), 1
  for i,name in ipairs{
      ".symtab", ".shstrtab", ".strtab", ".rodata", ".note.GNU-stack",
    } do
    local sect = o.sect[i]
    sect.align = fofs(1)
    sect.name = f32(ofs)
    ffi.copy(o.space+ofs, name)
    ofs = ofs + #name+1
  end
  o.sect[1].type = f32(2) -- .symtab
  o.sect[1].link = f32(3)
  o.sect[1].info = f32(1)
  o.sect[1].align = fofs(8)
  o.sect[1].ofs = fofs(ffi.offsetof(o, "sym"))
  o.sect[1].entsize = fofs(ffi.sizeof(o.sym[0]))
  o.sect[1].size = fofs(ffi.sizeof(o.sym))
  o.sym[1].name = f32(1)
  o.sym[1].sectidx = f16(4)
  o.sym[1].size = fofs(#s)
  o.sym[1].info = 17
  o.sect[2].type = f32(3) -- .shstrtab
  o.sect[2].ofs = fofs(sofs)
  o.sect[2].size = fofs(ofs)
  o.sect[3].type = f32(3) -- .strtab
  o.sect[3].ofs = fofs(sofs + ofs)
  o.sect[3].size = fofs(#symname+1)
  ffi.copy(o.space+ofs+1, symname)
  ofs = ofs + #symname + 2
  o.sect[4].type = f32(1) -- .rodata
  o.sect[4].flags = fofs(2)
  o.sect[4].ofs = fofs(sofs + ofs)
  o.sect[4].size = fofs(#s)
  o.sect[5].type = f32(1) -- .note.GNU-stack
  o.sect[5].ofs = fofs(sofs + ofs + #s)

  -- Write ELF object file.
  local fp = savefile(output, "wb")
  fp:write(ffi.string(o, ffi.sizeof(o)-4096+ofs))
  bcsave_tail(fp, output, s)
end

local function bcsave_peobj(ctx, output, s, ffi)
  ffi.cdef[[
typedef struct {
  uint16_t arch, nsects;
  uint32_t time, symtabofs, nsyms;
  uint16_t opthdrsz, flags;
} PEheader;
typedef struct {
  char name[8];
  uint32_t vsize, vaddr, size, ofs, relocofs, lineofs;
  uint16_t nreloc, nline;
  uint32_t flags;
} PEsection;
typedef struct __attribute((packed)) {
  union {
    char name[8];
    uint32_t nameref[2];
  };
  uint32_t value;
  int16_t sect;
  uint16_t type;
  uint8_t scl, naux;
} PEsym;
typedef struct __attribute((packed)) {
  uint32_t size;
  uint16_t nreloc, nline;
  uint32_t cksum;
  uint16_t assoc;
  uint8_t comdatsel, unused[3];
} PEsymaux;
typedef struct {
  PEheader hdr;
  PEsection sect[2];
  // Must be an even number of symbol structs.
  PEsym sym0;
  PEsymaux sym0aux;
  PEsym sym1;
  PEsymaux sym1aux;
  PEsym sym2;
  PEsym sym3;
  uint32_t strtabsize;
  uint8_t space[4096];
} PEobj;
]]
  local symname = LJBC_PREFIX..ctx.modname
  local is64 = false
  if ctx.arch == "x86" then
    symname = "_"..symname
  elseif ctx.arch == "x64" then
    is64 = true
  end
  local symexport = "   /EXPORT:"..symname..",DATA "

  -- The file format is always little-endian. Swap if the host is big-endian.
  local function f32(x) return x end
  local f16 = f32
  if ffi.abi("be") then
    f32 = bit.bswap
    function f16(x) return bit.rshift(bit.bswap(x), 16) end
  end

  -- Create PE object and fill in header.
  local o = ffi.new("PEobj")
  local hdr = o.hdr
  hdr.arch = f16(({ x86=0x14c, x64=0x8664, arm=0x1c0, ppc=0x1f2, mips=0x366, mipsel=0x366 })[ctx.arch])
  hdr.nsects = f16(2)
  hdr.symtabofs = f32(ffi.offsetof(o, "sym0"))
  hdr.nsyms = f32(6)

  -- Fill in sections and symbols.
  o.sect[0].name = ".drectve"
  o.sect[0].size = f32(#symexport)
  o.sect[0].flags = f32(0x00100a00)
  o.sym0.sect = f16(1)
  o.sym0.scl = 3
  o.sym0.name = ".drectve"
  o.sym0.naux = 1
  o.sym0aux.size = f32(#symexport)
  o.sect[1].name = ".rdata"
  o.sect[1].size = f32(#s)
  o.sect[1].flags = f32(0x40300040)
  o.sym1.sect = f16(2)
  o.sym1.scl = 3
  o.sym1.name = ".rdata"
  o.sym1.naux = 1
  o.sym1aux.size = f32(#s)
  o.sym2.sect = f16(2)
  o.sym2.scl = 2
  o.sym2.nameref[1] = f32(4)
  o.sym3.sect = f16(-1)
  o.sym3.scl = 2
  o.sym3.value = f32(1)
  o.sym3.name = "@feat.00" -- Mark as SafeSEH compliant.
  ffi.copy(o.space, symname)
  local ofs = #symname + 1
  o.strtabsize = f32(ofs + 4)
  o.sect[0].ofs = f32(ffi.offsetof(o, "space") + ofs)
  ffi.copy(o.space + ofs, symexport)
  ofs = ofs + #symexport
  o.sect[1].ofs = f32(ffi.offsetof(o, "space") + ofs)

  -- Write PE object file.
  local fp = savefile(output, "wb")
  fp:write(ffi.string(o, ffi.sizeof(o)-4096+ofs))
  bcsave_tail(fp, output, s)
end

local function bcsave_machobj(ctx, output, s, ffi)
  check(false, "NYI: no support for writing OSX object files")
end

local function bcsave_obj(ctx, output, s)
  local ok, ffi = pcall(require, "ffi")
  check(ok, "FFI library required to write this file type")
  if ctx.os == "windows" then
    return bcsave_peobj(ctx, output, s, ffi)
  elseif ctx.os == "osx" then
    return bcsave_machobj(ctx, output, s, ffi)
  else
    return bcsave_elfobj(ctx, output, s, ffi)
  end
end

------------------------------------------------------------------------------

local function bclist(input, output)
  local f = readfile(input)
  require("jit.bc").dump(f, savefile(output, "w"), true)
end

local function bcsave(ctx, input, output)
  local f = readfile(input)
  local s = string.dump(f, ctx.strip)
  local t = ctx.type
  if not t then
    t = detecttype(output)
    ctx.type = t
  end
  if t == "raw" then
    bcsave_raw(output, s)
  else
    if not ctx.modname then ctx.modname = detectmodname(input) end
    if t == "obj" then
      bcsave_obj(ctx, output, s)
    else
      bcsave_c(ctx, output, s)
    end
  end
end

local function docmd(...)
  local arg = {...}
  local n = 1
  local list = false
  local ctx = {
    strip = true, arch = jit.arch, os = string.lower(jit.os),
    type = false, modname = false,
  }
  while n <= #arg do
    local a = arg[n]
    if type(a) == "string" and string.sub(a, 1, 1) == "-" and a ~= "-" then
      table.remove(arg, n)
      if a == "--" then break end
      for m=2,#a do
	local opt = string.sub(a, m, m)
	if opt == "l" then
	  list = true
	elseif opt == "s" then
	  ctx.strip = true
	elseif opt == "g" then
	  ctx.strip = false
	else
	  if arg[n] == nil or m ~= #a then usage() end
	  if opt == "e" then
	    if n ~= 1 then usage() end
	    arg[1] = check(loadstring(arg[1]))
	  elseif opt == "n" then
	    ctx.modname = checkmodname(table.remove(arg, n))
	  elseif opt == "t" then
	    ctx.type = checkarg(table.remove(arg, n), map_type, "file type")
	  elseif opt == "a" then
	    ctx.arch = checkarg(table.remove(arg, n), map_arch, "architecture")
	  elseif opt == "o" then
	    ctx.os = checkarg(table.remove(arg, n), map_os, "OS name")
	  else
	    usage()
	  end
	end
      end
    else
      n = n + 1
    end
  end
  if list then
    if #arg == 0 or #arg > 2 then usage() end
    bclist(arg[1], arg[2] or "-")
  else
    if #arg ~= 2 then usage() end
    bcsave(ctx, arg[1], arg[2])
  end
end

------------------------------------------------------------------------------

-- Public module functions.
module(...)

start = docmd -- Process -b command line option.

