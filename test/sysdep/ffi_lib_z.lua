local ffi = require("ffi")

local compress, uncompress

if ffi.abi("win") then

  ffi.cdef[[
  int RtlGetCompressionWorkSpaceSize(uint16_t fmt,
    unsigned long *wsbufsz, unsigned long *wsfragsz);
  int RtlCompressBuffer(uint16_t fmt,
    const uint8_t *src, unsigned long srclen,
    uint8_t *dst, unsigned long dstsz,
    unsigned long chunk, unsigned long *dstlen, void *workspace);
  int RtlDecompressBuffer(uint16_t fmt,
    uint8_t *dst, unsigned long dstsz,
    const uint8_t *src, unsigned long srclen,
    unsigned long *dstlen);
  ]]

  local ntdll = ffi.load("ntdll")

  local fmt = 0x0102
  local workspace
  do
    local res = ffi.new("unsigned long[2]")
    ntdll.RtlGetCompressionWorkSpaceSize(fmt, res, res+1)
    workspace = ffi.new("uint8_t[?]", res[0])
  end

  function compress(txt)
    local buf = ffi.new("uint8_t[?]", 4096)
    local buflen = ffi.new("unsigned long[1]")
    local res = ntdll.RtlCompressBuffer(fmt, txt, #txt, buf, 4096,
					4096, buflen, workspace)
    assert(res == 0)
    return ffi.string(buf, buflen[0])
  end

  function uncompress(comp, n)
    local buf = ffi.new("uint8_t[?]", n)
    local buflen = ffi.new("unsigned long[1]")
    local res = ntdll.RtlDecompressBuffer(fmt, buf, n, comp, #comp, buflen)
    assert(res == 0)
    return ffi.string(buf, buflen[0])
  end

else

  ffi.cdef[[
  unsigned long compressBound(unsigned long sourceLen);
  int compress2(uint8_t *dest, unsigned long *destLen,
		const uint8_t *source, unsigned long sourceLen, int level);
  int uncompress(uint8_t *dest, unsigned long *destLen,
		 const uint8_t *source, unsigned long sourceLen);
  ]]

  local zlib = ffi.load("z")

  function compress(txt)
    local n = tonumber(zlib.compressBound(#txt))
    local buf = ffi.new("uint8_t[?]", n)
    local buflen = ffi.new("unsigned long[1]", n)
    local res = zlib.compress2(buf, buflen, txt, #txt, 9)
    assert(res == 0)
    return ffi.string(buf, tonumber(buflen[0]))
  end

  function uncompress(comp, n)
    local buf = ffi.new("uint8_t[?]", n)
    local buflen = ffi.new("unsigned long[1]", n)
    local res = zlib.uncompress(buf, buflen, comp, #comp)
    assert(res == 0)
    return ffi.string(buf, tonumber(buflen[0]))
  end

end

  local txt = [[Rebellious subjects, enemies to peace,
Profaners of this neighbour-stained steel,--
Will they not hear? What, ho! you men, you beasts,
That quench the fire of your pernicious rage
With purple fountains issuing from your veins,
On pain of torture, from those bloody hands
Throw your mistemper'd weapons to the ground,
And hear the sentence of your moved prince.
Three civil brawls, bred of an airy word,
By thee, old Capulet, and Montague,
Have thrice disturb'd the quiet of our streets,
And made Verona's ancient citizens
Cast by their grave beseeming ornaments,
To wield old partisans, in hands as old,
Canker'd with peace, to part your canker'd hate:
If ever you disturb our streets again,
Your lives shall pay the forfeit of the peace.
For this time, all the rest depart away:
You Capulet; shall go along with me:
And, Montague, come you this afternoon,
To know our further pleasure in this case,
To old Free-town, our common judgment-place.
Once more, on pain of death, all men depart.]]
txt = txt..txt..txt..txt

local c = compress(txt)
assert(2*#c < #txt)
local txt2 = uncompress(c, #txt)
assert(txt2 == txt)

