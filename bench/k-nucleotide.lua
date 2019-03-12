
local function kfrequency(seq, freq, k, frame)
  local sub = string.sub
  local k1 = k - 1
  for i=frame,#seq-k1,k do
    local c = sub(seq, i, i+k1)
    freq[c] = (freq[c] or 0) + 1
  end
end

local function count(seq, frag)
  local k = #frag
  local freq = {}
  for frame=1,k do kfrequency(seq, freq, k, frame) end
  io.write(freq[frag] or 0, "\t", frag, "\n")
end

local function frequency(seq, k)
  local freq = {}
  for frame=1,k do kfrequency(seq, freq, k, frame) end
  local sfreq, sn, sum = {}, 1, 0
  for c,v in pairs(freq) do sfreq[sn] = c; sn = sn + 1; sum = sum + v end
  table.sort(sfreq, function(a, b)
    local fa, fb = freq[a], freq[b]
    return fa == fb and a > b or fa > fb
  end)
  for _,c in ipairs(sfreq) do
    io.write(string.format("%s %0.3f\n", c, (freq[c]*100)/sum))
  end
  io.write("\n")
end

local function readseq(f)
  local sub = string.sub
  local lines, ln, seqstarted = {}, 0, false
  for line in io.lines(f) do
    if not seqstarted then
      if sub(line, 1, 1) == ">" and sub(line, 2, 6) == "THREE" then seqstarted = true end
    else
      local c = sub(line, 1, 1)
      if c == ">" then
        break
      elseif c ~= ";" then
        ln = ln + 1
        lines[ln] = line
      end
    end
  end
  return string.upper(table.concat(lines, "", 1, ln))
end

-- If we are called through a lua_pcall as a chunk, we need to fix up args.
if not arg then arg = {...} end

local seq = readseq(arg and arg[1])
frequency(seq, 1)
frequency(seq, 2)
count(seq, "GGT")
count(seq, "GGTA")
count(seq, "GGTATT")
count(seq, "GGTATTTTAATT")
count(seq, "GGTATTTTAATTTATAGT")
