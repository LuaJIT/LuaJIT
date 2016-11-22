local function hash(str)
    -- this function should exactly match lj_ks function below
    local h = 0
    local l, r
    for i=1,#str do
        h = h * 9 + string.byte(str, i)
        h = h % 0x100000000
        -- h = lj_rol(h, 7) = (h<<7) | (h>>25)
        l = h * 128
        r = h / 33554432
        h = l % 0x100000000 + (r - r%1)
    end
    return h
end
local all = {}
while true do
    s = io.read('*l')
    if not s then break end
    for w in string.gmatch(s, "LJ_KS_[_%w]+") do
        all[w] = true
    end
end
io.write([[
#ifndef LJ_KNOWN_STRINGS_H
#define LJ_KNOWN_STRINGS_H
/* small hash function for lj_cparse.c and lib_ffi.c */
static LJ_AINLINE uint32_t lj_ks(GCstr* s)
{
  uint32_t h = 0;
  const uint8_t* v = (const uint8_t*)strdata(s);
  /* do not use s->len for loop cause clang produces bad code then */
  for (; *v; v++) {
    h = h*9 + *v;
    h = lj_rol(h, 7);
  }
  return h;
}
]])
local keys = {}
for w, _ in pairs(all) do
    table.insert(keys, w)
end
table.sort(keys)
for _, w in ipairs(keys) do
    local h = hash(string.sub(w, 7, -1))
    local s = string.format("#define %s\t0x%08x\n", w, h)
    io.write(s)
end
io.write("#endif\n")
