-- From Robert G. Jakabosky, 2012-03-20

local N=tonumber(arg[1] or 10000)

local ffi=require"ffi"

ffi.cdef[[
struct Buffer { void *buf; };
typedef struct Buffer Buffer;
]]

local Buffer_mt = { __index = {} }
local Buffer = ffi.typeof("Buffer")

-- used to track alive objects
local nobj_obj_flags = {}

local function obj_to_id(ptr)
  return tonumber(ffi.cast('uintptr_t', ffi.cast('void *', ptr)))
end

function obj_type_Buffer_push(val)
  local obj = Buffer(val)
  local id = obj_to_id(obj)
  nobj_obj_flags[id] = true
  return obj
end

local function Buffer_new(len)
  local buf = ffi.cast('void *', 0xdeadbeef)
  return obj_type_Buffer_push(buf)
end

function obj_type_Buffer_delete(obj)
  local id = obj_to_id(obj)
  if not nobj_obj_flags[id] then return nil end
  nobj_obj_flags[id] = nil
  return obj.buf
end

local getmeta = debug.getmetatable

local function Buffer_close(self)
  local buf = obj_type_Buffer_delete(self)
  getmeta("Buffer_close") -- cause trace to abort
  if buf then
    self.buf = nil
  end
end
Buffer_mt.__gc = Buffer_close
Buffer_mt.__index.close = Buffer_close

ffi.metatype(Buffer, Buffer_mt)

local cdata = {}
for x=1,2 do
  cdata = {}
  for i=1,N do
    cdata[i] = Buffer_new(1)
  end
  for i=1,N do
    cdata[i]:close()
  end
  cdata = nil
end

