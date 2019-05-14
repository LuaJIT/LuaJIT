local ffi = require("ffi")

local array = ffi.new("struct { int x; } [1]")

-- This test forces the VM to unsink a pointer that was constructed
-- from a constant. The IR will include a 'cnewi' instruction to
-- allocate an FFI pointer object, the pointer value will be an IR
-- constant, the allocation will be sunk, and the allocation will
-- at some point be "unsunk" due to a reference in the snapshot for
-- a taken exit.

-- Note: JIT will recognize <array> as a "singleton" and allow its
-- address to be inlined ("constified") instead of looking up the
-- upvalue at runtime.

local function fn(i)
  local struct = array[0]   -- Load pointer that the JIT will constify.
  if i == 1000 then end     -- Force trace exit when i==1000.
  struct.x = 0              -- Ensure that 'struct' is live after exit.
end

-- Loop over the function to make it compile and take a trace exit
-- during the final iteration.
do --- unsink 64-bit pointers
  for i = 1, 1000 do
    fn(i)
  end
end
