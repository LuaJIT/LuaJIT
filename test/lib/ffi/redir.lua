local ffi = require("ffi")

do --- function
  ffi.cdef[[
  int redir_foo(const char *s) asm("strlen");
  ]]

  assert(ffi.C.redir_foo("abcd") == 4)
end

do --- variable -windows
  ffi.cdef[[
  int redir_bar asm("errno");
  ]]

  ffi.C.redir_bar = 14
  assert(ffi.C.redir_bar == 14)
  ffi.C.redir_bar = 0
end
