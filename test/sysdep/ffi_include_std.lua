local ffi = require("ffi")

dofile("../common/ffi_util.inc")

do
  local fp = assert(io.open("/tmp/__tmp.c", "w"))
  fp:write[[
#include <sqlite3.h>
#include <thread_db.h>
#include <resolv.h>
#include <mpfr.h>
#include <mpc.h>
#include <curses.h>
#include <form.h>
#include <aio.h>
#include <unistd.h>
#include <zlib.h>
#include <netdb.h>
#include <math.h>
#include <tgmath.h>
#include <complex.h>
#include <elf.h>
#include <mqueue.h>
#include <regex.h>
#include <fcntl.h>
]]
  fp:close()

  local flags = ffi.abi("32bit") and "-m32" or "-m64"
  fp = assert(io.popen("cc -E -P -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -D_GNU_SOURCE /tmp/__tmp.c "..flags))
  local s = fp:read("*a")
  fp:close()
  os.remove("/tmp/__tmp.c")
  ffi.cdef(s)
end

