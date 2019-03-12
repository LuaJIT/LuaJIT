local ffi = require("ffi")

dofile("../common/ffi_util.inc")

if cdefs == "" then
  cdefs = "-pthread -D_REENTRANT -I/usr/include/gtk-2.0 -I/usr/lib/gtk-2.0/include -I/usr/include/atk-1.0 -I/usr/include/cairo -I/usr/include/pango-1.0 -I/usr/include/gio-unix-2.0/ -I/usr/include/glib-2.0 -I/usr/lib/glib-2.0/include -I/usr/include/pixman-1 -I/usr/include/freetype2 -I/usr/include/directfb -I/usr/include/libpng12 -I/usr/lib/x86_64-linux-gnu/glib-2.0/include -I/usr/lib/x86_64-linux-gnu/gtk-2.0/include -I/usr/include/gdk-pixbuf-2.0"
end

include"/usr/include/gtk-2.0/gtk/gtk.h"
