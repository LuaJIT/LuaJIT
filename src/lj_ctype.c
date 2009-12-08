/*
** Internal CTYPE replacement.
** Donated to the public domain.
**
** This is intended to replace the problematic libc single-byte NLS functions.
** These just don't make sense anymore with UTF-8 locales becoming the norm
** on POSIX systems. It never worked too well on Windows systems since hardly
** anyone bothered to call setlocale().
**
** Instead this table is hardcoded for ASCII, except for identifiers. These
** include the characters 128-255, too. This allows for the use of all
** non-ASCII chars as identifiers in the lexer. This is a broad definition,
** but works well in practice for both UTF-8 locales and most single-byte
** locales (such as ISO-8859-*).
**
** If you really need proper ctypes for UTF-8 strings, please use an add-on
** library such as slnunicode: http://luaforge.net/projects/sln/
*/

#define lj_ctype_c
#define LUA_CORE

#include "lj_ctype.h"

LJ_DATADEF const uint8_t lj_ctype_bits[257] = {
    0,
    1,  1,  1,  1,  1,  1,  1,  1,  1,  3,  3,  3,  3,  3,  1,  1,
    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
    2,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
  152,152,152,152,152,152,152,152,152,152,  4,  4,  4,  4,  4,  4,
    4,176,176,176,176,176,176,160,160,160,160,160,160,160,160,160,
  160,160,160,160,160,160,160,160,160,160,160,  4,  4,  4,  4,132,
    4,208,208,208,208,208,208,192,192,192,192,192,192,192,192,192,
  192,192,192,192,192,192,192,192,192,192,192,  4,  4,  4,  4,  1,
  128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,
  128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,
  128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,
  128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,
  128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,
  128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,
  128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,
  128,128,128,128,128,128,128,128,128,128,128,128,128,128,128,128
};

