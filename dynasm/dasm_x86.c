/*
  Encoding engine to use with dasm.lua.

  Compile with:

    gcc dasm_x86.c -DDASM_CHECKS -shared -s -o dasm_x86.so
*/

#include "dasm_extern.h"
#include "dasm_proto.h"
#include "dasm_x86.h"
