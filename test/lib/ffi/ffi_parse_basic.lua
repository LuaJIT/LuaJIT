local ffi = require("ffi")

dofile("../common/ffi_util.inc")

checkfail{
  "",
  " ",
  "\n",
  "1",
  ".",
  ";",
  ",",
  "*",
  "[]",
  "()",
  "(*)",
  "//",
  "/*",
  "xyz",
  "const",
  "volatile",
  "typedef",
  "extern",
  "static",
  "auto",
  "register",
  "struct",
  "union",
  "sizeof",
  "int int",
  "int char",
  "int double",
  "int;",
}

checktypes{
  1,	1,	"char",
  1,	1,	" \n\r\t\vchar \n\r\t\v",
  1,	1,	"ch\\\nar",
  1,	1,	"char /* abc */",
  1,	1,	"char /* abc */ const",
  1,	1,	"char // abc\n const",
}

checktypes{
  nil,	1,	"void",
  1,	1,	"bool",
  1,	1,	"_Bool",
  4,	4,	"_Bool int",
  1,	1,	"char",
  1,	1,	"signed char",
  1,	1,	"unsigned char",
  2,	2,	"short",
  2,	2,	"signed short",
  2,	2,	"unsigned short",
  4,	4,	"int",
  4,	4,	"signed int",
  4,	4,	"unsigned int",
  4,	4,	"signed",
  4,	4,	"unsigned",
  4,	4,	"float",
  8,	8,	"long long",
  8,	8,	"signed long long",
  8,	8,	"unsigned long long",
  8,	8,	"double",
  -- NYI: long double is architecture- and compiler-specific.
  8,	4,	"_Complex float",
  16,	8,	"_Complex",
  16,	8,	"_Complex double",
}

-- mode/vector_size attributes
checktypes{
  1,	1,	"int __attribute__((mode(QI)))",
  2,	2,	"int __attribute__((mode(HI)))",
  4,	4,	"int __attribute__((mode(SI)))",
  8,	8,	"int __attribute__((mode(DI)))",
  16,	16,	"int __attribute__((mode(TI)))",
  32,	16,	"int __attribute__((mode(OI)))",
  4,	4,	"float __attribute__((mode(SF)))",
  8,	8,	"float __attribute__((mode(DF)))",
  2,	2,	"int __attribute__((mode(V2QI)))",
  16,	16,	"float __attribute__((mode(V4SF)))",
  32,	16,	"double __attribute__((mode(V8SF)))",
  8,	8,	"char __attribute__((vector_size(8)))",
  16,	16,	"int __attribute__((vector_size(16)))",
  32,	16,	"double __attribute__((vector_size(32)))",
  64,	16,	"double __attribute__((vector_size(64)))",
}

-- ABI-specific types:
local L = (ffi.abi("32bit") or ffi.abi("win")) and 4 or 8
local P = ffi.abi("32bit") and 4 or 8
local W = ffi.abi("win") and 2 or 4

checktypes{
  L,	L,	"long",
  L,	L,	"signed long",
  L,	L,	"unsigned long",
  P,	P,	"int *",
  P,	P,	"int **",
  4,	4,	"int * __ptr32",
}

checktypes{
  P,	P,	"ptrdiff_t",
  P,	P,	"size_t",
  W,	W,	"wchar_t",
  1,	1,	"int8_t",
  2,	2,	"int16_t",
  4,	4,	"int32_t",
  8,	8,	"int64_t",
  1,	1,	"uint8_t",
  2,	2,	"uint16_t",
  4,	4,	"uint32_t",
  8,	8,	"uint64_t",
  P,	P,	"intptr_t",
  P,	P,	"uintptr_t",
}

checktypes{
  1,	8,	"char __attribute__((aligned(8)))",
  1,	8,	"char __attribute((aligned(8)))",
  1,	8,	"char __attribute__((__aligned__(8)))",
  1,	8,	"__attribute__((aligned(8))) char",
  1,	8,	"char __declspec(align(8))",
  1,	8,	"__declspec(align(8)) char",
  1,	2,	"char __attribute__((aligned(8))) const __attribute__((aligned(2)))",
  1,	16,	"char __attribute__((aligned(8))) const __attribute__((aligned(16)))",
}

