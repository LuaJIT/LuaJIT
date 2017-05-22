#ifndef LJ_FILE_OPEN_H
#define LJ_FILE_OPEN_H

#include <stdio.h>

#ifdef _WIN32
FILE *_lua_fopen(const char *filename, const char *mode);
FILE *_lua_freopen(const char *filename, const char *mode, FILE *oldfile);
#else
#define _lua_fopen(file, mode) fopen(file, mode)
#define _lua_freopen(file, mode, oldfile) freopen(file, mode, oldfile)
#endif

#endif // LJ_FILE_OPEN_H
