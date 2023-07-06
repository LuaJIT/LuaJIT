/*
** LuaJIT -- a Just-In-Time Compiler for Lua. https://luajit.org/
**
** Copyright (C) 2005-2023 Mike Pall. All rights reserved.
**
** Permission is hereby granted, free of charge, to any person obtaining
** a copy of this software and associated documentation files (the
** "Software"), to deal in the Software without restriction, including
** without limitation the rights to use, copy, modify, merge, publish,
** distribute, sublicense, and/or sell copies of the Software, and to
** permit persons to whom the Software is furnished to do so, subject to
** the following conditions:
**
** The above copyright notice and this permission notice shall be
** included in all copies or substantial portions of the Software.
**
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
** EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
** MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
** IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
** CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
** SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
**
** [ MIT license: https://www.opensource.org/licenses/mit-license.php ]
*/

#ifndef _LUAJIT_H
#define _LUAJIT_H

#include "lua.h"

#define LUAJIT_VERSION		"LuaJIT 2.1.ROLLING"
#define LUAJIT_VERSION_NUM	20199  /* Deprecated. */
#define LUAJIT_VERSION_SYM	luaJIT_version_2_1_ROLLING
#define LUAJIT_COPYRIGHT	"Copyright (C) 2005-2023 Mike Pall"
#define LUAJIT_URL		"https://luajit.org/"

/* Modes for luaJIT_setmode. */
#define LUAJIT_MODE_MASK	0x00ff

enum {
  LUAJIT_MODE_ENGINE,		/* Set mode for whole JIT engine. */
  LUAJIT_MODE_DEBUG,		/* Set debug mode (idx = level). */

  LUAJIT_MODE_FUNC,		/* Change mode for a function. */
  LUAJIT_MODE_ALLFUNC,		/* Recurse into subroutine protos. */
  LUAJIT_MODE_ALLSUBFUNC,	/* Change only the subroutines. */

  LUAJIT_MODE_TRACE,		/* Flush a compiled trace. */

  LUAJIT_MODE_WRAPCFUNC = 0x10,	/* Set wrapper mode for C function calls. */

  LUAJIT_MODE_MAX
};

/* Flags or'ed in to the mode. */
#define LUAJIT_MODE_OFF		0x0000	/* Turn feature off. */
#define LUAJIT_MODE_ON		0x0100	/* Turn feature on. */
#define LUAJIT_MODE_FLUSH	0x0200	/* Flush JIT-compiled code. */

/* LuaJIT public C API. */

/* Control the JIT engine. */
LUA_API int luaJIT_setmode(lua_State *L, int idx, int mode);

/* Low-overhead profiling API. */
typedef void (*luaJIT_profile_callback)(void *data, lua_State *L,
					int samples, int vmstate);
LUA_API void luaJIT_profile_start(lua_State *L, const char *mode,
				  luaJIT_profile_callback cb, void *data);
LUA_API void luaJIT_profile_stop(lua_State *L);
LUA_API const char *luaJIT_profile_dumpstack(lua_State *L, const char *fmt,
					     int depth, size_t *len);

typedef unsigned (*luaJIT_allocpages)(void *ud, void **pages, unsigned n);
/* Free is called one last time with NULL, 0 to indicate any state should be released */
typedef void (*luaJIT_freepages)(void *ud, void **pages, unsigned n);
typedef void* (*luaJIT_reallochuge)(void *ud, void *p, size_t osz, size_t nsz);

/* This many bytes are reserved at the start of each huge arena for the allocator's use */
#define LUAJIT_HUGE_RESERVED_SPACE 20

LUA_API size_t luaJIT_getpagesize();

/* Custom allocator support.
 * Page allocators must yield naturally aligned regions in the size provided by
 * luaJIT_getpagesize. Page allocators allocate and free N pages at once. Huge
 * allocations are one at a time.
 * The lua_Alloc allocator is used for allocations that require variable size
 * and a fixed address.
 * Allocators are permitted to fail. The huge page allocator can return NULL and
 * the page allocator can return fewer than the requested amount. Allocators are not
 * permitted to throw.
 * Huge allocations have the same alignment requirements as normal arenas.
 * Either the normal or page allocation can be NULL and the default will be used.
 * Passing all NULL is functionally the same as luaL_newstate()
 */
LUA_API lua_State *luaJIT_newstate(lua_Alloc f, void *ud,
                                   luaJIT_allocpages allocp,
                                   luaJIT_freepages freep,
                                   luaJIT_reallochuge realloch,
                                   void *page_ud);

/* As lua_createtable, but can be used with __gc */
LUA_API void luaJIT_createtable(lua_State *L, int narray, int nrec);

/* Enforce (dynamic) linker error for version mismatches. Call from main. */
LUA_API void LUAJIT_VERSION_SYM(void);

#endif
