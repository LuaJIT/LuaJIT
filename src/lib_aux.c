/*
** Auxiliary library for the Lua/C API.
** Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
**
** Major parts taken verbatim or adapted from the Lua interpreter.
** Copyright (C) 1994-2008 Lua.org, PUC-Rio. See Copyright Notice in lua.h
*/

#include <errno.h>
#include <stdarg.h>
#include <stdio.h>

#define lib_aux_c
#define LUA_LIB

#include "lua.h"
#include "lauxlib.h"

#include "lj_obj.h"
#include "lj_err.h"
#include "lj_state.h"
#include "lj_lib.h"

/* -- Module registration ------------------------------------------------- */

LUALIB_API const char *luaL_findtable(lua_State *L, int idx,
				      const char *fname, int szhint)
{
  const char *e;
  lua_pushvalue(L, idx);
  do {
    e = strchr(fname, '.');
    if (e == NULL) e = fname + strlen(fname);
    lua_pushlstring(L, fname, (size_t)(e - fname));
    lua_rawget(L, -2);
    if (lua_isnil(L, -1)) {  /* no such field? */
      lua_pop(L, 1);  /* remove this nil */
      lua_createtable(L, 0, (*e == '.' ? 1 : szhint)); /* new table for field */
      lua_pushlstring(L, fname, (size_t)(e - fname));
      lua_pushvalue(L, -2);
      lua_settable(L, -4);  /* set new table into field */
    } else if (!lua_istable(L, -1)) {  /* field has a non-table value? */
      lua_pop(L, 2);  /* remove table and value */
      return fname;  /* return problematic part of the name */
    }
    lua_remove(L, -2);  /* remove previous table */
    fname = e + 1;
  } while (*e == '.');
  return NULL;
}

static int libsize(const luaL_Reg *l)
{
  int size = 0;
  for (; l->name; l++) size++;
  return size;
}

LUALIB_API void luaL_openlib(lua_State *L, const char *libname,
			     const luaL_Reg *l, int nup)
{
  lj_lib_checkfpu(L);
  if (libname) {
    int size = libsize(l);
    /* check whether lib already exists */
    luaL_findtable(L, LUA_REGISTRYINDEX, "_LOADED", 16);
    lua_getfield(L, -1, libname);  /* get _LOADED[libname] */
    if (!lua_istable(L, -1)) {  /* not found? */
      lua_pop(L, 1);  /* remove previous result */
      /* try global variable (and create one if it does not exist) */
      if (luaL_findtable(L, LUA_GLOBALSINDEX, libname, size) != NULL)
	lj_err_callerv(L, LJ_ERR_BADMODN, libname);
      lua_pushvalue(L, -1);
      lua_setfield(L, -3, libname);  /* _LOADED[libname] = new table */
    }
    lua_remove(L, -2);  /* remove _LOADED table */
    lua_insert(L, -(nup+1));  /* move library table to below upvalues */
  }
  for (; l->name; l++) {
    int i;
    for (i = 0; i < nup; i++)  /* copy upvalues to the top */
      lua_pushvalue(L, -nup);
    lua_pushcclosure(L, l->func, nup);
    lua_setfield(L, -(nup+2), l->name);
  }
  lua_pop(L, nup);  /* remove upvalues */
}

LUALIB_API void luaL_register(lua_State *L, const char *libname,
			      const luaL_Reg *l)
{
  luaL_openlib(L, libname, l, 0);
}

LUALIB_API const char *luaL_gsub(lua_State *L, const char *s,
				 const char *p, const char *r)
{
  const char *wild;
  size_t l = strlen(p);
  luaL_Buffer b;
  luaL_buffinit(L, &b);
  while ((wild = strstr(s, p)) != NULL) {
    luaL_addlstring(&b, s, (size_t)(wild - s));  /* push prefix */
    luaL_addstring(&b, r);  /* push replacement in place of pattern */
    s = wild + l;  /* continue after `p' */
  }
  luaL_addstring(&b, s);  /* push last suffix */
  luaL_pushresult(&b);
  return lua_tostring(L, -1);
}

/* -- Buffer handling ----------------------------------------------------- */

#define bufflen(B)	((size_t)((B)->p - (B)->buffer))
#define bufffree(B)	((size_t)(LUAL_BUFFERSIZE - bufflen(B)))

static int emptybuffer(luaL_Buffer *B)
{
  size_t l = bufflen(B);
  if (l == 0)
    return 0;  /* put nothing on stack */
  lua_pushlstring(B->L, B->buffer, l);
  B->p = B->buffer;
  B->lvl++;
  return 1;
}

static void adjuststack(luaL_Buffer *B)
{
  if (B->lvl > 1) {
    lua_State *L = B->L;
    int toget = 1;  /* number of levels to concat */
    size_t toplen = lua_strlen(L, -1);
    do {
      size_t l = lua_strlen(L, -(toget+1));
      if (!(B->lvl - toget + 1 >= LUA_MINSTACK/2 || toplen > l))
	break;
      toplen += l;
      toget++;
    } while (toget < B->lvl);
    lua_concat(L, toget);
    B->lvl = B->lvl - toget + 1;
  }
}

LUALIB_API char *luaL_prepbuffer(luaL_Buffer *B)
{
  if (emptybuffer(B))
    adjuststack(B);
  return B->buffer;
}

LUALIB_API void luaL_addlstring(luaL_Buffer *B, const char *s, size_t l)
{
  while (l--)
    luaL_addchar(B, *s++);
}

LUALIB_API void luaL_addstring(luaL_Buffer *B, const char *s)
{
  luaL_addlstring(B, s, strlen(s));
}

LUALIB_API void luaL_pushresult(luaL_Buffer *B)
{
  emptybuffer(B);
  lua_concat(B->L, B->lvl);
  B->lvl = 1;
}

LUALIB_API void luaL_addvalue(luaL_Buffer *B)
{
  lua_State *L = B->L;
  size_t vl;
  const char *s = lua_tolstring(L, -1, &vl);
  if (vl <= bufffree(B)) {  /* fit into buffer? */
    memcpy(B->p, s, vl);  /* put it there */
    B->p += vl;
    lua_pop(L, 1);  /* remove from stack */
  } else {
    if (emptybuffer(B))
      lua_insert(L, -2);  /* put buffer before new value */
    B->lvl++;  /* add new value into B stack */
    adjuststack(B);
  }
}

LUALIB_API void luaL_buffinit(lua_State *L, luaL_Buffer *B)
{
  B->L = L;
  B->p = B->buffer;
  B->lvl = 0;
}

/* -- Reference management ------------------------------------------------ */

#define FREELIST_REF	0

/* Convert a stack index to an absolute index. */
#define abs_index(L, i) \
  ((i) > 0 || (i) <= LUA_REGISTRYINDEX ? (i) : lua_gettop(L) + (i) + 1)

LUALIB_API int luaL_ref(lua_State *L, int t)
{
  int ref;
  t = abs_index(L, t);
  if (lua_isnil(L, -1)) {
    lua_pop(L, 1);  /* remove from stack */
    return LUA_REFNIL;  /* `nil' has a unique fixed reference */
  }
  lua_rawgeti(L, t, FREELIST_REF);  /* get first free element */
  ref = (int)lua_tointeger(L, -1);  /* ref = t[FREELIST_REF] */
  lua_pop(L, 1);  /* remove it from stack */
  if (ref != 0) {  /* any free element? */
    lua_rawgeti(L, t, ref);  /* remove it from list */
    lua_rawseti(L, t, FREELIST_REF);  /* (t[FREELIST_REF] = t[ref]) */
  } else {  /* no free elements */
    ref = (int)lua_objlen(L, t);
    ref++;  /* create new reference */
  }
  lua_rawseti(L, t, ref);
  return ref;
}

LUALIB_API void luaL_unref(lua_State *L, int t, int ref)
{
  if (ref >= 0) {
    t = abs_index(L, t);
    lua_rawgeti(L, t, FREELIST_REF);
    lua_rawseti(L, t, ref);  /* t[ref] = t[FREELIST_REF] */
    lua_pushinteger(L, ref);
    lua_rawseti(L, t, FREELIST_REF);  /* t[FREELIST_REF] = ref */
  }
}

/* -- Load Lua code ------------------------------------------------------- */

typedef struct FileReaderCtx {
  FILE *fp;
  char buf[LUAL_BUFFERSIZE];
} FileReaderCtx;

static const char *reader_file(lua_State *L, void *ud, size_t *size)
{
  FileReaderCtx *ctx = (FileReaderCtx *)ud;
  UNUSED(L);
  if (feof(ctx->fp)) return NULL;
  *size = fread(ctx->buf, 1, sizeof(ctx->buf), ctx->fp);
  return *size > 0 ? ctx->buf : NULL;
}

LUALIB_API int luaL_loadfile(lua_State *L, const char *filename)
{
  FileReaderCtx ctx;
  int status;
  const char *chunkname;
  if (filename) {
    ctx.fp = fopen(filename, "rb");
    if (ctx.fp == NULL) {
      lua_pushfstring(L, "cannot open %s: %s", filename, strerror(errno));
      return LUA_ERRFILE;
    }
    chunkname = lua_pushfstring(L, "@%s", filename);
  } else {
    ctx.fp = stdin;
    chunkname = "=stdin";
  }
  status = lua_load(L, reader_file, &ctx, chunkname);
  if (ferror(ctx.fp)) {
    L->top -= filename ? 2 : 1;
    lua_pushfstring(L, "cannot read %s: %s", chunkname+1, strerror(errno));
    if (filename)
      fclose(ctx.fp);
    return LUA_ERRFILE;
  }
  if (filename) {
    L->top--;
    copyTV(L, L->top-1, L->top);
    fclose(ctx.fp);
  }
  return status;
}

typedef struct StringReaderCtx {
  const char *str;
  size_t size;
} StringReaderCtx;

static const char *reader_string(lua_State *L, void *ud, size_t *size)
{
  StringReaderCtx *ctx = (StringReaderCtx *)ud;
  UNUSED(L);
  if (ctx->size == 0) return NULL;
  *size = ctx->size;
  ctx->size = 0;
  return ctx->str;
}

LUALIB_API int luaL_loadbuffer(lua_State *L, const char *buf, size_t size,
			       const char *name)
{
  StringReaderCtx ctx;
  ctx.str = buf;
  ctx.size = size;
  return lua_load(L, reader_string, &ctx, name);
}

LUALIB_API int luaL_loadstring(lua_State *L, const char *s)
{
  return luaL_loadbuffer(L, s, strlen(s), s);
}

/* -- Default allocator and panic function -------------------------------- */

static int panic(lua_State *L)
{
  fprintf(stderr, "PANIC: unprotected error in call to Lua API (%s)\n",
	  lua_tostring(L, -1));
  return 0;
}

#ifdef LUAJIT_USE_SYSMALLOC

#if LJ_64
#error "Must use builtin allocator for 64 bit target"
#endif

static void *mem_alloc(void *ud, void *ptr, size_t osize, size_t nsize)
{
  (void)ud;
  (void)osize;
  if (nsize == 0) {
    free(ptr);
    return NULL;
  } else {
    return realloc(ptr, nsize);
  }
}

LUALIB_API lua_State *luaL_newstate(void)
{
  lua_State *L = lua_newstate(mem_alloc, NULL);
  if (L) G(L)->panic = panic;
  return L;
}

#else

#include "lj_alloc.h"

LUALIB_API lua_State *luaL_newstate(void)
{
  lua_State *L;
  void *ud = lj_alloc_create();
  if (ud == NULL) return NULL;
#if LJ_64
  L = lj_state_newstate(lj_alloc_f, ud);
#else
  L = lua_newstate(lj_alloc_f, ud);
#endif
  if (L) G(L)->panic = panic;
  return L;
}

#if LJ_64
LUA_API lua_State *lua_newstate(lua_Alloc f, void *ud)
{
  UNUSED(f); UNUSED(ud);
  fprintf(stderr, "Must use luaL_newstate() for 64 bit target\n");
  return NULL;
}
#endif

#endif

