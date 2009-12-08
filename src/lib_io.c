/*
** I/O library.
** Copyright (C) 2005-2009 Mike Pall. See Copyright Notice in luajit.h
**
** Major portions taken verbatim or adapted from the Lua interpreter.
** Copyright (C) 1994-2008 Lua.org, PUC-Rio. See Copyright Notice in lua.h
*/

#include <errno.h>
#include <stdio.h>

#define lib_io_c
#define LUA_LIB

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "lj_obj.h"
#include "lj_err.h"
#include "lj_gc.h"
#include "lj_ff.h"
#include "lj_lib.h"

/* Index of standard handles in function environment. */
#define IO_INPUT	1
#define IO_OUTPUT	2

/* -- Error handling ------------------------------------------------------ */

static int io_pushresult(lua_State *L, int ok, const char *fname)
{
  if (ok) {
    setboolV(L->top++, 1);
    return 1;
  } else {
    int en = errno;  /* Lua API calls may change this value. */
    lua_pushnil(L);
    if (fname)
      lua_pushfstring(L, "%s: %s", fname, strerror(en));
    else
      lua_pushfstring(L, "%s", strerror(en));
    lua_pushinteger(L, en);
    return 3;
  }
}

static void io_file_error(lua_State *L, int arg, const char *fname)
{
  lua_pushfstring(L, "%s: %s", fname, strerror(errno));
  luaL_argerror(L, arg, lua_tostring(L, -1));
}

/* -- Open helpers -------------------------------------------------------- */

#define io_tofilep(L)	((FILE **)luaL_checkudata(L, 1, LUA_FILEHANDLE))

static FILE *io_tofile(lua_State *L)
{
  FILE **f = io_tofilep(L);
  if (*f == NULL)
    lj_err_caller(L, LJ_ERR_IOCLFL);
  return *f;
}

static FILE **io_file_new(lua_State *L)
{
  FILE **pf = (FILE **)lua_newuserdata(L, sizeof(FILE *));
  *pf = NULL;
  luaL_getmetatable(L, LUA_FILEHANDLE);
  lua_setmetatable(L, -2);
  return pf;
}

/* -- Close helpers ------------------------------------------------------- */

static int lj_cf_io_std_close(lua_State *L)
{
  lua_pushnil(L);
  lua_pushliteral(L, "cannot close standard file");
  return 2;
}

static int lj_cf_io_pipe_close(lua_State *L)
{
  FILE **p = io_tofilep(L);
#if defined(LUA_USE_POSIX)
  int ok = (pclose(*p) != -1);
#elif defined(LUA_USE_WIN)
  int ok = (_pclose(*p) != -1);
#else
  int ok = 0;
#endif
  *p = NULL;
  return io_pushresult(L, ok, NULL);
}

static int lj_cf_io_file_close(lua_State *L)
{
  FILE **p = io_tofilep(L);
  int ok = (fclose(*p) == 0);
  *p = NULL;
  return io_pushresult(L, ok, NULL);
}

static int io_file_close(lua_State *L)
{
  lua_getfenv(L, 1);
  lua_getfield(L, -1, "__close");
  return (lua_tocfunction(L, -1))(L);
}

/* -- Read/write helpers -------------------------------------------------- */

static int io_file_readnum(lua_State *L, FILE *fp)
{
  lua_Number d;
  if (fscanf(fp, LUA_NUMBER_SCAN, &d) == 1) {
    lua_pushnumber(L, d);
    return 1;
  } else {
    return 0;  /* read fails */
  }
}

static int test_eof(lua_State *L, FILE *fp)
{
  int c = getc(fp);
  ungetc(c, fp);
  lua_pushlstring(L, NULL, 0);
  return (c != EOF);
}

static int io_file_readline(lua_State *L, FILE *fp)
{
  luaL_Buffer b;
  luaL_buffinit(L, &b);
  for (;;) {
    size_t len;
    char *p = luaL_prepbuffer(&b);
    if (fgets(p, LUAL_BUFFERSIZE, fp) == NULL) {  /* EOF? */
      luaL_pushresult(&b);
      return (strV(L->top-1)->len > 0);  /* Anything read? */
    }
    len = strlen(p);
    if (len == 0 || p[len-1] != '\n') {  /* Partial line? */
      luaL_addsize(&b, len);
    } else {
      luaL_addsize(&b, len - 1);  /* Don't include EOL. */
      luaL_pushresult(&b);
      return 1;  /* Got at least an EOL. */
    }
  }
}

static int io_file_readchars(lua_State *L, FILE *fp, size_t n)
{
  size_t rlen;  /* how much to read */
  size_t nr;  /* number of chars actually read */
  luaL_Buffer b;
  luaL_buffinit(L, &b);
  rlen = LUAL_BUFFERSIZE;  /* try to read that much each time */
  do {
    char *p = luaL_prepbuffer(&b);
    if (rlen > n) rlen = n;  /* cannot read more than asked */
    nr = fread(p, 1, rlen, fp);
    luaL_addsize(&b, nr);
    n -= nr;  /* still have to read `n' chars */
  } while (n > 0 && nr == rlen);  /* until end of count or eof */
  luaL_pushresult(&b);  /* close buffer */
  return (n == 0 || lua_objlen(L, -1) > 0);
}

static int io_file_read(lua_State *L, FILE *fp, int start)
{
  int ok, n, nargs = (L->top - L->base) - start;
  clearerr(fp);
  if (nargs == 0) {
    ok = io_file_readline(L, fp);
    n = start+1;  /* Return 1 result. */
  } else {
    /* The results plus the buffers go on top of the args. */
    luaL_checkstack(L, nargs+LUA_MINSTACK, "too many arguments");
    ok = 1;
    for (n = start; nargs-- && ok; n++) {
      if (tvisstr(L->base+n)) {
	const char *p = strVdata(L->base+n);
	if (p[0] != '*')
	  lj_err_arg(L, n+1, LJ_ERR_INVOPT);
	if (p[1] == 'n')
	  ok = io_file_readnum(L, fp);
	else if (p[1] == 'l')
	  ok = io_file_readline(L, fp);
	else if (p[1] == 'a')
	  io_file_readchars(L, fp, ~((size_t)0));
	else
	  lj_err_arg(L, n+1, LJ_ERR_INVFMT);
      } else if (tvisnum(L->base+n)) {
	size_t len = (size_t)lj_lib_checkint(L, n+1);
	ok = len ? io_file_readchars(L, fp, len) : test_eof(L, fp);
      } else {
	lj_err_arg(L, n+1, LJ_ERR_INVOPT);
      }
    }
  }
  if (ferror(fp))
    return io_pushresult(L, 0, NULL);
  if (!ok)
    setnilV(L->top-1);  /* Replace last result with nil. */
  return n - start;
}

static int io_file_write(lua_State *L, FILE *fp, int start)
{
  cTValue *tv;
  int status = 1;
  for (tv = L->base+start; tv < L->top; tv++) {
    if (tvisstr(tv)) {
      MSize len = strV(tv)->len;
      status = status && (fwrite(strVdata(tv), 1, len, fp) == len);
    } else if (tvisnum(tv)) {
      status = status && (fprintf(fp, LUA_NUMBER_FMT, numV(tv)) > 0);
    } else {
      lj_lib_checkstr(L, tv-L->base+1);
    }
  }
  return io_pushresult(L, status, NULL);
}

/* -- I/O file methods ---------------------------------------------------- */

#define LJLIB_MODULE_io_method

LJLIB_CF(io_method_close)
{
  if (lua_isnone(L, 1))
    lua_rawgeti(L, LUA_ENVIRONINDEX, IO_OUTPUT);
  io_tofile(L);
  return io_file_close(L);
}

LJLIB_CF(io_method_read)
{
  return io_file_read(L, io_tofile(L), 1);
}

LJLIB_CF(io_method_write)
{
  return io_file_write(L, io_tofile(L), 1);
}

LJLIB_CF(io_method_flush)
{
  return io_pushresult(L, fflush(io_tofile(L)) == 0, NULL);
}

LJLIB_CF(io_method_seek)
{
  FILE *fp = io_tofile(L);
  int opt = lj_lib_checkopt(L, 2, 1, "\3set\3cur\3end");
  lua_Number ofs;
  int res;
  if (opt == 0) opt = SEEK_SET;
  else if (opt == 1) opt = SEEK_CUR;
  else if (opt == 2) opt = SEEK_END;
  lj_lib_opt(L, 3,
    ofs = lj_lib_checknum(L, 3);
    ,
    ofs = 0;
  )
#if defined(LUA_USE_POSIX)
  res = fseeko(fp, (int64_t)ofs, opt);
#elif _MSC_VER >= 1400
  res = _fseeki64(fp, (int64_t)ofs, opt);
#elif defined(__MINGW32__)
  res = fseeko64(fp, (int64_t)ofs, opt);
#else
  res = fseek(fp, (long)ofs, opt);
#endif
  if (res)
    return io_pushresult(L, 0, NULL);
#if defined(LUA_USE_POSIX)
  ofs = cast_num(ftello(fp));
#elif _MSC_VER >= 1400
  ofs = cast_num(_ftelli64(fp));
#elif defined(__MINGW32__)
  ofs = cast_num(ftello64(fp));
#else
  ofs = cast_num(ftell(fp));
#endif
  setnumV(L->top-1, ofs);
  return 1;
}

LJLIB_CF(io_method_setvbuf)
{
  FILE *fp = io_tofile(L);
  int opt = lj_lib_checkopt(L, 2, -1, "\4full\4line\2no");
  size_t sz = (size_t)lj_lib_optint(L, 3, LUAL_BUFFERSIZE);
  if (opt == 0) opt = _IOFBF;
  else if (opt == 1) opt = _IOLBF;
  else if (opt == 2) opt = _IONBF;
  return io_pushresult(L, (setvbuf(fp, NULL, opt, sz) == 0), NULL);
}

/* Forward declaration. */
static void io_file_lines(lua_State *L, int idx, int toclose);

LJLIB_CF(io_method_lines)
{
  io_tofile(L);
  io_file_lines(L, 1, 0);
  return 1;
}

LJLIB_CF(io_method___gc)
{
  FILE *fp = *io_tofilep(L);
  if (fp != NULL) io_file_close(L);
  return 0;
}

LJLIB_CF(io_method___tostring)
{
  FILE *fp = *io_tofilep(L);
  if (fp == NULL)
    lua_pushliteral(L, "file (closed)");
  else
    lua_pushfstring(L, "file (%p)", fp);
  return 1;
}

LJLIB_PUSH(top-1) LJLIB_SET(__index)

#include "lj_libdef.h"

/* -- I/O library functions ----------------------------------------------- */

#define LJLIB_MODULE_io

LJLIB_PUSH(top-2) LJLIB_SET(!)  /* Set environment. */

static FILE *io_file_get(lua_State *L, int findex)
{
  GCtab *fenv = tabref(curr_func(L)->c.env);
  GCudata *ud = udataV(&tvref(fenv->array)[findex]);
  FILE *fp = *(FILE **)uddata(ud);
  if (fp == NULL)
    lj_err_caller(L, LJ_ERR_IOSTDCL);
  return fp;
}

LJLIB_CF(io_open)
{
  const char *fname = luaL_checkstring(L, 1);
  const char *mode = luaL_optstring(L, 2, "r");
  FILE **pf = io_file_new(L);
  *pf = fopen(fname, mode);
  return (*pf == NULL) ? io_pushresult(L, 0, fname) : 1;
}

LJLIB_CF(io_tmpfile)
{
  FILE **pf = io_file_new(L);
  *pf = tmpfile();
  return (*pf == NULL) ? io_pushresult(L, 0, NULL) : 1;
}

LJLIB_CF(io_close)
{
  return lj_cf_io_method_close(L);
}

LJLIB_CF(io_read)
{
  return io_file_read(L, io_file_get(L, IO_INPUT), 0);
}

LJLIB_CF(io_write)
{
  return io_file_write(L, io_file_get(L, IO_OUTPUT), 0);
}

LJLIB_CF(io_flush)
{
  return io_pushresult(L, fflush(io_file_get(L, IO_OUTPUT)) == 0, NULL);
}

LJLIB_NOREG LJLIB_CF(io_lines_iter)
{
  FILE *fp = *(FILE **)uddata(udataV(lj_lib_upvalue(L, 1)));
  int ok;
  if (fp == NULL)
    lj_err_caller(L, LJ_ERR_IOCLFL);
  ok = io_file_readline(L, fp);
  if (ferror(fp))
    return luaL_error(L, "%s", strerror(errno));
  if (ok)
    return 1;
  if (tvistrue(lj_lib_upvalue(L, 2))) {  /* Need to close file? */
    L->top = L->base+1;
    setudataV(L, L->base, udataV(lj_lib_upvalue(L, 1)));
    io_file_close(L);
  }
  return 0;
}

static void io_file_lines(lua_State *L, int idx, int toclose)
{
  lua_pushvalue(L, idx);
  lua_pushboolean(L, toclose);
  lua_pushcclosure(L, lj_cf_io_lines_iter, 2);
  funcV(L->top-1)->c.ffid = FF_io_lines_iter;
}

LJLIB_CF(io_lines)
{
  if (lua_isnoneornil(L, 1)) {  /* no arguments? */
    /* will iterate over default input */
    lua_rawgeti(L, LUA_ENVIRONINDEX, IO_INPUT);
    return lj_cf_io_method_lines(L);
  } else {
    const char *fname = luaL_checkstring(L, 1);
    FILE **pf = io_file_new(L);
    *pf = fopen(fname, "r");
    if (*pf == NULL)
      io_file_error(L, 1, fname);
    io_file_lines(L, lua_gettop(L), 1);
    return 1;
  }
}

static int io_std_get(lua_State *L, int fp, const char *mode)
{
  if (!lua_isnoneornil(L, 1)) {
    const char *fname = lua_tostring(L, 1);
    if (fname) {
      FILE **pf = io_file_new(L);
      *pf = fopen(fname, mode);
      if (*pf == NULL)
	io_file_error(L, 1, fname);
    } else {
      io_tofile(L);  /* check that it's a valid file handle */
      lua_pushvalue(L, 1);
    }
    lua_rawseti(L, LUA_ENVIRONINDEX, fp);
  }
  /* return current value */
  lua_rawgeti(L, LUA_ENVIRONINDEX, fp);
  return 1;
}

LJLIB_CF(io_input)
{
  return io_std_get(L, IO_INPUT, "r");
}

LJLIB_CF(io_output)
{
  return io_std_get(L, IO_OUTPUT, "w");
}

LJLIB_CF(io_type)
{
  void *ud;
  luaL_checkany(L, 1);
  ud = lua_touserdata(L, 1);
  lua_getfield(L, LUA_REGISTRYINDEX, LUA_FILEHANDLE);
  if (ud == NULL || !lua_getmetatable(L, 1) || !lua_rawequal(L, -2, -1))
    lua_pushnil(L);  /* not a file */
  else if (*((FILE **)ud) == NULL)
    lua_pushliteral(L, "closed file");
  else
    lua_pushliteral(L, "file");
  return 1;
}

LJLIB_PUSH(top-3) LJLIB_SET(!)  /* Set environment. */

LJLIB_CF(io_popen)
{
#if defined(LUA_USE_POSIX) || defined(LUA_USE_WIN)
  const char *fname = luaL_checkstring(L, 1);
  const char *mode = luaL_optstring(L, 2, "r");
  FILE **pf = io_file_new(L);
#ifdef LUA_USE_POSIX
  fflush(NULL);
  *pf = popen(fname, mode);
#else
  *pf = _popen(fname, mode);
#endif
  return (*pf == NULL) ? io_pushresult(L, 0, fname) : 1;
#else
  luaL_error(L, LUA_QL("popen") " not supported");
#endif
}

#include "lj_libdef.h"

/* ------------------------------------------------------------------------ */

static void io_std_new(lua_State *L, FILE *fp, int k, const char *fname)
{
  FILE **pf = io_file_new(L);
  GCudata *ud = udataV(L->top-1);
  GCtab *envt = tabV(L->top-2);
  *pf = fp;
  setgcref(ud->env, obj2gco(envt));
  lj_gc_objbarrier(L, obj2gco(ud), envt);
  if (k > 0) {
    lua_pushvalue(L, -1);
    lua_rawseti(L, -5, k);
  }
  lua_setfield(L, -3, fname);
}

static void io_fenv_new(lua_State *L, int narr, lua_CFunction cls)
{
  lua_createtable(L, narr, 1);
  lua_pushcfunction(L, cls);
  lua_setfield(L, -2, "__close");
}

LUALIB_API int luaopen_io(lua_State *L)
{
  lua_getfield(L, LUA_REGISTRYINDEX, LUA_FILEHANDLE);
  if (tvisnil(L->top-1)) {
    LJ_LIB_REG_(L, NULL, io_method);
    lua_setfield(L, LUA_REGISTRYINDEX, LUA_FILEHANDLE);
  }
  io_fenv_new(L, 0, lj_cf_io_pipe_close);  /* top-3 */
  io_fenv_new(L, 2, lj_cf_io_file_close);  /* top-2 */
  LJ_LIB_REG(L, io);
  io_fenv_new(L, 0, lj_cf_io_std_close);
  io_std_new(L, stdin, IO_INPUT, "stdin");
  io_std_new(L, stdout, IO_OUTPUT, "stdout");
  io_std_new(L, stderr, 0, "stderr");
  L->top--;
  return 1;
}

