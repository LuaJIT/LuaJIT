/*
** I/O library.
** Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
**
** Major portions taken verbatim or adapted from the Lua interpreter.
** Copyright (C) 1994-2011 Lua.org, PUC-Rio. See Copyright Notice in lua.h
*/

#include <errno.h>
#include <stdio.h>

#define lib_io_c
#define LUA_LIB

#include "lua.h"
#include "lauxlib.h"
#include "lualib.h"

#include "lj_obj.h"
#include "lj_gc.h"
#include "lj_err.h"
#include "lj_str.h"
#include "lj_ff.h"
#include "lj_trace.h"
#include "lj_lib.h"

/* Userdata payload for I/O file. */
typedef struct IOFileUD {
  FILE *fp;		/* File handle. */
  uint32_t type;	/* File type. */
} IOFileUD;

#define IOFILE_TYPE_FILE	0	/* Regular file. */
#define IOFILE_TYPE_PIPE	1	/* Pipe. */
#define IOFILE_TYPE_STDF	2	/* Standard file handle. */
#define IOFILE_TYPE_MASK	3

#define IOFILE_FLAG_CLOSE	4	/* Close after io.lines() iterator. */

#define IOSTDF_UD(L, id)	(&gcref(G(L)->gcroot[(id)])->ud)
#define IOSTDF_IOF(L, id)	((IOFileUD *)uddata(IOSTDF_UD(L, (id))))

/* -- Error handling ------------------------------------------------------ */

static int io_pushresult(lua_State *L, int ok, const char *fname)
{
  if (ok) {
    setboolV(L->top++, 1);
    return 1;
  } else {
    int en = errno;  /* Lua API calls may change this value. */
    setnilV(L->top++);
    if (fname)
      lua_pushfstring(L, "%s: %s", fname, strerror(en));
    else
      lua_pushfstring(L, "%s", strerror(en));
    setintV(L->top++, en);
    lj_trace_abort(G(L));
    return 3;
  }
}

/* -- Open/close helpers -------------------------------------------------- */

static IOFileUD *io_tofilep(lua_State *L)
{
  if (!(L->base < L->top && tvisudata(L->base) &&
	udataV(L->base)->udtype == UDTYPE_IO_FILE))
    lj_err_argtype(L, 1, "FILE*");
  return (IOFileUD *)uddata(udataV(L->base));
}

static IOFileUD *io_tofile(lua_State *L)
{
  IOFileUD *iof = io_tofilep(L);
  if (iof->fp == NULL)
    lj_err_caller(L, LJ_ERR_IOCLFL);
  return iof;
}

static FILE *io_stdfile(lua_State *L, ptrdiff_t id)
{
  IOFileUD *iof = IOSTDF_IOF(L, id);
  if (iof->fp == NULL)
    lj_err_caller(L, LJ_ERR_IOSTDCL);
  return iof->fp;
}

static IOFileUD *io_file_new(lua_State *L)
{
  IOFileUD *iof = (IOFileUD *)lua_newuserdata(L, sizeof(IOFileUD));
  GCudata *ud = udataV(L->top-1);
  ud->udtype = UDTYPE_IO_FILE;
  /* NOBARRIER: The GCudata is new (marked white). */
  setgcrefr(ud->metatable, curr_func(L)->c.env);
  iof->fp = NULL;
  iof->type = IOFILE_TYPE_FILE;
  return iof;
}

static IOFileUD *io_file_open(lua_State *L, const char *mode)
{
  const char *fname = strdata(lj_lib_checkstr(L, 1));
  IOFileUD *iof = io_file_new(L);
  iof->fp = fopen(fname, mode);
  if (iof->fp == NULL)
    luaL_argerror(L, 1, lj_str_pushf(L, "%s: %s", fname, strerror(errno)));
  return iof;
}

static int io_file_close(lua_State *L, IOFileUD *iof)
{
  int ok;
  if ((iof->type & IOFILE_TYPE_MASK) == IOFILE_TYPE_FILE) {
    ok = (fclose(iof->fp) == 0);
  } else if ((iof->type & IOFILE_TYPE_MASK) == IOFILE_TYPE_PIPE) {
#if LJ_TARGET_POSIX
    ok = (pclose(iof->fp) != -1);
#elif LJ_TARGET_WINDOWS
    ok = (_pclose(iof->fp) != -1);
#else
    ok = 0;
#endif
  } else {
    lua_assert((iof->type & IOFILE_TYPE_MASK) == IOFILE_TYPE_STDF);
    setnilV(L->top++);
    lua_pushliteral(L, "cannot close standard file");
    return 2;
  }
  iof->fp = NULL;
  return io_pushresult(L, ok, NULL);
}

/* -- Read/write helpers -------------------------------------------------- */

static int io_file_readnum(lua_State *L, FILE *fp)
{
  lua_Number d;
  if (fscanf(fp, LUA_NUMBER_SCAN, &d) == 1) {
    if (LJ_DUALNUM) {
      int32_t i = lj_num2int(d);
      if (d == (lua_Number)i && !tvismzero((cTValue *)&d)) {
	setintV(L->top++, i);
	return 1;
      }
    }
    setnumV(L->top++, d);
    return 1;
  } else {
    setnilV(L->top++);
    return 0;
  }
}

static int io_file_testeof(lua_State *L, FILE *fp)
{
  int c = getc(fp);
  ungetc(c, fp);
  lua_pushlstring(L, NULL, 0);
  return (c != EOF);
}

static int io_file_readline(lua_State *L, FILE *fp, size_t chop)
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
      luaL_addsize(&b, len - chop);  /* Keep or remove EOL. */
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
  return (n == 0 || strV(L->top-1)->len > 0);
}

static int io_file_read(lua_State *L, FILE *fp, int start)
{
  int ok, n, nargs = (int)(L->top - L->base) - start;
  clearerr(fp);
  if (nargs == 0) {
    ok = io_file_readline(L, fp, 1);
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
	else if ((p[1] & ~0x20) == 'L')
	  ok = io_file_readline(L, fp, (p[1] == 'l'));
	else if (p[1] == 'a')
	  io_file_readchars(L, fp, ~((size_t)0));
	else
	  lj_err_arg(L, n+1, LJ_ERR_INVFMT);
      } else if (tvisnumber(L->base+n)) {
	size_t len = (size_t)lj_lib_checkint(L, n+1);
	ok = len ? io_file_readchars(L, fp, len) : io_file_testeof(L, fp);
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
    } else if (tvisint(tv)) {
      char buf[LJ_STR_INTBUF];
      char *p = lj_str_bufint(buf, intV(tv));
      size_t len = (size_t)(buf+LJ_STR_INTBUF-p);
      status = status && (fwrite(p, 1, len, fp) == len);
    } else if (tvisnum(tv)) {
      status = status && (fprintf(fp, LUA_NUMBER_FMT, numV(tv)) > 0);
    } else {
      lj_err_argt(L, (int)(tv - L->base) + 1, LUA_TSTRING);
    }
  }
  return io_pushresult(L, status, NULL);
}

/* -- I/O file methods ---------------------------------------------------- */

#define LJLIB_MODULE_io_method

LJLIB_CF(io_method_close)
{
  IOFileUD *iof = L->base < L->top ? io_tofile(L) :
		  IOSTDF_IOF(L, GCROOT_IO_OUTPUT);
  return io_file_close(L, iof);
}

LJLIB_CF(io_method_read)
{
  return io_file_read(L, io_tofile(L)->fp, 1);
}

LJLIB_CF(io_method_write)		LJLIB_REC(io_write 0)
{
  return io_file_write(L, io_tofile(L)->fp, 1);
}

LJLIB_CF(io_method_flush)		LJLIB_REC(io_flush 0)
{
  return io_pushresult(L, fflush(io_tofile(L)->fp) == 0, NULL);
}

LJLIB_CF(io_method_seek)
{
  FILE *fp = io_tofile(L)->fp;
  int opt = lj_lib_checkopt(L, 2, 1, "\3set\3cur\3end");
  int64_t ofs = 0;
  cTValue *o;
  int res;
  if (opt == 0) opt = SEEK_SET;
  else if (opt == 1) opt = SEEK_CUR;
  else if (opt == 2) opt = SEEK_END;
  o = L->base+2;
  if (o < L->top) {
    if (tvisint(o))
      ofs = (int64_t)intV(o);
    else if (tvisnum(o))
      ofs = (int64_t)numV(o);
    else if (!tvisnil(o))
      lj_err_argt(L, 3, LUA_TNUMBER);
  }
#if LJ_TARGET_POSIX
  res = fseeko(fp, ofs, opt);
#elif _MSC_VER >= 1400
  res = _fseeki64(fp, ofs, opt);
#elif defined(__MINGW32__)
  res = fseeko64(fp, ofs, opt);
#else
  res = fseek(fp, (long)ofs, opt);
#endif
  if (res)
    return io_pushresult(L, 0, NULL);
#if LJ_TARGET_POSIX
  ofs = ftello(fp);
#elif _MSC_VER >= 1400
  ofs = _ftelli64(fp);
#elif defined(__MINGW32__)
  ofs = ftello64(fp);
#else
  ofs = (int64_t)ftell(fp);
#endif
  setint64V(L->top-1, ofs);
  return 1;
}

LJLIB_CF(io_method_setvbuf)
{
  FILE *fp = io_tofile(L)->fp;
  int opt = lj_lib_checkopt(L, 2, -1, "\4full\4line\2no");
  size_t sz = (size_t)lj_lib_optint(L, 3, LUAL_BUFFERSIZE);
  if (opt == 0) opt = _IOFBF;
  else if (opt == 1) opt = _IOLBF;
  else if (opt == 2) opt = _IONBF;
  return io_pushresult(L, setvbuf(fp, NULL, opt, sz) == 0, NULL);
}

LJLIB_PUSH(top-2)  /* io_lines_iter */
LJLIB_CF(io_method_lines)
{
  io_tofile(L);
  setfuncV(L, L->top, funcV(lj_lib_upvalue(L, 1)));
  setudataV(L, L->top+1, udataV(L->base));
  L->top += 2;
  return 2;
}

LJLIB_CF(io_method___gc)
{
  IOFileUD *iof = io_tofilep(L);
  if (iof->fp != NULL && (iof->type & IOFILE_TYPE_MASK) != IOFILE_TYPE_STDF)
    io_file_close(L, iof);
  return 0;
}

LJLIB_CF(io_method___tostring)
{
  IOFileUD *iof = io_tofilep(L);
  if (iof->fp != NULL)
    lua_pushfstring(L, "file (%p)", iof->fp);
  else
    lua_pushliteral(L, "file (closed)");
  return 1;
}

LJLIB_PUSH(top-1) LJLIB_SET(__index)

#include "lj_libdef.h"

/* -- I/O library functions ----------------------------------------------- */

#define LJLIB_MODULE_io

LJLIB_PUSH(top-2) LJLIB_SET(!)  /* Set environment. */

LJLIB_CF(io_open)
{
  const char *fname = strdata(lj_lib_checkstr(L, 1));
  GCstr *s = lj_lib_optstr(L, 2);
  const char *mode = s ? strdata(s) : "r";
  IOFileUD *iof = io_file_new(L);
  iof->fp = fopen(fname, mode);
  return iof->fp != NULL ? 1 : io_pushresult(L, 0, fname);
}

LJLIB_CF(io_popen)
{
#if LJ_TARGET_POSIX || LJ_TARGET_WINDOWS
  const char *fname = strdata(lj_lib_checkstr(L, 1));
  GCstr *s = lj_lib_optstr(L, 2);
  const char *mode = s ? strdata(s) : "r";
  IOFileUD *iof = io_file_new(L);
  iof->type = IOFILE_TYPE_PIPE;
#if LJ_TARGET_POSIX
  fflush(NULL);
  iof->fp = popen(fname, mode);
#else
  iof->fp = _popen(fname, mode);
#endif
  return iof->fp != NULL ? 1 : io_pushresult(L, 0, fname);
#else
  luaL_error(L, LUA_QL("popen") " not supported");
#endif
}

LJLIB_CF(io_tmpfile)
{
  IOFileUD *iof = io_file_new(L);
  iof->fp = tmpfile();
  return iof->fp != NULL ? 1 : io_pushresult(L, 0, NULL);
}

LJLIB_CF(io_close)
{
  return lj_cf_io_method_close(L);
}

LJLIB_CF(io_read)
{
  return io_file_read(L, io_stdfile(L, GCROOT_IO_INPUT), 0);
}

LJLIB_CF(io_write)		LJLIB_REC(io_write GCROOT_IO_OUTPUT)
{
  return io_file_write(L, io_stdfile(L, GCROOT_IO_OUTPUT), 0);
}

LJLIB_CF(io_flush)		LJLIB_REC(io_flush GCROOT_IO_OUTPUT)
{
  return io_pushresult(L, fflush(io_stdfile(L, GCROOT_IO_OUTPUT)) == 0, NULL);
}

static int io_std_getset(lua_State *L, ptrdiff_t id, const char *mode)
{
  if (L->base < L->top && !tvisnil(L->base)) {
    if (tvisudata(L->base)) {
      io_tofile(L);
      L->top = L->base+1;
    } else {
      io_file_open(L, mode);
    }
    /* NOBARRIER: The standard I/O handles are GC roots. */
    setgcref(G(L)->gcroot[id], gcV(L->top-1));
  } else {
    setudataV(L, L->top++, IOSTDF_UD(L, id));
  }
  return 1;
}

LJLIB_CF(io_input)
{
  return io_std_getset(L, GCROOT_IO_INPUT, "r");
}

LJLIB_CF(io_output)
{
  return io_std_getset(L, GCROOT_IO_OUTPUT, "w");
}

LJLIB_NOREG LJLIB_CF(io_lines_iter)
{
  IOFileUD *iof = io_tofile(L);
  int ok = io_file_readline(L, iof->fp, 1);
  if (ferror(iof->fp))
    lj_err_callermsg(L, strerror(errno));
  if (!ok && (iof->type & IOFILE_FLAG_CLOSE))
    io_file_close(L, iof);  /* Return values are ignored (ok is 0). */
  return ok;
}

LJLIB_PUSH(top-3)  /* io_lines_iter */
LJLIB_CF(io_lines)
{
  if (L->base < L->top && !tvisnil(L->base)) {  /* io.lines(fname) */
    IOFileUD *iof = io_file_open(L, "r");
    iof->type = IOFILE_TYPE_FILE|IOFILE_FLAG_CLOSE;
    setfuncV(L, L->top-2, funcV(lj_lib_upvalue(L, 1)));
  } else {  /* io.lines() iterates over stdin. */
    setfuncV(L, L->top, funcV(lj_lib_upvalue(L, 1)));
    setudataV(L, L->top+1, IOSTDF_UD(L, GCROOT_IO_INPUT));
    L->top += 2;
  }
  return 2;
}

LJLIB_CF(io_type)
{
  cTValue *o = lj_lib_checkany(L, 1);
  if (!(tvisudata(o) && udataV(o)->udtype == UDTYPE_IO_FILE))
    setnilV(L->top++);
  else if (((IOFileUD *)uddata(udataV(o)))->fp != NULL)
    lua_pushliteral(L, "file");
  else
    lua_pushliteral(L, "closed file");
  return 1;
}

#include "lj_libdef.h"

/* ------------------------------------------------------------------------ */

static GCobj *io_std_new(lua_State *L, FILE *fp, const char *name)
{
  IOFileUD *iof = (IOFileUD *)lua_newuserdata(L, sizeof(IOFileUD));
  GCudata *ud = udataV(L->top-1);
  ud->udtype = UDTYPE_IO_FILE;
  /* NOBARRIER: The GCudata is new (marked white). */
  setgcref(ud->metatable, gcV(L->top-3));
  iof->fp = fp;
  iof->type = IOFILE_TYPE_STDF;
  lua_setfield(L, -2, name);
  return obj2gco(ud);
}

LUALIB_API int luaopen_io(lua_State *L)
{
  lj_lib_pushcf(L, lj_cf_io_lines_iter, FF_io_lines_iter);
  LJ_LIB_REG(L, NULL, io_method);
  copyTV(L, L->top, L->top-1); L->top++;
  lua_setfield(L, LUA_REGISTRYINDEX, LUA_FILEHANDLE);
  LJ_LIB_REG(L, LUA_IOLIBNAME, io);
  setgcref(G(L)->gcroot[GCROOT_IO_INPUT], io_std_new(L, stdin, "stdin"));
  setgcref(G(L)->gcroot[GCROOT_IO_OUTPUT], io_std_new(L, stdout, "stdout"));
  io_std_new(L, stderr, "stderr");
  return 1;
}

