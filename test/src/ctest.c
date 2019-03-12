
#define LUA_LIB
#include "lua.h"
#include "lauxlib.h"

/* ------------------------------------------------------------------------ */

#ifdef _MSC_VER
typedef __int8 int8_t;
typedef __int16 int16_t;
typedef __int32 int32_t;
typedef __int64 int64_t;
typedef unsigned __int8 uint8_t;
typedef unsigned __int16 uint16_t;
typedef unsigned __int32 uint32_t;
typedef unsigned __int64 uint64_t;
#else
#include <stdint.h>
#define complex _Complex
#endif

#if defined(__i386) || defined(__i386__) || defined(_M_IX86)
#ifdef _MSC_VER
#define LJ_FASTCALL	__fastcall
#define LJ_STDCALL	__stdcall
#else
#define LJ_FASTCALL	__attribute__((fastcall))
#define LJ_STDCALL	__attribute__((stdcall))
#endif
#endif

typedef struct s_ii { int x, y; } s_ii;
typedef struct s_jj { int64_t x, y; } s_jj;
typedef struct s_ff { float x, y; } s_ff;
typedef struct s_dd { double x, y; } s_dd;
typedef struct s_8i { int a,b,c,d,e,f,g,h; } s_8i;

LUA_API int call_i(int a) { return a+1; }
LUA_API int call_ii(int a, int b) { return a+b; }
LUA_API int call_10i(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j) { return a+b+c+d+e+f+g+h+i+j; }

LUA_API int64_t call_10j(int a, int b, int c, int d, int e, int f, int g, int h, int i, int64_t j) { return a+b+c+d+e+f+g+h+i+j; }

LUA_API int64_t call_ji(int64_t a, int b) { return a+b; }
LUA_API int64_t call_ij(int a, int64_t b) { return a+b; }
LUA_API int64_t call_jj(int64_t a, int64_t b) { return a+b; }

LUA_API double call_dd(double a, double b) { return a+b; }
LUA_API double call_10d(double a, double b, double c, double d, double e, double f, double g, double h, double i, double j) { return a+b+c+d+e+f+g+h+i+j; }

LUA_API float call_ff(float a, float b) { return a+b; }
LUA_API float call_10f(float a, float b, float c, float d, float e, float f, float g, float h, float i, float j) { return a+b+c+d+e+f+g+h+i+j; }

LUA_API double call_idifjd(int a, double b, int c, float d, int64_t e, double f) { return a+b+c+d+e+f; }

LUA_API int call_p_i(int *a) { return *a+1; }
LUA_API int *call_p_p(int *a) { return a+1; }
LUA_API int call_pp_i(int *a, int *b) { return (int)(a-b); }

#include <stdarg.h>

LUA_API double call_ividi(int a, ...)
{
  double y;
  va_list argp;
  va_start(argp, a);
  y = a;
  y += va_arg(argp, int);
  y += va_arg(argp, double);
  y += va_arg(argp, int);
  va_end(argp);
  return y;
}

#ifdef complex
LUA_API complex call_dd_cd(double a, double b) { return a+b*2i; }
LUA_API complex call_cd(complex a) { return a+1-2i; }
LUA_API complex call_cdcd(complex a, complex b) { return a+b; }

LUA_API complex float call_ff_cf(float a, float b) { return a+b*2i; }
LUA_API complex float call_cf(complex float a) { return a+1-2i; }
LUA_API complex float call_cfcf(complex float a, complex float b) { return a+b; }
#endif

LUA_API s_ii call_sii(s_ii a) { return a; }
LUA_API s_jj call_sjj(s_jj a) { return a; }
LUA_API s_ff call_sff(s_ff a) { return a; }
LUA_API s_dd call_sdd(s_dd a) { return a; }
LUA_API s_8i call_s8i(s_8i a) { return a; }
LUA_API s_ii call_siisii(s_ii a, s_ii b)
{
  s_ii c;
  c.x = a.x + b.x;
  c.y = a.y + b.y;
  return c;
}
LUA_API s_ff call_sffsff(s_ff a, s_ff b)
{
  s_ff c;
  c.x = a.x + b.x;
  c.y = a.y + b.y;
  return c;
}
LUA_API s_dd call_sddsdd(s_dd a, s_dd b)
{
  s_dd c;
  c.x = a.x + b.x;
  c.y = a.y + b.y;
  return c;
}
LUA_API s_8i call_s8is8i(s_8i a, s_8i b)
{
  s_8i c;
  c.a = a.a + b.a;
  c.b = a.b + b.b;
  c.c = a.c + b.c;
  c.d = a.d + b.d;
  c.e = a.e + b.e;
  c.f = a.f + b.f;
  c.g = a.g + b.g;
  c.h = a.h + b.h;
  return c;
}
LUA_API s_8i call_is8ii(int a, s_8i b, int c)
{
  b.a += a;
  b.c += c;
  return b;
}

#ifdef LJ_FASTCALL
LUA_API int LJ_FASTCALL fastcall_void(void) { return 1; }
LUA_API int LJ_FASTCALL fastcall_i(int a) { return a+1; }
LUA_API int LJ_FASTCALL fastcall_ii(int a, int b) { return a+b; }
LUA_API int LJ_FASTCALL fastcall_iii(int a, int b, int c) { return a+b+c; }
LUA_API int64_t LJ_FASTCALL fastcall_ji(int64_t a, int b) { return a+b; }
LUA_API double LJ_FASTCALL fastcall_dd(double a, double b) { return a+b; }
LUA_API int LJ_FASTCALL fastcall_pp_i(int *a, int *b) { return (int)(a-b); }
LUA_API s_ii LJ_FASTCALL fastcall_siisii(s_ii a, s_ii b)
{
  s_ii c;
  c.x = a.x + b.x;
  c.y = a.y + b.y;
  return c;
}
LUA_API s_dd LJ_FASTCALL fastcall_sddsdd(s_dd a, s_dd b)
{
  s_dd c;
  c.x = a.x + b.x;
  c.y = a.y + b.y;
  return c;
}
#endif

#if defined(LJ_STDCALL) && defined(_WIN32)
LUA_API int LJ_STDCALL stdcall_i(int a) { return a+1; }
LUA_API int LJ_STDCALL stdcall_ii(int a, int b) { return a+b; }
LUA_API double LJ_STDCALL stdcall_dd(double a, double b) { return a+b; }
LUA_API float LJ_STDCALL stdcall_ff(float a, float b) { return a+b; }
#endif

/* ------------------------------------------------------------------------ */

static int ct_call(lua_State *L)
{
  int nresults = luaL_checkint(L, 1);
  luaL_checkstack(L, nresults, "too many results");
  lua_call(L, lua_gettop(L)-2, nresults);
  return lua_gettop(L)-1;
}

static int ct_callon(lua_State *L)
{
  lua_State *co = lua_tothread(L, 1);
  int nargs = lua_gettop(L)-1;
  int nresults;
  lua_xmove(L, co, nargs);
  lua_call(co, nargs-1, LUA_MULTRET);
  nresults = lua_gettop(co);
  lua_xmove(co, L, nresults);
  return nresults;
}

static int ct_pcall_err(lua_State *L)
{
  int nresults = luaL_checkint(L, 1);
  luaL_checkstack(L, nresults, "too many results");
  if (lua_pcall(L, lua_gettop(L)-2, nresults, 0))
    lua_error(L);
  return lua_gettop(L)-1;
}

static int ct_pcall(lua_State *L)
{
  int status;
  luaL_checkany(L, 1);
  status = lua_pcall(L, lua_gettop(L) - 1, LUA_MULTRET, 0);
  lua_pushboolean(L, (status == 0));
  lua_insert(L, 1);
  return lua_gettop(L);  /* return status + all results */
}

static int ct_xpcall(lua_State *L)
{
  int status;
  luaL_checkany(L, 2);
  lua_settop(L, 2);
  lua_insert(L, 1);  /* put error function under function to be called */
  status = lua_pcall(L, 0, LUA_MULTRET, 1);
  lua_pushboolean(L, (status == 0));
  lua_replace(L, 1);
  return lua_gettop(L);  /* return status + all results */
}

#define CO_RUN	0	/* running */
#define CO_SUS	1	/* suspended */
#define CO_NOR	2	/* 'normal' (it resumed another coroutine) */
#define CO_DEAD	3

static const char *const statnames[] =
    {"running", "suspended", "normal", "dead"};

static int costatus(lua_State *L, lua_State *co) {
  if (L == co) return CO_RUN;
  switch (lua_status(co)) {
    case LUA_YIELD:
      return CO_SUS;
    case 0: {
      lua_Debug ar;
      if (lua_getstack(co, 0, &ar) > 0)  /* does it have frames? */
	return CO_NOR;  /* it is running */
      else if (lua_gettop(co) == 0)
	  return CO_DEAD;
      else
	return CO_SUS;  /* initial state */
    }
    default:  /* some error occured */
      return CO_DEAD;
  }
}

static int auxresume(lua_State *L, lua_State *co, int narg) {
  int status = costatus(L, co);
  if (!lua_checkstack(co, narg))
    luaL_error(L, "too many arguments to resume");
  if (status != CO_SUS) {
    lua_pushfstring(L, "cannot resume %s coroutine", statnames[status]);
    return -1;  /* error flag */
  }
  lua_xmove(L, co, narg);
  status = lua_resume(co, narg);
  if (status == 0 || status == LUA_YIELD) {
    int nres = lua_gettop(co);
    if (!lua_checkstack(L, nres + 1))
      luaL_error(L, "too many results to resume");
    lua_xmove(co, L, nres);  /* move yielded values */
    return nres;
  }
  else {
    lua_xmove(co, L, 1);  /* move error message */
    return -1;  /* error flag */
  }
}

static int ct_resume(lua_State *L) {
  lua_State *co = lua_tothread(L, 1);
  int r;
  luaL_argcheck(L, co, 1, "coroutine expected");
  r = auxresume(L, co, lua_gettop(L) - 1);
  if (r < 0) {
    lua_pushboolean(L, 0);
    lua_insert(L, -2);
    return 2;  /* return false + error message */
  }
  else {
    lua_pushboolean(L, 1);
    lua_insert(L, -(r + 1));
    return r + 1;  /* return true + `resume' returns */
  }
}

static int ct_auxwrap(lua_State *L) {
  lua_State *co = lua_tothread(L, lua_upvalueindex(1));
  int r = auxresume(L, co, lua_gettop(L));
  if (r < 0) {
    if (lua_isstring(L, -1)) {  /* error object is a string? */
      luaL_where(L, 1);  /* add extra info */
      lua_insert(L, -2);
      lua_concat(L, 2);
    }
    lua_error(L);  /* propagate error */
  }
  return r;
}

static int ct_cocreate(lua_State *L) {
  lua_State *NL = lua_newthread(L);
  luaL_argcheck(L, lua_isfunction(L, 1) && !lua_iscfunction(L, 1), 1,
    "Lua function expected");
  lua_pushvalue(L, 1);  /* move function to top */
  lua_xmove(L, NL, 1);  /* move function from L to NL */
  return 1;
}


static int ct_wrap(lua_State *L) {
  ct_cocreate(L);
  lua_pushcclosure(L, ct_auxwrap, 1);
  return 1;
}

static int ct_yield(lua_State *L) {
  return lua_yield(L, lua_gettop(L));
}

static int ct_lightud(lua_State *L)
{
  lua_pushlightuserdata(L, (void *)(ptrdiff_t)lua_tonumber(L, 1));
  return 1;
}

static luaL_Reg ct_funcs[] = {
  {"call",	ct_call },
  {"callon",	ct_callon },
  {"pcall",	ct_pcall },
  {"xpcall",	ct_xpcall },
  {"pcall_err",	ct_pcall_err },
  {"resume",	ct_resume },
  {"wrap",	ct_wrap },
  {"yield",	ct_yield },
  {"lightud",	ct_lightud },
  {NULL, NULL}
};

LUA_API int luaopen_ctest(lua_State *L)
{
  luaL_register(L, "ctest", ct_funcs);
  return 1;
}
