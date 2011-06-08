/*
** Debugging and introspection.
** Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_DEBUG_H
#define _LJ_DEBUG_H

#include "lj_obj.h"

LJ_FUNC cTValue *lj_debug_frame(lua_State *L, int level, int *size);
LJ_FUNC BCLine LJ_FASTCALL lj_debug_line(GCproto *pt, BCPos pc);
LJ_FUNC const char *lj_debug_uvname(GCproto *pt, uint32_t idx);
LJ_FUNC const char *lj_debug_uvnamev(cTValue *o, uint32_t idx, TValue **tvp);
LJ_FUNC const char *lj_debug_slotname(GCproto *pt, const BCIns *pc,
				      BCReg slot, const char **name);
LJ_FUNC const char *lj_debug_funcname(lua_State *L, TValue *frame,
				      const char **name);
LJ_FUNC void lj_debug_shortname(char *out, GCstr *str);
LJ_FUNC void lj_debug_addloc(lua_State *L, const char *msg,
			     cTValue *frame, cTValue *nextframe);
LJ_FUNC void lj_debug_pushloc(lua_State *L, GCproto *pt, BCPos pc);

/* Fixed internal variable names. */
#define VARNAMEDEF(_) \
  _(FOR_IDX, "(for index)") \
  _(FOR_STOP, "(for limit)") \
  _(FOR_STEP, "(for step)") \
  _(FOR_GEN, "(for generator)") \
  _(FOR_STATE, "(for state)") \
  _(FOR_CTL, "(for control)")

enum {
  VARNAME_END,
#define VARNAMEENUM(name, str)	VARNAME_##name,
  VARNAMEDEF(VARNAMEENUM)
#undef VARNAMEENUM
  VARNAME__MAX
};

#endif
