/*
** Miscellaneous object handling.
** Copyright (C) 2005-2010 Mike Pall. See Copyright Notice in luajit.h
*/

#define lj_obj_c
#define LUA_CORE

#include "lj_obj.h"

/* Object type names. */
LJ_DATADEF const char *const lj_obj_typename[] = {  /* ORDER LUA_T */
  "no value", "nil", "boolean", "userdata", "number", "string",
  "table", "function", "userdata", "thread", "proto", "upval"
};

LJ_DATADEF const char *const lj_obj_itypename[] = {  /* ORDER LJ_T */
  "nil", "boolean", "boolean", "userdata", "string", "upval", "thread",
  "proto", "function", "deadkey", "table", "userdata", "number"
};

/* Compare two objects without calling metamethods. */
int lj_obj_equal(cTValue *o1, cTValue *o2)
{
  if (itype(o1) == itype(o2)) {
    if (tvispri(o1))
      return 1;
    if (!tvisnum(o1)) {
#if LJ_64
      if (tvislightud(o1))
	return o1->u64 == o2->u64;
      else
#endif
	return gcrefeq(o1->gcr, o2->gcr);
    }
  } else if (!tvisnum(o1) || !tvisnum(o2)) {
    return 0;
  }
  return numV(o1) == numV(o2);
}

