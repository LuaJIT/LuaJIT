/*
** VM event handling.
** Copyright (C) 2005-2017 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_VMEVENT_H
#define _LJ_VMEVENT_H

#include "lj_obj.h"
#include "lj_dispatch.h"

/* Registry key for VM event handler table. */
#define LJ_VMEVENTS_REGKEY	"_VMEVENTS"
#define LJ_VMEVENTS_HSIZE	4

#define VMEVENT_MASK(ev)	((uint8_t)1 << ((int)(ev) & 7))
#define VMEVENT_HASH(ev)	((int)(ev) & ~7)
#define VMEVENT_HASHIDX(h)	((int)(h) << 3)
#define VMEVENT_NOCACHE		255

#define VMEVENT_DEF(name, hash) \
  LJ_VMEVENT_##name##_, \
  LJ_VMEVENT_##name = ((LJ_VMEVENT_##name##_) & 7)|((hash) << 3)

/* VM event IDs. */
typedef enum {
  VMEVENT_DEF(BC,	0x00003883),
  VMEVENT_DEF(TRACE,	0xb2d91467),
  VMEVENT_DEF(RECORD,	0x9284bf4f),
  VMEVENT_DEF(TEXIT,	0xb29df2b0),
  LJ_VMEVENT__MAX
} VMEvent;

typedef enum VMEvent2{
  VMEVENT_SHUTDOWN,
  VMEVENT_LOADSTRING,
  VMEVENT_BC,
  VMEVENT_TRACE_START,
  VMEVENT_TRACE_STOP,
  VMEVENT_TRACE_ABORT,
  VMEVENT_TRACE_EXIT,
  VMEVENT_TRACE_FLUSH,
  VMEVENT_RECORD,
  VMEVENT_PROTO_BLACKLISTED,
  VMEVENT__MAX
} VMEvent2;

#ifdef LUAJIT_DISABLE_VMEVENT
#define lj_vmevent_send(L, ev, args)		UNUSED(L)
#define lj_vmevent_send_(L, ev, args, post)	UNUSED(L)
#define lj_vmevent_send_trace(L, ev, args, post)	UNUSED(L)
#define lj_vmevent_send2(L, ev, callbackarg, args)	UNUSED(L)
#else
#define lj_vmevent_send(L, ev, args) \
  if (G(L)->vmevmask & VMEVENT_MASK(LJ_VMEVENT_##ev)) { \
    ptrdiff_t argbase = lj_vmevent_prepare(L, LJ_VMEVENT_##ev); \
    if (argbase) { \
      args \
      lj_vmevent_call(L, argbase); \
    } \
  }
#define lj_vmevent_send_(L, ev, args, post) \
  if(L2J(L)->vmevent_cb != NULL){\
    L2J(L)->vmevent_cb(L2J(L)->vmevent_data, L, VMEVENT_##ev, 0);\
  }\
  if (G(L)->vmevmask & VMEVENT_MASK(LJ_VMEVENT_##ev)) { \
    ptrdiff_t argbase = lj_vmevent_prepare(L, LJ_VMEVENT_##ev); \
    if (argbase) { \
      args \
      lj_vmevent_call(L, argbase); \
      post \
    } \
  }

#define lj_vmevent_callback(L, ev, args) \
  if(L2J(L)->vmevent_cb != NULL){\
    L2J(L)->vmevent_cb(L2J(L)->vmevent_data, L, ev, args);\
  }

#define lj_vmevent_send2(L, ev, callbackarg, args) \
  if(L2J(L)->vmevent_cb != NULL){\
    L2J(L)->vmevent_cb(L2J(L)->vmevent_data, L, VMEVENT_##ev, callbackarg);\
  }\
  lj_vmevent_send(L, ev, args) 

#define lj_vmevent_send_trace(L, subevent, args) \
  if(L2J(L)->vmevent_cb != NULL){\
    L2J(L)->vmevent_cb(L2J(L)->vmevent_data, L, VMEVENT_TRACE_##subevent, 0);\
  }\
  lj_vmevent_send(L, TRACE, args)

LJ_FUNC ptrdiff_t lj_vmevent_prepare(lua_State *L, VMEvent ev);
LJ_FUNC void lj_vmevent_call(lua_State *L, ptrdiff_t argbase);
#endif

#endif
