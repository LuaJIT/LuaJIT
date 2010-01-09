/*
** Bytecode instruction modes.
** Copyright (C) 2005-2010 Mike Pall. See Copyright Notice in luajit.h
*/

#define lj_bc_c
#define LUA_CORE

#include "lj_obj.h"
#include "lj_bc.h"

/* Bytecode instruction modes. */
LJ_DATADEF const uint16_t lj_bc_mode[BC__MAX+1] = {
BCDEF(BCMODE)
  0
};

