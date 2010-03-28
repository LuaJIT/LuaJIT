/*
** Memory access optimizations.
** AA: Alias Analysis using high-level semantic disambiguation.
** FWD: Load Forwarding (L2L) + Store Forwarding (S2L).
** DSE: Dead-Store Elimination.
** Copyright (C) 2005-2010 Mike Pall. See Copyright Notice in luajit.h
*/

#define lj_opt_mem_c
#define LUA_CORE

#include "lj_obj.h"

#if LJ_HASJIT

#include "lj_tab.h"
#include "lj_ir.h"
#include "lj_jit.h"
#include "lj_iropt.h"

/* Some local macros to save typing. Undef'd at the end. */
#define IR(ref)		(&J->cur.ir[(ref)])
#define fins		(&J->fold.ins)
#define fright		(&J->fold.right)

/*
** Caveat #1: return value is not always a TRef -- only use with tref_ref().
** Caveat #2: FWD relies on active CSE for xREF operands -- see lj_opt_fold().
*/

/* Return values from alias analysis. */
typedef enum {
  ALIAS_NO,	/* The two refs CANNOT alias (exact). */
  ALIAS_MAY,	/* The two refs MAY alias (inexact). */
  ALIAS_MUST	/* The two refs MUST alias (exact). */
} AliasRet;

/* -- ALOAD/HLOAD forwarding and ASTORE/HSTORE elimination ---------------- */

/* Alias analysis for array and hash access using key-based disambiguation. */
static AliasRet aa_ahref(jit_State *J, IRIns *refa, IRIns *refb)
{
  IRRef ka = refa->op2;
  IRRef kb = refb->op2;
  IRIns *keya, *keyb;
  if (refa == refb)
    return ALIAS_MUST;  /* Shortcut for same refs. */
  keya = IR(ka);
  if (keya->o == IR_KSLOT) { ka = keya->op1; keya = IR(ka); }
  keyb = IR(kb);
  if (keyb->o == IR_KSLOT) { kb = keyb->op1; keyb = IR(kb); }
  if (ka == kb) {
    /* Same key. Check for same table with different ref (NEWREF vs. HREF). */
    IRIns *ta = refa;
    IRIns *tb = refb;
    if (ta->o == IR_HREFK || ta->o == IR_AREF) ta = IR(ta->op1);
    if (tb->o == IR_HREFK || tb->o == IR_AREF) tb = IR(tb->op1);
    if (ta->op1 == tb->op1)
      return ALIAS_MUST;  /* Same key, same table. */
    else
      return ALIAS_MAY;  /* Same key, possibly different table. */
  }
  if (irref_isk(ka) && irref_isk(kb))
    return ALIAS_NO;  /* Different constant keys. */
  if (refa->o == IR_AREF) {
    /* Disambiguate array references based on index arithmetic. */
    lua_assert(refb->o == IR_AREF);
    if (refa->op1 == refb->op1) {
      /* Same table, different non-const array keys. */
      int32_t ofsa = 0, ofsb = 0;
      IRRef basea = ka, baseb = kb;
      /* Gather base and offset from t[base] or t[base+-ofs]. */
      if (keya->o == IR_ADD && irref_isk(keya->op2)) {
	basea = keya->op1;
	ofsa = IR(keya->op2)->i;
	if (basea == kb && ofsa != 0)
	  return ALIAS_NO;  /* t[base+-ofs] vs. t[base]. */
      }
      if (keyb->o == IR_ADD && irref_isk(keyb->op2)) {
	baseb = keyb->op1;
	ofsb = IR(keyb->op2)->i;
	if (ka == baseb && ofsb != 0)
	  return ALIAS_NO;  /* t[base] vs. t[base+-ofs]. */
      }
      if (basea == baseb && ofsa != ofsb)
	return ALIAS_NO;  /* t[base+-o1] vs. t[base+-o2] and o1 != o2. */
    }
  } else {
    /* Disambiguate hash references based on the type of their keys. */
    lua_assert((refa->o==IR_HREF || refa->o==IR_HREFK || refa->o==IR_NEWREF) &&
	       (refb->o==IR_HREF || refb->o==IR_HREFK || refb->o==IR_NEWREF));
    if (!irt_sametype(keya->t, keyb->t))
      return ALIAS_NO;  /* Different key types. */
  }
  return ALIAS_MAY;  /* Anything else: we just don't know. */
}

/* Array and hash load forwarding. */
static TRef fwd_ahload(jit_State *J, IRRef xref)
{
  IRIns *xr = IR(xref);
  IRRef lim = xref;  /* Search limit. */
  IRRef ref;

  /* Search for conflicting stores. */
  ref = J->chain[fins->o+IRDELTA_L2S];
  while (ref > xref) {
    IRIns *store = IR(ref);
    switch (aa_ahref(J, xr, IR(store->op1))) {
    case ALIAS_NO:   break;  /* Continue searching. */
    case ALIAS_MAY:  lim = ref; goto conflict;  /* Limit search for load. */
    case ALIAS_MUST: return store->op2;  /* Store forwarding. */
    }
    ref = store->prev;
  }

  /* No conflicting store (yet): const-fold loads from allocations. */
  {
    IRIns *ir = (xr->o == IR_HREFK || xr->o == IR_AREF) ? IR(xr->op1) : xr;
    IRRef tab = ir->op1;
    ir = IR(tab);
    if (ir->o == IR_TNEW || (ir->o == IR_TDUP && irref_isk(xr->op2))) {
      /* A NEWREF with a number key may end up pointing to the array part.
      ** But it's referenced from HSTORE and not found in the ASTORE chain.
      ** For now simply consider this a conflict without forwarding anything.
      */
      if (xr->o == IR_AREF) {
	IRRef ref2 = J->chain[IR_NEWREF];
	while (ref2 > tab) {
	  IRIns *newref = IR(ref2);
	  if (irt_isnum(IR(newref->op2)->t))
	    goto conflict;
	  ref2 = newref->prev;
	}
      }
      /* NEWREF inhibits CSE for HREF, and dependent FLOADs from HREFK/AREF.
      ** But the above search for conflicting stores was limited by xref.
      ** So continue searching, limited by the TNEW/TDUP. Store forwarding
      ** is ok, too. A conflict does NOT limit the search for a matching load.
      */
      while (ref > tab) {
	IRIns *store = IR(ref);
	switch (aa_ahref(J, xr, IR(store->op1))) {
	case ALIAS_NO:   break;  /* Continue searching. */
	case ALIAS_MAY:  goto conflict;  /* Conflicting store. */
	case ALIAS_MUST: return store->op2;  /* Store forwarding. */
	}
	ref = store->prev;
      }
      lua_assert(ir->o != IR_TNEW || irt_isnil(fins->t));
      if (irt_ispri(fins->t)) {
	return TREF_PRI(irt_type(fins->t));
      } else if (irt_isnum(fins->t) || irt_isstr(fins->t)) {
	TValue keyv;
	cTValue *tv;
	IRIns *key = IR(xr->op2);
	if (key->o == IR_KSLOT) key = IR(key->op1);
	lj_ir_kvalue(J->L, &keyv, key);
	tv = lj_tab_get(J->L, ir_ktab(IR(ir->op1)), &keyv);
	lua_assert(itype2irt(tv) == irt_type(fins->t));
	if (irt_isnum(fins->t))
	  return lj_ir_knum_nn(J, tv->u64);
	else
	  return lj_ir_kstr(J, strV(tv));
      }
      /* Othwerwise: don't intern as a constant. */
    }
  }

conflict:
  /* Try to find a matching load. Below the conflicting store, if any. */
  ref = J->chain[fins->o];
  while (ref > lim) {
    IRIns *load = IR(ref);
    if (load->op1 == xref)
      return ref;  /* Load forwarding. */
    ref = load->prev;
  }
  return 0;  /* Conflict or no match. */
}

/* Reassociate ALOAD across PHIs to handle t[i-1] forwarding case. */
static TRef fwd_aload_reassoc(jit_State *J)
{
  IRIns *irx = IR(fins->op1);
  IRIns *key = IR(irx->op2);
  if (key->o == IR_ADD && irref_isk(key->op2)) {
    IRIns *add2 = IR(key->op1);
    if (add2->o == IR_ADD && irref_isk(add2->op2) &&
	IR(key->op2)->i == -IR(add2->op2)->i) {
      IRRef ref = J->chain[IR_AREF];
      IRRef lim = add2->op1;
      if (irx->op1 > lim) lim = irx->op1;
      while (ref > lim) {
	IRIns *ir = IR(ref);
	if (ir->op1 == irx->op1 && ir->op2 == add2->op1)
	  return fwd_ahload(J, ref);
	ref = ir->prev;
      }
    }
  }
  return 0;
}

/* ALOAD forwarding. */
TRef LJ_FASTCALL lj_opt_fwd_aload(jit_State *J)
{
  IRRef ref;
  if ((ref = fwd_ahload(J, fins->op1)) ||
      (ref = fwd_aload_reassoc(J)))
    return ref;
  return EMITFOLD;
}

/* HLOAD forwarding. */
TRef LJ_FASTCALL lj_opt_fwd_hload(jit_State *J)
{
  IRRef ref = fwd_ahload(J, fins->op1);
  if (ref)
    return ref;
  return EMITFOLD;
}

/* Check whether HREF of TNEW/TDUP can be folded to niltv. */
int LJ_FASTCALL lj_opt_fwd_href_nokey(jit_State *J)
{
  IRRef lim = fins->op1;  /* Search limit. */
  IRRef ref;

  /* The key for an ASTORE may end up in the hash part after a NEWREF. */
  if (irt_isnum(fright->t) && J->chain[IR_NEWREF] > lim) {
    ref = J->chain[IR_ASTORE];
    while (ref > lim) {
      if (ref < J->chain[IR_NEWREF])
	return 0;  /* Conflict. */
      ref = IR(ref)->prev;
    }
  }

  /* Search for conflicting stores. */
  ref = J->chain[IR_HSTORE];
  while (ref > lim) {
    IRIns *store = IR(ref);
    if (aa_ahref(J, fins, IR(store->op1)) != ALIAS_NO)
      return 0;  /* Conflict. */
    ref = store->prev;
  }

  return 1;  /* No conflict. Can fold to niltv. */
}

/* ASTORE/HSTORE elimination. */
TRef LJ_FASTCALL lj_opt_dse_ahstore(jit_State *J)
{
  IRRef xref = fins->op1;  /* xREF reference. */
  IRRef val = fins->op2;  /* Stored value reference. */
  IRIns *xr = IR(xref);
  IRRef1 *refp = &J->chain[fins->o];
  IRRef ref = *refp;
  while (ref > xref) {  /* Search for redundant or conflicting stores. */
    IRIns *store = IR(ref);
    switch (aa_ahref(J, xr, IR(store->op1))) {
    case ALIAS_NO:
      break;  /* Continue searching. */
    case ALIAS_MAY:	/* Store to MAYBE the same location. */
      if (store->op2 != val)  /* Conflict if the value is different. */
	goto doemit;
      break;  /* Otherwise continue searching. */
    case ALIAS_MUST:	/* Store to the same location. */
      if (store->op2 == val)  /* Same value: drop the new store. */
	return DROPFOLD;
      /* Different value: try to eliminate the redundant store. */
      if (ref > J->chain[IR_LOOP]) {  /* Quick check to avoid crossing LOOP. */
	IRIns *ir;
	/* Check for any intervening guards (includes conflicting loads). */
	for (ir = IR(J->cur.nins-1); ir > store; ir--)
	  if (irt_isguard(ir->t))
	    goto doemit;  /* No elimination possible. */
	/* Remove redundant store from chain and replace with NOP. */
	*refp = store->prev;
	store->o = IR_NOP;  /* Unchained NOP -- does anybody care? */
	store->t.irt = IRT_NIL;
	store->op1 = store->op2 = 0;
	store->prev = 0;
	/* Now emit the new store instead. */
      }
      goto doemit;
    }
    ref = *(refp = &store->prev);
  }
doemit:
  return EMITFOLD;  /* Otherwise we have a conflict or simply no match. */
}

/* -- ULOAD forwarding ---------------------------------------------------- */

/* The current alias analysis for upvalues is very simplistic. It only
** disambiguates between the unique upvalues of the same function.
** This is good enough for now, since most upvalues are read-only.
**
** A more precise analysis would be feasible with the help of the parser:
** generate a unique key for every upvalue, even across all prototypes.
** Lacking a realistic use-case, it's unclear whether this is beneficial.
*/
static AliasRet aa_uref(IRIns *refa, IRIns *refb)
{
  if (refa->o != refb->o)
    return ALIAS_NO;  /* Different UREFx type. */
  if (refa->op1 == refb->op1) {  /* Same function. */
    if (refa->op2 == refb->op2)
      return ALIAS_MUST;  /* Same function, same upvalue idx. */
    else
      return ALIAS_NO;  /* Same function, different upvalue idx. */
  } else {  /* Different functions, check disambiguation hash values. */
    if (((refa->op2 ^ refb->op2) & 0xff))
      return ALIAS_NO;  /* Upvalues with different hash values cannot alias. */
    else
      return ALIAS_MAY;  /* No conclusion can be drawn for same hash value. */
  }
}

/* ULOAD forwarding. */
TRef LJ_FASTCALL lj_opt_fwd_uload(jit_State *J)
{
  IRRef uref = fins->op1;
  IRRef lim = uref;  /* Search limit. */
  IRIns *xr = IR(uref);
  IRRef ref;

  /* Search for conflicting stores. */
  ref = J->chain[IR_USTORE];
  while (ref > uref) {
    IRIns *store = IR(ref);
    switch (aa_uref(xr, IR(store->op1))) {
    case ALIAS_NO:   break;  /* Continue searching. */
    case ALIAS_MAY:  lim = ref; goto conflict;  /* Limit search for load. */
    case ALIAS_MUST: return store->op2;  /* Store forwarding. */
    }
    ref = store->prev;
  }

conflict:
  /* Try to find a matching load. Below the conflicting store, if any. */
  return lj_opt_cselim(J, lim);
}

/* USTORE elimination. */
TRef LJ_FASTCALL lj_opt_dse_ustore(jit_State *J)
{
  IRRef xref = fins->op1;  /* xREF reference. */
  IRRef val = fins->op2;  /* Stored value reference. */
  IRIns *xr = IR(xref);
  IRRef1 *refp = &J->chain[IR_USTORE];
  IRRef ref = *refp;
  while (ref > xref) {  /* Search for redundant or conflicting stores. */
    IRIns *store = IR(ref);
    switch (aa_uref(xr, IR(store->op1))) {
    case ALIAS_NO:
      break;  /* Continue searching. */
    case ALIAS_MAY:	/* Store to MAYBE the same location. */
      if (store->op2 != val)  /* Conflict if the value is different. */
	goto doemit;
      break;  /* Otherwise continue searching. */
    case ALIAS_MUST:	/* Store to the same location. */
      if (store->op2 == val)  /* Same value: drop the new store. */
	return DROPFOLD;
      /* Different value: try to eliminate the redundant store. */
      if (ref > J->chain[IR_LOOP]) {  /* Quick check to avoid crossing LOOP. */
	IRIns *ir;
	/* Check for any intervening guards (includes conflicting loads). */
	for (ir = IR(J->cur.nins-1); ir > store; ir--)
	  if (irt_isguard(ir->t))
	    goto doemit;  /* No elimination possible. */
	/* Remove redundant store from chain and replace with NOP. */
	*refp = store->prev;
	store->o = IR_NOP;  /* Unchained NOP -- does anybody care? */
	store->t.irt = IRT_NIL;
	store->op1 = store->op2 = 0;
	store->prev = 0;
	/* Now emit the new store instead. */
      }
      goto doemit;
    }
    ref = *(refp = &store->prev);
  }
doemit:
  return EMITFOLD;  /* Otherwise we have a conflict or simply no match. */
}

/* -- FLOAD forwarding and FSTORE elimination ----------------------------- */

/* Alias analysis for field access.
** Field loads are cheap and field stores are rare.
** Simple disambiguation based on field types is good enough.
*/
static AliasRet aa_fref(IRIns *refa, IRIns *refb)
{
  if (refa->op2 != refb->op2)
    return ALIAS_NO;  /* Different fields. */
  if (refa->op1 == refb->op1)
    return ALIAS_MUST;  /* Same field, same object. */
  else
    return ALIAS_MAY;  /* Same field, possibly different object. */
}

/* Only the loads for mutable fields end up here (see FOLD). */
TRef LJ_FASTCALL lj_opt_fwd_fload(jit_State *J)
{
  IRRef oref = fins->op1;  /* Object reference. */
  IRRef fid = fins->op2;  /* Field ID. */
  IRRef lim = oref;  /* Search limit. */
  IRRef ref;

  /* Search for conflicting stores. */
  ref = J->chain[IR_FSTORE];
  while (ref > oref) {
    IRIns *store = IR(ref);
    switch (aa_fref(fins, IR(store->op1))) {
    case ALIAS_NO:   break;  /* Continue searching. */
    case ALIAS_MAY:  lim = ref; goto conflict;  /* Limit search for load. */
    case ALIAS_MUST: return store->op2;  /* Store forwarding. */
    }
    ref = store->prev;
  }

  /* No conflicting store: const-fold field loads from allocations. */
  if (fid == IRFL_TAB_META) {
    IRIns *ir = IR(oref);
    if (ir->o == IR_TNEW || ir->o == IR_TDUP)
      return lj_ir_knull(J, IRT_TAB);
  }

conflict:
  /* Try to find a matching load. Below the conflicting store, if any. */
  return lj_opt_cselim(J, lim);
}

/* FSTORE elimination. */
TRef LJ_FASTCALL lj_opt_dse_fstore(jit_State *J)
{
  IRRef fref = fins->op1;  /* FREF reference. */
  IRRef val = fins->op2;  /* Stored value reference. */
  IRIns *xr = IR(fref);
  IRRef1 *refp = &J->chain[IR_FSTORE];
  IRRef ref = *refp;
  while (ref > fref) {  /* Search for redundant or conflicting stores. */
    IRIns *store = IR(ref);
    switch (aa_fref(xr, IR(store->op1))) {
    case ALIAS_NO:
      break;  /* Continue searching. */
    case ALIAS_MAY:
      if (store->op2 != val)  /* Conflict if the value is different. */
	goto doemit;
      break;  /* Otherwise continue searching. */
    case ALIAS_MUST:
      if (store->op2 == val)  /* Same value: drop the new store. */
	return DROPFOLD;
      /* Different value: try to eliminate the redundant store. */
      if (ref > J->chain[IR_LOOP]) {  /* Quick check to avoid crossing LOOP. */
	IRIns *ir;
	/* Check for any intervening guards or conflicting loads. */
	for (ir = IR(J->cur.nins-1); ir > store; ir--)
	  if (irt_isguard(ir->t) || (ir->o == IR_FLOAD && ir->op2 == xr->op2))
	    goto doemit;  /* No elimination possible. */
	/* Remove redundant store from chain and replace with NOP. */
	*refp = store->prev;
	store->o = IR_NOP;  /* Unchained NOP -- does anybody care? */
	store->t.irt = IRT_NIL;
	store->op1 = store->op2 = 0;
	store->prev = 0;
	/* Now emit the new store instead. */
      }
      goto doemit;
    }
    ref = *(refp = &store->prev);
  }
doemit:
  return EMITFOLD;  /* Otherwise we have a conflict or simply no match. */
}

/* -- Forwarding of lj_tab_len -------------------------------------------- */

/* This is rather simplistic right now, but better than nothing. */
TRef LJ_FASTCALL lj_opt_fwd_tab_len(jit_State *J)
{
  IRRef tab = fins->op1;  /* Table reference. */
  IRRef lim = tab;  /* Search limit. */
  IRRef ref;

  /* Any ASTORE is a conflict and limits the search. */
  if (J->chain[IR_ASTORE] > lim) lim = J->chain[IR_ASTORE];

  /* Search for conflicting HSTORE with numeric key. */
  ref = J->chain[IR_HSTORE];
  while (ref > lim) {
    IRIns *store = IR(ref);
    IRIns *href = IR(store->op1);
    IRIns *key = IR(href->op2);
    if (irt_isnum(key->o == IR_KSLOT ? IR(key->op1)->t : key->t)) {
      lim = ref;  /* Conflicting store found, limits search for TLEN. */
      break;
    }
    ref = store->prev;
  }

  /* Try to find a matching load. Below the conflicting store, if any. */
  return lj_opt_cselim(J, lim);
}

/* -- ASTORE/HSTORE previous type analysis -------------------------------- */

/* Check whether the previous value for a table store is non-nil.
** This can be derived either from a previous store or from a previous
** load (because all loads from tables perform a type check).
**
** The result of the analysis can be used to avoid the metatable check
** and the guard against HREF returning niltv. Both of these are cheap,
** so let's not spend too much effort on the analysis.
**
** A result of 1 is exact: previous value CANNOT be nil.
** A result of 0 is inexact: previous value MAY be nil.
*/
int lj_opt_fwd_wasnonnil(jit_State *J, IROpT loadop, IRRef xref)
{
  /* First check stores. */
  IRRef ref = J->chain[loadop+IRDELTA_L2S];
  while (ref > xref) {
    IRIns *store = IR(ref);
    if (store->op1 == xref) {  /* Same xREF. */
      /* A nil store MAY alias, but a non-nil store MUST alias. */
      return !irt_isnil(store->t);
    } else if (irt_isnil(store->t)) {  /* Must check any nil store. */
      IRRef skref = IR(store->op1)->op2;
      IRRef xkref = IR(xref)->op2;
      /* Same key type MAY alias. Need ALOAD check due to multiple int types. */
      if (loadop == IR_ALOAD || irt_sametype(IR(skref)->t, IR(xkref)->t)) {
	if (skref == xkref || !irref_isk(skref) || !irref_isk(xkref))
	  return 0;  /* A nil store with same const key or var key MAY alias. */
	/* Different const keys CANNOT alias. */
      }  /* Different key types CANNOT alias. */
    }  /* Other non-nil stores MAY alias. */
    ref = store->prev;
  }

  /* Check loads since nothing could be derived from stores. */
  ref = J->chain[loadop];
  while (ref > xref) {
    IRIns *load = IR(ref);
    if (load->op1 == xref) {  /* Same xREF. */
      /* A nil load MAY alias, but a non-nil load MUST alias. */
      return !irt_isnil(load->t);
    }  /* Other non-nil loads MAY alias. */
    ref = load->prev;
  }
  return 0;  /* Nothing derived at all, previous value MAY be nil. */
}

/* ------------------------------------------------------------------------ */

#undef IR
#undef fins
#undef fright

#endif
