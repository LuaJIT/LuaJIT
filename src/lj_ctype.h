/*
** Internal CTYPE replacement.
** Donated to the public domain.
*/

#ifndef _LJ_CTYPE_H
#define _LJ_CTYPE_H

#include "lj_def.h"

#define LJ_CTYPE_CNTRL	0x01
#define LJ_CTYPE_SPACE	0x02
#define LJ_CTYPE_PUNCT	0x04
#define LJ_CTYPE_DIGIT	0x08
#define LJ_CTYPE_XDIGIT	0x10
#define LJ_CTYPE_UPPER	0x20
#define LJ_CTYPE_LOWER	0x40
#define LJ_CTYPE_IDENT	0x80
#define LJ_CTYPE_ALPHA	(LJ_CTYPE_LOWER|LJ_CTYPE_UPPER)
#define LJ_CTYPE_ALNUM	(LJ_CTYPE_ALPHA|LJ_CTYPE_DIGIT)

/* Only pass -1 or 0..255 to these macros. Never pass a signed char! */
#define lj_ctype_isa(c, t)	(lj_ctype_bits[(c)+1] & t)
#define lj_ctype_iscntrl(c)	lj_ctype_isa((c), LJ_CTYPE_CNTRL)
#define lj_ctype_isspace(c)	lj_ctype_isa((c), LJ_CTYPE_SPACE)
#define lj_ctype_ispunct(c)	lj_ctype_isa((c), LJ_CTYPE_PUNCT)
#define lj_ctype_isdigit(c)	lj_ctype_isa((c), LJ_CTYPE_DIGIT)
#define lj_ctype_isxdigit(c)	lj_ctype_isa((c), LJ_CTYPE_XDIGIT)
#define lj_ctype_isupper(c)	lj_ctype_isa((c), LJ_CTYPE_UPPER)
#define lj_ctype_islower(c)	lj_ctype_isa((c), LJ_CTYPE_LOWER)
#define lj_ctype_isident(c)	lj_ctype_isa((c), LJ_CTYPE_IDENT)
#define lj_ctype_isalpha(c)	lj_ctype_isa((c), LJ_CTYPE_ALPHA)
#define lj_ctype_isalnum(c)	lj_ctype_isa((c), LJ_CTYPE_ALNUM)

#define lj_ctype_toupper(c)	((c) - (lj_ctype_islower(c) >> 1))
#define lj_ctype_tolower(c)	((c) + lj_ctype_isupper(c))

LJ_DATA const uint8_t lj_ctype_bits[257];

#endif
