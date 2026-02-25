/*
*   $Id$
*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   External interface to get.c
*/
#ifndef _GET_H
#define _GET_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "ctags.h"  /* to define langType */

/*
*   RUST FFI DECLARATIONS
*/
extern int rs_isBraceFormat(void);

/*
*   MACROS
*/
/*  Is the character valid as a character of a C identifier?
 *  VMS allows '$' in identifiers.
 */
#define isident(c)  (isalnum(c) || (c) == '_' || (c) == '$')

/*  Is the character valid as the first character of a C identifier?
 *  C++ allows '~' in destructors.
 *  VMS allows '$' in identifiers.
 */
#define isident1(c)  (isalpha(c) || (c) == '_' || (c) == '~' || (c) == '$')

/* isBraceFormat - now a macro delegating to Rust */
#define isBraceFormat() ((boolean) rs_isBraceFormat())

/*
*   FUNCTION PROTOTYPES
*/
/* isBraceFormat is now a macro - see above */
/* getDirectiveNestLevel now implemented in Rust */
extern unsigned int rs_getDirectiveNestLevel (void);
#define getDirectiveNestLevel() rs_getDirectiveNestLevel()

/* cpp* functions now implemented in Rust (src/ctags_rs/clex.rs) */
extern void rs_cppInit (int state, int hasAtLiteralStrings);
extern void rs_cppTerminate (void);
extern void rs_cppBeginStatement (void);
extern void rs_cppEndStatement (void);
extern void rs_cppUngetc (int c);

extern int cppGetc (void);
extern int skipOverCComment (void);

#endif  /* _GET_H */

/* vi:set tabstop=4 shiftwidth=4: */
