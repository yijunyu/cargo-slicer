/*
*   $Id$
*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   Provides the external interface for resizeable strings.
*/
#ifndef _VSTRING_H
#define _VSTRING_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#if defined(HAVE_STDLIB_H)
# include <stdlib.h>  /* to define size_t */
#endif

/*
*   MACROS
*/
#ifndef DEBUG
# define VSTRING_PUTC_MACRO 1
#endif
#ifdef VSTRING_PUTC_MACRO
#define rs_vStringPut(s,c) \
	(void)(((s)->length + 1 == (s)->size ? rs_vStringAutoResize (s) : 0), \
	((s)->buffer [(s)->length] = (c)), \
	((c) == '\0' ? 0 : ((s)->buffer [++(s)->length] = '\0')))
#endif

#define vStringValue(vs)      ((vs)->buffer)
#define vStringItem(vs,i)     ((vs)->buffer[i])
#define vStringLast(vs)       ((vs)->buffer[(vs)->length - 1])
#define vStringLength(vs)     ((vs)->length)
#define vStringSize(vs)       ((vs)->size)
#define vStringCat(vs,s)      rs_vStringCatS((vs), vStringValue((s)))
#define vStringNCat(vs,s,l)   rs_vStringNCatS((vs), vStringValue((s)), (l))
#define vStringCopy(vs,s)     rs_vStringCopyS((vs), vStringValue((s)))
#define vStringNCopy(vs,s,l)  rs_vStringNCopyS((vs), vStringValue((s)), (l))
#define vStringChar(vs,i)     ((vs)->buffer[i])
#define vStringTerminate(vs)  rs_vStringPut(vs, '\0')
#define vStringLower(vs)      toLowerString((vs)->buffer)
#define vStringUpper(vs)      toUpperString((vs)->buffer)

/*
*   DATA DECLARATIONS
*/

typedef struct sVString {
	size_t  length;  /* size of buffer used */
	size_t  size;    /* allocated size of buffer */
	char   *buffer;  /* location of buffer */
} vString;

/*
*   RUST FFI FUNCTION DECLARATIONS
*   These are implemented in src/ctags_rs/vstring.rs
*/
extern vString *rs_vStringNew (void);
extern vString *rs_vStringNewInit (const char *const s);
extern vString *rs_vStringNewCopy (const vString *const string);
extern void rs_vStringDelete (vString *string);
extern void rs_vStringClear (vString *string);
extern int rs_vStringAutoResize (vString *string);
extern void rs_vStringPut (vString *string, int c);
extern void rs_vStringCatS (vString *string, const char *s);
extern void rs_vStringNCatS (vString *string, const char *s, size_t length);
extern void rs_vStringCopyS (vString *string, const char *s);
extern void rs_vStringNCopyS (vString *string, const char *s, size_t length);
extern void rs_vStringStripNewline (vString *string);
extern void rs_vStringStripLeading (vString *string);
extern void rs_vStringStripTrailing (vString *string);
extern void rs_vStringChop (vString *string);
extern void rs_vStringCopyToLower (vString *dest, const vString *src);
extern void rs_vStringSetLength (vString *string);
extern void rs_vStringTruncate (vString *string, size_t new_len);
extern void rs_freeTagFileResources (char **directory, vString **vLine);

/*
*   SIGNATURE GLOBAL STATE FFI
*   Signature vString is now managed in Rust (vstring.rs)
*/
extern void rs_initSignature (void);
extern vString *rs_getSignature (void);
extern void rs_deleteSignature (void);
extern size_t rs_signatureLength (void);
extern const char *rs_signatureValue (void);
extern void rs_signatureCatVString (const vString *other);

#endif  /* _VSTRING_H */

/* vi:set tabstop=4 shiftwidth=4: */
