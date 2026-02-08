/*
*   $Id$
*
*   Copyright (c) 1999-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   Defines external interface to resizable string lists.
*/
#ifndef _STRLIST_H
#define _STRLIST_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include "vstring.h"

/*
*   DATA DECLARATIONS
*/
typedef struct sStringList {
	unsigned int max;
	unsigned int count;
	vString    **list;
} stringList;

/*
*   FUNCTION PROTOTYPES
*/
extern stringList *stringListNew (void);
extern void stringListAdd (stringList *const current, vString *string);
extern void stringListRemoveLast (stringList *const current);
extern void stringListCombine (stringList *const current, stringList *const from);
extern stringList* stringListNewFromArgv (const char* const* const list);
extern stringList* stringListNewFromFile (const char* const fileName);
extern void stringListClear (stringList *const current);
extern unsigned int stringListCount (const stringList *const current);
extern vString* stringListItem (const stringList *const current, const unsigned int indx);
extern vString* stringListLast (const stringList *const current);
extern void stringListDelete (stringList *const current);
extern boolean stringListHasInsensitive (const stringList *const current, const char *const string);
extern boolean stringListHas (const stringList *const current, const char *const string);
extern boolean stringListHasTest (const stringList *const current, boolean (*test)(const char *s));
extern boolean stringListRemoveExtension (stringList* const current, const char* const extension);
extern boolean stringListExtensionMatched (const stringList* const list, const char* const extension);
extern boolean stringListFileMatched (const stringList* const list, const char* const str);
extern void stringListPrint (const stringList *const current);

/*
*   RUST FFI FUNCTION DECLARATIONS
*   These are implemented in src/ctags_rs/strlist.rs
*/
extern stringList *rs_stringListNew (void);
extern void rs_stringListDelete (stringList *current);
extern void rs_freeList (stringList **pList);
extern void rs_stringListAdd (stringList *current, vString *string);
extern void rs_stringListRemoveLast (stringList *current);
extern void rs_stringListCombine (stringList *current, stringList *from);
extern stringList *rs_stringListNewFromArgv (const char *const *argv);
extern unsigned int rs_stringListCount (const stringList *current);
extern vString *rs_stringListItem (const stringList *current, unsigned int index);
extern vString *rs_stringListLast (const stringList *current);
extern void rs_stringListClear (stringList *current);
extern int rs_stringListHas (const stringList *current, const char *string);
extern int rs_stringListHasInsensitive (const stringList *current, const char *string);
extern int rs_stringListHasTest (const stringList *current, int (*test)(const char *s));
extern int rs_stringListRemoveExtension (stringList *current, const char *extension);
extern int rs_stringListExtensionMatched (const stringList *current, const char *extension);
extern int rs_stringListFileMatched (const stringList *current, const char *filename);
extern void rs_stringListPrint (const stringList *current);
extern void rs_stringListAddString (stringList **list_ptr, const char *string);

#endif  /* _STRLIST_H */

/* vi:set tabstop=4 shiftwidth=4: */
