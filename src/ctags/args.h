/*
*   $Id$
*
*   Copyright (c) 1999-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   Defines external interface to command line argument reading.
*/
#ifndef _ARGS_H
#define _ARGS_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <stdio.h>

/*
*   DATA DECLARATIONS
*/

typedef enum { ARG_NONE, ARG_STRING, ARG_ARGV, ARG_FILE } argType;

typedef struct sArgs {
	argType type;
	union {
		struct sStringArgs {
			const char* string;
			const char* next;
			const char* item;
		} stringArgs;
		struct sArgvArgs {
			char* const* argv;
			char* const* item;
		} argvArgs;
		struct sFileArgs {
			FILE* fp;
		} fileArgs;
	} u;
	char* item;
	boolean lineMode;
} Arguments;

/*
*   FUNCTION PROTOTYPES
*/
extern Arguments* argNewFromString (const char* const string);
extern Arguments* argNewFromArgv (char* const* const argv);
extern Arguments* argNewFromFile (FILE* const fp);
extern Arguments* argNewFromLineFile (FILE* const fp);
extern char *argItem (const Arguments* const current);
extern boolean argOff (const Arguments* const current);
extern void argSetWordMode (Arguments* const current);
extern void argSetLineMode (Arguments* const current);
extern void argForth (Arguments* const current);
extern void argDelete (Arguments* const current);

/*
*   RUST FFI FUNCTION DECLARATIONS
*   These are implemented in src/ctags_rs/args.rs
*/
extern Arguments *rs_argNewFromString (const char *string);
extern Arguments *rs_argNewFromArgv (char *const *argv);
extern Arguments *rs_argNewFromFile (void *fp);
extern Arguments *rs_argNewFromLineFile (void *fp);
extern char *rs_argItem (const Arguments *current);
extern int rs_argOff (const Arguments *current);
extern void rs_argSetWordMode (Arguments *current);
extern void rs_argSetLineMode (Arguments *current);
extern void rs_argForth (Arguments *current);
extern void rs_argDelete (Arguments *current);

#endif  /* _ARGS_H */

/* vi:set tabstop=4 shiftwidth=4: */
