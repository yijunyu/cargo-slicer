/*
*   $Id$
*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   External interface to debug.c
*/
#ifndef _DEBUG_H
#define _DEBUG_H

/*
*   Include files
*/
#include "general.h"  /* must always come first */

#ifdef DEBUG
# include <assert.h>
#endif
#include "entry.h"

/*
*   Macros
*/

#ifdef DEBUG
# define debug(level)      ((Option.debugLevel & (long)(level)) != 0)
# define DebugStatement(x) x
# define PrintStatus(x)    if (debug(DEBUG_STATUS)) printf x;
# define Assert(c)         assert(c)
#else
# define DebugStatement(x)
# define PrintStatus(x)
# define Assert(c)
# ifndef NDEBUG
#  define NDEBUG
# endif
#endif

/*
*   Data declarations
*/

/*  Defines the debugging levels.
 */
enum eDebugLevels {
	DEBUG_READ   = 0x01,  /* echo raw (filtered) characters */
	DEBUG_PARSE  = 0x02,  /* echo parsing results */
	DEBUG_STATUS = 0x04,  /* echo file status information */
	DEBUG_OPTION = 0x08,  /* echo option parsing */
	DEBUG_CPP    = 0x10,  /* echo characters out of pre-processor */
	DEBUG_RAW    = 0x20   /* echo raw (filtered) characters */
};

/*
*   Function prototypes
*/
extern void lineBreak (void);
extern void debugPrintf (const enum eDebugLevels level, const char *const format, ...) __printf__ (2, 3);
extern void debugParseNest (const boolean increase, const unsigned int level);
extern void debugCppNest (const boolean begin, const unsigned int level);
extern void debugCppIgnore (const boolean ignore);
extern void debugEntry (const tagEntryInfo *const tag);

/*
 * Fast path for debugPutc - macro to eliminate function call overhead
 * For PRECC_FAST_PATH, we inline the buffer write directly
 */
#ifdef PRECC_FAST_PATH
/* Buffer declared in debug.c */
extern char precc_debug_buffer[];
extern size_t precc_debug_buffer_len;
#define PRECC_DEBUG_BUFFER_SIZE (1024 * 1024)  /* 1MB to handle large array initializers */

/* Inline macro version - no function call overhead */
/* Capture characters from both DEBUG_READ (raw file) and DEBUG_CPP (preprocessor output) */
#define debugPutc(level, c) do { \
    if (((level) == DEBUG_READ || (level) == DEBUG_CPP) && (c) != EOF) { \
        unsigned char _c = (unsigned char)(c); \
        if (_c < 0x80 && precc_debug_buffer_len < PRECC_DEBUG_BUFFER_SIZE) { \
            precc_debug_buffer[precc_debug_buffer_len++] = (char)_c; \
        } else if (_c >= 0x80) { \
            debugPutc_special(_c); \
        } \
    } \
} while(0)

/* Handle special symbols (STRING_SYMBOL, CHAR_SYMBOL) - rare path */
extern void debugPutc_special(int c);
#else
extern void debugPutc (const int level, const int c);
#endif

#endif  /* _DEBUG_H */

/* vi:set tabstop=4 shiftwidth=4: */
