/*
*   $Id$
*
*   Copyright (c) 2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   External interface to routines.c
*/
#ifndef _ROUTINES_H
#define _ROUTINES_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

/*
*   RUST FFI DECLARATIONS (types that don't need fileStatus)
*/
extern void rs_eFree(void *ptr);
extern char* rs_eStrdup(const char* str);
extern void rs_toLowerString(char* str);
extern void rs_toUpperString(char* str);
extern char* rs_newLowerString(const char* str);
extern char* rs_newUpperString(const char* str);
extern int rs_isPathSeparator(int c);
extern const char* rs_baseFilename(const char* filePath);
extern const char* rs_fileExtension(const char* fileName);
extern int rs_isAbsolutePath(const char* path);

/*
*   MACROS
*/
#define xMalloc(n,Type)    (Type *)eMalloc((size_t)(n) * sizeof (Type))
#define xCalloc(n,Type)    (Type *)eCalloc((size_t)(n), sizeof (Type))
#define xRealloc(p,n,Type) (Type *)eRealloc((p), (n) * sizeof (Type))

/* Rust-delegating macros for string/memory functions */
#define eFree(ptr) rs_eFree((void *)(ptr))
#define eStrdup(str) rs_eStrdup(str)
#define toLowerString(str) rs_toLowerString(str)
#define toUpperString(str) rs_toUpperString(str)
#define newLowerString(str) rs_newLowerString(str)
#define newUpperString(str) rs_newUpperString(str)

/* Rust-delegating macros for path functions */
#define isPathSeparator(c) ((boolean) rs_isPathSeparator(c))
#define baseFilename(filePath) rs_baseFilename(filePath)
#define fileExtension(fileName) rs_fileExtension(fileName)
#define isAbsolutePath(path) ((boolean) rs_isAbsolutePath(path))

/*
 *  Portability macros
 */
#ifndef PATH_SEPARATOR
# if defined (MSDOS_STYLE_PATH)
#  define PATH_SEPARATOR '\\'
# elif defined (QDOS)
#  define PATH_SEPARATOR '_'
# else
#  define PATH_SEPARATOR '/'
# endif
#endif

#if defined (MSDOS_STYLE_PATH) && defined (UNIX_PATH_SEPARATOR)
# define OUTPUT_PATH_SEPARATOR	'/'
#else
# define OUTPUT_PATH_SEPARATOR	PATH_SEPARATOR
#endif

/*
*   DATA DECLARATIONS
*/
#if defined (MSDOS_STYLE_PATH) || defined (VMS)
extern const char *const PathDelimiters;
#endif

/* CurrentDirectory is now Rust-owned in cparser.rs */
extern char *rs_getCurrentDirectory(void);
extern void rs_setCurrentDirectory(char *dir);
#define CurrentDirectory (rs_getCurrentDirectory())
typedef int errorSelection;
enum eErrorTypes { FATAL = 1, WARNING = 2, PERROR = 4 };

typedef struct {
		/* Name of file for which status is valid */
	char* name;

		/* Does file exist? If not, members below do not contain valid data. */
	boolean exists;

		/* is file path a symbolic link to another file? */
	boolean isSymbolicLink;

		/* Is file (pointed to) a directory? */
	boolean isDirectory;

		/* Is file (pointed to) a normal file? */
	boolean isNormalFile;

		/* Is file (pointed to) executable? */
	boolean isExecutable;

		/* Is file (pointed to) setuid? */
	boolean isSetuid;

		/* Size of file (pointed to) */
	unsigned long size;
} fileStatus;

/* Rust FFI for fileStatus (must be after fileStatus typedef) */
extern void rs_eStatFree(fileStatus *status);
#define eStatFree(status) rs_eStatFree(status)

/*
*   FUNCTION PROTOTYPES
*/
extern void freeRoutineResources (void);
extern void setExecutableName (const char *const path);
extern const char *getExecutableName (void);
extern const char *getExecutablePath (void);
extern void error (const errorSelection selection, const char *const format, ...) __printf__ (2, 3);

/* Memory allocation functions */
#ifdef NEED_PROTO_MALLOC
extern void *malloc (size_t);
extern void *realloc (void *ptr, size_t);
#endif
extern void *eMalloc (const size_t size);
extern void *eCalloc (const size_t count, const size_t size);
extern void *eRealloc (void *const ptr, const size_t size);
/* eFree is now a macro - see above */

/* String manipulation functions - struppercmp/strnuppercmp now implemented in Rust */
extern int rs_struppercmp (const char *s1, const char *s2);
extern int rs_strnuppercmp (const char *s1, const char *s2, size_t n);
#define struppercmp(s1, s2) rs_struppercmp(s1, s2)
#define strnuppercmp(s1, s2, n) rs_strnuppercmp(s1, s2, n)
#ifndef HAVE_STRSTR
extern char* strstr (const char *str, const char *substr);
#endif
/* eStrdup, toLowerString, toUpperString, newLowerString, newUpperString are now macros */

/* File system functions */
extern void setCurrentDirectory (void);
extern fileStatus *eStat (const char *const fileName);
extern boolean doesFileExist (const char *const fileName);
extern boolean isRecursiveLink (const char* const dirName);
extern boolean isSameFile (const char *const name1, const char *const name2);
#if defined(NEED_PROTO_FGETPOS)
extern int fgetpos  (FILE *stream, fpos_t *pos);
extern int fsetpos  (FILE *stream, fpos_t *pos);
#endif
/* baseFilename, fileExtension, isAbsolutePath are now macros - see above */
extern vString *combinePathAndFile (const char *const path, const char *const file);
extern char* absoluteFilename (const char *file);
extern char* absoluteDirname (char *file);
extern char* relativeFilename (const char *file, const char *dir);
extern FILE *tempFile (const char *const mode, char **const pName);

#endif  /* _ROUTINES_H */

/* vi:set tabstop=4 shiftwidth=4: */
