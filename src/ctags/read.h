/*
*   $Id$
*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   External interface to read.c
*/
#ifndef _READ_H
#define _READ_H

#if defined(FILE_WRITE) || defined(VAXC)
# define CONST_FILE
#else
# define CONST_FILE const
#endif

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <stdio.h>
#include <ctype.h>

#include "parse.h"
#include "vstring.h"

/*
*   MACROS
*/
#define getInputLineNumber()     File.lineNumber
#define getInputFileName()       vStringValue (File.source.name)
#define getInputFilePosition()   File.filePosition
#define getSourceFileName()      vStringValue (File.source.name)
#define getSourceFileTagPath()   File.source.tagPath
#define getSourceLanguage()      File.source.language
#define getSourceLanguageName()  getLanguageName (File.source.language)
#define getSourceLineNumber()    File.source.lineNumber
#define isLanguage(lang)         (boolean)((lang) == File.source.language)
#define isHeaderFile()           File.source.isHeader

/*
*   DATA DECLARATIONS
*/

enum eCharacters {
	/* white space characters */
	SPACE         = ' ',
	NEWLINE       = '\n',
	CRETURN       = '\r',
	FORMFEED      = '\f',
	TAB           = '\t',
	VTAB          = '\v',

	/* some hard to read characters */
	DOUBLE_QUOTE  = '"',
	SINGLE_QUOTE  = '\'',
	BACKSLASH     = '\\',

	STRING_SYMBOL = ('S' + 0x80),
	CHAR_SYMBOL   = ('C' + 0x80)
};

/*  Maintains the state of the current source file.
 */
typedef struct sInputFile {
	vString    *name;          /* name of input file */
	vString    *path;          /* path of input file (if any) */
	vString    *line;          /* last line read from file */
	const unsigned char* currentLine;  /* current line being worked on */
	FILE       *fp;            /* stream used for reading the file */
	unsigned long lineNumber;  /* line number in the input file */
	fpos_t      filePosition;  /* file position of current line */
	int         ungetch;       /* a single character that was ungotten */
	boolean     eof;           /* have we reached the end of file? */
	boolean     newLine;       /* will the next character begin a new line? */

	/*  Contains data pertaining to the original source file in which the tag
	 *  was defined. This may be different from the input file when #line
	 *  directives are processed (i.e. the input file is preprocessor output).
	 */
	struct sSource {
		vString *name;           /* name to report for source file */
		char    *tagPath;        /* path of source file relative to tag file */
		unsigned long lineNumber;/* line number in the source file */
		boolean  isHeader;       /* is source file a header file? */
		langType language;       /* language of source file */
	} source;
} inputFile;

/*
*   GLOBAL VARIABLES
*   File is now owned by Rust - access via rs_getFile() macro
*/
extern inputFile *rs_getFile(void);
#define File (*rs_getFile())

/*
*   FUNCTION PROTOTYPES
*/
extern void freeSourceFileResources (void);
extern boolean fileOpen (const char *const fileName, const langType language);
/* fileEOF now implemented in Rust */
extern int rs_fileEOF (void);
#define fileEOF() ((boolean) rs_fileEOF())
extern void fileClose (void);

/* fileGetc - now implemented in Rust */
extern int rs_fileGetc (void);
#define fileGetc() rs_fileGetc()

extern int fileSkipToCharacter (int c);

/* fileUngetc - now implemented in Rust */
extern void rs_fileUngetc (int c);
#define fileUngetc(c) rs_fileUngetc((int)(c))

/* fileReadLine - now implemented in Rust */
extern const unsigned char *rs_fileReadLine(void);
#define fileReadLine() rs_fileReadLine()

extern char *readLine (vString *const vLine, FILE *const fp);
extern char *readSourceLine (vString *const vLine, fpos_t location, long *const pSeekValue);

/*
*   RUST FFI FUNCTION DECLARATIONS
*   These are implemented in src/ctags_rs/read.rs
*/
extern inputFile *rs_inputFileNew (void);
extern void rs_inputFileDelete (inputFile *file);
extern unsigned long rs_inputFileGetLineNumber (const inputFile *file);
extern int rs_inputFileIsEof (const inputFile *file);
extern int rs_inputFileGetUngetch (const inputFile *file);
extern void rs_inputFileSetUngetch (inputFile *file, int c);
extern int rs_isWhitespace (int c);
extern int rs_isNewline (int c);

#endif  /* _READ_H */

/* vi:set tabstop=4 shiftwidth=4: */
