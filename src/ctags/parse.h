/*
*   $Id$
*
*   Copyright (c) 1998-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   Private definitions for parsing support.
*/
#ifndef _PARSE_H
#define _PARSE_H

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include "parsers.h"  /* contains list of parsers */
#include "strlist.h"

/*
*   MACROS
*/
#define KIND_COUNT(kindTable) (sizeof(kindTable)/sizeof(kindOption))

#define LANG_AUTO   (-1)
#define LANG_IGNORE (-2)

/*
*   DATA DECLARATIONS
*/
typedef int langType;

typedef enum {
	RESCAN_NONE,   /* No rescan needed */
	RESCAN_FAILED, /* Scan failed, clear out tags added, rescan */
	RESCAN_APPEND  /* Scan succeeded, rescan */
} rescanReason;

typedef void (*createRegexTag) (const vString* const name);
typedef void (*simpleParser) (void);
typedef rescanReason (*rescanParser) (const unsigned int passCount);
typedef void (*parserInitialize) (langType language);

typedef struct sKindOption {
	boolean enabled;          /* are tags for kind enabled? */
	int letter;               /* kind letter */
	const char* name;         /* kind name */
	const char* description;  /* displayed in --help output */
} kindOption;

typedef struct {
	/* defined by parser */
	char* name;                    /* name of language */
	kindOption* kinds;             /* tag kinds handled by parser */
	unsigned int kindCount;        /* size of `kinds' list */
	const char *const *extensions; /* list of default extensions */
	const char *const *patterns;   /* list of default file name patterns */
	parserInitialize initialize;   /* initialization routine, if needed */
	simpleParser parser;           /* simple parser (common case) */
	rescanParser parser2;          /* rescanning parser (unusual case) */
	boolean regex;                 /* is this a regex parser? */

	/* used internally */
	unsigned int id;               /* id assigned to language */
	boolean enabled;               /* currently enabled? */
	stringList* currentPatterns;   /* current list of file name patterns */
	stringList* currentExtensions; /* current list of extensions */
} parserDefinition;

typedef parserDefinition* (parserDefinitionFunc) (void);

typedef struct {
	size_t start;   /* character index in line where match starts */
	size_t length;  /* length of match */
} regexMatch;

typedef void (*regexCallback) (const char *line, const regexMatch *matches, unsigned int count);

/*
*   FUNCTION PROTOTYPES
*/

/* Each parsers' definition function is called. The routine is expected to
 * return a structure allocated using parserNew(). This structure must,
 * at minimum, set the `parser' field.
 */
extern parserDefinitionFunc PARSER_LIST;

/* Legacy interface */
extern boolean includingDefineTags (void);

/* Language processing and parsing */
extern void makeSimpleTag (const vString* const name, kindOption* const kinds, const int kind);
extern parserDefinition* rs_parserNew (const char* name);
#define parserNew(name) rs_parserNew(name)
extern const char *getLanguageName (const langType language);
extern langType getNamedLanguage (const char *const name);
extern langType getFileLanguage (const char *const fileName);
extern void installLanguageMapDefault (const langType language);
extern void installLanguageMapDefaults (void);
extern void clearLanguageMap (const langType language);
extern boolean removeLanguageExtensionMap (const char *const extension);
extern void addLanguageExtensionMap (const langType language, const char* extension);
extern void addLanguagePatternMap (const langType language, const char* ptrn);
extern void printLanguageMap (const langType language);
extern void printLanguageMaps (const langType language);
extern void enableLanguages (const boolean state);
extern void enableLanguage (const langType language, const boolean state);
extern void initializeParsing (void);
extern void freeParserResources (void);
extern void processLanguageDefineOption (const char *const option, const char *const parameter);
extern boolean processKindOption (const char *const option, const char *const parameter);
extern void printKindOptions (void);
extern void printLanguageKinds (const langType language);
extern void printLanguageList (void);
extern boolean parseFile (const char *const fileName);

/* Regex interface */
#ifdef HAVE_REGEX
extern void findRegexTags (void);
extern boolean matchRegex (const vString* const line, const langType language);
#endif
extern boolean processRegexOption (const char *const option, const char *const parameter);
extern void addLanguageRegex (const langType language, const char* const regex);
extern void addTagRegex (const langType language, const char* const regex, const char* const name, const char* const kinds, const char* const flags);
extern void addCallbackRegex (const langType language, const char *const regex, const char *const flags, const regexCallback callback);
extern void disableRegexKinds (const langType language);
extern boolean enableRegexKind (const langType language, const int kind, const boolean mode);
extern void printRegexKinds (const langType language, boolean indent);
/* freeRegexResources now implemented directly in Rust as rs_freeRegexResourcesDirect */
extern void rs_freeRegexResourcesDirect (void);
extern void checkRegex (void);

/*
*   RUST FFI FUNCTION DECLARATIONS
*   These are implemented in src/ctags_rs/parse.rs
*/
extern kindOption *rs_kindOptionNew (void);
extern void rs_kindOptionDelete (kindOption *kind);
extern int rs_kindOptionIsEnabled (const kindOption *kind);
extern int rs_kindOptionGetLetter (const kindOption *kind);
extern parserDefinition *rs_parserDefinitionNew (void);
extern void rs_parserDefinitionDelete (parserDefinition *parser);
extern int rs_parserDefinitionIsEnabled (const parserDefinition *parser);
extern int rs_parserDefinitionIsRegex (const parserDefinition *parser);
extern unsigned int rs_parserDefinitionGetId (const parserDefinition *parser);
extern unsigned int rs_parserDefinitionGetKindCount (const parserDefinition *parser);
extern regexMatch rs_regexMatchNew (size_t start, size_t length);
extern size_t rs_regexMatchEnd (const regexMatch *match);
extern int rs_regexMatchIsEmpty (const regexMatch *match);

/* LanguageTable and LanguageCount are now Rust-owned in parse.rs */
extern parserDefinition **rs_getLanguageTable (void);
extern void rs_setLanguageTable (parserDefinition **table);
extern unsigned int rs_getLanguageCount (void);
extern void rs_setLanguageCount (unsigned int count);
extern void rs_incrLanguageCount (void);
extern void rs_freeParserResourcesDirect (void);

#define LanguageTable (rs_getLanguageTable())
#define LanguageCount (rs_getLanguageCount())

#endif  /* _PARSE_H */

/* vi:set tabstop=4 shiftwidth=4: */
