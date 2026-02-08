/* ctags amalgamation - defines for writable globals */
#define OPTION_WRITE
#define FILE_WRITE
/*
*   $Id$
*
*   Copyright (c) 1996-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for parsing and scanning C, C++ and Java
*   source files.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"        /* must always come first */

#include <string.h>
#include <setjmp.h>

#include "debug.h"
#include "entry.h"
#include "get.h"
#include "keyword.h"
#include "options.h"
#include "parse.h"
#include "read.h"
#include "routines.h"

/*
*   MACROS
*/

#define activeToken(st)     ((st)->token [(int) (st)->tokenIndex])
#define parentDecl(st)      ((st)->parent == NULL ? \
                            DECL_NONE : (st)->parent->declaration)
#define isType(token,t)     (boolean) ((token)->type == (t))
#define insideEnumBody(st)  ((st)->parent == NULL ? FALSE : \
                            (boolean) ((st)->parent->declaration == DECL_ENUM))
#define isExternCDecl(st,c) (boolean) ((c) == STRING_SYMBOL  && \
                    ! (st)->haveQualifyingName  && (st)->scope == SCOPE_EXTERN)

#define isOneOf(c,s)        (boolean) (strchr ((s), (c)) != NULL)

#define isHighChar(c)       ((c) != EOF && (unsigned char)(c) >= 0xc0)

/*
*   DATA DECLARATIONS
*/

enum { NumTokens = 3 };

typedef enum eException {
	ExceptionNone, ExceptionEOF, ExceptionFormattingError,
	ExceptionBraceFormattingError
} exception_t;

/*  Used to specify type of keyword.
 */
typedef enum eKeywordId {
	KEYWORD_NONE = -1,
	KEYWORD_ATTRIBUTE, KEYWORD_ABSTRACT,
	KEYWORD_BOOLEAN, KEYWORD_BYTE, KEYWORD_BAD_STATE, KEYWORD_BAD_TRANS,
	KEYWORD_BIND, KEYWORD_BIND_VAR, KEYWORD_BIT,
	KEYWORD_CASE, KEYWORD_CATCH, KEYWORD_CHAR, KEYWORD_CLASS, KEYWORD_CONST,
	KEYWORD_CONSTRAINT, KEYWORD_COVERAGE_BLOCK, KEYWORD_COVERAGE_DEF,
	KEYWORD_DEFAULT, KEYWORD_DELEGATE, KEYWORD_DELETE, KEYWORD_DO,
	KEYWORD_DOUBLE,
	KEYWORD_ELSE, KEYWORD_ENUM, KEYWORD_EXPLICIT, KEYWORD_EXTERN,
	KEYWORD_EXTENDS, KEYWORD_EVENT,
	KEYWORD_FINAL, KEYWORD_FLOAT, KEYWORD_FOR, KEYWORD_FOREACH,
	KEYWORD_FRIEND, KEYWORD_FUNCTION,
	KEYWORD_GOTO,
	KEYWORD_IF, KEYWORD_IMPLEMENTS, KEYWORD_IMPORT, KEYWORD_INLINE, KEYWORD_INT,
	KEYWORD_INOUT, KEYWORD_INPUT, KEYWORD_INTEGER, KEYWORD_INTERFACE,
	KEYWORD_INTERNAL,
	KEYWORD_LOCAL, KEYWORD_LONG,
	KEYWORD_M_BAD_STATE, KEYWORD_M_BAD_TRANS, KEYWORD_M_STATE, KEYWORD_M_TRANS,
	KEYWORD_MUTABLE,
	KEYWORD_NAMESPACE, KEYWORD_NEW, KEYWORD_NEWCOV, KEYWORD_NATIVE,
	KEYWORD_OPERATOR, KEYWORD_OUTPUT, KEYWORD_OVERLOAD, KEYWORD_OVERRIDE,
	KEYWORD_PACKED, KEYWORD_PORT, KEYWORD_PACKAGE, KEYWORD_PRIVATE,
	KEYWORD_PROGRAM, KEYWORD_PROTECTED, KEYWORD_PUBLIC,
	KEYWORD_REGISTER, KEYWORD_RETURN,
	KEYWORD_SHADOW, KEYWORD_STATE,
	KEYWORD_SHORT, KEYWORD_SIGNED, KEYWORD_STATIC, KEYWORD_STRING,
	KEYWORD_STRUCT, KEYWORD_SWITCH, KEYWORD_SYNCHRONIZED,
	KEYWORD_TASK, KEYWORD_TEMPLATE, KEYWORD_THIS, KEYWORD_THROW,
	KEYWORD_THROWS, KEYWORD_TRANSIENT, KEYWORD_TRANS, KEYWORD_TRANSITION,
	KEYWORD_TRY, KEYWORD_TYPEDEF, KEYWORD_TYPENAME,
	KEYWORD_UINT, KEYWORD_ULONG, KEYWORD_UNION, KEYWORD_UNSIGNED, KEYWORD_USHORT,
	KEYWORD_USING,
	KEYWORD_VIRTUAL, KEYWORD_VOID, KEYWORD_VOLATILE,
	KEYWORD_WCHAR_T, KEYWORD_WHILE
} keywordId;

/*  Used to determine whether keyword is valid for the current language and
 *  what its ID is.
 */
typedef struct sKeywordDesc {
	const char *name;
	keywordId id;
	short isValid [5]; /* indicates languages for which kw is valid */
} keywordDesc;

/*  Used for reporting the type of object parsed by nextToken ().
 */
typedef enum eTokenType {
	TOKEN_NONE,          /* none */
	TOKEN_ARGS,          /* a parenthetical pair and its contents */
	TOKEN_BRACE_CLOSE,
	TOKEN_BRACE_OPEN,
	TOKEN_COLON,         /* the colon character */
	TOKEN_COMMA,         /* the comma character */
	TOKEN_DOUBLE_COLON,  /* double colon indicates nested-name-specifier */
	TOKEN_KEYWORD,
	TOKEN_NAME,          /* an unknown name */
	TOKEN_PACKAGE,       /* a Java package name */
	TOKEN_PAREN_NAME,    /* a single name in parentheses */
	TOKEN_SEMICOLON,     /* the semicolon character */
	TOKEN_SPEC,          /* a storage class specifier, qualifier, type, etc. */
	TOKEN_COUNT
} tokenType;

/*  This describes the scoping of the current statement.
 */
typedef enum eTagScope {
	SCOPE_GLOBAL,        /* no storage class specified */
	SCOPE_STATIC,        /* static storage class */
	SCOPE_EXTERN,        /* external storage class */
	SCOPE_FRIEND,        /* declares access only */
	SCOPE_TYPEDEF,       /* scoping depends upon context */
	SCOPE_COUNT
} tagScope;

typedef enum eDeclaration {
	DECL_NONE,
	DECL_BASE,           /* base type (default) */
	DECL_CLASS,
	DECL_ENUM,
	DECL_EVENT,
	DECL_FUNCTION,
	DECL_IGNORE,         /* non-taggable "declaration" */
	DECL_INTERFACE,
	DECL_NAMESPACE,
	DECL_NOMANGLE,       /* C++ name demangling block */
	DECL_PACKAGE,
	DECL_PROGRAM,        /* Vera program */
	DECL_STRUCT,
	DECL_TASK,           /* Vera task */
	DECL_UNION,
	DECL_COUNT
} declType;

typedef enum eVisibilityType {
	ACCESS_UNDEFINED,
	ACCESS_LOCAL,
	ACCESS_PRIVATE,
	ACCESS_PROTECTED,
	ACCESS_PUBLIC,
	ACCESS_DEFAULT,      /* Java-specific */
	ACCESS_COUNT
} accessType;

/*  Information about the parent class of a member (if any).
 */
typedef struct sMemberInfo {
	accessType access;           /* access of current statement */
	accessType accessDefault;    /* access default for current statement */
} memberInfo;

typedef struct sTokenInfo {
	tokenType     type;
	keywordId     keyword;
	vString*      name;          /* the name of the token */
	unsigned long lineNumber;    /* line number of tag */
	fpos_t        filePosition;  /* file position of line containing name */
} tokenInfo;

typedef enum eImplementation {
	IMP_DEFAULT,
	IMP_ABSTRACT,
	IMP_VIRTUAL,
	IMP_PURE_VIRTUAL,
	IMP_COUNT
} impType;

/*  Describes the statement currently undergoing analysis.
 */
typedef struct sStatementInfo {
	tagScope	scope;
	declType	declaration;    /* specifier associated with TOKEN_SPEC */
	boolean		gotName;        /* was a name parsed yet? */
	boolean		haveQualifyingName;  /* do we have a name we are considering? */
	boolean		gotParenName;   /* was a name inside parentheses parsed yet? */
	boolean		gotArgs;        /* was a list of parameters parsed yet? */
	boolean		isPointer;      /* is 'name' a pointer? */
	boolean     inFunction;     /* are we inside of a function? */
	boolean		assignment;     /* have we handled an '='? */
	boolean		notVariable;    /* has a variable declaration been disqualified ? */
	impType		implementation; /* abstract or concrete implementation? */
	unsigned int tokenIndex;    /* currently active token */
	tokenInfo*	token [(int) NumTokens];
	tokenInfo*	context;        /* accumulated scope of current statement */
	tokenInfo*	blockName;      /* name of current block */
	memberInfo	member;         /* information regarding parent class/struct */
	vString*	parentClasses;  /* parent classes */
	struct sStatementInfo *parent;  /* statement we are nested within */
} statementInfo;

/*  Describes the type of tag being generated.
 */
typedef enum eTagType {
	TAG_UNDEFINED,
	TAG_CLASS,       /* class name */
	TAG_ENUM,        /* enumeration name */
	TAG_ENUMERATOR,  /* enumerator (enumeration value) */
	TAG_EVENT,       /* event */
	TAG_FIELD,       /* field (Java) */
	TAG_FUNCTION,    /* function definition */
	TAG_INTERFACE,   /* interface declaration */
	TAG_LOCAL,       /* local variable definition */
	TAG_MEMBER,      /* structure, class or interface member */
	TAG_METHOD,      /* method declaration */
	TAG_NAMESPACE,   /* namespace name */
	TAG_PACKAGE,     /* package name */
	TAG_PROGRAM,     /* program name */
	TAG_PROPERTY,    /* property name */
	TAG_PROTOTYPE,   /* function prototype or declaration */
	TAG_STRUCT,      /* structure name */
	TAG_TASK,        /* task name */
	TAG_TYPEDEF,     /* typedef name */
	TAG_UNION,       /* union name */
	TAG_VARIABLE,    /* variable definition */
	TAG_EXTERN_VAR,  /* external variable declaration */
	TAG_COUNT        /* must be last */
} tagType;

typedef struct sParenInfo {
	boolean isPointer;
	boolean isParamList;
	boolean isKnrParamList;
	boolean isNameCandidate;
	boolean invalidContents;
	boolean nestedArgs;
	unsigned int parameterCount;
} parenInfo;

/*
*   DATA DEFINITIONS
*/

jmp_buf Exception;

/* Note: Signature moved to Rust (SIGNATURE in vstring.rs) */
/* Access via rs_getSignature(), or use these macros for convenience: */
#define Signature rs_getSignature()
#define SignatureLength() rs_signatureLength()
#define SignatureValue() rs_signatureValue()
/* Note: CollectingSignature moved to Rust (COLLECTING_SIGNATURE in cparser.rs) */

/* Rust FFI declarations for cparser.rs */
extern void rs_setLangC(langType lang);
extern void rs_setLangCpp(langType lang);
extern void rs_setLangCsharp(langType lang);
extern void rs_setLangJava(langType lang);
extern void rs_setLangVera(langType lang);
extern langType rs_getLangC(void);
extern langType rs_getLangCpp(void);
extern langType rs_getLangCsharp(void);
extern langType rs_getLangJava(void);
extern langType rs_getLangVera(void);
/* Keyword table now in Rust - these build the hash for each language */
extern void rs_buildKeywordHashC(langType lang);
extern void rs_buildKeywordHashCpp(langType lang);
extern void rs_buildKeywordHashCsharp(langType lang);
extern void rs_buildKeywordHashJava(langType lang);
extern void rs_buildKeywordHashVera(langType lang);

/* FFI declarations for Kinds arrays (now Rust-owned) */
extern kindOption* rs_getCKinds(void);
extern int rs_getCKindsCount(void);
extern kindOption* rs_getCsharpKinds(void);
extern int rs_getCsharpKindsCount(void);
extern kindOption* rs_getJavaKinds(void);
extern int rs_getJavaKindsCount(void);
extern kindOption* rs_getVeraKinds(void);
extern int rs_getVeraKindsCount(void);
extern kindOption* rs_getRustKinds(void);
extern int rs_getRustKindsCount(void);
extern int rs_isCKindDefineEnabled(void);

/* RustKinds pointer - allows direct array access via Rust-owned data */
#define RustKinds (rs_getRustKinds())

/* HeaderExtensions now in Rust - see HEADER_EXTENSIONS in options.rs */
extern const char *const *rs_getHeaderExtensions(void);
#define HeaderExtensions (rs_getHeaderExtensions())

/* License strings now in Rust - see LICENSE1/LICENSE2 in options.rs */
extern const char *rs_getLicense1(void);
extern const char *rs_getLicense2(void);
#define License1 (rs_getLicense1())
#define License2 (rs_getLicense2())

/* TagsToStdout now in Rust - see TAGS_TO_STDOUT in entry.rs */
extern int rs_getTagsToStdout(void);
extern void rs_setTagsToStdout(int value);
#define TagsToStdout (rs_getTagsToStdout())

/* filesRequired function moved to Rust - see rs_filesRequired in cparser.rs */
extern int rs_filesRequired(void);
#define filesRequired() ((boolean)rs_filesRequired())

/* Note: Lang_* variables moved to Rust (LANG_* atomics in cparser.rs) */
/* Use these macros for read access - writes must use rs_setLang*() */
#define Lang_c (rs_getLangC())
#define Lang_cpp (rs_getLangCpp())
#define Lang_csharp (rs_getLangCsharp())
#define Lang_java (rs_getLangJava())
#define Lang_vera (rs_getLangVera())
extern void rs_setCollectingSignature(int value);
extern int rs_isCollectingSignature(void);
extern int rs_isContextualKeyword(const tokenInfo *token);
extern int rs_isContextualStatement(const statementInfo *st);
extern int rs_isMember(const statementInfo *st);
extern int rs_isValidTypeSpecifier(int declaration);
extern int rs_isInheritingDeclaration(int decl);
extern const char *rs_accessString(int access);
extern const char *rs_implementationString(int imp);
extern const char *rs_scopeString(int scope);
extern const char *rs_declString(int decl);
extern const char *rs_tokenString(int token_type);
extern int rs_advanceTokenIndex(int index);
extern int rs_retardTokenIndex(int index);
extern int rs_cTagKind(int tag_type);
extern int rs_csharpTagKind(int tag_type);
extern int rs_javaTagKind(int tag_type);
extern int rs_veraTagKind(int tag_type);
extern int rs_declToTagType(int declaration);
extern const char *rs_keywordString(int keyword);
extern int rs_prevTokenIndex(int current_index, int n);
extern int rs_languageSupportsGenerics(langType lang);
extern int rs_inheritingDeclaration(int decl, langType lang);
extern const char *rs_getContextSeparator(langType lang);
extern int rs_isCOrCpp(langType lang);
extern int rs_isJava(langType lang);
extern int rs_isCsharp(langType lang);
extern int rs_isVera(langType lang);
extern int rs_memberAccessDefault(int parent_decl, langType lang);
extern const char *rs_accessFieldString(langType lang, int scope, int access);
extern void rs_setEtagsMode(void);

/* Character classification FFI - from clex.rs and read.rs */
extern int rs_isWhitespace(int c);
extern int rs_isAscii(int c);
extern int rs_isIdentStart(int c);
extern int rs_isIdent(int c);
extern int rs_isHighChar(int c);

/* String checking FFI - from cparser.rs */
extern int rs_isFalse(const char *parameter);
extern int rs_isTrue(const char *parameter);

/* Tag type FFI - from cparser.rs */
extern int rs_tagLetter(int tag_type, langType lang);
extern const char *rs_tagName(int tag_type, langType lang);
extern int rs_includeTag(int tag_type, int is_file_scope, langType lang);

/* Option global FFI - from cparser.rs (accessing C globals via Rust) */
extern int rs_getOptionIncludeFileScope(void);
extern int rs_getOptionIncludeQualifiedTags(void);
extern int rs_getOptionExtFieldInheritance(void);
extern int rs_getOptionExtFieldAccess(void);
extern int rs_getOptionExtFieldImplementation(void);
extern int rs_getOptionExtFieldTypeRef(void);
extern int rs_getOptionExtFieldFileScope(void);
extern unsigned int rs_getOptionTagFileFormat(void);
extern int rs_getOptionEtags(void);
extern int rs_getOptionXref(void);
extern int rs_getOptionSorted(void);
extern int rs_getOptionBackward(void);

/* AnonymousID FFI - moved to Rust (ANONYMOUS_ID in cparser.rs) */
extern int rs_incrementAndGetAnonymousID(void);
extern int rs_getNextAnonymousID(void);
extern void rs_resetAnonymousID(void);
extern int rs_getAnonymousID(void);

/* Kind enums and arrays moved to Rust - see C_KINDS, CSHARP_KINDS, etc. in parse.rs
 * The Rust implementation provides:
 * - rs_getCKinds(), rs_getCKindsCount(), rs_isCKindDefineEnabled()
 * - rs_getCsharpKinds(), rs_getCsharpKindsCount()
 * - rs_getJavaKinds(), rs_getJavaKindsCount()
 * - rs_getVeraKinds(), rs_getVeraKindsCount()
 */
/*
*   FUNCTION PROTOTYPES
*/
void createTags (const unsigned int nestLevel, statementInfo *const parent);

/*
*   FUNCTION DEFINITIONS
*/

/* includingDefineTags - now implemented in Rust (cparser.rs) */
extern boolean includingDefineTags(void);

/*
*   Token management
*/

/* Rust FFI declarations for token functions */
extern void rs_initToken (tokenInfo* token);
extern tokenInfo *rs_prevToken (const statementInfo *st, unsigned int n);
extern void rs_setToken (statementInfo *st, int type);
extern void rs_advanceToken (statementInfo *st);
extern void rs_retardToken (statementInfo *st);

/* advanceToken - now a macro delegating to Rust */
#define advanceToken(st) rs_advanceToken((statementInfo *)(st))

/* retardToken - now a macro delegating to Rust */
#define retardToken(st) rs_retardToken((statementInfo *)(st))

/* newToken - now a macro delegating to Rust */
extern tokenInfo *rs_newToken(void);
#define newToken() rs_newToken()

/* deleteToken - now a macro delegating to Rust */
extern void rs_deleteToken(tokenInfo *token);
#define deleteToken(token) rs_deleteToken((tokenInfo *)(token))

/*
*   Debugging functions
*/

#ifdef DEBUG

#define boolString(c)   ((c) ? "TRUE" : "FALSE")

void __unused__ pt (tokenInfo *const token)
{
	if (isType (token, TOKEN_NAME))
		printf ("type: %-12s: %-13s   line: %lu\n",
			rs_tokenString ((int) token->type), vStringValue (token->name),
			token->lineNumber);
	else if (isType (token, TOKEN_KEYWORD))
		printf ("type: %-12s: %-13s   line: %lu\n",
			rs_tokenString ((int) token->type), rs_keywordString ((int) token->keyword),
			token->lineNumber);
	else
		printf ("type: %-12s                  line: %lu\n",
			rs_tokenString ((int) token->type), token->lineNumber);
}

void __unused__ ps (statementInfo *const st)
{
	unsigned int i;
	printf ("scope: %s   decl: %s   gotName: %s   gotParenName: %s\n",
		rs_scopeString ((int) st->scope), rs_declString ((int) st->declaration),
		boolString (st->gotName), boolString (st->gotParenName));
	printf ("haveQualifyingName: %s\n", boolString (st->haveQualifyingName));
	printf ("access: %s   default: %s\n", rs_accessString ((int) st->member.access),
		rs_accessString ((int) st->member.accessDefault));
	printf ("token  : ");
	pt (activeToken (st));
	for (i = 1  ;  i < (unsigned int) NumTokens  ;  ++i)
	{
		printf ("prev %u : ", i);
		pt (rs_prevToken (st, i));
	}
	printf ("context: ");
	pt (st->context);
}

#endif

/*
*   Statement management
*/

/* initMemberInfo - now a macro delegating to Rust */
extern void rs_initMemberInfo(statementInfo *st);
#define initMemberInfo(st) rs_initMemberInfo((statementInfo *)(st))

/* reinitStatement - now a macro delegating to Rust */
extern void rs_reinitStatement(statementInfo *st, int partial);
#define reinitStatement(st, partial) rs_reinitStatement((statementInfo *)(st), (int)(partial))

/* initStatement - now a macro delegating to Rust */
extern void rs_initStatement(statementInfo *st, statementInfo *parent);
#define initStatement(st, parent) rs_initStatement((statementInfo *)(st), (statementInfo *)(parent))

/*
*   Tag generation functions - now implemented in Rust (token.rs)
*/
/* addOtherFields - now implemented in Rust */
extern void addOtherFields(tagEntryInfo *tag, int type, const statementInfo *st,
                           vString *scope, vString *typeRef);

/* findScopeHierarchy - now implemented in Rust */
extern void findScopeHierarchy(vString *string, const statementInfo *st);

/* makeExtraTagEntry - now implemented in Rust */
extern void makeExtraTagEntry(int type, tagEntryInfo *e, vString *scope);

/* makeTag - now a macro delegating to Rust */
extern void rs_makeTag(const tokenInfo *token, const statementInfo *st,
                       int isFileScope, int type);
#define makeTag(token, st, isFileScope, type) \
    rs_makeTag((const tokenInfo *)(token), (const statementInfo *)(st), \
               (int)(isFileScope), (int)(type))

/* qualifyEnumeratorTag - now a macro delegating to Rust */
extern void rs_qualifyEnumeratorTag(const statementInfo *st, const tokenInfo *nameToken);
#define qualifyEnumeratorTag(st, nameToken) rs_qualifyEnumeratorTag((const statementInfo *)(st), (const tokenInfo *)(nameToken))

/* qualifyFunctionTag - now a macro delegating to Rust */
extern void rs_qualifyFunctionTag(const statementInfo *st, const tokenInfo *nameToken);
#define qualifyFunctionTag(st, nameToken) rs_qualifyFunctionTag((const statementInfo *)(st), (const tokenInfo *)(nameToken))

/* qualifyFunctionDeclTag - now a macro delegating to Rust */
extern void rs_qualifyFunctionDeclTag(const statementInfo *st, const tokenInfo *nameToken);
#define qualifyFunctionDeclTag(st, nameToken) rs_qualifyFunctionDeclTag((const statementInfo *)(st), (const tokenInfo *)(nameToken))

/* qualifyCompoundTag - now a macro delegating to Rust */
extern void rs_qualifyCompoundTag(const statementInfo *st, const tokenInfo *nameToken);
#define qualifyCompoundTag(st, nameToken) rs_qualifyCompoundTag((const statementInfo *)(st), (const tokenInfo *)(nameToken))

/* qualifyBlockTag - now a macro delegating to Rust */
extern void rs_qualifyBlockTag(statementInfo *st, const tokenInfo *nameToken);
#define qualifyBlockTag(st, nameToken) rs_qualifyBlockTag((statementInfo *)(st), (const tokenInfo *)(nameToken))

/* qualifyVariableTag - now a macro delegating to Rust */
extern void rs_qualifyVariableTag(const statementInfo *st, const tokenInfo *nameToken);
#define qualifyVariableTag(st, nameToken) rs_qualifyVariableTag((const statementInfo *)(st), (const tokenInfo *)(nameToken))

/* insideBrackets - now managed in Rust (cparser.rs) */
extern int rs_getInsideBrackets(void);
extern void rs_setInsideBrackets(int value);
extern int rs_incInsideBrackets(void);
extern int rs_decInsideBrackets(void);

 void parseIdentifier (statementInfo *const st, const int c);
/*
*   Parsing functions
*/

/* skipToOneOf - now a macro delegating to Rust */
extern int rs_skipToOneOf(statementInfo *st, const char *chars);
#define skipToOneOf(st, chars) rs_skipToOneOf((statementInfo *)(st), (const char *)(chars))

/* skipToNonWhite - now a macro delegating to Rust */
extern int rs_skipToNonWhite(void);
#define skipToNonWhite() rs_skipToNonWhite()

/* skipToFormattedBraceMatch - now a macro delegating to Rust */
extern void rs_skipToFormattedBraceMatch(void);
#define skipToFormattedBraceMatch() rs_skipToFormattedBraceMatch()

/* parseIdentifier - now a macro delegating to Rust */
extern void rs_parseIdentifier(statementInfo *st, int c);
#define parseIdentifier(st, c) rs_parseIdentifier((statementInfo *)(st), (int)(c))

/* skipToMatch - now a macro delegating to Rust */
extern void rs_skipToMatch(statementInfo *st, const char *pair);
#define skipToMatch(st, pair) rs_skipToMatch((statementInfo *)(st), (const char *)(pair))

/* skipParens - now a macro delegating to Rust */
extern void rs_skipParens(statementInfo *st);
#define skipParens(st) rs_skipParens((statementInfo *)(st))

/* skipBraces - now a macro delegating to Rust */
extern void rs_skipBraces(statementInfo *st);
#define skipBraces(st) rs_skipBraces((statementInfo *)(st))

/* analyzeIdentifier - now a macro delegating to Rust */
extern void rs_analyzeIdentifier(statementInfo *st, tokenInfo *token);
#define analyzeIdentifier(st, token) rs_analyzeIdentifier((statementInfo *)(st), (tokenInfo *)(token))

/* readIdentifier - now a macro delegating to Rust */
extern void rs_readIdentifier(statementInfo *st, tokenInfo *token, int firstChar);
#define readIdentifier(st, token, firstChar) rs_readIdentifier((statementInfo *)(st), (tokenInfo *)(token), (int)(firstChar))

/* readPackageName - now a macro delegating to Rust */
extern void rs_readPackageName(tokenInfo *token, int firstChar);
#define readPackageName(token, firstChar) rs_readPackageName((tokenInfo *)(token), (int)(firstChar))

/* readPackageOrNamespace - now a macro delegating to Rust */
extern void rs_readPackageOrNamespace(statementInfo *st, int declaration);
#define readPackageOrNamespace(st, declaration) rs_readPackageOrNamespace((statementInfo *)(st), (int)(declaration))

/* processName - now a macro delegating to Rust */
extern void rs_processName(statementInfo *st);
#define processName(st) do { \
	Assert (isType (activeToken (st), TOKEN_NAME)); \
	rs_processName((statementInfo *)(st)); \
} while(0)

/* readOperator - now a macro delegating to Rust */
extern void rs_readOperator(statementInfo *st);
#define readOperator(st) rs_readOperator((statementInfo *)(st))

/* Token copy - now a macro delegating to Rust */
extern void rs_copyToken(tokenInfo *dest, const tokenInfo *src);
#define copyToken(dest, src) rs_copyToken((tokenInfo *)(dest), (const tokenInfo *)(src))

/* setAccess - now a macro delegating to Rust */
extern void rs_setAccess(statementInfo *st, int access);
#define setAccess(st, access) rs_setAccess((statementInfo *)(st), (int)(access))

/* discardTypeList - now a macro delegating to Rust */
extern void rs_discardTypeList(statementInfo *st, tokenInfo *token);
#define discardTypeList(st, token) rs_discardTypeList((statementInfo *)(st), (tokenInfo *)(token))

/* addParentClass - now a macro delegating to Rust */
extern void rs_addParentClass(statementInfo *st, const tokenInfo *token);
#define addParentClass(st, token) rs_addParentClass((statementInfo *)(st), (const tokenInfo *)(token))

/* readParents - now a macro delegating to Rust */
extern void rs_readParents(statementInfo *st, int qualifier);
#define readParents(st, qualifier) rs_readParents((statementInfo *)(st), (int)(qualifier))

/* skipStatement - now a macro delegating to Rust */
extern void rs_skipStatement(statementInfo *st);
#define skipStatement(st) rs_skipStatement((statementInfo *)(st))

/* processInterface - now a macro delegating to Rust */
extern void rs_processInterface(statementInfo *st);
#define processInterface(st) rs_processInterface((statementInfo *)(st))

/* processToken - now a macro delegating to Rust */
extern void rs_processToken(tokenInfo *token, statementInfo *st);
#define processToken(token, st) rs_processToken((tokenInfo *)(token), (statementInfo *)(st))

/*
*   Parenthesis handling functions
*/

/* restartStatement - now a macro delegating to Rust */
extern void rs_restartStatement(statementInfo *st);
#define restartStatement(st) rs_restartStatement((statementInfo *)(st))

/* skipMemIntializerList - now a macro delegating to Rust
 * Skips over a mem-initializer-list of a ctor-initializer
 */
extern void rs_skipMemIntializerList(statementInfo *st, tokenInfo *token);
#define skipMemIntializerList(st, token) rs_skipMemIntializerList((statementInfo *)(st), (tokenInfo *)(token))

/* skipMacro - now a macro delegating to Rust */
extern void rs_skipMacro(statementInfo *st);
#define skipMacro(st) rs_skipMacro((statementInfo *)(st))

/* skipPostArgumentStuff - now a macro delegating to Rust */
extern int rs_skipPostArgumentStuff(statementInfo *st, parenInfo *info);
#define skipPostArgumentStuff(st, info) rs_skipPostArgumentStuff((statementInfo *)(st), (parenInfo *)(info))

/* skipJavaThrows - now a macro delegating to Rust */
extern void rs_skipJavaThrows(statementInfo *st);
#define skipJavaThrows(st) rs_skipJavaThrows((statementInfo *)(st))

/* analyzePostParens - now a macro delegating to Rust */
extern void rs_analyzePostParens(statementInfo *st, parenInfo *info);
#define analyzePostParens(st, info) rs_analyzePostParens((statementInfo *)(st), (parenInfo *)(info))

/* processAngleBracket - now a macro delegating to Rust */
extern void rs_processAngleBracket(statementInfo *st);
#define processAngleBracket(st) rs_processAngleBracket((statementInfo *)(st))

/* parseJavaAnnotation - now a macro delegating to Rust */
extern void rs_parseJavaAnnotation(statementInfo *st);
#define parseJavaAnnotation(st) rs_parseJavaAnnotation((statementInfo *)(st))

/* parseParens - now a macro delegating to Rust */
extern int rs_parseParens(statementInfo *st, parenInfo *info);
#define parseParens(st, info) rs_parseParens((statementInfo *)(st), (parenInfo *)(info))

/* initParenInfo - now a macro delegating to Rust */
extern void rs_initParenInfo(parenInfo *info, langType lang);
#define initParenInfo(info) rs_initParenInfo((parenInfo *)(info), getSourceLanguage())

/* parens_level - now managed in Rust (cparser.rs) */
extern int rs_getParensLevel(void);
extern void rs_setParensLevel(int value);
extern int rs_incParensLevel(void);
extern int rs_decParensLevel(void);

/* analyzeParens - now a macro delegating to Rust */
extern void rs_analyzeParens(statementInfo *st);
#define analyzeParens(st) rs_analyzeParens((statementInfo *)(st))

/*
*   Token parsing functions
*/

/* addContext - now a macro delegating to Rust */
extern void rs_addContext(statementInfo *st, const tokenInfo *token);
#define addContext(st, token) rs_addContext((statementInfo *)(st), (const tokenInfo *)(token))

/* processColon - now a macro delegating to Rust */
extern void rs_processColon(statementInfo *st);
#define processColon(st) rs_processColon((statementInfo *)(st))

/* skipInitializer - now a macro delegating to Rust */
extern int rs_skipInitializer(statementInfo *st);
#define skipInitializer(st) rs_skipInitializer((statementInfo *)(st))

/* processInitializer - now a macro delegating to Rust */
extern void rs_processInitializer(statementInfo *st);
#define processInitializer(st) rs_processInitializer((statementInfo *)(st))

/* parseIdentifier - now a macro delegating to Rust (see rs_parseIdentifier in token.rs)
 * Original implementation commented out:
 * void parseIdentifier (statementInfo *const st, const int c)
 * {
 *     tokenInfo *const token = activeToken (st);
 *     readIdentifier (st, token, c);
 *     if (! isType (token, TOKEN_NONE))
 *         processToken (token, st);
 * }
 */

/* parseGeneralToken - now a macro delegating to Rust */
extern void rs_parseGeneralToken(statementInfo *st, int c);
#define parseGeneralToken(st, c) rs_parseGeneralToken((statementInfo *)(st), (int)(c))

/* nextToken - now a macro delegating to Rust */
extern void rs_nextToken(statementInfo *st);
#define nextToken(st) rs_nextToken((statementInfo *)(st))

/*
*   Scanning support functions
*/

/* CurrentStatement - now managed in Rust (cparser.rs) */
extern statementInfo* rs_getCurrentStatement(void);
extern void rs_setCurrentStatement(statementInfo *st);

/* newStatement - now implemented in Rust (token.rs) */
extern statementInfo *newStatement(statementInfo *parent);

/* deleteStatement - now a macro delegating to Rust */
extern void rs_deleteStatement(void);
#define deleteStatement() rs_deleteStatement()

/* deleteAllStatements - now a macro delegating to Rust */
extern void rs_deleteAllStatements(void);
#define deleteAllStatements() rs_deleteAllStatements()

/* isStatementEnd - now a macro delegating to Rust */
extern int rs_isStatementEndSt(const statementInfo *st);
#define isStatementEnd(st) ((boolean) rs_isStatementEndSt((const statementInfo *)(st)))

/* checkStatementEnd - now implemented in Rust (token.rs) */
extern void checkStatementEnd(statementInfo *st, unsigned int nestLevel);

/* nest - now implemented in Rust (token.rs) */
extern void nest(statementInfo *st, unsigned int nestLevel);

/* tagCheck - now implemented in Rust (token.rs) */
extern void tagCheck(statementInfo *st);

/* createTags - now implemented in Rust (token.rs) */
extern void createTags(unsigned int nestLevel, statementInfo *parent);

/* findCTags - now implemented in Rust (token.rs) */
extern int findCTags(unsigned int passCount);

/* buildKeywordHash moved to Rust - see rs_buildKeywordHash* in keyword.rs */

/* Parser initialization functions moved to Rust - see cparser.rs */
extern void initializeCParser(langType language);
extern void initializeCppParser(langType language);
extern void initializeCsharpParser(langType language);
extern void initializeJavaParser(langType language);
extern void initializeVeraParser(langType language);

extern parserDefinition* CParser (void)
{
	static const char *const extensions [] = { "c", NULL };
	parserDefinition* def = parserNew ("C");
	def->kinds      = rs_getCKinds();
	def->kindCount  = rs_getCKindsCount();
	def->extensions = extensions;
	def->parser2    = findCTags;
	def->initialize = initializeCParser;
	return def;
}

extern parserDefinition* CppParser (void)
{
	static const char *const extensions [] = {
		"c++", "cc", "cp", "cpp", "cxx", "h", "h++", "hh", "hp", "hpp", "hxx",
#ifndef CASE_INSENSITIVE_FILENAMES
		"C", "H",
#endif
		NULL
	};
	parserDefinition* def = parserNew ("C++");
	def->kinds      = rs_getCKinds();
	def->kindCount  = rs_getCKindsCount();
	def->extensions = extensions;
	def->parser2    = findCTags;
	def->initialize = initializeCppParser;
	return def;
}

extern parserDefinition* CsharpParser (void)
{
	static const char *const extensions [] = { "cs", NULL };
	parserDefinition* def = parserNew ("C#");
	def->kinds      = rs_getCsharpKinds();
	def->kindCount  = rs_getCsharpKindsCount();
	def->extensions = extensions;
	def->parser2    = findCTags;
	def->initialize = initializeCsharpParser;
	return def;
}

extern parserDefinition* JavaParser (void)
{
	static const char *const extensions [] = { "java", NULL };
	parserDefinition* def = parserNew ("Java");
	def->kinds      = rs_getJavaKinds();
	def->kindCount  = rs_getJavaKindsCount();
	def->extensions = extensions;
	def->parser2    = findCTags;
	def->initialize = initializeJavaParser;
	return def;
}

extern parserDefinition* VeraParser (void)
{
	static const char *const extensions [] = { "vr", "vri", "vrh", NULL };
	parserDefinition* def = parserNew ("Vera");
	def->kinds      = rs_getVeraKinds();
	def->kindCount  = rs_getVeraKindsCount();
	def->extensions = extensions;
	def->parser2    = findCTags;
	def->initialize = initializeVeraParser;
	return def;
}

/* vi:set tabstop=4 shiftwidth=4 noexpandtab: */

/* Reset static state for multi-file processing in same process */
/* resetCParserState - now a macro delegating to Rust */
extern void rs_resetCParserState(void);
#define resetCParserState() rs_resetCParserState()

/* Rust parser moved to rust.c - uses RustParser() */
/*
*   $Id$
*
*   Copyright (c) 1999-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for reading command line arguments.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "args.h"
#include "debug.h"
#include "routines.h"

/*
*   FUNCTION DEFINITIONS
*/

/* Helper functions nextStringArg, nextStringLine, nextString, nextFileArg,
 * nextFileLine, nextFileString, argNewFromString, argNewFromArgv,
 * argNewFromFile, argNewFromLineFile, argItem removed - now handled by
 * Rust implementation in src/ctags_rs/args.rs */


/* vi:set tabstop=4 shiftwidth=4: */
/*
*   $Id$
*
*   Copyright (c) 1996-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains debugging functions.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <ctype.h>
#include <stdarg.h>

#include "debug.h"
#include "options.h"
#include "read.h"
#include "../pu_type.h"  /* Shared C-Rust enum for PuType */

/*
*   FUNCTION DEFINITIONS
*/

#ifdef DEBUG

extern void lineBreak (void) {}  /* provides a line-specified break point */

extern void debugPrintf (
		const enum eDebugLevels level, const char *const format, ... )
{
	va_list ap;

	va_start (ap, format);
	if (debug (level))
		vprintf (format, ap);
	fflush (stdout);
	va_end (ap);
}

/*
 * C-side character buffer for precc - eliminates ~2M FFI calls per file
 * by buffering characters and passing the buffer to Rust in batches
 */
#ifdef PRECC_FAST_PATH
/* Buffer accessible from header macro - NOT static */
char precc_debug_buffer[PRECC_DEBUG_BUFFER_SIZE];
size_t precc_debug_buffer_len = 0;

/* Get buffer pointer for Rust to read directly */
const char* precc_get_buffer(void) {
	return precc_debug_buffer;
}

/* Get current buffer length */
size_t precc_get_buffer_len(void) {
	return precc_debug_buffer_len;
}

/* Clear the buffer (called from Rust after reading) */
void precc_clear_buffer(void) {
	precc_debug_buffer_len = 0;
}

/* Rust FFI declarations for literal buffer */
extern const char* rs_getLiteralBuffer(void);
extern size_t rs_getLiteralBufferLen(void);

/* Handle special symbols (STRING_SYMBOL, CHAR_SYMBOL) - rare path only */
/* Now uses the actual literal content stored by Rust during tokenization */
void debugPutc_special(int c) {
	if (c == STRING_SYMBOL || c == CHAR_SYMBOL) {
		/* Get the actual literal content from Rust */
		const char* literal = rs_getLiteralBuffer();
		size_t literal_len = rs_getLiteralBufferLen();
		if (literal != NULL && literal_len > 0) {
			if (precc_debug_buffer_len + literal_len <= PRECC_DEBUG_BUFFER_SIZE) {
				for (size_t i = 0; i < literal_len; i++) {
					precc_debug_buffer[precc_debug_buffer_len++] = literal[i];
				}
			}
		}
	}
	/* else: ignore other high-bit chars */
}
#else
void precc_putchar(char c);

extern void debugPutc (const int level, const int c)
{
	if (debug (level)  &&  c != EOF)
	{
		     if (c == STRING_SYMBOL)  printf ("\"string\"");
		else if (c == CHAR_SYMBOL)    printf ("'c'");
		else                          precc_putchar (c);

		fflush (stdout);
	}
}
#endif

extern void debugParseNest (const boolean increase, const unsigned int level)
{
	debugPrintf (DEBUG_PARSE, "<*%snesting:%d*>", increase ? "++" : "--", level);
}

extern void debugCppNest (const boolean begin, const unsigned int level)
{
	debugPrintf (DEBUG_CPP, "<*cpp:%s level %d*>", begin ? "begin":"end", level);
}

extern void debugCppIgnore (const boolean ignore)
{
	debugPrintf (DEBUG_CPP, "<*cpp:%s ignore*>", ignore ? "begin":"end");
}

/* debugEntry - now implemented in Rust (entry.rs) */
extern void debugEntry (const tagEntryInfo *const tag);

#endif

/* vi:set tabstop=4 shiftwidth=4: */
/*
*   $Id$
*
*   Copyright (c) 1996-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for creating tag entries.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include <stdbool.h>

#include <string.h>
#include <ctype.h>        /* to define isspace () */
#include <errno.h>

#if defined (HAVE_SYS_TYPES_H)
# include <sys/types.h>	  /* to declare off_t on some hosts */
#endif
#if defined (HAVE_TYPES_H)
# include <types.h>       /* to declare off_t on some hosts */
#endif
#if defined (HAVE_UNISTD_H)
# include <unistd.h>      /* to declare close (), ftruncate (), truncate () */
#endif

/*  These header files provide for the functions necessary to do file
 *  truncation.
 */
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif
#ifdef HAVE_IO_H
# include <io.h>
#endif

#include "debug.h"
#include "ctags.h"
#include "entry.h"
#include "main.h"
#include "options.h"
#include "read.h"
#include "routines.h"
#include "sort.h"
#include "strlist.h"

/*
*   MACROS
*/
#define PSEUDO_TAG_PREFIX       "!_"

#define includeExtensionFlags()         (Option.tagFileFormat > 1)

/*
 *  Portability defines
 */
#if !defined(HAVE_TRUNCATE) && !defined(HAVE_FTRUNCATE) && !defined(HAVE_CHSIZE)
# define USE_REPLACEMENT_TRUNCATE
#endif

/*  Hack for rediculous practice of Microsoft Visual C++.
 */
#if defined (WIN32) && defined (_MSC_VER)
# define chsize         _chsize
# define open           _open
# define close          _close
# define O_RDWR         _O_RDWR
#endif

/*
*   DATA DEFINITIONS
*/

/* TagFile is now owned by Rust - see entry.h for macro definition */

/* TagsToStdout moved to Rust - see TAGS_TO_STDOUT in entry.rs
 * Access via rs_getTagsToStdout()/rs_setTagsToStdout() or TagsToStdout macro
 */

/*
*   FUNCTION PROTOTYPES
*/
#ifdef NEED_PROTO_TRUNCATE
extern int truncate (const char *path, off_t length);
#endif

#ifdef NEED_PROTO_FTRUNCATE
extern int ftruncate (int fd, off_t length);
#endif

/*
*   FUNCTION DEFINITIONS
*/

/* freeTagFileResources - now implemented directly in Rust (entry.rs) */
extern void rs_freeTagFileResourcesDirect (void);

/* tagFileName - now implemented in Rust (entry.rs) */
extern const char *tagFileName (void);

/*
*   Pseudo tag support - now implemented in Rust (entry.rs)
*/
extern void rememberMaxLengths(size_t nameLength, size_t lineLength);
extern void writePseudoTag(const char *tagName, const char *fileName, const char *pattern);
extern void addPseudoTags(void);

/* updateSortedFlag - now implemented in Rust (entry.rs) */
extern void updateSortedFlag(const char *line, FILE *fp, fpos_t startOfLine);

/* updatePseudoTags - now implemented in Rust (entry.rs) */
extern unsigned long updatePseudoTags(FILE *fp);

/*
 *  Tag file management
 */

/* isValidTagAddress - now implemented in Rust (entry.rs) */
extern bool isValidTagAddress(const char *excmd);

/* isCtagsLine - now implemented in Rust (entry.rs) */
extern bool isCtagsLine(const char *line);

/* isEtagsLine - now implemented in Rust (entry.rs) */
extern bool isEtagsLine(const char *line);

/* isTagFile - now implemented in Rust (entry.rs) */
extern bool isTagFile(const char *filename);

/* copyBytes - now implemented in Rust (entry.rs) */
extern void copyBytes(FILE *fromFp, FILE *toFp, long size);

/* copyFile - now implemented in Rust (entry.rs) */
extern void copyFile(const char *from, const char *to, long size);

extern void openTagFile (void)
{
	setDefaultTagFileName ();
	rs_setTagsToStdout (isDestinationStdout ());

	if (TagFile.vLine == NULL)
		TagFile.vLine = rs_vStringNew ();

#if DEBUG3
	/*  Open the tags file.
	 */
	if (TagsToStdout)
		TagFile.fp = tempFile ("w", &TagFile.name);
	else
	{
		bool fileExists;

		setDefaultTagFileName ();
		TagFile.name = eStrdup (Option.tagFileName);
		fileExists = doesFileExist (TagFile.name);
		if (fileExists  &&  ! isTagFile (TagFile.name))
			error (FATAL,
			  "\"%s\" doesn't look like a tag file; I refuse to overwrite it.",
				  TagFile.name);

		if (Option.etags)
		{
			if (Option.append  &&  fileExists)
				TagFile.fp = fopen (TagFile.name, "a+b");
			else
				TagFile.fp = fopen (TagFile.name, "w+b");
		}
		else
		{
			if (Option.append  &&  fileExists)
			{
				TagFile.fp = fopen (TagFile.name, "r+");
				if (TagFile.fp != NULL)
				{
					TagFile.numTags.prev = updatePseudoTags (TagFile.fp);
					fclose (TagFile.fp);
					TagFile.fp = fopen (TagFile.name, "a+");
				}
			}
			else
			{
				TagFile.fp = fopen (TagFile.name, "w");
				if (TagFile.fp != NULL)
					addPseudoTags ();
			}
		}
		if (TagFile.fp == NULL)
		{
			error (FATAL | PERROR, "cannot open tag file");
			exit (1);
		}
	}
	if (TagsToStdout)
		TagFile.directory = eStrdup (CurrentDirectory);
	else
		TagFile.directory = absoluteDirname (TagFile.name);
#endif
}

#ifdef USE_REPLACEMENT_TRUNCATE

/*  Replacement for missing library function.
 */
int replacementTruncate (const char *const name, const long size)
{
	char *tempName = NULL;
	FILE *fp = tempFile ("w", &tempName);
	fclose (fp);
	copyFile (name, tempName, size);
	copyFile (tempName, name, WHOLE_FILE);
	remove (tempName);
	eFree (tempName);

	return 0;
}

#endif

void sortTagFile (void)
{
	if (TagFile.numTags.added > 0L)
	{
		if (Option.sorted != SO_UNSORTED)
		{
			verbose ("sorting tag file\n");
#ifdef EXTERNAL_SORT
			externalSortTags (TagsToStdout);
#else
			internalSortTags (TagsToStdout);
#endif
		}
		else if (TagsToStdout)
			catFile (tagFileName ());
	}
	if (TagsToStdout)
		remove (tagFileName ());  /* remove temporary file */
}

void resizeTagFile (const long newSize)
{
	int result;

#ifdef USE_REPLACEMENT_TRUNCATE
	result = replacementTruncate (TagFile.name, newSize);
#else
# ifdef HAVE_TRUNCATE
	result = truncate (TagFile.name, (off_t) newSize);
# else
	const int fd = open (TagFile.name, O_RDWR);

	if (fd == -1)
		result = -1;
	else
	{
#  ifdef HAVE_FTRUNCATE
		result = ftruncate (fd, (off_t) newSize);
#  else
#   ifdef HAVE_CHSIZE
		result = chsize (fd, newSize);
#   endif
#  endif
		close (fd);
	}
# endif
#endif
	if (result == -1)
		fprintf (errout, "Cannot shorten tag file: errno = %d\n", errno);
}

void writeEtagsIncludes (FILE *const fp)
{
	if (Option.etagsInclude)
	{
		unsigned int i;
		for (i = 0  ;  i < rs_stringListCount (Option.etagsInclude)  ;  ++i)
		{
			vString *item = rs_stringListItem (Option.etagsInclude, i);
			fprintf (fp, "\f\n%s,include\n", vStringValue (item));
		}
	}
}

extern void closeTagFile (const bool resize)
{
	long desiredSize, size;

	if (Option.etags)
		writeEtagsIncludes (TagFile.fp);
	desiredSize = ftell (TagFile.fp);
	fseek (TagFile.fp, 0L, SEEK_END);
	size = ftell (TagFile.fp);
	fclose (TagFile.fp);
	if (resize  &&  desiredSize < size)
	{
		DebugStatement (
			debugPrintf (DEBUG_STATUS, "shrinking %s from %ld to %ld bytes\n",
				TagFile.name, size, desiredSize); )
		resizeTagFile (desiredSize);
	}
	sortTagFile ();
	eFree (TagFile.name);
	TagFile.name = NULL;
}

extern void beginEtagsFile (void)
{
	TagFile.etags.fp = tempFile ("w+b", &TagFile.etags.name);
	TagFile.etags.byteCount = 0;
}

extern void endEtagsFile (const char *const name)
{
	const char *line;

	fprintf (TagFile.fp, "\f\n%s,%ld\n", name, (long) TagFile.etags.byteCount);
	if (TagFile.etags.fp != NULL)
	{
		rewind (TagFile.etags.fp);
		while ((line = readLine (TagFile.vLine, TagFile.etags.fp)) != NULL)
			fputs (line, TagFile.fp);
		fclose (TagFile.etags.fp);
		remove (TagFile.etags.name);
		eFree (TagFile.etags.name);
		TagFile.etags.fp = NULL;
		TagFile.etags.name = NULL;
	}
}

/*
 *  Tag entry management
 */

/* writeSourceLine - now implemented in Rust (entry.rs) */
extern size_t writeSourceLine(FILE *fp, const char *line);

/* writeCompactSourceLine - now implemented in Rust (entry.rs) */
extern size_t writeCompactSourceLine(FILE *fp, const char *line);

int writeXrefEntry (const tagEntryInfo *const tag)
{
	const char *const line =
			readSourceLine (TagFile.vLine, tag->filePosition, NULL);
	int length;

	if (Option.tagFileFormat == 1)
		length = fprintf (TagFile.fp, "%-16s %4lu %-16s ", tag->name,
				tag->lineNumber, tag->sourceFileName);
	else
		length = fprintf (TagFile.fp, "%-16s %-10s %4lu %-16s ", tag->name,
				tag->kindName, tag->lineNumber, tag->sourceFileName);

	length += writeCompactSourceLine (TagFile.fp, line);
	putc (NEWLINE, TagFile.fp);
	++length;

	return length;
}

/* truncateTagLine - now implemented in Rust (entry.rs) */
extern void truncateTagLine(char *line, const char *token, bool discardNewline);

int writeEtagsEntry (const tagEntryInfo *const tag)
{
	int length;

	if (tag->isFileEntry)
		length = fprintf (TagFile.etags.fp, "\177%s\001%lu,0\n",
				tag->name, tag->lineNumber);
	else
	{
		long seekValue;
		char *const line =
				readSourceLine (TagFile.vLine, tag->filePosition, &seekValue);

		if (tag->truncateLine)
			truncateTagLine (line, tag->name, TRUE);
		else
			line [strlen (line) - 1] = '\0';

		length = fprintf (TagFile.etags.fp, "%s\177%s\001%lu,%ld\n", line,
				tag->name, tag->lineNumber, seekValue);
	}
	TagFile.etags.byteCount += length;

	return length;
}

int addExtensionFields (const tagEntryInfo *const tag)
{
	const char* const kindKey = Option.extensionFields.kindKey ? "kind:" : "";
	bool first = TRUE;
	const char* separator = ";\"";
	const char* const empty = "";
	int length = 0;
/* "sep" returns a value only the first time it is evaluated */
#define sep (first ? (first = FALSE, separator) : empty)

	if (tag->kindName != NULL && (Option.extensionFields.kindLong  ||
		 (Option.extensionFields.kind  && tag->kind == '\0')))
		length += fprintf (TagFile.fp,"%s\t%s%s", sep, kindKey, tag->kindName);
	else if (tag->kind != '\0'  && (Option.extensionFields.kind  ||
			(Option.extensionFields.kindLong  &&  tag->kindName == NULL)))
		length += fprintf (TagFile.fp, "%s\t%s%c", sep, kindKey, tag->kind);

	if (Option.extensionFields.lineNumber)
		length += fprintf (TagFile.fp, "%s\tline:%ld", sep, tag->lineNumber);

	if (Option.extensionFields.language  &&  tag->language != NULL)
		length += fprintf (TagFile.fp, "%s\tlanguage:%s", sep, tag->language);

	if (Option.extensionFields.scope  &&
			tag->extensionFields.scope [0] != NULL  &&
			tag->extensionFields.scope [1] != NULL)
		length += fprintf (TagFile.fp, "%s\t%s:%s", sep,
				tag->extensionFields.scope [0],
				tag->extensionFields.scope [1]);

	if (Option.extensionFields.typeRef  &&
			tag->extensionFields.typeRef [0] != NULL  &&
			tag->extensionFields.typeRef [1] != NULL)
		length += fprintf (TagFile.fp, "%s\ttyperef:%s:%s", sep,
				tag->extensionFields.typeRef [0],
				tag->extensionFields.typeRef [1]);

	if (Option.extensionFields.fileScope  &&  tag->isFileScope)
		length += fprintf (TagFile.fp, "%s\tfile:", sep);

	if (Option.extensionFields.inheritance  &&
			tag->extensionFields.inheritance != NULL)
		length += fprintf (TagFile.fp, "%s\tinherits:%s", sep,
				tag->extensionFields.inheritance);

	if (Option.extensionFields.access  &&  tag->extensionFields.access != NULL)
		length += fprintf (TagFile.fp, "%s\taccess:%s", sep,
				tag->extensionFields.access);

	if (Option.extensionFields.implementation  &&
			tag->extensionFields.implementation != NULL)
		length += fprintf (TagFile.fp, "%s\timplementation:%s", sep,
				tag->extensionFields.implementation);

	if (Option.extensionFields.signature  &&
			tag->extensionFields.signature != NULL)
		length += fprintf (TagFile.fp, "%s\tsignature:%s", sep,
				tag->extensionFields.signature);

	return length;
#undef sep
}

int writePatternEntry (const tagEntryInfo *const tag)
{
	char *const line = readSourceLine (TagFile.vLine, tag->filePosition, NULL);
	const int searchChar = Option.backward ? '?' : '/';
	bool newlineTerminated;
	int length = 0;

	if (line == NULL)
		error (FATAL, "bad tag in %s", vStringValue (File.name));
	if (tag->truncateLine)
		truncateTagLine (line, tag->name, FALSE);
	newlineTerminated = (bool) (line [strlen (line) - 1] == '\n');

	length += fprintf (TagFile.fp, "%c^", searchChar);
	length += writeSourceLine (TagFile.fp, line);
	length += fprintf (TagFile.fp, "%s%c", newlineTerminated ? "$":"", searchChar);

	return length;
}

/* writeLineNumberEntry - now implemented in Rust (entry.rs) */
extern int writeLineNumberEntry(const tagEntryInfo *tag);

int writeCtagsEntry (const tagEntryInfo *const tag)
{
	int length = fprintf (TagFile.fp, "%s\t%s\t",
		tag->name, tag->sourceFileName);

	if (tag->lineNumberEntry)
		length += writeLineNumberEntry (tag);
	else
		length += writePatternEntry (tag);

	if (includeExtensionFlags ())
		length += addExtensionFields (tag);

	length += fprintf (TagFile.fp, "\n");

	return length;
}

/* makeTagEntry - now implemented in Rust (entry.rs) */
extern void makeTagEntry (const tagEntryInfo *const tag);

/* initTagEntry - now implemented in Rust (entry.rs) */
extern void initTagEntry (tagEntryInfo *const e, const char *const name);

/* vi:set tabstop=4 shiftwidth=4: */
/*
*   $Id$
*
*   Copyright (c) 1996-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains the high level source read functions (preprocessor
*   directives are handled within this level).
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#include "debug.h"
#include "entry.h"
#include "get.h"
#include "options.h"
#include "read.h"
#include "vstring.h"

/*
*   MACROS
*/
#define stringMatch(s1,s2)		(strcmp (s1,s2) == 0)
#define isspacetab(c)			((c) == SPACE || (c) == TAB)

/*
*   DATA DECLARATIONS
*/
typedef enum { COMMENT_NONE, COMMENT_C, COMMENT_CPLUS } Comment;

enum eCppLimits {
	MaxCppNestingLevel = 20,
	MaxDirectiveName = 10
};

/*  Defines the one nesting level of a preprocessor conditional.
 */
typedef struct sConditionalInfo {
	boolean ignoreAllBranches;  /* ignoring parent conditional branch */
	boolean singleBranch;       /* choose only one branch */
	boolean branchChosen;       /* branch already selected */
	boolean ignoring;           /* current ignore state */
} conditionalInfo;

enum eState {
	DRCTV_NONE,    /* no known directive - ignore to end of line */
	DRCTV_DEFINE,  /* "#define" encountered */
	DRCTV_HASH,    /* initial '#' read; determine directive */
	DRCTV_IF,      /* "#if" or "#ifdef" encountered */
	DRCTV_PRAGMA,  /* #pragma encountered */
	DRCTV_UNDEF    /* "#undef" encountered */
};

/*  Defines the current state of the pre-processor.
 */
typedef struct sCppState {
	int		ungetch, ungetch2;   /* ungotten characters, if any */
	boolean resolveRequired;     /* must resolve if/else/elif/endif branch */
	boolean hasAtLiteralStrings; /* supports @"c:\" strings */
	struct sDirective {
		enum eState state;       /* current directive being processed */
		boolean	accept;          /* is a directive syntactically permitted? */
		vString * name;          /* macro name */
		unsigned int nestLevel;  /* level 0 is not used */
		conditionalInfo ifdef [MaxCppNestingLevel];
	} directive;
} cppState;

/*
*   DATA DEFINITIONS
*/

/*  Use brace formatting to detect end of block.
 *  Note: BraceFormat moved to Rust (BRACE_FORMAT in cparser.rs)
 */
extern int rs_isBraceFormat(void);
extern void rs_setBraceFormat(int state);
#define BraceFormat rs_isBraceFormat()

/* Cpp is now owned by Rust - access via rs_getCpp() macro */
extern cppState *rs_getCpp(void);
#define Cpp (*rs_getCpp())

/*
*   FUNCTION DEFINITIONS
*/

/* isBraceFormat is now a macro in get.h */

/* Rust FFI declarations for Cpp accessor functions */
extern unsigned int rs_getDirectiveNestLevel(void);
extern void rs_cppBeginStatement(void);
extern void rs_cppEndStatement(void);
extern int rs_isIgnore(void);
extern int rs_setIgnore(int ignore);
extern void rs_cppUngetc(int c);
extern conditionalInfo *rs_currentConditional(void);

/* Macros to replace C wrapper functions */
#define currentConditional() rs_currentConditional()
#define isIgnore() ((boolean) rs_isIgnore())
#define setIgnore(ignore) ((boolean) rs_setIgnore(ignore))

/* getDirectiveNestLevel is now a macro in get.h */

/* Rust FFI declarations for cppInit/cppTerminate */
/* cpp* wrapper functions have been removed - callers now use rs_cpp* directly.
 * Rust implementations are in src/ctags_rs/clex.rs
 */

/*
*   Scanning functions
*
*   This section handles preprocessor directives.  It strips out all
*   directives and may emit a tag for #define directives.
*/

/* readDirective - now a macro delegating to Rust */
extern int rs_readDirective(int c, char *name, unsigned int maxLength);
#define readDirective(c, name, maxLength) ((boolean) rs_readDirective((c), (name), (maxLength)))

/* cppReadIdentifier - now a macro delegating to Rust */
extern void rs_cppReadIdentifier(int c, vString *const name);
#define cppReadIdentifier(c, name) rs_cppReadIdentifier((c), (name))

/* currentConditional, isIgnore, setIgnore are now macros - see above */

/* isIgnoreBranch - now a macro delegating to Rust */
extern int rs_isIgnoreBranch(void);
#define isIgnoreBranch() ((boolean) rs_isIgnoreBranch())

/* chooseBranch - now a macro delegating to Rust */
extern void rs_chooseBranch(void);
#define chooseBranch() rs_chooseBranch()

/* pushConditional - now a macro delegating to Rust */
extern int rs_pushConditional(int firstBranchChosen);
#define pushConditional(firstBranchChosen) ((boolean) rs_pushConditional((int)(firstBranchChosen)))

/* popConditional - now a macro delegating to Rust */
extern int rs_popConditional(void);
#define popConditional() ((boolean) rs_popConditional())

/* makeDefineTag - now a macro delegating to Rust */
extern void rs_makeDefineTag(const char *name);
#define makeDefineTag(name) rs_makeDefineTag((name))

/* directiveDefine - now a macro delegating to Rust */
extern void rs_directiveDefine(int c);
#define directiveDefine(c) rs_directiveDefine((c))

/* directivePragma - now a macro delegating to Rust */
extern void rs_directivePragma(int c);
#define directivePragma(c) rs_directivePragma((c))

/* directiveIf - now a macro delegating to Rust */
extern int rs_directiveIf(int c);
#define directiveIf(c) ((boolean) rs_directiveIf((c)))

/* directiveHash - now a macro delegating to Rust */
extern int rs_directiveHash(int c);
#define directiveHash(c) ((boolean) rs_directiveHash((c)))

/* handleDirective - now a macro delegating to Rust */
extern int rs_handleDirective(int c);
#define handleDirective(c) ((boolean) rs_handleDirective((c)))

/* isComment - now a macro delegating to Rust */
extern int rs_isComment(void);
#define isComment() ((Comment) rs_isComment())

/* skipOverCComment - now a macro delegating to Rust */
extern int rs_skipOverCComment(void);
#define skipOverCComment() rs_skipOverCComment()

/* skipOverCplusComment - now a macro delegating to Rust */
extern int rs_skipOverCplusComment(void);
#define skipOverCplusComment() rs_skipOverCplusComment()

/* skipToEndOfString - now a macro delegating to Rust */
extern int rs_skipToEndOfString(int ignoreBackslash);
#define skipToEndOfString(ignoreBackslash) rs_skipToEndOfString((int)(ignoreBackslash))

/* skipToEndOfChar - now a macro delegating to Rust */
extern int rs_skipToEndOfChar(void);
#define skipToEndOfChar() rs_skipToEndOfChar()

/* cppGetc - now a macro delegating to Rust */
extern int rs_cppGetc(void);
#define cppGetc() rs_cppGetc()

/* vi:set tabstop=4 shiftwidth=4: */
/*
*   $Id$
*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   Manages a keyword hash.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include <pthread.h>

#include "debug.h"
#include "keyword.h"
#include "options.h"
#include "routines.h"

/*
*   MACROS
*/
#define HASH_EXPONENT 7  /* must be less than 17 */

/*
*   DATA DECLARATIONS
*/
typedef struct sHashEntry {
	struct sHashEntry *next;
	const char *string;
	langType language;
	int value;
} hashEntry;

/*
*   DATA DEFINITIONS
*/
const unsigned int TableSize = 1 << HASH_EXPONENT;
hashEntry **HashTable = NULL;
volatile boolean allocated = FALSE;
pthread_mutex_t hashTableMutex = PTHREAD_MUTEX_INITIALIZER;

/*
*   FUNCTION DEFINITIONS
*/

hashEntry **getHashTable (void)
{
	if (!allocated)
	{
		pthread_mutex_lock(&hashTableMutex);
		if (!allocated)  /* Double-check pattern */
		{
			unsigned int i;
			hashEntry **table = xMalloc (TableSize, hashEntry*);

			for (i = 0; i < TableSize; ++i)
				table[i] = NULL;

			/* Memory barrier before setting allocated flag */
			__sync_synchronize();
			HashTable = table;
			allocated = TRUE;
		}
		pthread_mutex_unlock(&hashTableMutex);
	}
	return HashTable;
}

hashEntry *getHashTableEntry (unsigned long hashedValue)
{
	hashEntry **const table = getHashTable ();
	hashEntry *entry;

	Assert (hashedValue < TableSize);
	entry = table [hashedValue];

	return entry;
}

/* hashValue moved to Rust - see rs_hashValue in keyword.rs */
extern unsigned long rs_hashValue(const char *string);
#define hashValue(s) rs_hashValue(s)

hashEntry *newEntry (
		const char *const string, langType language, int value)
{
	hashEntry *const entry = xMalloc (1, hashEntry);

	entry->next     = NULL;
	entry->string   = string;
	entry->language = language;
	entry->value    = value;

	return entry;
}

/*  Note that it is assumed that a "value" of zero means an undefined keyword
 *  and clients of this function should observe this. Also, all keywords added
 *  should be added in lower case. If we encounter a case-sensitive language
 *  whose keywords are in upper case, we will need to redesign this.
 *
 *  All keyword functions now implemented in Rust: src/ctags_rs/keyword.rs
 */

#ifdef DEBUG

void printEntry (const hashEntry *const entry)
{
	printf ("  %-15s %-7s\n", entry->string, getLanguageName (entry->language));
}

unsigned int printBucket (const unsigned int i)
{
	hashEntry **const table = getHashTable ();
	hashEntry *entry = table [i];
	unsigned int measure = 1;
	boolean first = TRUE;

	printf ("%2d:", i);
	if (entry == NULL)
		printf ("\n");
	else while (entry != NULL)
	{
		if (! first)
			printf ("    ");
		else
		{
			printf (" ");
			first = FALSE;
		}
		printEntry (entry);
		entry = entry->next;
		measure = 2 * measure;
	}
	return measure - 1;
}

extern void printKeywordTable (void)
{
	unsigned long emptyBucketCount = 0;
	unsigned long measure = 0;
	unsigned int i;

	for (i = 0; i < TableSize; ++i)
	{
		const unsigned int pass = printBucket (i);

		measure += pass;
		if (pass == 0)
			++emptyBucketCount;
	}

	printf ("spread measure = %ld\n", measure);
	printf ("%ld empty buckets\n", emptyBucketCount);
}

#endif

/* vi:set tabstop=4 shiftwidth=4: */
/*
*   $Id$
*
*   Copyright (c) 2000-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for applying regular expression matching.
*
*   The code for utlizing the Gnu regex package with regards to processing the
*   regex option and checking for regex matches was adapted from routines in
*   Gnu etags.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>

#ifdef HAVE_REGCOMP
# include <ctype.h>
# include <stddef.h>
# ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>  /* declare off_t (not known to regex.h on FreeBSD) */
# endif
# include <regex.h>
#endif

#include "debug.h"
#include "entry.h"
#include "parse.h"
#include "read.h"
#include "routines.h"

#ifdef HAVE_REGEX

/*
*   MACROS
*/

/* Back-references \0 through \9 */
#define BACK_REFERENCE_COUNT 10

#if defined (HAVE_REGCOMP) && !defined (REGCOMP_BROKEN)
# define POSIX_REGEX
#endif

#define REGEX_NAME "Regex"

/*
*   DATA DECLARATIONS
*/
#if defined (POSIX_REGEX)

struct sKind {
	boolean enabled;
	char letter;
	char* name;
	char* description;
};

enum pType { PTRN_TAG, PTRN_CALLBACK };

typedef struct {
	regex_t *pattern;
	enum pType type;
	union {
		struct {
			char *name_pattern;
			struct sKind kind;
		} tag;
		struct {
			regexCallback function;
		} callback;
	} u;
} regexPattern;

#endif

typedef struct {
	regexPattern *patterns;
	unsigned int count;
} patternSet;

/*
*   DATA DEFINITIONS
*/

/* regexBroken moved to Rust - see REGEX_BROKEN in options.rs
 * Access via rs_getRegexBroken()/rs_setRegexBroken() or regexBroken macro
 */
extern int rs_getRegexBroken(void);
extern void rs_setRegexBroken(int value);
#define regexBroken (rs_getRegexBroken())

/* Sets and SetUpper are now Rust-owned in cparser.rs */
extern patternSet *rs_getSets(void);
extern void rs_setSets(patternSet *sets);
extern int rs_getSetUpper(void);
extern void rs_setSetUpper(int upper);
extern void rs_freeRegexResourcesDirect(void);

#define Sets (rs_getSets())
#define SetUpper (rs_getSetUpper())

/*
*   FUNCTION DEFINITIONS
*/

void clearPatternSet (const langType language)
{
	if (language <= SetUpper)
	{
		patternSet* const set = Sets + language;
		unsigned int i;
		for (i = 0  ;  i < set->count  ;  ++i)
		{
			regexPattern *p = &set->patterns [i];
#if defined (POSIX_REGEX)
			regfree (p->pattern);
#endif
			eFree (p->pattern);
			p->pattern = NULL;

			if (p->type == PTRN_TAG)
			{
				eFree (p->u.tag.name_pattern);
				p->u.tag.name_pattern = NULL;
				eFree (p->u.tag.kind.name);
				p->u.tag.kind.name = NULL;
				if (p->u.tag.kind.description != NULL)
				{
					eFree (p->u.tag.kind.description);
					p->u.tag.kind.description = NULL;
				}
			}
		}
		if (set->patterns != NULL)
			eFree (set->patterns);
		set->patterns = NULL;
		set->count = 0;
	}
}

/*
*   Regex psuedo-parser
*/

void makeRegexTag (
		const vString* const name, const struct sKind* const kind)
{
	if (kind->enabled)
	{
		tagEntryInfo e;
		Assert (name != NULL  &&  vStringLength (name) > 0);
		Assert (kind != NULL);
		initTagEntry (&e, vStringValue (name));
		e.kind     = kind->letter;
		e.kindName = kind->name;
		makeTagEntry (&e);
	}
}

/*
*   Regex pattern definition
*/

/* Take a string like "/blah/" and turn it into "blah", making sure
 * that the first and last characters are the same, and handling
 * quoted separator characters.  Actually, stops on the occurrence of
 * an unquoted separator.  Also turns "\t" into a Tab character.
 * Returns pointer to terminating separator.  Works in place.  Null
 * terminates name string.
 */
char* scanSeparators (char* name)
{
	char sep = name [0];
	char *copyto = name;
	boolean quoted = FALSE;

	for (++name ; *name != '\0' ; ++name)
	{
		if (quoted)
		{
			if (*name == sep)
				*copyto++ = sep;
			else if (*name == 't')
				*copyto++ = '\t';
			else
			{
				/* Something else is quoted, so preserve the quote. */
				*copyto++ = '\\';
				*copyto++ = *name;
			}
			quoted = FALSE;
		}
		else if (*name == '\\')
			quoted = TRUE;
		else if (*name == sep)
		{
			break;
		}
		else
			*copyto++ = *name;
	}
	*copyto = '\0';
	return name;
}

/* Parse `regexp', in form "/regex/name/[k,Kind/]flags" (where the separator
 * character is whatever the first character of `regexp' is), by breaking it
 * up into null terminated strings, removing the separators, and expanding
 * '\t' into tabs. When complete, `regexp' points to the line matching
 * pattern, a pointer to the name matching pattern is written to `name', a
 * pointer to the kinds is written to `kinds' (possibly NULL), and a pointer
 * to the trailing flags is written to `flags'. If the pattern is not in the
 * correct format, a false value is returned.
 */
boolean parseTagRegex (
		char* const regexp, char** const name,
		char** const kinds, char** const flags)
{
	boolean result = FALSE;
	const int separator = (unsigned char) regexp [0];

	*name = scanSeparators (regexp);
	if (*regexp == '\0')
		error (WARNING, "empty regexp");
	else if (**name != separator)
		error (WARNING, "%s: incomplete regexp", regexp);
	else
	{
		char* const third = scanSeparators (*name);
		if (**name == '\0')
			error (WARNING, "%s: regexp missing name pattern", regexp);
		if ((*name) [strlen (*name) - 1] == '\\')
			error (WARNING, "error in name pattern: \"%s\"", *name);
		if (*third != separator)
			error (WARNING, "%s: regexp missing final separator", regexp);
		else
		{
			char* const fourth = scanSeparators (third);
			if (*fourth == separator)
			{
				*kinds = third;
				scanSeparators (fourth);
				*flags = fourth;
			}
			else
			{
				*flags = third;
				*kinds = NULL;
			}
			result = TRUE;
		}
	}
	return result;
}

void addCompiledTagPattern (
		const langType language, regex_t* const pattern,
		char* const name, const char kind, char* const kindName,
		char *const description)
{
	patternSet* set;
	regexPattern *ptrn;
	if (language > SetUpper)
	{
		int i;
		rs_setSets (xRealloc (Sets, (language + 1), patternSet));
		for (i = SetUpper + 1  ;  i <= language  ;  ++i)
		{
			Sets [i].patterns = NULL;
			Sets [i].count = 0;
		}
		rs_setSetUpper (language);
	}
	set = Sets + language;
	set->patterns = xRealloc (set->patterns, (set->count + 1), regexPattern);
	ptrn = &set->patterns [set->count];
	set->count += 1;

	ptrn->pattern = pattern;
	ptrn->type    = PTRN_TAG;
	ptrn->u.tag.name_pattern = name;
	ptrn->u.tag.kind.enabled = TRUE;
	ptrn->u.tag.kind.letter  = kind;
	ptrn->u.tag.kind.name    = kindName;
	ptrn->u.tag.kind.description = description;
}

void addCompiledCallbackPattern (
		const langType language, regex_t* const pattern,
		const regexCallback callback)
{
	patternSet* set;
	regexPattern *ptrn;
	if (language > SetUpper)
	{
		int i;
		rs_setSets (xRealloc (Sets, (language + 1), patternSet));
		for (i = SetUpper + 1  ;  i <= language  ;  ++i)
		{
			Sets [i].patterns = NULL;
			Sets [i].count = 0;
		}
		rs_setSetUpper (language);
	}
	set = Sets + language;
	set->patterns = xRealloc (set->patterns, (set->count + 1), regexPattern);
	ptrn = &set->patterns [set->count];
	set->count += 1;

	ptrn->pattern = pattern;
	ptrn->type    = PTRN_CALLBACK;
	ptrn->u.callback.function = callback;
}

#if defined (POSIX_REGEX)

regex_t* compileRegex (const char* const regexp, const char* const flags)
{
	int cflags = REG_EXTENDED | REG_NEWLINE;
	regex_t *result = NULL;
	int errcode;
	int i;
	for (i = 0  ; flags != NULL  &&  flags [i] != '\0'  ;  ++i)
	{
		switch ((int) flags [i])
		{
			case 'b': cflags &= ~REG_EXTENDED; break;
			case 'e': cflags |= REG_EXTENDED;  break;
			case 'i': cflags |= REG_ICASE;     break;
			default: error (WARNING, "unknown regex flag: '%c'", *flags); break;
		}
	}
	result = xMalloc (1, regex_t);
	errcode = regcomp (result, regexp, cflags);
	if (errcode != 0)
	{
		char errmsg[256];
		regerror (errcode, result, errmsg, 256);
		error (WARNING, "regcomp %s: %s", regexp, errmsg);
		regfree (result);
		eFree (result);
		result = NULL;
	}
	return result;
}

#endif

void parseKinds (
		const char* const kinds, char* const kind, char** const kindName,
		char **description)
{
	*kind = '\0';
	*kindName = NULL;
	*description = NULL;
	if (kinds == NULL  ||  kinds [0] == '\0')
	{
		*kind = 'r';
		*kindName = eStrdup ("regex");
	}
	else if (kinds [0] != '\0')
	{
		const char* k = kinds;
		if (k [0] != ','  &&  (k [1] == ','  ||  k [1] == '\0'))
			*kind = *k++;
		else
			*kind = 'r';
		if (*k == ',')
			++k;
		if (k [0] == '\0')
			*kindName = eStrdup ("regex");
		else
		{
			const char *const comma = strchr (k, ',');
			if (comma == NULL)
				*kindName = eStrdup (k);
			else
			{
				*kindName = (char*) eMalloc (comma - k + 1);
				strncpy (*kindName, k, comma - k);
				(*kindName) [comma - k] = '\0';
				k = comma + 1;
				if (k [0] != '\0')
					*description = eStrdup (k);
			}
		}
	}
}

void printRegexKind (const regexPattern *pat, unsigned int i, boolean indent)
{
	const struct sKind *const kind = &pat [i].u.tag.kind;
	const char *const indentation = indent ? "    " : "";
	Assert (pat [i].type == PTRN_TAG);
	printf ("%s%c  %s %s\n", indentation,
			kind->letter != '\0' ? kind->letter : '?',
			kind->description != NULL ? kind->description : kind->name,
			kind->enabled ? "" : " [off]");
}

void processLanguageRegex (const langType language,
		const char* const parameter)
{
	if (parameter == NULL  ||  parameter [0] == '\0')
		clearPatternSet (language);
	else if (parameter [0] != '@')
		addLanguageRegex (language, parameter);
	else if (! doesFileExist (parameter + 1))
		error (WARNING, "cannot open regex file");
	else
	{
		const char* regexfile = parameter + 1;
		FILE* const fp = fopen (regexfile, "r");
		if (fp == NULL)
			error (WARNING | PERROR, "%s", regexfile);
		else
		{
			vString* const regex = rs_vStringNew ();
			while (readLine (regex, fp))
				addLanguageRegex (language, vStringValue (regex));
			fclose (fp);
			rs_vStringDelete (regex);
		}
	}
}

/*
*   Regex pattern matching
*/

#if defined (POSIX_REGEX)

vString* substitute (
		const char* const in, const char* out,
		const int nmatch, const regmatch_t* const pmatch)
{
	vString* result = rs_vStringNew ();
	const char* p;
	for (p = out  ;  *p != '\0'  ;  p++)
	{
		if (*p == '\\'  &&  isdigit ((int) *++p))
		{
			const int dig = *p - '0';
			if (0 < dig  &&  dig < nmatch  &&  pmatch [dig].rm_so != -1)
			{
				const int diglen = pmatch [dig].rm_eo - pmatch [dig].rm_so;
				rs_vStringNCatS (result, in + pmatch [dig].rm_so, diglen);
			}
		}
		else if (*p != '\n'  &&  *p != '\r')
			rs_vStringPut (result, *p);
	}
	vStringTerminate (result);
	return result;
}

void matchTagPattern (const vString* const line,
		const regexPattern* const patbuf,
		const regmatch_t* const pmatch)
{
	vString *const name = substitute (vStringValue (line),
			patbuf->u.tag.name_pattern, BACK_REFERENCE_COUNT, pmatch);
	rs_vStringStripLeading (name);
	rs_vStringStripTrailing (name);
	if (vStringLength (name) > 0)
		makeRegexTag (name, &patbuf->u.tag.kind);
	else
		error (WARNING, "%s:%ld: null expansion of name pattern \"%s\"",
			getInputFileName (), getInputLineNumber (),
			patbuf->u.tag.name_pattern);
	rs_vStringDelete (name);
}

void matchCallbackPattern (
		const vString* const line, const regexPattern* const patbuf,
		const regmatch_t* const pmatch)
{
	regexMatch matches [BACK_REFERENCE_COUNT];
	unsigned int count = 0;
	int i;
	for (i = 0  ;  i < BACK_REFERENCE_COUNT  &&  pmatch [i].rm_so != -1  ;  ++i)
	{
		matches [i].start  = pmatch [i].rm_so;
		matches [i].length = pmatch [i].rm_eo - pmatch [i].rm_so;
		++count;
	}
	patbuf->u.callback.function (vStringValue (line), matches, count);
}

boolean matchRegexPattern (const vString* const line,
		const regexPattern* const patbuf)
{
	boolean result = FALSE;
	regmatch_t pmatch [BACK_REFERENCE_COUNT];
	const int match = regexec (patbuf->pattern, vStringValue (line),
							   BACK_REFERENCE_COUNT, pmatch, 0);
	if (match == 0)
	{
		result = TRUE;
		if (patbuf->type == PTRN_TAG)
			matchTagPattern (line, patbuf, pmatch);
		else if (patbuf->type == PTRN_CALLBACK)
			matchCallbackPattern (line, patbuf, pmatch);
		else
		{
			Assert ("invalid pattern type" == NULL);
			result = FALSE;
		}
	}
	return result;
}

#endif

/* PUBLIC INTERFACE */

/* Match against all patterns for specified language. Returns true if at least
 * on pattern matched.
 */
extern boolean matchRegex (const vString* const line, const langType language)
{
	boolean result = FALSE;
	if (language != LANG_IGNORE  &&  language <= SetUpper  &&
		Sets [language].count > 0)
	{
		const patternSet* const set = Sets + language;
		unsigned int i;
		for (i = 0  ;  i < set->count  ;  ++i)
			if (matchRegexPattern (line, set->patterns + i))
				result = TRUE;
	}
	return result;
}

extern void findRegexTags (void)
{
	/* merely read all lines of the file */
	while (fileReadLine () != NULL)
		;
}

#endif  /* HAVE_REGEX */

extern void addTagRegex (
		const langType language __unused__,
		const char* const regex __unused__,
		const char* const name __unused__,
		const char* const kinds __unused__,
		const char* const flags __unused__)
{
#ifdef HAVE_REGEX
	Assert (regex != NULL);
	Assert (name != NULL);
	if (! regexBroken)
	{
		regex_t* const cp = compileRegex (regex, flags);
		if (cp != NULL)
		{
			char kind;
			char* kindName;
			char* description;
			parseKinds (kinds, &kind, &kindName, &description);
			addCompiledTagPattern (language, cp, eStrdup (name),
					kind, kindName, description);
		}
	}
#endif
}

extern void addCallbackRegex (
		const langType language __unused__,
		const char* const regex __unused__,
		const char* const flags __unused__,
		const regexCallback callback __unused__)
{
#ifdef HAVE_REGEX
	Assert (regex != NULL);
	if (! regexBroken)
	{
		regex_t* const cp = compileRegex (regex, flags);
		if (cp != NULL)
			addCompiledCallbackPattern (language, cp, callback);
	}
#endif
}

extern void addLanguageRegex (
		const langType language __unused__, const char* const regex __unused__)
{
#ifdef HAVE_REGEX
	if (! regexBroken)
	{
		char *const regex_pat = eStrdup (regex);
		char *name, *kinds, *flags;
		if (parseTagRegex (regex_pat, &name, &kinds, &flags))
		{
			addTagRegex (language, regex_pat, name, kinds, flags);
			eFree (regex_pat);
		}
	}
#endif
}

/*
*   Regex option parsing
*/

extern boolean processRegexOption (const char *const option,
								   const char *const parameter __unused__)
{
	boolean handled = FALSE;
	const char* const dash = strchr (option, '-');
	if (dash != NULL  &&  strncmp (option, "regex", dash - option) == 0)
	{
#ifdef HAVE_REGEX
		langType language;
		language = getNamedLanguage (dash + 1);
		if (language == LANG_IGNORE)
			error (WARNING, "unknown language \"%s\" in --%s option", (dash + 1), option);
		else
			processLanguageRegex (language, parameter);
#else
		error (WARNING, "regex support not available; required for --%s option",
		   option);
#endif
		handled = TRUE;
	}
	return handled;
}

extern void disableRegexKinds (const langType language __unused__)
{
#ifdef HAVE_REGEX
	if (language <= SetUpper  &&  Sets [language].count > 0)
	{
		patternSet* const set = Sets + language;
		unsigned int i;
		for (i = 0  ;  i < set->count  ;  ++i)
			if (set->patterns [i].type == PTRN_TAG)
				set->patterns [i].u.tag.kind.enabled = FALSE;
	}
#endif
}

extern boolean enableRegexKind (
		const langType language __unused__,
		const int kind __unused__, const boolean mode __unused__)
{
	boolean result = FALSE;
#ifdef HAVE_REGEX
	if (language <= SetUpper  &&  Sets [language].count > 0)
	{
		patternSet* const set = Sets + language;
		unsigned int i;
		for (i = 0  ;  i < set->count  ;  ++i)
			if (set->patterns [i].type == PTRN_TAG &&
				set->patterns [i].u.tag.kind.letter == kind)
			{
				set->patterns [i].u.tag.kind.enabled = mode;
				result = TRUE;
			}
	}
#endif
	return result;
}

extern void printRegexKinds (const langType language __unused__, boolean indent __unused__)
{
#ifdef HAVE_REGEX
	if (language <= SetUpper  &&  Sets [language].count > 0)
	{
		patternSet* const set = Sets + language;
		unsigned int i;
		for (i = 0  ;  i < set->count  ;  ++i)
			if (set->patterns [i].type == PTRN_TAG)
				printRegexKind (set->patterns, i, indent);
	}
#endif
}

/* freeRegexResources - now implemented directly in Rust (cparser.rs) */
/* Uses rs_freeRegexResourcesDirect() which accesses SETS and SET_UPPER directly */

/* Check for broken regcomp() on Cygwin */
extern void checkRegex (void)
{
#if defined (HAVE_REGEX) && defined (CHECK_REGCOMP)
	regex_t patbuf;
	int errcode;
	if (regcomp (&patbuf, "/hello/", 0) != 0)
	{
		error (WARNING, "Disabling broken regex");
		rs_setRegexBroken(TRUE);
	}
#endif
}

/* vi:set tabstop=4 shiftwidth=4: */
/*
*   $Id$
*
*   Copyright (c) 1996-2003, Darren Hiebert
*
*   Author: Darren Hiebert <dhiebert@users.sourceforge.net>
*           http://ctags.sourceforge.net
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License. It is provided on an as-is basis and no
*   responsibility is accepted for its failure to perform as expected.
*
*   This is a reimplementation of the ctags (1) program. It is an attempt to
*   provide a fully featured ctags program which is free of the limitations
*   which most (all?) others are subject to.
*
*   This module contains the start-up code and routines to determine the list
*   of files to parsed for tags.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include <time.h>

/* Timing instrumentation for profiling */
#ifdef PROFILE_CTAGS
struct timespec profile_start, profile_end;
#define PROFILE_START() clock_gettime(CLOCK_MONOTONIC, &profile_start)
#define PROFILE_END(name) do { \
    clock_gettime(CLOCK_MONOTONIC, &profile_end); \
    fprintf(stderr, "PROFILE %s: %.3f ms\n", name, \
        ((profile_end.tv_sec - profile_start.tv_sec) * 1000.0 + \
         (profile_end.tv_nsec - profile_start.tv_nsec) / 1000000.0)); \
} while(0)
#else
#define PROFILE_START()
#define PROFILE_END(name)
#endif

/*  To provide timings features if available.
 */
#ifdef HAVE_CLOCK
# ifdef HAVE_TIME_H
#  include <time.h>
# endif
#else
# ifdef HAVE_TIMES
#  ifdef HAVE_SYS_TIMES_H
#   include <sys/times.h>
#  endif
# endif
#endif

/*  To provide directory searching for recursion feature.
 */
#ifdef AMIGA
# include <dos/dosasl.h>       /* for struct AnchorPath */
# include <clib/dos_protos.h>  /* function prototypes */
# define ANCHOR_BUF_SIZE 512
# define ANCHOR_SIZE (sizeof (struct AnchorPath) + ANCHOR_BUF_SIZE)
# ifdef __SASC
   extern struct DosLibrary *DOSBase;
#  include <pragmas/dos_pragmas.h>
# endif
#endif

#ifdef HAVE_DIRENT_H
# ifdef __BORLANDC__
#  define boolean BORLAND_boolean
# endif
# ifdef HAVE_SYS_TYPES_H
#  include <sys/types.h>  /* required by dirent.h */
# endif
# define __unused
# include <dirent.h>  /* to declare opendir() */
# undef boolean
#endif
#ifdef HAVE_DIRECT_H
# include <direct.h>  /* to _getcwd() */
#endif
#ifdef HAVE_DOS_H
# include <dos.h>  /* to declare FA_DIREC */
#endif
#ifdef HAVE_DIR_H
# include <dir.h>  /* to declare findfirst() and findnext */
#endif
#ifdef HAVE_IO_H
# include <io.h>  /* to declare _findfirst() */
#endif


#include "debug.h"
#include "keyword.h"
#include "main.h"
#include "options.h"
#include "read.h"
#include "routines.h"

/*
*   MACROS
*/
#define plural(value)  (((unsigned long)(value) == 1L) ? "" : "s")

/*
*   DATA DEFINITIONS
*/

/* Totals struct moved to Rust - see TOTALS in options.rs
 * Access via rs_getTotals() or Totals macro, rs_addTotals() for updates
 */
typedef struct { long files, lines, bytes; } TotalsStruct;
extern TotalsStruct *rs_getTotals(void);
extern void rs_addTotals(unsigned int files, unsigned long lines, unsigned long bytes);
extern long rs_getTotalsFiles(void);
extern long rs_getTotalsLines(void);
extern long rs_getTotalsBytes(void);
#define Totals (*rs_getTotals())

#ifdef AMIGA
# include "ctags.h"
  static const char *VERsion = "$VER: "PROGRAM_NAME" "PROGRAM_VERSION" "
# ifdef __SASC
  __AMIGADATE__
# else
  __DATE__
# endif
  " "AUTHOR_NAME" $";
#endif

/*
*   FUNCTION PROTOTYPES
*/
boolean createTagsForEntry (const char *const entryName);

/*
*   FUNCTION DEFINITIONS
*/

/* addTotals - now a macro calling Rust implementation */
#define addTotals(files, lines, bytes) rs_addTotals((files), (lines), (bytes))

/* isDestinationStdout - now implemented in Rust (cparser.rs) */
extern boolean isDestinationStdout(void);

#if defined (HAVE_OPENDIR)
boolean recurseUsingOpendir (const char *const dirName)
{
	boolean resize = FALSE;
	DIR *const dir = opendir (dirName);
	if (dir == NULL)
		error (WARNING | PERROR, "cannot recurse into directory \"%s\"", dirName);
	else
	{
		struct dirent *entry;
		while ((entry = readdir (dir)) != NULL)
		{
			if (strcmp (entry->d_name, ".") != 0  &&
				strcmp (entry->d_name, "..") != 0)
			{
				vString *filePath;
				if (strcmp (dirName, ".") == 0)
					filePath = rs_vStringNewInit (entry->d_name);
				else
					filePath = combinePathAndFile (dirName, entry->d_name);
				resize |= createTagsForEntry (vStringValue (filePath));
				rs_vStringDelete (filePath);
			}
		}
		closedir (dir);
	}
	return resize;
}

#elif defined (HAVE_FINDFIRST) || defined (HAVE__FINDFIRST)

boolean createTagsForWildcardEntry (
		const char *const pattern, const size_t dirLength,
		const char *const entryName)
{
	boolean resize = FALSE;
	/* we must not recurse into the directories "." or ".." */
	if (strcmp (entryName, ".") != 0  &&  strcmp (entryName, "..") != 0)
	{
		vString *const filePath = rs_vStringNew ();
		rs_vStringNCopyS (filePath, pattern, dirLength);
		rs_vStringCatS (filePath, entryName);
		resize = createTagsForEntry (vStringValue (filePath));
		rs_vStringDelete (filePath);
	}
	return resize;
}

boolean createTagsForWildcardUsingFindfirst (const char *const pattern)
{
	boolean resize = FALSE;
	const size_t dirLength = baseFilename (pattern) - pattern;
#if defined (HAVE_FINDFIRST)
	struct ffblk fileInfo;
	int result = findfirst (pattern, &fileInfo, FA_DIREC);
	while (result == 0)
	{
		const char *const entry = (const char *) fileInfo.ff_name;
		resize |= createTagsForWildcardEntry (pattern, dirLength, entry);
		result = findnext (&fileInfo);
	}
#elif defined (HAVE__FINDFIRST)
	struct _finddata_t fileInfo;
	findfirst_t hFile = _findfirst (pattern, &fileInfo);
	if (hFile != -1L)
	{
		do
		{
			const char *const entry = (const char *) fileInfo.name;
			resize |= createTagsForWildcardEntry (pattern, dirLength, entry);
		} while (_findnext (hFile, &fileInfo) == 0);
		_findclose (hFile);
	}
#endif
	return resize;
}

#elif defined (AMIGA)

boolean createTagsForAmigaWildcard (const char *const pattern)
{
	boolean resize = FALSE;
	struct AnchorPath *const anchor =
			(struct AnchorPath *) eMalloc ((size_t) ANCHOR_SIZE);
	LONG result;

	memset (anchor, 0, (size_t) ANCHOR_SIZE);
	anchor->ap_Strlen = ANCHOR_BUF_SIZE;
	/* Allow '.' for current directory */
#ifdef APF_DODOT
	anchor->ap_Flags = APF_DODOT | APF_DOWILD;
#else
	anchor->ap_Flags = APF_DoDot | APF_DoWild;
#endif
	result = MatchFirst ((UBYTE *) pattern, anchor);
	while (result == 0)
	{
		resize |= createTagsForEntry ((char *) anchor->ap_Buf);
		result = MatchNext (anchor);
	}
	MatchEnd (anchor);
	eFree (anchor);
	return resize;
}
#endif

boolean recurseIntoDirectory (const char *const dirName)
{
	boolean resize = FALSE;
	if (isRecursiveLink (dirName))
		verbose ("ignoring \"%s\" (recursive link)\n", dirName);
	else if (! Option.recurse)
		verbose ("ignoring \"%s\" (directory)\n", dirName);
	else
	{
		verbose ("RECURSING into directory \"%s\"\n", dirName);
#if defined (HAVE_OPENDIR)
		resize = recurseUsingOpendir (dirName);
#elif defined (HAVE_FINDFIRST) || defined (HAVE__FINDFIRST)
		{
			vString *const pattern = rs_vStringNew ();
			rs_vStringCopyS (pattern, dirName);
			rs_vStringPut (pattern, OUTPUT_PATH_SEPARATOR);
			rs_vStringCatS (pattern, "*.*");
			resize = createTagsForWildcardUsingFindfirst (vStringValue (pattern));
			rs_vStringDelete (pattern);
		}
#elif defined (AMIGA)
		{
			vString *const pattern = rs_vStringNew ();
			if (*dirName != '\0'  &&  strcmp (dirName, ".") != 0)
			{
				rs_vStringCopyS (pattern, dirName);
				if (dirName [strlen (dirName) - 1] != '/')
					rs_vStringPut (pattern, '/');
			}
			rs_vStringCatS (pattern, "#?");
			resize = createTagsForAmigaWildcard (vStringValue (pattern));
			rs_vStringDelete (pattern);
		}
#endif
	}
	return resize;
}

boolean createTagsForEntry (const char *const entryName)
{
	boolean resize = FALSE;
	fileStatus *status = eStat (entryName);

	Assert (entryName != NULL);
	if (isExcludedFile (entryName))
		verbose ("excluding \"%s\"\n", entryName);
	else if (status->isSymbolicLink  &&  ! Option.followLinks)
		verbose ("ignoring \"%s\" (symbolic link)\n", entryName);
	else if (! status->exists)
		error (WARNING | PERROR, "cannot open source file \"%s\"", entryName);
	else if (status->isDirectory)
		resize = recurseIntoDirectory (entryName);
	else if (! status->isNormalFile)
		verbose ("ignoring \"%s\" (special file)\n", entryName);
	else
		resize = parseFile (entryName);

	eStatFree (status);
	return resize;
}

#ifdef MANUAL_GLOBBING

boolean createTagsForWildcardArg (const char *const arg)
{
	boolean resize = FALSE;
	vString *const pattern = rs_vStringNewInit (arg);
	char *patternS = vStringValue (pattern);

#if defined (HAVE_FINDFIRST) || defined (HAVE__FINDFIRST)
	/*  We must transform the "." and ".." forms into something that can
	 *  be expanded by the findfirst/_findfirst functions.
	 */
	if (Option.recurse  &&
		(strcmp (patternS, ".") == 0  ||  strcmp (patternS, "..") == 0))
	{
		rs_vStringPut (pattern, OUTPUT_PATH_SEPARATOR);
		rs_vStringCatS (pattern, "*.*");
	}
	resize |= createTagsForWildcardUsingFindfirst (patternS);
#endif
	rs_vStringDelete (pattern);
	return resize;
}

#endif

boolean createTagsForArgs (cookedArgs *const args)
{
	boolean resize = FALSE;

	/*  Generate tags for each argument on the command line.
	 */
	while (! cArgOff (args))
	{
		const char *const arg = cArgItem (args);

#ifdef MANUAL_GLOBBING
		resize |= createTagsForWildcardArg (arg);
#else
		resize |= createTagsForEntry (arg);
#endif
		cArgForth (args);
		parseOptions (args);
	}
	return resize;
}

/*  Read from an opened file a list of file names for which to generate tags.
 */
boolean createTagsFromFileInput (FILE *const fp, const boolean filter)
{
	boolean resize = FALSE;
	if (fp != NULL)
	{
		cookedArgs *args = cArgNewFromLineFile (fp);
		parseOptions (args);
		while (! cArgOff (args))
		{
			resize |= createTagsForEntry (cArgItem (args));
			if (filter)
			{
				if (Option.filterTerminator != NULL)
					fputs (Option.filterTerminator, stdout);
				fflush (stdout);
			}
			cArgForth (args);
			parseOptions (args);
		}
		rs_cArgDelete (args);
	}
	return resize;
}

/*  Read from a named file a list of file names for which to generate tags.
 */
boolean createTagsFromListFile (const char *const fileName)
{
	boolean resize;
	Assert (fileName != NULL);
	if (strcmp (fileName, "-") == 0)
		resize = createTagsFromFileInput (stdin, FALSE);
	else
	{
		FILE *const fp = fopen (fileName, "r");
		if (fp == NULL)
			error (FATAL | PERROR, "cannot open list file \"%s\"", fileName);
		resize = createTagsFromFileInput (fp, FALSE);
		fclose (fp);
	}
	return resize;
}

#if defined (HAVE_CLOCK)
# define CLOCK_AVAILABLE
# ifndef CLOCKS_PER_SEC
#  define CLOCKS_PER_SEC		1000000
# endif
#elif defined (HAVE_TIMES)
# define CLOCK_AVAILABLE
# define CLOCKS_PER_SEC	60
clock_t clock (void)
{
	struct tms buf;

	times (&buf);
	return (buf.tms_utime + buf.tms_stime);
}
#else
# define clock()  (clock_t)0
#endif

void printTotals (const clock_t *const timeStamps)
{
	const unsigned long totalTags = TagFile.numTags.added +
									TagFile.numTags.prev;

	fprintf (errout, "%ld file%s, %ld line%s (%ld kB) scanned",
			Totals.files, plural (Totals.files),
			Totals.lines, plural (Totals.lines),
			Totals.bytes/1024L);
#ifdef CLOCK_AVAILABLE
	{
		const double interval = ((double) (timeStamps [1] - timeStamps [0])) /
								CLOCKS_PER_SEC;

		fprintf (errout, " in %.01f seconds", interval);
		if (interval != (double) 0.0)
			fprintf (errout, " (%lu kB/s)",
					(unsigned long) (Totals.bytes / interval) / 1024L);
	}
#endif
	fputc ('\n', errout);

	fprintf (errout, "%lu tag%s added to tag file",
			TagFile.numTags.added, plural (TagFile.numTags.added));
	if (Option.append)
		fprintf (errout, " (now %lu tags)", totalTags);
	fputc ('\n', errout);

	if (totalTags > 0  &&  Option.sorted != SO_UNSORTED)
	{
		fprintf (errout, "%lu tag%s sorted", totalTags, plural (totalTags));
#ifdef CLOCK_AVAILABLE
		fprintf (errout, " in %.02f seconds",
				((double) (timeStamps [2] - timeStamps [1])) / CLOCKS_PER_SEC);
#endif
		fputc ('\n', errout);
	}

#ifdef DEBUG
	fprintf (errout, "longest tag line = %lu\n",
			(unsigned long) TagFile.max.line);
#endif
}

/* etagsInclude moved to Rust - see rs_etagsInclude in options.rs */
extern int rs_etagsInclude(void);
#define etagsInclude() ((boolean)rs_etagsInclude())

void makeTags (cookedArgs *args)
{
	clock_t timeStamps [3];
	boolean resize = FALSE;
	boolean files = (boolean)(! cArgOff (args) || Option.fileList != NULL
							  || Option.filter);

	if (! files)
	{
		if (filesRequired ())
			error (FATAL, "No files specified. Try \"%s --help\".",
				getExecutableName ());
		else if (! Option.recurse && ! etagsInclude ())
			return;
	}

#define timeStamp(n) timeStamps[(n)]=(Option.printTotals ? clock():(clock_t)0)
	if (! Option.filter)
		openTagFile ();

	timeStamp (0);

	if (! cArgOff (args))
	{
		verbose ("Reading command line arguments\n");
		resize = createTagsForArgs (args);
	}
	if (Option.fileList != NULL)
	{
		verbose ("Reading list file\n");
		resize = (boolean) (createTagsFromListFile (Option.fileList) || resize);
	}
	if (Option.filter)
	{
		verbose ("Reading filter input\n");
		resize = (boolean) (createTagsFromFileInput (stdin, TRUE) || resize);
	}
	if (! files  &&  Option.recurse)
		resize = recurseIntoDirectory (".");

#if DEBUG3
	timeStamp (1);

	if (! Option.filter)
		closeTagFile (resize);

	timeStamp (2);

	if (Option.printTotals)
		printTotals (timeStamps);
#endif
#undef timeStamp
}

/*
 *		Start up code
 */

#if DEBUG
bool initialized = false;
extern int main_ffi (int __unused__ argc, char **argv)
#else
extern int main(int __unused__ argc, char **argv)
#endif
{
	cookedArgs *args;
#ifdef VMS
	extern int getredirection (int *ac, char ***av);

	/* do wildcard expansion and I/O redirection */
	getredirection (&argc, &argv);
#endif

#ifdef AMIGA
	/* This program doesn't work when started from the Workbench */
	if (argc == 0)
		exit (1);
#endif

#ifdef __EMX__
	_wildcard (&argc, &argv);  /* expand wildcards in argument list */
#endif

#if defined (macintosh) && BUILD_MPW_TOOL == 0
	argc = ccommand (&argv);
#endif

	PROFILE_START();
	setCurrentDirectory ();
	setExecutableName (*argv++);
	checkRegex ();

	args = cArgNewFromArgv (argv);
	previewFirstOption (args);
	testEtagsInvocation ();
	PROFILE_END("setup");

	PROFILE_START();
#if DEBUG
	if (!initialized) {
		initializeParsing ();
		initialized = true;
        }
#else
		initializeParsing ();
#endif
	PROFILE_END("initializeParsing");

	PROFILE_START();
	initOptions ();
	PROFILE_END("initOptions");

	PROFILE_START();
	readOptionConfiguration ();
	PROFILE_END("readOptionConfiguration");

	PROFILE_START();
	verbose ("Reading initial options from command line\n");
	parseOptions (args);
	PROFILE_END("parseOptions");

	PROFILE_START();
	checkOptions ();
	PROFILE_END("checkOptions");

	PROFILE_START();
	makeTags (args);
	PROFILE_END("makeTags");

	/*  Clean up.
	 */
	PROFILE_START();
	rs_cArgDelete (args);
	rs_freeKeywordTable ();
#ifndef DEBUG
	rs_freeRoutineResourcesDirect ();
	rs_freeSourceFileResourcesDirect ();
	rs_freeTagFileResourcesDirect ();
#endif
	rs_freeOptionResourcesDirect ();
#ifndef DEBUG
	rs_freeParserResourcesDirect ();
#endif
	rs_freeRegexResourcesDirect ();
	PROFILE_END("cleanup");

	return 0;
}

/* vi:set tabstop=4 shiftwidth=4: */
/*
*   $Id$
*
*   Copyright (c) 1996-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions to process command line options.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#define _GNU_SOURCE   /* for asprintf */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>  /* to declare isspace () */

#include "ctags.h"
#include "debug.h"
#include "main.h"
#define OPTION_WRITE
#include "options.h"
#include "parse.h"
#include "routines.h"

/*
*   MACROS
*/
#define INVOCATION  "Usage: %s [options] [file(s)]\n"

#define CTAGS_ENVIRONMENT  "CTAGS"
#define ETAGS_ENVIRONMENT  "ETAGS"

#define CTAGS_FILE  "tags"
#define ETAGS_FILE  "TAGS"

#ifndef ETAGS
# define ETAGS	"etags"  /* name which causes default use of to -e */
#endif

/*  The following separators are permitted for list options.
 */
#define EXTENSION_SEPARATOR '.'
#define PATTERN_START '('
#define PATTERN_STOP  ')'
#define IGNORE_SEPARATORS   ", \t\n"

#ifndef DEFAULT_FILE_FORMAT
# define DEFAULT_FILE_FORMAT  2
#endif

#if defined (HAVE_OPENDIR) || defined (HAVE_FINDFIRST) || defined (HAVE__FINDFIRST) || defined (AMIGA)
# define RECURSE_SUPPORTED
#endif

#define isCompoundOption(c)  (boolean) (strchr ("fohiILpDb", (c)) != NULL)

/*
*   Data declarations
*/

enum eOptionLimits {
	MaxHeaderExtensions	= 100,  /* maximum number of extensions in -h option */
	MaxSupportedTagFormat = 2
};

typedef struct sOptionDescription {
	int usedByEtags;
	const char *description;
} optionDescription;

typedef void (*parametricOptionHandler) (const char *const option, const char *const parameter);

typedef const struct {
	const char* name;   /* name of option as specified by user */
	parametricOptionHandler handler;  /* routine to handle option */
	boolean initOnly;   /* option must be specified before any files */
} parametricOption;

typedef struct {
	const char* name;   /* name of option as specified by user */
	boolean* pValue;    /* pointer to option value - initialized at runtime */
	boolean initOnly;   /* option must be specified before any files */
} booleanOption;

/*
*   DATA DEFINITIONS
*/

/* Option state variables - moved to Rust (cparser.rs) */
extern int rs_isNonOptionEncountered(void);
extern void rs_setNonOptionEncountered(int state);
extern int rs_isFilesRequired(void);
extern void rs_setFilesRequired(int state);
extern int rs_isSkipConfiguration(void);
extern void rs_setSkipConfiguration(int state);
#define NonOptionEncountered rs_isNonOptionEncountered()
#define FilesRequired rs_isFilesRequired()
#define SkipConfiguration rs_isSkipConfiguration()

/* OptionFiles and Excluded are now Rust-owned in options.rs
 * Accessed via rs_getOptionFiles() / rs_getExcluded() and setters
 * C code uses macros: #define OptionFiles (rs_getOptionFiles())
 *                     #define Excluded (rs_getExcluded())
 */

/* HeaderExtensions array moved to Rust - see HEADER_EXTENSIONS in options.rs
 * Access via rs_getHeaderExtensions() or HeaderExtensions macro
 */

/* Option global is now owned by Rust - see src/ctags_rs/options.rs
 * Accessed via rs_getOption() defined in options.h as:
 * #define Option (*rs_getOption())
 */

/* Option accessor functions (c_option*) and File accessor functions (c_get*)
 * have been moved to Rust. Now implemented in:
 *   - src/ctags_rs/options.rs (rs_option* functions)
 *   - src/ctags_rs/read.rs (rs_getSourceLanguage, etc.)
 * They access OPTION_INSTANCE and FILE_INSTANCE directly in Rust.
 */

/* Kind table accessors moved to Rust - see parse.rs for:
 * - rs_getCKinds(), rs_getCKindsCount(), rs_isCKindDefineEnabled()
 * - rs_getCsharpKinds(), rs_getCsharpKindsCount()
 * - rs_getJavaKinds(), rs_getJavaKindsCount()
 * - rs_getVeraKinds(), rs_getVeraKindsCount()
 */

/*
*   OTHER GLOBAL ACCESSOR FUNCTIONS
*/

/* Language globals now in Rust (Lang_* macros call rs_getLang*()) */
/* c_getLang* functions removed - Rust calls rs_getLang*() directly */

/* AnonymousID and CurrentStatement accessors removed - use rs_* functions directly */

/*
-   Locally used only
*/

optionDescription LongOptionDescription [] = {
 {1,"  -a   Append the tags to an existing tag file."},
#ifdef DEBUG
 {1,"  -b <line>"},
 {1,"       Set break line."},
#endif
 {0,"  -B   Use backward searching patterns (?...?)."},
#ifdef DEBUG
 {1,"  -D <level>"},
 {1,"       Set debug level."},
#endif
 {0,"  -e   Output tag file for use with Emacs."},
 {1,"  -f <name>"},
 {1,"       Write tags to specified file. Value of \"-\" writes tags to stdout"},
 {1,"       [\"tags\"; or \"TAGS\" when -e supplied]."},
 {0,"  -F   Use forward searching patterns (/.../) (default)."},
 {1,"  -h <list>"},
 {1,"       Specify list of file extensions to be treated as include files."},
 {1,"       [\".h.H.hh.hpp.hxx.h++\"]."},
 {1,"  -I <list|@file>"},
 {1,"       A list of tokens to be specially handled is read from either the"},
 {1,"       command line or the specified file."},
 {1,"  -L <file>"},
 {1,"       A list of source file names are read from the specified file."},
 {1,"       If specified as \"-\", then standard input is read."},
 {0,"  -n   Equivalent to --excmd=number."},
 {0,"  -N   Equivalent to --excmd=pattern."},
 {1,"  -o   Alternative for -f."},
#ifdef RECURSE_SUPPORTED
 {1,"  -R   Equivalent to --recurse."},
#else
 {1,"  -R   Not supported on this platform."},
#endif
 {0,"  -u   Equivalent to --sort=no."},
 {1,"  -V   Equivalent to --verbose."},
 {1,"  -x   Print a tabular cross reference file to standard output."},
 {1,"  --append=[yes|no]"},
 {1,"       Should tags should be appended to existing tag file [no]?"},
 {1,"  --config-filename=fileName"},
 {1,"      Use 'fileName' instead of 'ctags' in option file names."},
 {1,"  --etags-include=file"},
 {1,"      Include reference to 'file' in Emacs-style tag file (requires -e)."},
 {1,"  --exclude=pattern"},
 {1,"      Exclude files and directories matching 'pattern'."},
 {0,"  --excmd=number|pattern|mix"},
#ifdef MACROS_USE_PATTERNS
 {0,"       Uses the specified type of EX command to locate tags [pattern]."},
#else
 {0,"       Uses the specified type of EX command to locate tags [mix]."},
#endif
 {1,"  --extra=[+|-]flags"},
 {1,"      Include extra tag entries for selected information (flags: \"fq\")."},
 {1,"  --fields=[+|-]flags"},
 {1,"      Include selected extension fields (flags: \"afmikKlnsStz\") [fks]."},
 {1,"  --file-scope=[yes|no]"},
 {1,"       Should tags scoped only for a single file (e.g. \"static\" tags"},
 {1,"       be included in the output [yes]?"},
 {1,"  --filter=[yes|no]"},
 {1,"       Behave as a filter, reading file names from standard input and"},
 {1,"       writing tags to standard output [no]."},
 {1,"  --filter-terminator=string"},
 {1,"       Specify string to print to stdout following the tags for each file"},
 {1,"       parsed when --filter is enabled."},
 {0,"  --format=level"},
#if DEFAULT_FILE_FORMAT == 1
 {0,"       Force output of specified tag file format [1]."},
#else
 {0,"       Force output of specified tag file format [2]."},
#endif
 {1,"  --help"},
 {1,"       Print this option summary."},
 {1,"  --if0=[yes|no]"},
 {1,"       Should C code within #if 0 conditional branches be parsed [no]?"},
 {1,"  --<LANG>-kinds=[+|-]kinds"},
 {1,"       Enable/disable tag kinds for language <LANG>."},
 {1,"  --langdef=name"},
 {1,"       Define a new language to be parsed with regular expressions."},
 {1,"  --langmap=map(s)"},
 {1,"       Override default mapping of language to source file extension."},
 {1,"  --language-force=language"},
 {1,"       Force all files to be interpreted using specified language."},
 {1,"  --languages=[+|-]list"},
 {1,"       Restrict files scanned for tags to those mapped to langauges"},
 {1,"       specified in the comma-separated 'list'. The list can contain any"},
 {1,"       built-in or user-defined language [all]."},
 {1,"  --license"},
 {1,"       Print details of software license."},
 {0,"  --line-directives=[yes|no]"},
 {0,"       Should #line directives be processed [no]?"},
 {1,"  --links=[yes|no]"},
 {1,"       Indicate whether symbolic links should be followed [yes]."},
 {1,"  --list-kinds=[language|all]"},
 {1,"       Output a list of all tag kinds for specified language or all."},
 {1,"  --list-languages"},
 {1,"       Output list of supported languages."},
 {1,"  --list-maps=[language|all]"},
 {1,"       Output list of language mappings."},
 {1,"  --options=file"},
 {1,"       Specify file from which command line options should be read."},
 {1,"  --recurse=[yes|no]"},
#ifdef RECURSE_SUPPORTED
 {1,"       Recurse into directories supplied on command line [no]."},
#else
 {1,"       Not supported on this platform."},
#endif
#ifdef HAVE_REGEX
 {1,"  --regex-<LANG>=/line_pattern/name_pattern/[flags]"},
 {1,"       Define regular expression for locating tags in specific language."},
#endif
 {0,"  --sort=[yes|no|foldcase]"},
 {0,"       Should tags be sorted (optionally ignoring case) [yes]?."},
 {0,"  --tag-relative=[yes|no]"},
 {0,"       Should paths be relative to location of tag file [no; yes when -e]?"},
 {1,"  --totals=[yes|no]"},
 {1,"       Print statistics about source and tag files [no]."},
 {1,"  --verbose=[yes|no]"},
 {1,"       Enable verbose messages describing actions on each source file."},
 {1,"  --version"},
 {1,"       Print version identifier to standard output."},
 {1, NULL}
};

/* License strings moved to Rust - see LICENSE1/LICENSE2 in options.rs
 * Access via rs_getLicense1()/rs_getLicense2() or License1/License2 macros
 */

/*  Contains a set of strings describing the set of "features" compiled into
 *  the code.
 */
const char *const Features [] = {
#ifdef WIN32
	"win32",
#endif
#ifdef DJGPP
	"msdos_32",
#else
# ifdef MSDOS
	"msdos_16",
# endif
#endif
#ifdef OS2
	"os2",
#endif
#ifdef AMIGA
	"amiga",
#endif
#ifdef VMS
	"vms",
#endif
#ifdef HAVE_FNMATCH
	"wildcards",
#endif
#ifdef HAVE_REGEX
	"regex",
#endif
#ifndef EXTERNAL_SORT
	"internal-sort",
#endif
#ifdef CUSTOM_CONFIGURATION_FILE
	"custom-conf",
#endif
#if (defined (MSDOS) || defined (WIN32) || defined (OS2)) && defined (UNIX_PATH_SEPARATOR)
	"unix-path-separator",
#endif
#ifdef DEBUG
	"debug",
#endif
	NULL
};

/*
*   FUNCTION PROTOTYPES
*/
boolean parseFileOptions (const char *const fileName);

/*
*   FUNCTION DEFINITIONS
*/
#if (defined (__SVR4) && defined (__sun))
int vasprintf(char **ret, const char *format, va_list args)
{
	va_list copy;
	va_copy(copy, args);

	/* Make sure it is determinate, despite manuals indicating otherwise */
	*ret = 0;

	int count = vsnprintf(NULL, 0, format, args);
	if (count >= 0) {
		char* buffer = malloc(count + 1);
		if (buffer != NULL) {
			count = vsnprintf(buffer, count + 1, format, copy);
			if (count < 0)
				free(buffer);
			else
				*ret = buffer;
		}
	}
	va_end(args);  // Each va_start() or va_copy() needs a va_end()

	return count;
}

int asprintf(char **strp, const char *fmt, ...)
{
	s32 size;
	va_list args;
	va_start(args, fmt);
	size = vasprintf(strp, fmt, args);
	va_end(args);
	return size;
}
#endif

/* verbose - C wrapper handles varargs, calls Rust for logic */
extern void rs_verbose_impl(const char *message);
extern void verbose (const char *const format, ...)
{
	char buffer[1024];
	va_list ap;
	va_start (ap, format);
	vsnprintf (buffer, sizeof(buffer), format, ap);
	va_end (ap);
	rs_verbose_impl(buffer);
}

/* stringCopy moved to Rust - see rs_stringCopy in vstring.rs */
extern char *rs_stringCopy(const char *string);
#define stringCopy(s) rs_stringCopy(s)

/* freeString - now a macro delegating to Rust */
extern void rs_freeString(char **pString);
#define freeString(pString) rs_freeString((char **)(pString))

extern void setDefaultTagFileName (void)
{
	if (Option.tagFileName != NULL)
		;  /* accept given name */
	else if (Option.etags)
		Option.tagFileName = stringCopy (ETAGS_FILE);
	else
		Option.tagFileName = stringCopy (CTAGS_FILE);
}

/* filesRequired() function moved to Rust - see rs_filesRequired in cparser.rs
 * Declaration and macro at top of file (before first use)
 */

extern void checkOptions (void)
{
	const char* notice;
	if (Option.xref)
	{
		notice = "xref output";
		if (Option.include.fileNames)
		{
			error (WARNING, "%s disables file name tags", notice);
			Option.include.fileNames = FALSE;
		}
	}
	if (Option.append)
	{
		notice = "append mode is not compatible with";
		if (isDestinationStdout ())
			error (FATAL, "%s tags to stdout", notice);
	}
	if (Option.filter)
	{
		notice = "filter mode";
		if (Option.printTotals)
		{
			error (WARNING, "%s disables totals", notice);
			Option.printTotals = FALSE;
		}
		if (Option.tagFileName != NULL)
			error (WARNING, "%s ignores output tag file name", notice);
	}
}

void setEtagsMode (void)
{
	rs_setEtagsMode();
}

extern void testEtagsInvocation (void)
{
	char* const execName = eStrdup (getExecutableName ());
	char* const etags = eStrdup (ETAGS);
#ifdef CASE_INSENSITIVE_FILENAMES
	toLowerString (execName);
	toLowerString (etags);
#endif
	if (strstr (execName, etags) != NULL)
	{
		verbose ("Running in etags mode\n");
		setEtagsMode ();
	}
	eFree (execName);
	eFree (etags);
}

/*
 *  Cooked argument parsing
 */

void parseShortOption (cookedArgs *const args)
{
	args->simple [0] = *args->shortOptions++;
	args->simple [1] = '\0';
	args->item = args->simple;
	if (! isCompoundOption (*args->simple))
		args->parameter = "";
	else if (*args->shortOptions == '\0')
	{
		rs_argForth (args->args);
		if (rs_argOff (args->args))
			args->parameter = NULL;
		else
			args->parameter = rs_argItem (args->args);
		args->shortOptions = NULL;
	}
	else
	{
		args->parameter = args->shortOptions;
		args->shortOptions = NULL;
	}
}

void parseLongOption (cookedArgs *const args, const char *item)
{
	const char* const equal = strchr (item, '=');
	if (equal == NULL)
	{
		args->item = eStrdup (item); /* FIXME: memory leak. */
		args->parameter = "";
	}
	else
	{
		const size_t length = equal - item;
		args->item = xMalloc (length + 1, char); /* FIXME: memory leak. */
		strncpy (args->item, item, length);
		args->item [length] = '\0';
		args->parameter = equal + 1;
	}
	Assert (args->item != NULL);
	Assert (args->parameter != NULL);
}

void cArgRead (cookedArgs *const current)
{
	char* item;

	Assert (current != NULL);
	if (! rs_argOff (current->args))
	{
		item = rs_argItem (current->args);
		current->shortOptions = NULL;
		Assert (item != NULL);
		if (strncmp (item, "--", (size_t) 2) == 0)
		{
			current->isOption = TRUE;
			current->longOption = TRUE;
			parseLongOption (current, item + 2);
			Assert (current->item != NULL);
			Assert (current->parameter != NULL);
		}
		else if (*item == '-')
		{
			current->isOption = TRUE;
			current->longOption = FALSE;
			current->shortOptions = item + 1;
			parseShortOption (current);
		}
		else
		{
			current->isOption = FALSE;
			current->longOption = FALSE;
			current->item = item;
			current->parameter = NULL;
		}
	}
}

extern cookedArgs* cArgNewFromString (const char* string)
{
	cookedArgs* const result = xMalloc (1, cookedArgs);
	memset (result, 0, sizeof (cookedArgs));
	result->args = rs_argNewFromString (string);
	cArgRead (result);
	return result;
}

extern cookedArgs* cArgNewFromArgv (char* const* const argv)
{
	cookedArgs* const result = xMalloc (1, cookedArgs);
	memset (result, 0, sizeof (cookedArgs));
	result->args = rs_argNewFromArgv (argv);
	cArgRead (result);
	return result;
}

extern cookedArgs* cArgNewFromFile (FILE* const fp)
{
	cookedArgs* const result = xMalloc (1, cookedArgs);
	memset (result, 0, sizeof (cookedArgs));
	result->args = rs_argNewFromFile ((void*)fp);
	cArgRead (result);
	return result;
}

extern cookedArgs* cArgNewFromLineFile (FILE* const fp)
{
	cookedArgs* const result = xMalloc (1, cookedArgs);
	memset (result, 0, sizeof (cookedArgs));
	result->args = rs_argNewFromLineFile ((void*)fp);
	cArgRead (result);
	return result;
}

boolean cArgOptionPending (cookedArgs* const current)
{
	boolean result = FALSE;
	if (current->shortOptions != NULL)
		if (*current->shortOptions != '\0')
			result = TRUE;
	return result;
}

extern boolean cArgOff (cookedArgs* const current)
{
	Assert (current != NULL);
	return (boolean) (rs_argOff (current->args) && ! cArgOptionPending (current));
}

extern boolean cArgIsOption (cookedArgs* const current)
{
	Assert (current != NULL);
	return current->isOption;
}

extern const char* cArgItem (cookedArgs* const current)
{
	Assert (current != NULL);
	return current->item;
}

extern void cArgForth (cookedArgs* const current)
{
	Assert (current != NULL);
	Assert (! cArgOff (current));
	if (cArgOptionPending (current))
		parseShortOption (current);
	else
	{
		Assert (! rs_argOff (current->args));
		rs_argForth (current->args);
		if (! rs_argOff (current->args))
			cArgRead (current);
		else
		{
			current->isOption = FALSE;
			current->longOption = FALSE;
			current->shortOptions = NULL;
			current->item = NULL;
			current->parameter = NULL;
		}
	}
}

/*
 *  File extension and language mapping
 */

void addExtensionList (
		stringList *const slist, const char *const elist, const boolean clear)
{
	char *const extensionList = eStrdup (elist);
	const char *extension = NULL;
	boolean first = TRUE;

	if (clear)
	{
		verbose ("      clearing\n");
		rs_stringListClear (slist);
	}
	verbose ("      adding: ");
	if (elist != NULL  &&  *elist != '\0')
	{
		extension = extensionList;
		if (elist [0] == EXTENSION_SEPARATOR)
			++extension;
	}
	while (extension != NULL)
	{
		char *separator = strchr (extension, EXTENSION_SEPARATOR);
		if (separator != NULL)
			*separator = '\0';
		verbose ("%s%s", first ? "" : ", ",
				*extension == '\0' ? "(NONE)" : extension);
		rs_stringListAdd (slist, rs_vStringNewInit (extension));
		first = FALSE;
		if (separator == NULL)
			extension = NULL;
		else
			extension = separator + 1;
	}
	if (Option.verbose)
	{
		printf ("\n      now: ");
		rs_stringListPrint (slist);
		putchar ('\n');
	}
	eFree (extensionList);
}

/*  Determines whether the specified file name is considered to be a header
 *  file for the purposes of determining whether enclosed tags are global or
 *  static.
 */
extern boolean isIncludeFile (const char *const fileName)
{
	boolean result = FALSE;
	const char *const extension = fileExtension (fileName);
	if (Option.headerExt != NULL)
		result = rs_stringListExtensionMatched (Option.headerExt, extension);
	return result;
}

/*
 *  Specific option processing
 */

 static void processConfigFilenameOption (
 		const char *const option __unused__, const char *const parameter)
 {
 	freeString (&Option.configFilename);
 	Option.configFilename = stringCopy (parameter);
 }

void processEtagsInclude (
		const char *const option, const char *const parameter)
{
	if (! Option.etags)
		error (FATAL, "Etags must be enabled to use \"%s\" option", option);
	else
	{
		vString *const file = rs_vStringNewInit (parameter);
		if (Option.etagsInclude == NULL)
			Option.etagsInclude = rs_stringListNew ();
		rs_stringListAdd (Option.etagsInclude, file);
		rs_setFilesRequired(FALSE);
	}
}

void processExcludeOption (
		const char *const option __unused__, const char *const parameter)
{
	const char *const fileName = parameter + 1;
	if (parameter [0] == '\0')
		rs_freeList (rs_getExcludedPtr());
	else if (parameter [0] == '@')
	{
		stringList* const sl = stringListNewFromFile (fileName);
		if (sl == NULL)
			error (FATAL | PERROR, "cannot open \"%s\"", fileName);
		if (Excluded == NULL)
			rs_setExcluded(sl);
		else
			rs_stringListCombine (Excluded, sl);
		verbose ("    adding exclude patterns from %s\n", fileName);
	}
	else
	{
		vString *const item = rs_vStringNewInit (parameter);
		if (Excluded == NULL)
			rs_setExcluded(rs_stringListNew ());
		rs_stringListAdd (Excluded, item);
		verbose ("    adding exclude pattern: %s\n", parameter);
	}
}

extern boolean isExcludedFile (const char* const name)
{
	const char* base = baseFilename (name);
	boolean result = FALSE;
	if (Excluded != NULL)
	{
		result = rs_stringListFileMatched (Excluded, base);
		if (! result  &&  name != base)
			result = rs_stringListFileMatched (Excluded, name);
	}
#ifdef AMIGA
	/* not a good solution, but the only one which works often */
	if (! result)
		result = (boolean) (strcmp (name, TagFile.name) == 0);
#endif
	return result;
}

void processExcmdOption (
		const char *const option, const char *const parameter)
{
	switch (*parameter)
	{
		case 'm': Option.locate = EX_MIX;     break;
		case 'n': Option.locate = EX_LINENUM; break;
		case 'p': Option.locate = EX_PATTERN; break;
		default:
			error (FATAL, "Invalid value for \"%s\" option", option);
			break;
	}
}

void processExtraTagsOption (
		const char *const option, const char *const parameter)
{
	struct sInclude *const inc = &Option.include;
	const char *p = parameter;
	boolean mode = TRUE;
	int c;

	if (*p != '+'  &&  *p != '-')
	{
		inc->fileNames     = FALSE;
		inc->qualifiedTags = FALSE;
	}
	while ((c = *p++) != '\0') switch (c)
	{
		case '+': mode = TRUE;                break;
		case '-': mode = FALSE;               break;

		case 'f': inc->fileNames     = mode;  break;
		case 'q': inc->qualifiedTags = mode;  break;

		default: error(WARNING, "Unsupported parameter '%c' for \"%s\" option",
					   c, option);
			break;
	}
}

void processFieldsOption (
		const char *const option, const char *const parameter)
{
	struct sExtFields *field = &Option.extensionFields;
	const char *p = parameter;
	boolean mode = TRUE;
	int c;

	if (*p != '+'  &&  *p != '-')
	{
		field->access           = FALSE;
		field->fileScope        = FALSE;
		field->implementation   = FALSE;
		field->inheritance      = FALSE;
		field->kind             = FALSE;
		field->kindKey          = FALSE;
		field->kindLong         = FALSE;
		field->language         = FALSE;
		field->scope            = FALSE;
		field->typeRef          = FALSE;
	}
	while ((c = *p++) != '\0') switch (c)
	{
		case '+': mode = TRUE;                  break;
		case '-': mode = FALSE;                 break;

		case 'a': field->access         = mode; break;
		case 'f': field->fileScope      = mode; break;
		case 'm': field->implementation = mode; break;
		case 'i': field->inheritance    = mode; break;
		case 'k': field->kind           = mode; break;
		case 'K': field->kindLong       = mode; break;
		case 'l': field->language       = mode; break;
		case 'n': field->lineNumber     = mode; break;
		case 's': field->scope          = mode; break;
		case 'S': field->signature      = mode; break;
		case 'z': field->kindKey        = mode; break;
		case 't': field->typeRef        = mode; break;

		default: error(WARNING, "Unsupported parameter '%c' for \"%s\" option",
					c, option);
			break;
	}
}

void processFilterTerminatorOption (
		const char *const option __unused__, const char *const parameter)
{
	freeString (&Option.filterTerminator);
	Option.filterTerminator = stringCopy (parameter);
}

void processFormatOption (
		const char *const option, const char *const parameter)
{
	unsigned int format;

	if (sscanf (parameter, "%u", &format) < 1)
		error (FATAL, "Invalid value for \"%s\" option",option);
	else if (format <= (unsigned int) MaxSupportedTagFormat)
		Option.tagFileFormat = format;
	else
		error (FATAL, "Unsupported value for \"%s\" option", option);
}

void printInvocationDescription (void)
{
	printf (INVOCATION, getExecutableName ());
}

void printOptionDescriptions (const optionDescription *const optDesc)
{
	int i;
	for (i = 0 ; optDesc [i].description != NULL ; ++i)
	{
		if (! Option.etags || optDesc [i].usedByEtags)
			puts (optDesc [i].description);
	}
}

void printFeatureList (void)
{
	int i;

	for (i = 0 ; Features [i] != NULL ; ++i)
	{
		if (i == 0)
			printf ("  Optional compiled features: ");
		printf ("%s+%s", (i>0 ? ", " : ""), Features [i]);
#ifdef CUSTOM_CONFIGURATION_FILE
		if (strcmp (Features [i], "custom-conf") == 0)
			printf ("=%s", CUSTOM_CONFIGURATION_FILE);
#endif
	}
	if (i > 0)
		putchar ('\n');
}

void printProgramIdentification (void)
{
	printf ("%s %s, %s %s\n",
	        PROGRAM_NAME, PROGRAM_VERSION,
	        PROGRAM_COPYRIGHT, AUTHOR_NAME);
	printf ("  Compiled: %s, %s\n", __DATE__, __TIME__);
	printf ("  Addresses: <%s>, %s\n", AUTHOR_EMAIL, PROGRAM_URL);
	printFeatureList ();
}

void processHelpOption (
		const char *const option __unused__,
		const char *const parameter __unused__)
{
	printProgramIdentification ();
	putchar ('\n');
	printInvocationDescription ();
	putchar ('\n');
	printOptionDescriptions (LongOptionDescription);
	exit (0);
}

void processLanguageForceOption (
		const char *const option, const char *const parameter)
{
	langType language;
	if (strcasecmp (parameter, "auto") == 0)
		language = LANG_AUTO;
	else
		language = getNamedLanguage (parameter);

	if (strcmp (option, "lang") == 0  ||  strcmp (option, "language") == 0)
		error (WARNING,
			   "\"--%s\" option is obsolete; use \"--language-force\" instead",
			   option);
	if (language == LANG_IGNORE)
		error (FATAL, "Unknown language \"%s\" in \"%s\" option", parameter, option);
	else
		Option.language = language;
}
char* skipPastMap (char* p)
{
	while (*p != EXTENSION_SEPARATOR  &&
			*p != PATTERN_START  &&  *p != ','  &&  *p != '\0')
		++p;
	return p;
}

/* Parses the mapping beginning at `map', adds it to the language map, and
 * returns first character past the map.
 */
char* addLanguageMap (const langType language, char* map)
{
	char* p = NULL;
	const char first = *map;
	if (first == EXTENSION_SEPARATOR)  /* extension map */
	{
		++map;
		p = skipPastMap (map);
		if (*p == '\0')
		{
			verbose (" .%s", map);
			addLanguageExtensionMap (language, map);
			p = map + strlen (map);
		}
		else
		{
			const char separator = *p;
			*p = '\0';
			verbose (" .%s", map);
			addLanguageExtensionMap (language, map);
			*p = separator;
		}
	}
	else if (first == PATTERN_START)  /* pattern map */
	{
		++map;
		for (p = map  ;  *p != PATTERN_STOP  &&  *p != '\0'  ;  ++p)
		{
			if (*p == '\\'  &&  *(p + 1) == PATTERN_STOP)
				++p;
		}
		if (*p == '\0')
			error (FATAL, "Unterminated file name pattern for %s language",
			   getLanguageName (language));
		else
		{
			*p++ = '\0';
			verbose (" (%s)", map);
			addLanguagePatternMap (language, map);
		}
	}
	else
		error (FATAL, "Badly formed language map for %s language",
				getLanguageName (language));
	return p;
}

char* processLanguageMap (char* map)
{
	char* const separator = strchr (map, ':');
	char* result = NULL;
	if (separator != NULL)
	{
		langType language;
		char *list = separator + 1;
		boolean clear = FALSE;
		*separator = '\0';
		language = getNamedLanguage (map);
		if (language != LANG_IGNORE)
		{
			const char *const deflt = "default";
			char* p;
			if (*list == '+')
				++list;
			else
				clear = TRUE;
			for (p = list  ;  *p != ','  &&  *p != '\0'  ;  ++p)  /*no-op*/ ;
			if ((size_t) (p - list) == strlen (deflt) &&
				strncasecmp (list, deflt, p - list) == 0)
			{
				verbose ("    Restoring default %s language map: ", getLanguageName (language));
				installLanguageMapDefault (language);
				list = p;
			}
			else
			{
				if (clear)
				{
					verbose ("    Setting %s language map:", getLanguageName (language));
					clearLanguageMap (language);
				}
				else
					verbose ("    Adding to %s language map:", getLanguageName (language));
				while (list != NULL  &&  *list != '\0'  &&  *list != ',')
					list = addLanguageMap (language, list);
				verbose ("\n");
			}
			if (list != NULL  &&  *list == ',')
				++list;
			result = list;
		}
	}
	return result;
}

void processLanguageMapOption (
		const char *const option, const char *const parameter)
{
	char *const maps = eStrdup (parameter);
	char *map = maps;

	if (strcmp (parameter, "default") == 0)
	{
		verbose ("    Restoring default language maps:\n");
		installLanguageMapDefaults ();
	}
	else while (map != NULL  &&  *map != '\0')
	{
		char* const next = processLanguageMap (map);
		if (next == NULL)
			error (WARNING, "Unknown language \"%s\" in \"%s\" option", parameter, option);
		map = next;
	}
	eFree (maps);
}

void processLanguagesOption (
		const char *const option, const char *const parameter)
{
	char *const langs = eStrdup (parameter);
	enum { Add, Remove, Replace } mode = Replace;
	boolean first = TRUE;
	char *lang = langs;
	const char* prefix = "";
	verbose ("    Enabled languages: ");
	while (lang != NULL)
	{
		char *const end = strchr (lang, ',');
		if (lang [0] == '+')
		{
			++lang;
			mode = Add;
			prefix = "+ ";
		}
		else if (lang [0] == '-')
		{
			++lang;
			mode = Remove;
			prefix = "- ";
		}
		if (mode == Replace)
			enableLanguages (FALSE);
		if (end != NULL)
			*end = '\0';
		if (lang [0] != '\0')
		{
			if (strcmp (lang, "all") == 0)
				enableLanguages ((boolean) (mode != Remove));
			else
			{
				const langType language = getNamedLanguage (lang);
				if (language == LANG_IGNORE)
					error (WARNING, "Unknown language \"%s\" in \"%s\" option", lang, option);
				else
					enableLanguage (language, (boolean) (mode != Remove));
			}
			verbose ("%s%s%s", (first ? "" : ", "), prefix, lang);
			prefix = "";
			first = FALSE;
			if (mode == Replace)
				mode = Add;
		}
		lang = (end != NULL ? end + 1 : NULL);
	}
	verbose ("\n");
	eFree (langs);
}

void processLicenseOption (
		const char *const option __unused__,
		const char *const parameter __unused__)
{
	printProgramIdentification ();
	puts ("");
	puts (License1);
	puts (License2);
	exit (0);
}

void processListKindsOption (
		const char *const option, const char *const parameter)
{
	if (parameter [0] == '\0' || strcasecmp (parameter, "all") == 0)
	    printLanguageKinds (LANG_AUTO);
	else
	{
		langType language = getNamedLanguage (parameter);
		if (language == LANG_IGNORE)
			error (FATAL, "Unknown language \"%s\" in \"%s\" option", parameter, option);
		else
			printLanguageKinds (language);
	}
	exit (0);
}

void processListMapsOption (
		const char *const __unused__ option,
		const char *const __unused__ parameter)
{
	if (parameter [0] == '\0' || strcasecmp (parameter, "all") == 0)
	    printLanguageMaps (LANG_AUTO);
	else
	{
		langType language = getNamedLanguage (parameter);
		if (language == LANG_IGNORE)
			error (FATAL, "Unknown language \"%s\" in \"%s\" option", parameter, option);
		else
			printLanguageMaps (language);
	}
	exit (0);
}

void processListLanguagesOption (
		const char *const option __unused__,
		const char *const parameter __unused__)
{
	printLanguageList ();
	exit (0);
}

void processOptionFile (
		const char *const option, const char *const parameter)
{
	if (parameter [0] == '\0')
		error (WARNING, "no option file supplied for \"%s\"", option);
	else if (! parseFileOptions (parameter))
		error (FATAL | PERROR, "cannot open option file \"%s\"", parameter);
}

void processSortOption (
		const char *const option, const char *const parameter)
{
	if (rs_isFalse (parameter))
		Option.sorted = SO_UNSORTED;
	else if (rs_isTrue (parameter))
		Option.sorted = SO_SORTED;
	else if (strcasecmp (parameter, "f") == 0 ||
			strcasecmp (parameter, "fold") == 0 ||
			strcasecmp (parameter, "foldcase") == 0)
		Option.sorted = SO_FOLDSORTED;
	else
		error (FATAL, "Invalid value for \"%s\" option", option);
}

void installHeaderListDefaults (void)
{
	Option.headerExt = rs_stringListNewFromArgv (HeaderExtensions);
	if (Option.verbose)
	{
		printf ("    Setting default header extensions: ");
		rs_stringListPrint (Option.headerExt);
		putchar ('\n');
	}
}

void processHeaderListOption (const int option, const char *parameter)
{
	/*  Check to make sure that the user did not enter "ctags -h *.c"
	 *  by testing to see if the list is a filename that exists.
	 */
	if (doesFileExist (parameter))
		error (FATAL, "-%c: Invalid list", option);
	if (strcmp (parameter, "default") == 0)
		installHeaderListDefaults ();
	else
	{
		boolean clear = TRUE;

		if (parameter [0] == '+')
		{
			++parameter;
			clear = FALSE;
		}
		if (Option.headerExt == NULL)
			Option.headerExt = rs_stringListNew ();
		verbose ("    Header Extensions:\n");
		addExtensionList (Option.headerExt, parameter, clear);
	}
}

/*
 *  Token ignore processing
 */

/* isIgnoreToken - now implemented in Rust (cparser.rs) */
extern boolean isIgnoreToken (
		const char *const name, boolean *const pIgnoreParens,
		const char **const replacement);

void saveIgnoreToken (vString *const ignoreToken)
{
	if (Option.ignore == NULL)
		Option.ignore = rs_stringListNew ();
	rs_stringListAdd (Option.ignore, ignoreToken);
	verbose ("    ignore token: %s\n", vStringValue (ignoreToken));
}

void readIgnoreList (const char *const list)
{
	char* newList = stringCopy (list);
	const char *token = strtok (newList, IGNORE_SEPARATORS);

	while (token != NULL)
	{
		vString *const entry = rs_vStringNewInit (token);

		saveIgnoreToken (entry);
		token = strtok (NULL, IGNORE_SEPARATORS);
	}
	eFree (newList);
}

void addIgnoreListFromFile (const char *const fileName)
{
	stringList* tokens = stringListNewFromFile (fileName);
	if (tokens == NULL)
		error (FATAL | PERROR, "cannot open \"%s\"", fileName);
	if (Option.ignore == NULL)
		Option.ignore = tokens;
	else
		rs_stringListCombine (Option.ignore, tokens);
}

void processIgnoreOption (const char *const list)
{
	if (strchr ("@./\\", list [0]) != NULL)
	{
		const char* fileName = (*list == '@') ? list + 1 : list;
		addIgnoreListFromFile (fileName);
	}
#if defined (MSDOS) || defined (WIN32) || defined (OS2)
	else if (isalpha (list [0])  &&  list [1] == ':')
		addIgnoreListFromFile (list);
#endif
	else if (strcmp (list, "-") == 0)
	{
		rs_freeList (&Option.ignore);
		verbose ("    clearing list\n");
	}
	else
		readIgnoreList (list);
}

void processVersionOption (
		const char *const option __unused__,
		const char *const parameter __unused__)
{
	printProgramIdentification ();
	exit (0);
}

/*
 *  Option tables
 */

parametricOption ParametricOptions [] = {
	{ "config-filename",      	processConfigFilenameOption,  	TRUE    },
	{ "etags-include",          processEtagsInclude,            FALSE   },
	{ "exclude",                processExcludeOption,           FALSE   },
	{ "excmd",                  processExcmdOption,             FALSE   },
	{ "extra",                  processExtraTagsOption,         FALSE   },
	{ "fields",                 processFieldsOption,            FALSE   },
	{ "filter-terminator",      processFilterTerminatorOption,  TRUE    },
	{ "format",                 processFormatOption,            TRUE    },
	{ "help",                   processHelpOption,              TRUE    },
	{ "lang",                   processLanguageForceOption,     FALSE   },
	{ "language",               processLanguageForceOption,     FALSE   },
	{ "language-force",         processLanguageForceOption,     FALSE   },
	{ "languages",              processLanguagesOption,         FALSE   },
	{ "langdef",                processLanguageDefineOption,    FALSE   },
	{ "langmap",                processLanguageMapOption,       FALSE   },
	{ "license",                processLicenseOption,           TRUE    },
	{ "list-kinds",             processListKindsOption,         TRUE    },
	{ "list-maps",              processListMapsOption,          TRUE    },
	{ "list-languages",         processListLanguagesOption,     TRUE    },
	{ "options",                processOptionFile,              FALSE   },
	{ "sort",                   processSortOption,              TRUE    },
	{ "version",                processVersionOption,           TRUE    },
};

/* BooleanOptions: pValue pointers initialized at runtime in initBooleanOptions()
 * because Option may be a macro that dereferences a function call.
 */
booleanOption BooleanOptions [] = {
	{ "append",         NULL,                           TRUE    },
	{ "file-scope",     NULL,                           FALSE   },
	{ "file-tags",      NULL,                           FALSE   },
	{ "filter",         NULL,                           TRUE    },
	{ "if0",            NULL,                           FALSE   },
	{ "kind-long",      NULL,                           TRUE    },
	{ "line-directives",NULL,                           FALSE   },
	{ "links",          NULL,                           FALSE   },
#ifdef RECURSE_SUPPORTED
	{ "recurse",        NULL,                           FALSE   },
#endif
	{ "tag-relative",   NULL,                           TRUE    },
	{ "totals",         NULL,                           TRUE    },
	{ "verbose",        NULL,                           FALSE   },
};

/* Initialize BooleanOptions pValue pointers at runtime.
 * This is needed because Option may be defined as a macro that
 * calls a function, which cannot be used in static initializers.
 */
static void initBooleanOptions (void)
{
	int i = 0;
	BooleanOptions[i++].pValue = &Option.append;
	BooleanOptions[i++].pValue = &Option.include.fileScope;
	BooleanOptions[i++].pValue = &Option.include.fileNames;
	BooleanOptions[i++].pValue = &Option.filter;
	BooleanOptions[i++].pValue = &Option.if0;
	BooleanOptions[i++].pValue = &Option.kindLong;
	BooleanOptions[i++].pValue = &Option.lineDirectives;
	BooleanOptions[i++].pValue = &Option.followLinks;
#ifdef RECURSE_SUPPORTED
	BooleanOptions[i++].pValue = &Option.recurse;
#endif
	BooleanOptions[i++].pValue = &Option.tagRelative;
	BooleanOptions[i++].pValue = &Option.printTotals;
	BooleanOptions[i++].pValue = &Option.verbose;
}

/*
 *  Generic option parsing
 */

void checkOptionOrder (const char* const option)
{
	if (NonOptionEncountered)
		error (FATAL, "-%s option may not follow a file name", option);
}

boolean processParametricOption (
		const char *const option, const char *const parameter)
{
	const int count = sizeof (ParametricOptions) / sizeof (parametricOption);
	boolean found = FALSE;
	int i;

	for (i = 0  ;  i < count  &&  ! found  ;  ++i)
	{
		parametricOption* const entry = &ParametricOptions [i];
		if (strcmp (option, entry->name) == 0)
		{
			found = TRUE;
			if (entry->initOnly)
				checkOptionOrder (option);
			(entry->handler) (option, parameter);
		}
	}
	return found;
}

boolean getBooleanOption (
		const char *const option, const char *const parameter)
{
	boolean selection = TRUE;

	if (parameter [0] == '\0')
		selection = TRUE;
	else if (rs_isFalse (parameter))
		selection = FALSE;
	else if (rs_isTrue (parameter))
		selection = TRUE;
	else
		error (FATAL, "Invalid value for \"%s\" option", option);

	return selection;
}

boolean processBooleanOption (
		const char *const option, const char *const parameter)
{
	const int count = sizeof (BooleanOptions) / sizeof (booleanOption);
	boolean found = FALSE;
	int i;

	for (i = 0  ;  i < count  &&  ! found  ;  ++i)
	{
		booleanOption* const entry = &BooleanOptions [i];
		if (strcmp (option, entry->name) == 0)
		{
			found = TRUE;
			if (entry->initOnly)
				checkOptionOrder (option);
			*entry->pValue = getBooleanOption (option, parameter);
		}
	}
	return found;
}

void processLongOption (
		const char *const option, const char *const parameter)
{
	Assert (parameter != NULL);
	if (parameter == NULL  &&  parameter [0] == '\0')
		verbose ("  Option: --%s\n", option);
	else
		verbose ("  Option: --%s=%s\n", option, parameter);

	if (processBooleanOption (option, parameter))
		;
	else if (processParametricOption (option, parameter))
		;
	else if (processKindOption (option, parameter))
		;
	else if (processRegexOption (option, parameter))
		;
#ifndef RECURSE_SUPPORTED
	else if (strcmp (option, "recurse") == 0)
		error (WARNING, "%s option not supported on this host", option);
#endif
	else
		error (FATAL, "Unknown option: --%s", option);
}

void processShortOption (
		const char *const option, const char *const parameter)
{
	if (parameter == NULL  ||  parameter [0] == '\0')
		verbose ("  Option: -%s\n", option);
	else
		verbose ("  Option: -%s %s\n", option, parameter);

	if (isCompoundOption (*option) && (parameter == NULL  ||  parameter [0] == '\0'))
		error (FATAL, "Missing parameter for \"%s\" option", option);
	else switch (*option)
	{
		case '?':
			processHelpOption ("?", NULL);
			exit (0);
			break;
		case 'a':
			checkOptionOrder (option);
			Option.append = TRUE;
			break;
#ifdef DEBUG
		case 'b':
			if (atol (parameter) < 0)
				error (FATAL, "-%s: Invalid line number", option);
			Option.breakLine = atol (parameter);
			break;
		case 'D':
			Option.debugLevel = strtol (parameter, NULL, 0);
			if (debug (DEBUG_STATUS))
				Option.verbose = TRUE;
			break;
#endif
		case 'B':
			Option.backward = TRUE;
			break;
		case 'e':
			checkOptionOrder (option);
			setEtagsMode ();
			break;
		case 'f':
		case 'o':
			checkOptionOrder (option);
			if (Option.tagFileName != NULL)
			{
				error (WARNING,
					"-%s option specified more than once, last value used",
					option);
				freeString (&Option.tagFileName);
			}
			else if (parameter [0] == '-'  &&  parameter [1] != '\0')
				error (FATAL, "output file name may not begin with a '-'");
			Option.tagFileName = stringCopy (parameter);
			break;
		case 'F':
			Option.backward = FALSE;
			break;
		case 'h':
			processHeaderListOption (*option, parameter);
			break;
		case 'I':
			processIgnoreOption (parameter);
			break;
		case 'L':
			if (Option.fileList != NULL)
			{
				error (WARNING,
					"-%s option specified more than once, last value used",
					option);
				freeString (&Option.fileList);
			}
			Option.fileList = stringCopy (parameter);
			break;
		case 'n':
			Option.locate = EX_LINENUM;
			break;
		case 'N':
			Option.locate = EX_PATTERN;
			break;
		case 'R':
#ifdef RECURSE_SUPPORTED
			Option.recurse = TRUE;
#else
			error (WARNING, "-%s option not supported on this host", option);
#endif
			break;
		case 'u':
			checkOptionOrder (option);
			Option.sorted = SO_UNSORTED;
			break;
		case 'V':
			Option.verbose = TRUE;
			break;
		case 'w':
			/* silently ignored */
			break;
		case 'x':
			checkOptionOrder (option);
			Option.xref = TRUE;
			break;
		default:
			error (FATAL, "Unknown option: -%s", option);
			break;
	}
}

extern void parseOption (cookedArgs* const args)
{
	Assert (! cArgOff (args));
	if (args->isOption)
	{
		if (args->longOption)
			processLongOption (args->item, args->parameter);
		else
		{
			const char *parameter = args->parameter;
			while (*parameter == ' ')
				++parameter;
			processShortOption (args->item, parameter);
		}
		cArgForth (args);
	}
}

extern void parseOptions (cookedArgs* const args)
{
	rs_setNonOptionEncountered(FALSE);
	while (! cArgOff (args)  &&  cArgIsOption (args))
		parseOption (args);
	if (! cArgOff (args)  &&  ! cArgIsOption (args))
		rs_setNonOptionEncountered(TRUE);
}

/* CheckFile moved to Rust - see CHECK_FILE in options.rs
 * Access via rs_getCheckFile()/rs_setCheckFile() or CheckFile macro
 */
extern const char *rs_getCheckFile(void);
extern void rs_setCheckFile(const char *file);
#define CheckFile (rs_getCheckFile())

boolean checkSameFile (const char *const fileName)
{
	return isSameFile (CheckFile, fileName);
}

boolean parseFileOptions (const char* const fileName)
{
	boolean fileFound = FALSE;
	const char* const format = "Considering option file %s: %s\n";
	rs_setCheckFile(fileName);
	if (rs_stringListHasTest (OptionFiles, (int (*)(const char *))checkSameFile))
		verbose (format, fileName, "already considered");
	else
	{
		FILE* const fp = fopen (fileName, "r");
		if (fp == NULL)
			verbose (format, fileName, "not found");
		else
		{
			cookedArgs* const args = cArgNewFromLineFile (fp);
			vString* file = rs_vStringNewInit (fileName);
			rs_stringListAdd (OptionFiles, file);
			verbose (format, fileName, "reading...");
			parseOptions (args);
			if (NonOptionEncountered)
				error (WARNING, "Ignoring non-option in %s\n", fileName);
			rs_cArgDelete (args);
			fclose (fp);
			fileFound = TRUE;
		}
	}
	return fileFound;
}

/* Actions to be taken before reading any other options */
extern void previewFirstOption (cookedArgs* const args)
{
	while (cArgIsOption (args))
	{
		if (strcmp (args->item, "V") == 0 || strcmp (args->item, "verbose") == 0 || strcmp (args->item, "config-filename") == 0 )
			parseOption (args);
		else if (strcmp (args->item, "options") == 0  &&
				strcmp (args->parameter, "NONE") == 0)
		{
			fprintf (stderr, "No options will be read from files or environment\n");
			rs_setSkipConfiguration(TRUE);
			cArgForth (args);
		}
		else
			break;
	}
}

void parseConfigurationFileOptionsInDirectoryWithLeafname (const char* directory, const char* leafname)
{
	vString* const pathname = combinePathAndFile (directory, leafname);
	parseFileOptions (vStringValue (pathname));
	rs_vStringDelete (pathname);
}

void parseConfigurationFileOptionsInDirectory (const char* directory)
{
	char	*leafname = NULL;
	
	asprintf (&leafname,".%s",(Option.configFilename)?Option.configFilename:"ctags");
	parseConfigurationFileOptionsInDirectoryWithLeafname (directory, leafname);
	free (leafname);
#ifdef MSDOS_STYLE_PATH
	asprintf (&leafname,"%s.cnf",(Option.configFilename)?Option.configFilename:"ctags");
	parseConfigurationFileOptionsInDirectoryWithLeafname (directory, leafname);
	free (leafname);
#endif
}

void parseConfigurationFileOptions (void)
{
	/* We parse .ctags on all systems, and additionally ctags.cnf on DOS. */
	const char* const home = getenv ("HOME");
	char *filename = NULL;
	
#ifdef CUSTOM_CONFIGURATION_FILE
	parseFileOptions (CUSTOM_CONFIGURATION_FILE);
#endif
#ifdef MSDOS_STYLE_PATH
	
	asprintf (&filename,"/%s.cnf",(Option.configFilename)?Option.configFilename:"ctags");
	parseFileOptions (filename);
	free (filename);
#endif
	asprintf (&filename,"/etc/%s.conf",(Option.configFilename)?Option.configFilename:"ctags");
	parseFileOptions (filename);
	free (filename);
	asprintf (&filename,"/usr/local/etc/%s.conf",(Option.configFilename)?Option.configFilename:"ctags");
	parseFileOptions (filename);
	free (filename);
	if (home != NULL)
	{
		parseConfigurationFileOptionsInDirectory (home);
	}
	else
	{
#ifdef MSDOS_STYLE_PATH
		/*
		 * Windows users don't usually set HOME.
		 * The OS sets HOMEDRIVE and HOMEPATH for them.
		 */
		const char* homeDrive = getenv ("HOMEDRIVE");
		const char* homePath = getenv ("HOMEPATH");
		if (homeDrive != NULL && homePath != NULL)
		{
			vString* const windowsHome = rs_vStringNew ();
			rs_vStringCatS (windowsHome, homeDrive);
			rs_vStringCatS (windowsHome, homePath);
			parseConfigurationFileOptionsInDirectory (vStringValue (windowsHome));
			rs_vStringDelete (windowsHome);
		}
#endif
	}
	parseConfigurationFileOptionsInDirectory (".");
}

void parseEnvironmentOptions (void)
{
	const char *envOptions = NULL;
	const char* var = NULL;

	if (Option.etags)
	{
		var = ETAGS_ENVIRONMENT;
		envOptions = getenv (var);
	}
	if (envOptions == NULL)
	{
		var = CTAGS_ENVIRONMENT;
		envOptions = getenv (var);
	}
	if (envOptions != NULL  &&  envOptions [0] != '\0')
	{
		cookedArgs* const args = cArgNewFromString (envOptions);
		verbose ("Reading options from $CTAGS\n");
		parseOptions (args);
		rs_cArgDelete (args);
		if (NonOptionEncountered)
			error (WARNING, "Ignoring non-option in %s variable", var);
	}
}

extern void readOptionConfiguration (void)
{
	if (! SkipConfiguration)
	{
		parseConfigurationFileOptions ();
		parseEnvironmentOptions ();
	}
}

/*
*   Option initialization
*/

extern void initOptions (void)
{
	initBooleanOptions ();
	rs_setOptionFiles(rs_stringListNew ());
	verbose ("Setting option defaults\n");
	installHeaderListDefaults ();
	verbose ("  Installing default language mappings:\n");
	installLanguageMapDefaults ();

	/* always excluded by default */
	verbose ("  Installing default exclude patterns:\n");
	processExcludeOption (NULL, "{arch}");
	processExcludeOption (NULL, ".arch-ids");
	processExcludeOption (NULL, ".arch-inventory");
	processExcludeOption (NULL, "autom4te.cache");
	processExcludeOption (NULL, "BitKeeper");
	processExcludeOption (NULL, ".bzr");
	processExcludeOption (NULL, ".bzrignore");
	processExcludeOption (NULL, "CVS");
	processExcludeOption (NULL, ".cvsignore");
	processExcludeOption (NULL, "_darcs");
	processExcludeOption (NULL, ".deps");
	processExcludeOption (NULL, "EIFGEN");
	processExcludeOption (NULL, ".git");
	processExcludeOption (NULL, ".hg");
	processExcludeOption (NULL, "PENDING");
	processExcludeOption (NULL, "RCS");
	processExcludeOption (NULL, "RESYNC");
	processExcludeOption (NULL, "SCCS");
	processExcludeOption (NULL, ".svn");
}

/* freeOptionResources is now implemented directly in Rust as rs_freeOptionResourcesDirect
 * It accesses OPTION_INSTANCE, EXCLUDED, and OPTION_FILES globals directly.
 */

/* vi:set tabstop=4 shiftwidth=4: */
/*
*   $Id$
*
*   Copyright (c) 1996-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions for managing source languages and
*   dispatching files to the appropriate language parser.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */
#include <time.h>

#ifdef PROFILE_CTAGS
struct timespec parse_start, parse_end;
double total_file_io = 0, total_parse = 0;
#define PARSE_PROFILE_START() clock_gettime(CLOCK_MONOTONIC, &parse_start)
#define PARSE_PROFILE_END(acc) do { \
    clock_gettime(CLOCK_MONOTONIC, &parse_end); \
    acc += ((parse_end.tv_sec - parse_start.tv_sec) * 1000.0 + \
            (parse_end.tv_nsec - parse_start.tv_nsec) / 1000000.0); \
} while(0)
#define PARSE_PROFILE_REPORT() fprintf(stderr, "PROFILE file_io: %.3f ms, parse: %.3f ms\n", total_file_io, total_parse)
#else
#define PARSE_PROFILE_START()
#define PARSE_PROFILE_END(acc)
#define PARSE_PROFILE_REPORT()
#endif

#include <string.h>

#include "debug.h"
#include "entry.h"
#include "main.h"
#define OPTION_WRITE
#include "options.h"
#include "parsers.h" 
#include "read.h"
#include "routines.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
parserDefinitionFunc* BuiltInParsers[] = { PARSER_LIST };

/* LanguageTable and LanguageCount are now Rust-owned in parse.rs */
/* Accessed via #define in parse.h */

/*
*   FUNCTION DEFINITIONS
*/

extern void makeSimpleTag (
		const vString* const name, kindOption* const kinds, const int kind)
{
	if (kinds [kind].enabled  &&  name != NULL  &&  vStringLength (name) > 0)
	{
	    tagEntryInfo e;
	    initTagEntry (&e, vStringValue (name));

	    e.kindName = kinds [kind].name;
	    e.kind     = kinds [kind].letter;

	    makeTagEntry (&e);
	}
}

/*
*   parserDescription mapping management
*/

/* parserNew - now implemented in Rust (parse.rs) */
extern parserDefinition* rs_parserNew(const char* name);
#define parserNew(name) rs_parserNew(name)

/* getLanguageName - now implemented in Rust (parse.rs) */
extern const char *getLanguageName (const langType language);

/* getNamedLanguage, getExtensionLanguage, getPatternLanguage
 * - now implemented in Rust (parse.rs)
 * These functions iterate over LanguageTable to find matching languages.
 */
extern langType getNamedLanguage(const char *const name);
extern langType getExtensionLanguage(const char *const extension);
extern langType getPatternLanguage(const char *const fileName);

#ifdef SYS_INTERPRETER

/*  The name of the language interpreter, either directly or as the argument
 *  to "env".
 */
vString* determineInterpreter (const char* const cmd)
{
	vString* const interpreter = rs_vStringNew ();
	const char* p = cmd;
	do
	{
		rs_vStringClear (interpreter);
		for ( ;  isspace ((int) *p)  ;  ++p)
			;  /* no-op */
		for ( ;  *p != '\0'  &&  ! isspace ((int) *p)  ;  ++p)
			rs_vStringPut (interpreter, (int) *p);
		vStringTerminate (interpreter);
	} while (strcmp (vStringValue (interpreter), "env") == 0);
	return interpreter;
}

langType getInterpreterLanguage (const char *const fileName)
{
	langType result = LANG_IGNORE;
	FILE* const fp = fopen (fileName, "r");
	if (fp != NULL)
	{
		vString* const vLine = rs_vStringNew ();
		const char* const line = readLine (vLine, fp);
		if (line != NULL  &&  line [0] == '#'  &&  line [1] == '!')
		{
			const char* const lastSlash = strrchr (line, '/');
			const char *const cmd = lastSlash != NULL ? lastSlash+1 : line+2;
			vString* const interpreter = determineInterpreter (cmd);
			result = getExtensionLanguage (vStringValue (interpreter));
			if (result == LANG_IGNORE)
				result = getNamedLanguage (vStringValue (interpreter));
			rs_vStringDelete (interpreter);
		}
		rs_vStringDelete (vLine);
		fclose (fp);
	}
	return result;
}

#endif

extern langType getFileLanguage (const char *const fileName)
{
	langType language = Option.language;
	if (language == LANG_AUTO)
	{
		language = getExtensionLanguage (fileExtension (fileName));
		if (language == LANG_IGNORE)
			language = getPatternLanguage (fileName);
#ifdef SYS_INTERPRETER
		if (language == LANG_IGNORE)
		{
			fileStatus *status = eStat (fileName);
			if (status->isExecutable)
				language = getInterpreterLanguage (fileName);
		}
#endif
	}
	return language;
}

extern void printLanguageMap (const langType language)
{
	boolean first = TRUE;
	unsigned int i;
	stringList* map = LanguageTable [language]->currentPatterns;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	for (i = 0  ;  map != NULL  &&  i < rs_stringListCount (map)  ;  ++i)
	{
		printf ("%s(%s)", (first ? "" : " "),
				vStringValue (rs_stringListItem (map, i)));
		first = FALSE;
	}
	map = LanguageTable [language]->currentExtensions;
	for (i = 0  ;  map != NULL  &&  i < rs_stringListCount (map)  ;  ++i)
	{
		printf ("%s.%s", (first ? "" : " "),
				vStringValue (rs_stringListItem (map, i)));
		first = FALSE;
	}
}

extern void installLanguageMapDefault (const langType language)
{
	parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (lang->currentPatterns != NULL)
		rs_stringListDelete (lang->currentPatterns);
	if (lang->currentExtensions != NULL)
		rs_stringListDelete (lang->currentExtensions);

	if (lang->patterns == NULL)
		lang->currentPatterns = rs_stringListNew ();
	else
	{
		lang->currentPatterns =
			rs_stringListNewFromArgv (lang->patterns);
	}
	if (lang->extensions == NULL)
		lang->currentExtensions = rs_stringListNew ();
	else
	{
		lang->currentExtensions =
			rs_stringListNewFromArgv (lang->extensions);
	}
	if (Option.verbose)
		printLanguageMap (language);
	verbose ("\n");
}

extern void installLanguageMapDefaults (void)
{
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  ;  ++i)
	{
		verbose ("    %s: ", getLanguageName (i));
		installLanguageMapDefault (i);
	}
}

extern void clearLanguageMap (const langType language)
{
	Assert (0 <= language  &&  language < (int) LanguageCount);
	rs_stringListClear (LanguageTable [language]->currentPatterns);
	rs_stringListClear (LanguageTable [language]->currentExtensions);
}

extern void addLanguagePatternMap (const langType language, const char* ptrn)
{
	Assert (0 <= language  &&  language < (int) LanguageCount);
	rs_stringListAddString (&LanguageTable [language]->currentPatterns, ptrn);
}

extern boolean removeLanguageExtensionMap (const char *const extension)
{
	boolean result = FALSE;
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  &&  ! result ;  ++i)
	{
		stringList* const exts = LanguageTable [i]->currentExtensions;
		if (exts != NULL  &&  rs_stringListRemoveExtension (exts, extension))
		{
			verbose (" (removed from %s)", getLanguageName (i));
			result = TRUE;
		}
	}
	return result;
}

extern void addLanguageExtensionMap (
		const langType language, const char* extension)
{
	Assert (0 <= language  &&  language < (int) LanguageCount);
	removeLanguageExtensionMap (extension);
	rs_stringListAddString (&LanguageTable [language]->currentExtensions, extension);
}

extern void enableLanguage (const langType language, const boolean state)
{
	Assert (0 <= language  &&  language < (int) LanguageCount);
	LanguageTable [language]->enabled = state;
}

extern void enableLanguages (const boolean state)
{
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  ;  ++i)
		enableLanguage (i, state);
}

void initializeParsers (void)
{
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  ;  ++i)
		if (LanguageTable [i]->initialize != NULL)
			(LanguageTable [i]->initialize) ((langType) i);
}

extern void initializeParsing (void)
{
	unsigned int builtInCount;
	unsigned int i;

	builtInCount = sizeof (BuiltInParsers) / sizeof (BuiltInParsers [0]);
	rs_setLanguageTable (xMalloc (builtInCount, parserDefinition*));

	verbose ("Installing parsers: ");
	for (i = 0  ;  i < builtInCount  ;  ++i)
	{
		parserDefinition* const def = (*BuiltInParsers [i]) ();
		if (def != NULL)
		{
			boolean accepted = FALSE;
			if (def->name == NULL  ||  def->name[0] == '\0')
				error (FATAL, "parser definition must contain name\n");
			else if (def->regex)
			{
#ifdef HAVE_REGEX
				def->parser = findRegexTags;
				accepted = TRUE;
#endif
			}
			else if ((def->parser == NULL)  ==  (def->parser2 == NULL))
				error (FATAL,
		"%s parser definition must define one and only one parsing routine\n",
					   def->name);
			else
				accepted = TRUE;
			if (accepted)
			{
				verbose ("%s%s", i > 0 ? ", " : "", def->name);
				def->id = LanguageCount;
				rs_incrLanguageCount ();
				LanguageTable [def->id] = def;
			}
		}
	}
	verbose ("\n");
	enableLanguages (TRUE);
	initializeParsers ();
}

/* freeParserResources - now implemented directly in Rust (parse.rs) */
/* Uses rs_freeParserResourcesDirect() which accesses LANGUAGE_TABLE and LANGUAGE_COUNT directly */

/*
*   Option parsing
*/

extern void processLanguageDefineOption (
		const char *const option, const char *const parameter __unused__)
{
#ifdef HAVE_REGEX
	if (parameter [0] == '\0')
		error (WARNING, "No language specified for \"%s\" option", option);
	else if (getNamedLanguage (parameter) != LANG_IGNORE)
		error (WARNING, "Language \"%s\" already defined", parameter);
	else
	{
		unsigned int i = LanguageCount;
		rs_incrLanguageCount ();
		parserDefinition* const def = parserNew (parameter);
		def->parser            = findRegexTags;
		def->currentPatterns   = rs_stringListNew ();
		def->currentExtensions = rs_stringListNew ();
		def->regex             = TRUE;
		def->enabled           = TRUE;
		def->id                = i;
		rs_setLanguageTable (xRealloc (LanguageTable, i + 1, parserDefinition*));
		LanguageTable [i] = def;
	}
#else
	error (WARNING, "regex support not available; required for --%s option",
		   option);
#endif
}

kindOption *langKindOption (const langType language, const int flag)
{
	unsigned int i;
	kindOption* result = NULL;
	const parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	for (i=0  ;  i < lang->kindCount  &&  result == NULL  ;  ++i)
		if (lang->kinds [i].letter == flag)
			result = &lang->kinds [i];
	return result;
}

void disableLanguageKinds (const langType language)
{
	const parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (lang->regex)
		disableRegexKinds (language);
	else
	{
		unsigned int i;
		for (i = 0  ;  i < lang->kindCount  ;  ++i)
			lang->kinds [i].enabled = FALSE;
	}
}

boolean enableLanguageKind (
		const langType language, const int kind, const boolean mode)
{
	boolean result = FALSE;
	if (LanguageTable [language]->regex)
		result = enableRegexKind (language, kind, mode);
	else
	{
		kindOption* const opt = langKindOption (language, kind);
		if (opt != NULL)
		{
			opt->enabled = mode;
			result = TRUE;
		}
	}
	return result;
}

void processLangKindOption (
		const langType language, const char *const option,
		const char *const parameter)
{
	const char *p = parameter;
	boolean mode = TRUE;
	int c;

	Assert (0 <= language  &&  language < (int) LanguageCount);
	if (*p != '+'  &&  *p != '-')
		disableLanguageKinds (language);
	while ((c = *p++) != '\0') switch (c)
	{
		case '+': mode = TRUE;  break;
		case '-': mode = FALSE; break;
		default:
			if (! enableLanguageKind (language, c, mode))
				error (WARNING, "Unsupported parameter '%c' for --%s option",
					c, option);
			break;
	}
}

extern boolean processKindOption (
		const char *const option, const char *const parameter)
{
	boolean handled = FALSE;
	const char* const dash = strchr (option, '-');
	if (dash != NULL  &&
		(strcmp (dash + 1, "kinds") == 0  ||  strcmp (dash + 1, "types") == 0))
	{
		langType language;
		vString* langName = rs_vStringNew ();
		rs_vStringNCopyS (langName, option, dash - option);
		language = getNamedLanguage (vStringValue (langName));
		if (language == LANG_IGNORE)
			error (WARNING, "Unknown language \"%s\" in \"%s\" option", vStringValue (langName), option);
		else
			processLangKindOption (language, option, parameter);
		rs_vStringDelete (langName);
		handled = TRUE;
	}
	return handled;
}

void printLanguageKind (const kindOption* const kind, boolean indent)
{
	const char *const indentation = indent ? "    " : "";
	printf ("%s%c  %s%s\n", indentation, kind->letter,
		kind->description != NULL ? kind->description :
			(kind->name != NULL ? kind->name : ""),
		kind->enabled ? "" : " [off]");
}

void printKinds (langType language, boolean indent)
{
	const parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (lang->kinds != NULL  ||  lang->regex)
	{
		unsigned int i;
		for (i = 0  ;  i < lang->kindCount  ;  ++i)
			printLanguageKind (lang->kinds + i, indent);
		printRegexKinds (language, indent);
	}
}

extern void printLanguageKinds (const langType language)
{
	if (language == LANG_AUTO)
	{
		unsigned int i;
		for (i = 0  ;  i < LanguageCount  ;  ++i)
		{
			const parserDefinition* const lang = LanguageTable [i];
			printf ("%s%s\n", lang->name, lang->enabled ? "" : " [disabled]");
			printKinds (i, TRUE);
		}
	}
	else
		printKinds (language, FALSE);
}

void printMaps (const langType language)
{
	const parserDefinition* lang;
	unsigned int i;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	printf ("%-8s", lang->name);
	if (lang->currentExtensions != NULL)
		for (i = 0  ;  i < rs_stringListCount (lang->currentExtensions)  ;  ++i)
			printf (" *.%s", vStringValue (
						rs_stringListItem (lang->currentExtensions, i)));
	if (lang->currentPatterns != NULL)
		for (i = 0  ;  i < rs_stringListCount (lang->currentPatterns)  ;  ++i)
			printf (" %s", vStringValue (
						rs_stringListItem (lang->currentPatterns, i)));
	putchar ('\n');
}

extern void printLanguageMaps (const langType language)
{
	if (language == LANG_AUTO)
	{
		unsigned int i;
		for (i = 0  ;  i < LanguageCount  ;  ++i)
			printMaps (i);
	}
	else
		printMaps (language);
}

void printLanguage (const langType language)
{
	const parserDefinition* lang;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	lang = LanguageTable [language];
	if (lang->kinds != NULL  ||  lang->regex)
		printf ("%s%s\n", lang->name, lang->enabled ? "" : " [disabled]");
}
	    
extern void printLanguageList (void)
{
	unsigned int i;
	for (i = 0  ;  i < LanguageCount  ;  ++i)
		printLanguage (i);
}

/*
*   File parsing
*/

void makeFileTag (const char *const fileName)
{
	if (Option.include.fileNames)
	{
		tagEntryInfo tag;
		initTagEntry (&tag, baseFilename (fileName));

		tag.isFileEntry     = TRUE;
		tag.lineNumberEntry = TRUE;
		tag.lineNumber      = 1;
		tag.kindName        = "file";
		tag.kind            = 'F';

		makeTagEntry (&tag);
	}
}

rescanReason createTagsForFile (
		const char *const fileName, const langType language,
		const unsigned int passCount)
{
	rescanReason rescan = RESCAN_NONE;
	Assert (0 <= language  &&  language < (int) LanguageCount);
	PARSE_PROFILE_START();
	if (fileOpen (fileName, language))
	{
		PARSE_PROFILE_END(total_file_io);
		const parserDefinition* const lang = LanguageTable [language];
		if (Option.etags)
			beginEtagsFile ();

		makeFileTag (fileName);

		PARSE_PROFILE_START();
		if (lang->parser != NULL)
			lang->parser ();
		else if (lang->parser2 != NULL)
			rescan = lang->parser2 (passCount);
		PARSE_PROFILE_END(total_parse);

		if (Option.etags)
			endEtagsFile (getSourceFileTagPath ());

		PARSE_PROFILE_START();
		fileClose ();
		PARSE_PROFILE_END(total_file_io);
		PARSE_PROFILE_REPORT();
	}

	return rescan;
}

boolean createTagsWithFallback (
		const char *const fileName, const langType language)
{
	unsigned long numTags	= TagFile.numTags.added;
	fpos_t tagFilePosition;
	unsigned int passCount = 0;
	boolean tagFileResized = FALSE;
	rescanReason whyRescan;

#if DEBUG3
	fgetpos (TagFile.fp, &tagFilePosition);
#endif
	while ( ( whyRescan =
	            createTagsForFile (fileName, language, ++passCount) )
	                != RESCAN_NONE)
	{
		if (whyRescan == RESCAN_FAILED)
		{
			/*  Restore prior state of tag file.
			*/
#if DEBUG3
			fsetpos (TagFile.fp, &tagFilePosition);
			TagFile.numTags.added = numTags;
#endif
			tagFileResized = TRUE;
		}
		else if (whyRescan == RESCAN_APPEND)
		{
			fgetpos(TagFile.fp, &tagFilePosition);
			numTags = TagFile.numTags.added;
		}
	}
	return tagFileResized;
}

extern boolean parseFile (const char *const fileName)
{
	boolean tagFileResized = FALSE;
	langType language = Option.language;
	if (Option.language == LANG_AUTO)
		language = getFileLanguage (fileName);
	Assert (language != LANG_AUTO);
	if (language == LANG_IGNORE)
		verbose ("ignoring %s (unknown language)\n", fileName);
	else if (! LanguageTable [language]->enabled)
		verbose ("ignoring %s (language disabled)\n", fileName);
	else
	{
		if (Option.filter)
			openTagFile ();

		tagFileResized = createTagsWithFallback (fileName, language);

		if (Option.filter)
			closeTagFile (tagFileResized);
		addTotals (1, 0L, 0L);

		return tagFileResized;
	}
	return tagFileResized;
}

/* vi:set tabstop=4 shiftwidth=4 nowrap: */
/*
*   $Id$
*
*   Copyright (c) 1996-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains low level source and tag file read functions (newline
*   conversion for source files are performed at this level).
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#include <ctype.h>

#define FILE_WRITE
#include "read.h"
#include "debug.h"
#include "entry.h"
#include "main.h"
#include "routines.h"
#include "options.h"

/*
*   DATA DEFINITIONS
*/
/* File is now owned by Rust - see read.h for macro definition */
/* StartOfLine is now owned by Rust - access via rs_getStartOfLine() macro */
extern fpos_t *rs_getStartOfLine(void);
#define StartOfLine (*rs_getStartOfLine())

/* Option.lineDirectives and Option.if0 accessors now in Rust (options.rs) */

/*
*   FUNCTION DEFINITIONS
*/

/* freeSourceFileResources - now implemented directly in Rust (read.rs) */
extern void rs_freeSourceFileResourcesDirect (void);

/*
 *   Source file access functions
 */

void setInputFileName (const char *const fileName)
{
	const char *const head = fileName;
	const char *const tail = baseFilename (head);

	if (File.name != NULL)
		rs_vStringDelete (File.name);
	File.name = rs_vStringNewInit (fileName);

	if (File.path != NULL)
		rs_vStringDelete (File.path);
	if (tail == head)
		File.path = NULL;
	else
	{
		const size_t length = tail - head - 1;
		File.path = rs_vStringNew ();
		rs_vStringNCopyS (File.path, fileName, length);
	}
}

void setSourceFileParameters (vString *const fileName)
{
	if (File.source.name != NULL)
		rs_vStringDelete (File.source.name);
	File.source.name = fileName;

	if (File.source.tagPath != NULL)
		eFree (File.source.tagPath);
	if (! Option.tagRelative || isAbsolutePath (vStringValue (fileName)))
		File.source.tagPath = eStrdup (vStringValue (fileName));
	else
		File.source.tagPath =
				relativeFilename (vStringValue (fileName), TagFile.directory);

	if (vStringLength (fileName) > TagFile.max.file)
		TagFile.max.file = vStringLength (fileName);

	File.source.isHeader = isIncludeFile (vStringValue (fileName));
	File.source.language = getFileLanguage (vStringValue (fileName));
}

boolean setSourceFileName (vString *const fileName)
{
	boolean result = FALSE;
	if (getFileLanguage (vStringValue (fileName)) != LANG_IGNORE)
	{
		vString *pathName;
		if (isAbsolutePath (vStringValue (fileName)) || File.path == NULL)
			pathName = rs_vStringNewCopy (fileName);
		else
			pathName = combinePathAndFile (
					vStringValue (File.path), vStringValue (fileName));
		setSourceFileParameters (pathName);
		result = TRUE;
	}
	return result;
}

/*
 *   Line directive parsing
 */

int skipWhite (void)
{
	int c;
	do
		c = getc (File.fp);
	while (c == ' '  ||  c == '\t');
	return c;
}

unsigned long readLineNumber (void)
{
	unsigned long lNum = 0;
	int c = skipWhite ();
	while (c != EOF  &&  isdigit (c))
	{
		lNum = (lNum * 10) + (c - '0');
		c = getc (File.fp);
	}
	ungetc (c, File.fp);
	if (c != ' '  &&  c != '\t')
		lNum = 0;

	return lNum;
}

/* While ANSI only permits lines of the form:
 *   # line n "filename"
 * Earlier compilers generated lines of the form
 *   # n filename
 * GNU C will output lines of the form:
 *   # n "filename"
 * So we need to be fairly flexible in what we accept.
 */
vString *readFileName (void)
{
	vString *const fileName = rs_vStringNew ();
	boolean quoteDelimited = FALSE;
	int c = skipWhite ();

	if (c == '"')
	{
		c = getc (File.fp);  /* skip double-quote */
		quoteDelimited = TRUE;
	}
	while (c != EOF  &&  c != '\n'  &&
			(quoteDelimited ? (c != '"') : (c != ' '  &&  c != '\t')))
	{
		rs_vStringPut (fileName, c);
		c = getc (File.fp);
	}
	if (c == '\n')
		ungetc (c, File.fp);
	rs_vStringPut (fileName, '\0');

	return fileName;
}

boolean parseLineDirective (void)
{
	boolean result = FALSE;
	int c = skipWhite ();
	DebugStatement ( const char* lineStr = ""; )

	if (isdigit (c))
	{
		ungetc (c, File.fp);
		result = TRUE;
	}
	else if (c == 'l'  &&  getc (File.fp) == 'i'  &&
			 getc (File.fp) == 'n'  &&  getc (File.fp) == 'e')
	{
		c = getc (File.fp);
		if (c == ' '  ||  c == '\t')
		{
			DebugStatement ( lineStr = "line"; )
			result = TRUE;
		}
	}
	if (result)
	{
		const unsigned long lNum = readLineNumber ();
		if (lNum == 0)
			result = FALSE;
		else
		{
			vString *const fileName = readFileName ();
			if (vStringLength (fileName) == 0)
			{
				File.source.lineNumber = lNum - 1;  /* applies to NEXT line */
				DebugStatement ( debugPrintf (DEBUG_RAW, "#%s %ld", lineStr, lNum); )
			}
			else if (setSourceFileName (fileName))
			{
				File.source.lineNumber = lNum - 1;  /* applies to NEXT line */
				DebugStatement ( debugPrintf (DEBUG_RAW, "#%s %ld \"%s\"",
								lineStr, lNum, vStringValue (fileName)); )
			}

			if (Option.include.fileNames && vStringLength (fileName) > 0 &&
				lNum == 1)
			{
				tagEntryInfo tag;
				initTagEntry (&tag, baseFilename (vStringValue (fileName)));

				tag.isFileEntry     = TRUE;
				tag.lineNumberEntry = TRUE;
				tag.lineNumber      = 1;
				tag.kindName        = "file";
				tag.kind            = 'F';

				makeTagEntry (&tag);
			}
			rs_vStringDelete (fileName);
			result = TRUE;
		}
	}
	return result;
}

/*
 *   Source file I/O operations
 */

/*  This function opens a source file, and resets the line counter.  If it
 *  fails, it will display an error message and leave the File.fp set to NULL.
 */
extern boolean fileOpen (const char *const fileName, const langType language)
{
#ifdef VMS
	const char *const openMode = "r";
#else
	const char *const openMode = "rb";
#endif
	boolean opened = FALSE;

	/*	If another file was already open, then close it.
	 */
	if (File.fp != NULL)
	{
		fclose (File.fp);  /* close any open source file */
		File.fp = NULL;
	}

	File.fp = fopen (fileName, openMode);
	if (File.fp == NULL)
		error (WARNING | PERROR, "cannot open \"%s\"", fileName);
	else
	{
		opened = TRUE;

		setInputFileName (fileName);
		fgetpos (File.fp, &StartOfLine);
		fgetpos (File.fp, &File.filePosition);
		File.currentLine  = NULL;
		File.lineNumber   = 0L;
		File.eof          = FALSE;
		File.newLine      = TRUE;

		if (File.line != NULL)
			rs_vStringClear (File.line);

		setSourceFileParameters (rs_vStringNewInit (fileName));
		File.source.lineNumber = 0L;

		verbose ("OPENING %s as %s language %sfile\n", fileName,
				getLanguageName (language),
				File.source.isHeader ? "include " : "");
	}
	return opened;
}

extern void fileClose (void)
{
	if (File.fp != NULL)
	{
		/*  The line count of the file is 1 too big, since it is one-based
		 *  and is incremented upon each newline.
		 */
		if (Option.printTotals)
		{
			fileStatus *status = eStat (vStringValue (File.name));
			addTotals (0, File.lineNumber - 1L, status->size);
		}
		fclose (File.fp);
		File.fp = NULL;
	}
}

/* fileEOF is now a macro in read.h */

/* fileNewline - now a macro delegating to Rust */
extern void rs_fileNewline(void);
#define fileNewline() rs_fileNewline()

/* iFileGetc - now a macro delegating to Rust */
extern int rs_iFileGetc(void);
#define iFileGetc() rs_iFileGetc()

/* fileUngetc - now a macro delegating to Rust */
extern void rs_fileUngetc(int c);
#define fileUngetc(c) rs_fileUngetc((int)(c))

/* iFileGetLine - now a macro delegating to Rust */
extern vString *rs_iFileGetLine(void);
#define iFileGetLine() rs_iFileGetLine()

/* fileGetc - now a macro delegating to Rust */
extern int rs_fileGetc(void);
#define fileGetc() rs_fileGetc()

extern int fileSkipToCharacter (int c)
{
	int d;
	do
	{
		d = fileGetc ();
	} while (d != EOF && d != c);
	return d;
}

/* fileReadLine - now a macro in read.h */

/*
 *   Source file line reading with automatic buffer sizing
 */
extern char *readLine (vString *const vLine, FILE *const fp)
{
	char *result = NULL;

	rs_vStringClear (vLine);
	if (fp == NULL)  /* to free memory allocated to buffer */
		error (FATAL, "NULL file pointer");
	else
	{
		boolean reReadLine;

		/*  If reading the line places any character other than a null or a
		 *  newline at the last character position in the buffer (one less
		 *  than the buffer size), then we must resize the buffer and
		 *  reattempt to read the line.
		 */
		do
		{
			char *const pLastChar = vStringValue (vLine) + vStringSize (vLine) -2;
			fpos_t startOfLine;

			fgetpos (fp, &startOfLine);
			reReadLine = FALSE;
			*pLastChar = '\0';
			result = fgets (vStringValue (vLine), (int) vStringSize (vLine), fp);
			if (result == NULL)
			{
				if (! feof (fp))
					error (FATAL | PERROR, "Failure on attempt to read file");
			}
			else if (*pLastChar != '\0'  &&
					 *pLastChar != '\n'  &&  *pLastChar != '\r')
			{
				/*  buffer overflow */
				reReadLine = rs_vStringAutoResize (vLine) ? TRUE : FALSE;
				if (reReadLine)
					fsetpos (fp, &startOfLine);
				else
					error (FATAL | PERROR, "input line too big; out of memory");
			}
			else
			{
				char* eol;
				rs_vStringSetLength (vLine);
				/* canonicalize new line */
				eol = vStringValue (vLine) + vStringLength (vLine) - 1;
				if (*eol == '\r')
					*eol = '\n';
				else if (*(eol - 1) == '\r'  &&  *eol == '\n')
				{
					*(eol - 1) = '\n';
					*eol = '\0';
					--vLine->length;
				}
			}
		} while (reReadLine);
	}
	return result;
}

/*  Places into the line buffer the contents of the line referenced by
 *  "location".
 */
extern char *readSourceLine (
		vString *const vLine, fpos_t location, long *const pSeekValue)
{
	fpos_t orignalPosition;
	char *result;

	fgetpos (File.fp, &orignalPosition);
	fsetpos (File.fp, &location);
	if (pSeekValue != NULL)
		*pSeekValue = ftell (File.fp);
	result = readLine (vLine, File.fp);
	if (result == NULL)
		error (FATAL, "Unexpected end of file: %s", vStringValue (File.name));
	fsetpos (File.fp, &orignalPosition);

	return result;
}

/* vi:set tabstop=4 shiftwidth=4: */
/*
*   $Id$
*
*   Copyright (c) 2002-2003, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains a lose assortment of shared functions.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#ifdef HAVE_STDLIB_H
# include <stdlib.h>  /* to declare malloc (), realloc () */
#endif
#include <ctype.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include <stdio.h>  /* to declare tempnam(), and SEEK_SET (hopefully) */

#ifdef HAVE_FCNTL_H
# include <fcntl.h>  /* to declar O_RDWR, O_CREAT, O_EXCL */
#endif
#ifdef HAVE_UNISTD_H
# include <unistd.h>  /* to declare mkstemp () */
#endif

/*  To declare "struct stat" and stat ().
 */
#if defined (HAVE_SYS_TYPES_H)
# include <sys/types.h>
#else
# if defined (HAVE_TYPES_H)
#  include <types.h>
# endif
#endif
#ifdef HAVE_SYS_STAT_H
# include <sys/stat.h>
#else
# ifdef HAVE_STAT_H
#  include <stat.h>
# endif
#endif

#ifdef HAVE_DOS_H
# include <dos.h>  /* to declare MAXPATH */
#endif
#ifdef HAVE_DIRECT_H
# include <direct.h>  /* to _getcwd */
#endif
#ifdef HAVE_DIR_H
# include <dir.h>  /* to declare findfirst() and findnext() */
#endif
#ifdef HAVE_IO_H
# include <io.h>  /* to declare open() */
#endif
#include "debug.h"
#include "routines.h"

/*
*   MACROS
*/
#ifndef TMPDIR
# define TMPDIR "/tmp"
#endif

/*  File type tests.
 */
#ifndef S_ISREG
# if defined (S_IFREG) && ! defined (AMIGA)
#  define S_ISREG(mode)		((mode) & S_IFREG)
# else
#  define S_ISREG(mode)		TRUE  /* assume regular file */
# endif
#endif

#ifndef S_ISLNK
# ifdef S_IFLNK
#  define S_ISLNK(mode)		(((mode) & S_IFMT) == S_IFLNK)
# else
#  define S_ISLNK(mode)		FALSE  /* assume no soft links */
# endif
#endif

#ifndef S_ISDIR
# ifdef S_IFDIR
#  define S_ISDIR(mode)		(((mode) & S_IFMT) == S_IFDIR)
# else
#  define S_ISDIR(mode)		FALSE  /* assume no soft links */
# endif
#endif

#ifndef S_IFMT
# define S_IFMT 0
#endif

#ifndef S_IXUSR
# define S_IXUSR 0
#endif
#ifndef S_IXGRP
# define S_IXGRP 0
#endif
#ifndef S_IXOTH
# define S_IXOTH 0
#endif

#ifndef S_IRUSR
# define S_IRUSR 0400
#endif
#ifndef S_IWUSR
# define S_IWUSR 0200
#endif

#ifndef S_ISUID
# define S_ISUID 0
#endif

/*  Hack for rediculous practice of Microsoft Visual C++.
 */
#if defined (WIN32)
# if defined (_MSC_VER)
#  define stat    _stat
#  define getcwd  _getcwd
#  define currentdrive() (_getdrive() + 'A' - 1)
#  define PATH_MAX  _MAX_PATH
# elif defined (__BORLANDC__)
#  define PATH_MAX  MAXPATH
#  define currentdrive() (getdisk() + 'A')
# elif defined (DJGPP)
#  define currentdrive() (getdisk() + 'A')
# else
#  define currentdrive() 'C'
# endif
#endif

#ifndef PATH_MAX
# define PATH_MAX 256
#endif

/*
 *  Miscellaneous macros
 */
#define selected(var,feature)	(((int)(var) & (int)(feature)) == (int)feature)

/*
*   DATA DEFINITIONS
*/
#if defined (MSDOS_STYLE_PATH)
const char *const PathDelimiters = ":/\\";
#elif defined (VMS)
const char *const PathDelimiters = ":]>";
#endif

/* CurrentDirectory is now Rust-owned in cparser.rs */
/* Accessed via #define CurrentDirectory (rs_getCurrentDirectory()) in routines.h */

/* ExecutableProgram and ExecutableName moved to Rust - see cparser.rs
 * Access via rs_getExecutableProgram()/rs_setExecutableProgram() and
 * rs_getExecutableName()/rs_setExecutableName()
 */
extern const char *rs_getExecutableProgram(void);
extern void rs_setExecutableProgram(const char *path);
extern const char *rs_getExecutableName(void);
extern void rs_setExecutableName(const char *name);
#define ExecutableProgram (rs_getExecutableProgram())
#define ExecutableName (rs_getExecutableName())

/*
*   FUNCTION PROTOTYPES
*/
#ifdef NEED_PROTO_STAT
extern int stat (const char *, struct stat *);
#endif
#ifdef NEED_PROTO_LSTAT
extern int lstat (const char *, struct stat *);
#endif
#if defined (MSDOS) || defined (WIN32) || defined (VMS) || defined (__EMX__) || defined (AMIGA)
# define lstat(fn,buf) stat(fn,buf)
#endif

/*
*   FUNCTION DEFINITIONS
*/

/* freeRoutineResources - now implemented directly in Rust (cparser.rs) */
extern void rs_freeRoutineResourcesDirect (void);

/* setExecutableName - now implemented in Rust (cparser.rs) */
extern void setExecutableName(const char *path);

/* getExecutableName - now implemented in Rust (cparser.rs) */
extern const char *getExecutableName(void);

/* getExecutablePath - now implemented in Rust (cparser.rs) */
extern const char *getExecutablePath(void);

/* rs_error_impl - Rust implementation of error logic (cparser.rs) */
extern void rs_error_impl(int selection, const char *message);

/* error - thin C wrapper that handles variadic formatting */
extern void error (
		const errorSelection selection, const char *const format, ...)
{
	char buffer[1024];
	va_list ap;

	va_start (ap, format);
	vsnprintf (buffer, sizeof(buffer), format, ap);
	va_end (ap);

	rs_error_impl((int)selection, buffer);
}

/*
 *  Memory allocation functions
 *  Now implemented in Rust (cparser.rs) - exported directly as eMalloc, eCalloc, eRealloc
 */
extern void *eMalloc(size_t size);
extern void *eCalloc(size_t count, size_t size);
extern void *eRealloc(void *ptr, size_t size);
/* eFree is a macro in routines.h that calls rs_eFree */

/*
 *  String manipulation functions
 */

/*
 * Compare two strings, ignoring case.
 * Return 0 for match, < 0 for smaller, > 0 for bigger
 * Make sure case is folded to uppercase in comparison (like for 'sort -f')
 * This makes a difference when one of the chars lies between upper and lower
 * ie. one of the chars [ \ ] ^ _ ` for ascii. (The '_' in particular !)
 */
/* struppercmp and strnuppercmp are now macros in routines.h */

#ifndef HAVE_STRSTR
extern char* strstr (const char *str, const char *substr)
{
	const size_t length = strlen (substr);
	const char *match = NULL;
	const char *p;

	for (p = str  ;  *p != '\0'  &&  match == NULL  ;  ++p)
		if (strncmp (p, substr, length) == 0)
			match = p;
	return (char*) match;
}
#endif

/* String utility functions - now delegated to Rust */
extern char* rs_eStrdup(const char* str);
extern void rs_toLowerString(char* str);
extern void rs_toUpperString(char* str);
extern char* rs_newLowerString(const char* str);
extern char* rs_newUpperString(const char* str);

/* Path utility functions - now delegated to Rust */
extern int rs_isPathSeparator(int c);
extern const char* rs_baseFilename(const char* filePath);
extern const char* rs_fileExtension(const char* fileName);
extern int rs_isAbsolutePath(const char* path);

/* String functions (eStrdup, toLowerString, etc.) are now macros in routines.h */

/*
 * File system functions
 */

/* setCurrentDirectory - now implemented in Rust (cparser.rs) */
extern void setCurrentDirectory(void);

#ifdef AMIGA
boolean isAmigaDirectory (const char *const name)
{
	boolean result = FALSE;
	struct FileInfoBlock *const fib = xMalloc (1, struct FileInfoBlock);
	if (fib != NULL)
	{
		const BPTR flock = Lock ((UBYTE *) name, (long) ACCESS_READ);

		if (flock != (BPTR) NULL)
		{
			if (Examine (flock, fib))
				result = ((fib->fib_DirEntryType >= 0) ? TRUE : FALSE);
			UnLock (flock);
		}
		eFree (fib);
	}
	return result;
}
#endif

/* eStat - now implemented in Rust (cparser.rs) */
extern fileStatus *eStat (const char *const fileName);

/* eStatFree is now a macro in routines.h */

/* doesFileExist - now implemented in Rust (cparser.rs) */
extern boolean doesFileExist(const char *fileName);

/* isRecursiveLink - now implemented in Rust (cparser.rs) */
extern boolean isRecursiveLink(const char *dirName);

#ifndef HAVE_FGETPOS

extern int fgetpos (FILE *stream, fpos_t *pos)
{
	int result = 0;

	*pos = ftell (stream);
	if (*pos == -1L)
		result = -1;

	return result;
}

extern int fsetpos (FILE *stream, fpos_t const *pos)
{
	return fseek (stream, *pos, SEEK_SET);
}

#endif

/*
 *  Pathname manipulation (O/S dependent!!!)
 */

/* isPathSeparator is now a macro in routines.h */

#if ! defined (HAVE_STAT_ST_INO)

void canonicalizePath (char *const path __unused__)
{
#if defined (MSDOS_STYLE_PATH)
	char *p;
	for (p = path  ;  *p != '\0'  ;  ++p)
		if (isPathSeparator (*p)  &&  *p != ':')
			*p = PATH_SEPARATOR;
#endif
}

#endif

/* isSameFile - now implemented in Rust (cparser.rs) */
extern boolean isSameFile(const char *name1, const char *name2);

/* Path functions (baseFilename, fileExtension, isAbsolutePath) are now macros in routines.h */

/* combinePathAndFile - now implemented in Rust (cparser.rs) */
extern vString *combinePathAndFile(const char *path, const char *file);

/* concat - now implemented in Rust (cparser.rs) */
extern char* concat(const char *s1, const char *s2, const char *s3);

/* absoluteFilename - now implemented in Rust (cparser.rs) */
extern char* absoluteFilename(const char *file);

/* absoluteDirname - now implemented in Rust (cparser.rs) */
extern char* absoluteDirname(char *file);

/* relativeFilename - now implemented in Rust (cparser.rs) */
extern char* relativeFilename(const char *file, const char *dir);

extern FILE *tempFile (const char *const mode, char **const pName)
{
	char *name;
	FILE *fp;
	int fd;
#if defined(HAVE_MKSTEMP)
	const char *const pattern = "tags.XXXXXX";
	const char *tmpdir = NULL;
	fileStatus *file = eStat (ExecutableProgram);
	if (! file->isSetuid)
		tmpdir = getenv ("TMPDIR");
	if (tmpdir == NULL)
		tmpdir = TMPDIR;
	name = xMalloc (strlen (tmpdir) + 1 + strlen (pattern) + 1, char);
	sprintf (name, "%s%c%s", tmpdir, OUTPUT_PATH_SEPARATOR, pattern);
	fd = mkstemp (name);
	eStatFree (file);
#elif defined(HAVE_TEMPNAM)
	name = tempnam (TMPDIR, "tags");
	if (name == NULL)
		error (FATAL | PERROR, "cannot allocate temporary file name");
	fd = open (name, O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);
#else
	name = xMalloc (L_tmpnam, char);
	if (tmpnam (name) != name)
		error (FATAL | PERROR, "cannot assign temporary file name");
	fd = open (name, O_RDWR | O_CREAT | O_EXCL, S_IRUSR | S_IWUSR);
#endif
	if (fd == -1)
		error (FATAL | PERROR, "cannot open temporary file");
	fp = fdopen (fd, mode);
	if (fp == NULL)
		error (FATAL | PERROR, "cannot open temporary file");
	DebugStatement (
		debugPrintf (DEBUG_STATUS, "opened temporary file %s\n", name); )
	Assert (*pName == NULL);
	*pName = name;
	return fp;
}

/* vi:set tabstop=4 shiftwidth=4: */
/*
*   $Id$
*
*   Copyright (c) 1996-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions to sort the tag entries.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#if defined (HAVE_STDLIB_H)
# include <stdlib.h>  /* to declare malloc () */
#endif
#include <string.h>
#include <stdio.h>

#include "debug.h"
#include "entry.h"
#include "options.h"
#include "read.h"
#include "routines.h"
#include "sort.h"

/*
*   FUNCTION DEFINITIONS
*/

/* catFile - now implemented in Rust (cparser.rs) */
extern void catFile(const char *name);

#ifdef EXTERNAL_SORT

#ifdef NON_CONST_PUTENV_PROTOTYPE
# define PE_CONST
#else
# define PE_CONST const
#endif

extern void externalSortTags (const boolean toStdout)
{
	const char *const sortNormalCommand = "sort -u -o";
	const char *const sortFoldedCommand = "sort -u -f -o";
	const char *sortCommand =
		Option.sorted == SO_FOLDSORTED ? sortFoldedCommand : sortNormalCommand;
	PE_CONST char *const sortOrder1 = "LC_COLLATE=C";
	PE_CONST char *const sortOrder2 = "LC_ALL=C";
	const size_t length = 4 + strlen (sortOrder1) + strlen (sortOrder2) +
			strlen (sortCommand) + (2 * strlen (tagFileName ()));
	char *const cmd = (char *) malloc (length + 1);
	int ret = -1;

	if (cmd != NULL)
	{
		/*  Ensure ASCII value sort order.
		 */
#ifdef HAVE_SETENV
		setenv ("LC_COLLATE", "C", 1);
		setenv ("LC_ALL", "C", 1);
		sprintf (cmd, "%s %s %s", sortCommand, tagFileName (), tagFileName ());
#else
# ifdef HAVE_PUTENV
		putenv (sortOrder1);
		putenv (sortOrder2);
		sprintf (cmd, "%s %s %s", sortCommand, tagFileName (), tagFileName ());
# else
		sprintf (cmd, "%s %s %s %s %s", sortOrder1, sortOrder2, sortCommand,
				tagFileName (), tagFileName ());
# endif
#endif
		verbose ("system (\"%s\")\n", cmd);
		ret = system (cmd);
		free (cmd);

	}
	if (ret != 0)
		error (FATAL | PERROR, "cannot sort tag file");
	else if (toStdout)
		catFile (tagFileName ());
}

#else

/*
 *  These functions provide a basic internal sort. No great memory
 *  optimization is performed (e.g. recursive subdivided sorts),
 *  so have lots of memory if you have large tag files.
 */

void failedSort (FILE *const fp, const char* msg)
{
	const char* const cannotSort = "cannot sort tag file";
	if (fp != NULL)
		fclose (fp);
	if (msg == NULL)
		error (FATAL | PERROR, "%s", cannotSort);
	else
		error (FATAL, "%s: %s", msg, cannotSort);
}

int compareTagsFolded(const void *const one, const void *const two)
{
	const char *const line1 = *(const char* const*) one;
	const char *const line2 = *(const char* const*) two;

	return struppercmp (line1, line2);
}

int compareTags (const void *const one, const void *const two)
{
	const char *const line1 = *(const char* const*) one;
	const char *const line2 = *(const char* const*) two;

	return strcmp (line1, line2);
}

void writeSortedTags (
		char **const table, const size_t numTags, const boolean toStdout)
{
	FILE *fp;
	size_t i;

	/*  Write the sorted lines back into the tag file.
	 */
	if (toStdout)
		fp = stdout;
	else
	{
		fp = fopen (tagFileName (), "w");
		if (fp == NULL)
			failedSort (fp, NULL);
	}
	for (i = 0 ; i < numTags ; ++i)
	{
		/*  Here we filter out identical tag *lines* (including search
		 *  pattern) if this is not an xref file.
		 */
		if (i == 0  ||  Option.xref  ||  strcmp (table [i], table [i-1]) != 0)
			if (fputs (table [i], fp) == EOF)
				failedSort (fp, NULL);
	}
	if (toStdout)
		fflush (fp);
	else
		fclose (fp);
}

extern void internalSortTags (const boolean toStdout)
{
	vString *vLine = rs_vStringNew ();
	FILE *fp = NULL;
	const char *line;
	size_t i;
	int (*cmpFunc)(const void *, const void *);

	/*  Allocate a table of line pointers to be sorted.
	 */
	size_t numTags = TagFile.numTags.added + TagFile.numTags.prev;
	const size_t tableSize = numTags * sizeof (char *);
	char **const table = (char **) malloc (tableSize);  /* line pointers */
	DebugStatement ( size_t mallocSize = tableSize; )  /* cumulative total */


	cmpFunc = Option.sorted == SO_FOLDSORTED ? compareTagsFolded : compareTags;
	if (table == NULL)
		failedSort (fp, "out of memory");

	/*  Open the tag file and place its lines into allocated buffers.
	 */
	fp = fopen (tagFileName (), "r");
	if (fp == NULL)
		failedSort (fp, NULL);
	for (i = 0  ;  i < numTags  &&  ! feof (fp)  ;  )
	{
		line = readLine (vLine, fp);
		if (line == NULL)
		{
			if (! feof (fp))
				failedSort (fp, NULL);
			break;
		}
		else if (*line == '\0'  ||  strcmp (line, "\n") == 0)
			;  /* ignore blank lines */
		else
		{
			const size_t stringSize = strlen (line) + 1;

			table [i] = (char *) malloc (stringSize);
			if (table [i] == NULL)
				failedSort (fp, "out of memory");
			DebugStatement ( mallocSize += stringSize; )
			strcpy (table [i], line);
			++i;
		}
	}
	numTags = i;
	fclose (fp);
	rs_vStringDelete (vLine);

	/*  Sort the lines.
	 */
	qsort (table, numTags, sizeof (*table), cmpFunc);

	writeSortedTags (table, numTags, toStdout);

	PrintStatus (("sort memory: %ld bytes\n", (long) mallocSize));
	for (i = 0 ; i < numTags ; ++i)
		free (table [i]);
	free (table);
}

#endif

/* vi:set tabstop=4 shiftwidth=4: */
/*
*   $Id$
*
*   Copyright (c) 1999-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions managing resizable string lists.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <string.h>
#ifdef HAVE_FNMATCH_H
# include <fnmatch.h>
#endif

#include "debug.h"
#include "read.h"
#include "routines.h"
#include "strlist.h"

/*
*   FUNCTION DEFINITIONS
*   Most stringList functions now call Rust directly (rs_* FFI).
*/

extern stringList* stringListNewFromFile (const char* const fileName)
{
	stringList* result = NULL;
	FILE* const fp = fopen (fileName, "r");
	if (fp != NULL)
	{
		result = rs_stringListNew ();
		while (! feof (fp))
		{
			vString* const str = rs_vStringNew ();
			readLine (str, fp);
			rs_vStringStripTrailing (str);
			if (vStringLength (str) > 0)
				rs_stringListAdd (result, str);
			else
				rs_vStringDelete (str);
		}
	}
	return result;
}

/* Helper functions compareString, compareStringInsensitive, stringListIndex,
 * stringListHas, stringListHasInsensitive, stringListHasTest,
 * stringListRemoveExtension, stringListExtensionMatched removed - now
 * handled by Rust implementation in src/ctags_rs/strlist.rs */

/* vi:set tabstop=4 shiftwidth=4: */
/*
*   $Id$
*
*   Copyright (c) 1998-2002, Darren Hiebert
*
*   This source code is released for free distribution under the terms of the
*   GNU General Public License.
*
*   This module contains functions supporting resizeable strings.
*/

/*
*   INCLUDE FILES
*/
#include "general.h"  /* must always come first */

#include <limits.h>  /* to define INT_MAX */
#include <string.h>
#include <ctype.h>

#include "debug.h"
#include "routines.h"
#include "vstring.h"

/*
*   DATA DEFINITIONS
*/
/*
*   External interface
*   All vString functions are now implemented in Rust: src/ctags_rs/vstring.rs
*/







/* vi:set tabstop=4 shiftwidth=4: */
