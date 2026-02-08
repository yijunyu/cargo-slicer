/*
 *   precc_ffi.h - Shared FFI interface for precc ctags parsers
 *
 *   This header provides compatibility macros and declarations for
 *   language parsers adapted from universal-ctags to precc's infrastructure.
 *
 *   Copyright (c) 2024-2025, precc contributors
 *   Released under the GNU General Public License.
 */
#ifndef PRECC_FFI_H
#define PRECC_FFI_H

#include "general.h"
#include "vstring.h"
#include "keyword.h"
#include "read.h"

/*
 *   UTILITY MACROS
 */

#ifndef ARRAY_SIZE
#define ARRAY_SIZE(arr) (sizeof(arr) / sizeof((arr)[0]))
#endif

#ifndef CTAGS_ATTR_UNUSED
#define CTAGS_ATTR_UNUSED __attribute__((unused))
#endif

#ifndef KIND_GHOST_INDEX
#define KIND_GHOST_INDEX -1
#endif

/*
 *   VSTRING COMPATIBILITY MACROS
 *
 *   Map universal-ctags vString functions to precc's Rust FFI versions.
 *   The rs_* functions are implemented in src/ctags_rs/vstring.rs
 */

#define vStringNew()         rs_vStringNew()
#define vStringDelete(s)     rs_vStringDelete(s)
#define vStringClear(s)      rs_vStringClear(s)
#define vStringPut(s,c)      rs_vStringPut((s),(c))
#define vStringCopyS(d,s)    rs_vStringCopyS((d),(s))

/*
 *   KEYWORD COMPATIBILITY MACROS
 *
 *   Map universal-ctags keyword functions to precc's Rust FFI versions.
 *   The rs_* functions are implemented in src/ctags_rs/keyword.rs
 */

#define addKeyword(s,l,v)    rs_addKeyword((s),(l),(v))
#define lookupKeyword(s,l)   rs_lookupKeyword((s),(l))

/*
 *   FILE INPUT HELPERS
 *
 *   These provide line-based input for parsers that read line-by-line
 *   rather than character-by-character like the C parser.
 */

/* Macro for parsers that need to read lines from input file */
#define PRECC_DEFINE_READ_LINE_HELPER(prefix) \
    static vString *prefix##_line_vstr = NULL; \
    static const unsigned char *prefix##ReadLineFromInputFile(void) { \
        if (prefix##_line_vstr == NULL) \
            prefix##_line_vstr = rs_vStringNew(); \
        rs_vStringClear(prefix##_line_vstr); \
        if (readLine(prefix##_line_vstr, File.fp) == NULL) \
            return NULL; \
        return (const unsigned char *)vStringValue(prefix##_line_vstr); \
    }

/*
 *   PARSER SCOPE HELPERS
 *
 *   The addToScope function is commonly used by parsers to build
 *   hierarchical scope strings.
 */

/* addToScope - delegate to Rust implementation */
extern void rs_addToScope(vString *scope, const vString *name);
#define addToScope(scope, name) rs_addToScope((vString *)(scope), (const vString *)(name))

#endif /* PRECC_FFI_H */
