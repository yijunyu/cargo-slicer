/* pu_type.h - Shared enum for processing unit types between C and Rust
 *
 * This header defines numeric values for PU types to eliminate string
 * comparisons in the FFI boundary between ctags (C) and precc (Rust).
 */

#ifndef PU_TYPE_H
#define PU_TYPE_H

/* Processing Unit Type identifiers - must match Rust PuType enum exactly */
typedef enum {
    PU_TYPE_FUNCTION   = 0,
    PU_TYPE_VARIABLE   = 1,
    PU_TYPE_EXTERNVAR  = 2,
    PU_TYPE_TYPEDEF    = 3,
    PU_TYPE_ENUM       = 4,
    PU_TYPE_STRUCT     = 5,
    PU_TYPE_UNION      = 6,
    PU_TYPE_ENUMERATOR = 7,
    PU_TYPE_PROTOTYPE  = 8,
    PU_TYPE_MEMBER     = 9,
    PU_TYPE_UNKNOWN    = 255
} PuTypeId;

/* Convert kind string to PuTypeId - fast first-char dispatch */
static inline PuTypeId pu_type_from_kind_name(const char *kind_name) {
    if (kind_name == NULL || kind_name[0] == '\0') {
        return PU_TYPE_UNKNOWN;
    }

    switch (kind_name[0]) {
        case 'f':
            if (kind_name[1] == 'u' && kind_name[2] == 'n') {
                return PU_TYPE_FUNCTION; /* "function" */
            }
            if (kind_name[1] == 'n' && kind_name[2] == '\0') {
                return PU_TYPE_FUNCTION; /* "fn" */
            }
            break;
        case 'v':
            if (kind_name[1] == 'a') {
                return PU_TYPE_VARIABLE; /* "variable" */
            }
            break;
        case 'e':
            if (kind_name[1] == 'x') {
                return PU_TYPE_EXTERNVAR; /* "externvar" */
            }
            if (kind_name[1] == 'n' && kind_name[2] == 'u') {
                if (kind_name[3] == 'm' && kind_name[4] == 'e') {
                    return PU_TYPE_ENUMERATOR; /* "enumerator" */
                }
                return PU_TYPE_ENUM; /* "enum" */
            }
            break;
        case 't':
            if (kind_name[1] == 'y') {
                return PU_TYPE_TYPEDEF; /* "typedef" */
            }
            break;
        case 's':
            if (kind_name[1] == 't') {
                return PU_TYPE_STRUCT; /* "struct" */
            }
            break;
        case 'u':
            if (kind_name[1] == 'n') {
                return PU_TYPE_UNION; /* "union" */
            }
            break;
        case 'p':
            if (kind_name[1] == 'r') {
                return PU_TYPE_PROTOTYPE; /* "prototype" */
            }
            break;
        case 'm':
            if (kind_name[1] == 'e') {
                return PU_TYPE_MEMBER; /* "member" */
            }
            break;
    }
    return PU_TYPE_UNKNOWN;
}

#endif /* PU_TYPE_H */
