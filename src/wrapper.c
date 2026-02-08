#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "general.h"
#include "main.h"
#include "options.h"
#include "read.h"
#include "routines.h"
#include "parse.h"
#include "entry.h"

extern int main_ffi(int argc, char **argv);
extern void rs_resetCParserState(void);

// Thread-local storage for per-thread cached state
// This allows multiple threads to process files concurrently
static __thread char *cached_argv[8] = {NULL};
static __thread char *cached_filename = NULL;
static __thread size_t cached_filename_len = 0;
static __thread int tls_initialized = 0;

// Process a single file with direct event handling (thread-safe version)
int dctags_process_file_direct(const char* filename) {
    if (!filename) {
        return 1;
    }

    // Initialize argv array for this thread on first call
    if (!tls_initialized) {
        // Static strings - no need to duplicate
        cached_argv[0] = (char*)"dctags";
        cached_argv[1] = (char*)"-D";
        cached_argv[2] = (char*)"1";
        cached_argv[3] = (char*)"-u";
        cached_argv[4] = (char*)"--c-kinds=+pxeg-d-m";  // Include enums (g) and enumerators (e) for anonymous enum support
        cached_argv[5] = (char*)"--langmap=c:.c.i";
        cached_argv[6] = NULL;  // Will be set to filename
        cached_argv[7] = NULL;  // NULL terminator
        tls_initialized = 1;
    }

    // Reuse or reallocate filename buffer as needed
    size_t filename_len = strlen(filename);
    if (cached_filename == NULL || filename_len > cached_filename_len) {
        // Need larger buffer
        free(cached_filename);
        cached_filename_len = filename_len + 64;  // Add some headroom
        cached_filename = (char*)malloc(cached_filename_len);
        if (!cached_filename) {
            perror("Failed to allocate filename buffer");
            return 1;
        }
    }

    // Copy filename into buffer
    strcpy(cached_filename, filename);
    cached_argv[6] = cached_filename;

    // Reset C parser state before processing each file
    // This prevents AnonymousID and other state from corrupting subsequent files
    rs_resetCParserState();

    return main_ffi(7, cached_argv);
}

// Thread-local storage for Rust-specific cached state
static __thread char *rust_cached_argv[8] = {NULL};
static __thread char *rust_cached_filename = NULL;
static __thread size_t rust_cached_filename_len = 0;
static __thread int rust_tls_initialized = 0;

// Process a Rust file with ctags Rust parser (thread-safe version)
int dctags_process_rust_file(const char* filename) {
    if (!filename) {
        return 1;
    }

    // Initialize argv array for this thread on first call
    if (!rust_tls_initialized) {
        // Static strings - no need to duplicate
        rust_cached_argv[0] = (char*)"dctags";
        rust_cached_argv[1] = (char*)"-D";
        rust_cached_argv[2] = (char*)"1";
        rust_cached_argv[3] = (char*)"-u";
        // Enable all Rust kinds: modules, structs, traits, impls, functions, enums, types, statics, macros, fields, variants, methods, consts
        rust_cached_argv[4] = (char*)"--rust-kinds=+nmstfeTVcCMSFaP";
        rust_cached_argv[5] = (char*)"--langmap=rust:.rs";
        rust_cached_argv[6] = NULL;  // Will be set to filename
        rust_cached_argv[7] = NULL;  // NULL terminator
        rust_tls_initialized = 1;
    }

    // Reuse or reallocate filename buffer as needed
    size_t filename_len = strlen(filename);
    if (rust_cached_filename == NULL || filename_len > rust_cached_filename_len) {
        // Need larger buffer
        free(rust_cached_filename);
        rust_cached_filename_len = filename_len + 64;  // Add some headroom
        rust_cached_filename = (char*)malloc(rust_cached_filename_len);
        if (!rust_cached_filename) {
            perror("Failed to allocate filename buffer");
            return 1;
        }
    }

    // Copy filename into buffer
    strcpy(rust_cached_filename, filename);
    rust_cached_argv[6] = rust_cached_filename;

    return main_ffi(7, rust_cached_argv);
}
