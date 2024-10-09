/* Copyright (c) 2016-2019 Doug Rogers under the Zero Clause BSD License. */
/* You are free to do whatever you want with this software. See LICENSE.txt. */

#include <assert.h>
#include <ctype.h>
#include <libgen.h>
#include <limits.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "wd-util.h"

/**
 * Name of this program; this is set to `basename(argv[0])`.
 */
//const char *g_program = "wd-nodup";

/**
 * Letters to use, in order, for the canonical representation of a pattern to
 * match.
 */
const unsigned char CANONICAL_MAPPING[] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-";

/**
 * Whether verbose messages should be emitted.
 */
//int g_verbose = 0;

/* ------------------------------------------------------------------------- */
/**
 * Prints usage information to @p file.
 *
 * Note: This function does not return.
 *
 * @param file - FILE stream to which to write the usage information.
 *
 * @param exit_code - value to pass to exit() when ending program.
 */
void usage(FILE* file, int exit_code) __attribute__((noreturn));
void usage(FILE* file, int exit_code) {
    fprintf(file,
            "\n"
            "USAGE\n"
            "    %s [options] pattern...\n"
            "\n"
            "DESCRIPTION\n"
            "    %s reads lines from stdin and echoes them only if they contain\n"
            "    no duplicated letters.\n"
            "\n"
            "OPTIONS\n"
            "    -h  Show this usage information.\n"
            "    -v  Print verbose (debug) messages to stderr.\n"
            "    -r  Reverse - print only lines with duplicated characters.\n"
            "    -i  Comparisons and patterns are case-insensitive.\n"
            "\n"
            "\n", g_program, g_program);
    exit(exit_code);
}

/* ------------------------------------------------------------------------- */
/**
 * Print a formatted message if `g_verbose` is non-zero.
 */
void print_verbose(const char* format, ...) {
    if (g_verbose) {
        char text[0x400] = "";
        va_list va;
        va_start(va, format);
        vsnprintf(text, sizeof(text), format, va);
        va_end(va);
        printf("%s: %s\n", g_program, text);
    }
}

/* ------------------------------------------------------------------------- */
/**
 * Main program. Parses command line arguments. See Usage().
 *
 * @param argc - number of command line arguments, including program name.
 *
 * @param argv - list of pointers to command line argument strings.
 *
 * @return the program's exit code: 0 on success, something else on failure.
 */
int main(int argc, char* argv[]) {
    int ch = 0;
    int case_insensitive = 0;
    int reverse_no_dup = 0;
    unsigned char line_bytes[0x100] = "";

    g_program = basename(argv[0]);

    while ((ch = getopt(argc, argv, "hvir")) != EOF) {
        switch (ch) {
        case 'h': usage(stdout, 0); break;
        case 'v': g_verbose = 1; break;
        case 'i': case_insensitive = 1; break;
        case 'r': reverse_no_dup = 1; break;
        default:
            fprintf(stderr, "%s: invalid option -%c; use '%s -h' for usage\n", g_program, ch, g_program);
            exit(1);
        }
    }

    argc -= optind;
    argv += optind;

    if (argc >= 1) {
        fprintf(stderr, "%s: no arguments allowed\n\n", g_program);
        usage(stderr, 2);
    }

    int line_no = 0;
    char line[0x0400];
    while (NULL != fgets(line, sizeof(line), stdin)) {
        int dup_found = 0;
        line_no++;
        memset(line_bytes, 0, sizeof(line_bytes));
        for (unsigned char *bytes = (unsigned char *)&line[0]; *bytes; bytes++) {
            unsigned char byte = case_insensitive ? tolower(*bytes) : *bytes;
            if (line_bytes[byte]) {
                dup_found = 1;
                break;
            }
            line_bytes[byte] = 1;
        }
        if (dup_found ^ !reverse_no_dup) {
            printf("%s", line);
        }
    }

    return 0;
}   /* main() */
