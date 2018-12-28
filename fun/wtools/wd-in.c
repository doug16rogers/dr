/* Copyright (c) 2016 Doug Rogers under the terms of the MIT License. */
/* See http://www.opensource.org/licenses/mit-license.html.. */
/* $Id$ */

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "wd-util.h"

/**
 * Default value for shift setting.
 */
#define kDefaultShift 0

/**
 * Whether or not to check shifted versions of the pattern against each word.
 */
int g_shift = kDefaultShift;

/* ------------------------------------------------------------------------- */
/**
 * Prints usage information to @a file.
 *
 * Note: This function does not return.
 *
 * @param file - FILE stream to which to write the usage information.
 *
 * @param exit_code - value to pass to exit() when ending program.
 */
void Usage(FILE* file, int exit_code) __attribute__((noreturn));
void Usage(FILE* file, int exit_code) {
    fprintf(file,
            "\n"
            "USAGE\n"
            "    %s [options] letters\n"
            , g_program);
    fprintf(file,
            "\n"
            "DESCRIPTION\n"
            "    %s reads lines from stdin and echoes them only if they can be composed\n"
            "    by the characters in <letters>. <letters> may contain '.' or '?' to match\n"
            "    any letter.\n"
            , g_program);
    fprintf(file,
            "\n"
            "OPTIONS\n"
            "    Options may begin with '-' or '--'. A ':' indicates where options may be\n"
            "    abbreviated\n");
    fprintf(file,
            "\n"
            "    -h:elp                      Show this usage information.\n");
    fprintf(file,
            "\n"
            "    -[no-]v:erbose              Print verbose (debug) messages. [-no-verbose]\n");
    exit(exit_code);
}   /* Usage() */

/* ------------------------------------------------------------------------- */
/**
 * Parse options from the command line, removing them from @a argv[].
 *
 * Note that on error this function does not return.
 *
 * If an argument starts with '-' then it will be treated as an option. See
 * the OPTIONS section of Usage() for the list of options available to this
 * program.
 *
 * If "--" is encountered as an argument, no further option processing will
 * occur, even if a later argument begins with '-'.
 *
 * @param argc - number of argument pointers in array @a argv[].
 *
 * @param argv - array of argument string pointers to parse.
 *
 * @return the number of (non-option) command line arguments that remain in
 * @a argv[] after option processing.
 */
int ParseOptions(int argc, char* argv[]) {
    int rval = 1;       /* Skip program name. */
    char** non_option_argument_list = argv;
    int i = 0;
    int end_of_options = 0;
    for (i = 1; i < argc; ++i) {
        char* arg = argv[i];
        if (end_of_options || ('-' != *arg)) {
            non_option_argument_list[rval++] = arg;
        } else if (('-' == arg[1]) && (0 == arg[2])) {
            end_of_options = 1;
        } else if (IsOption(arg, NULL, "h:elp")) {
            Usage(stdout, 0);
        } else if (IsFlagOption(arg, &g_shift, "s:hift")) {
        } else if (IsFlagOption(arg, &g_verbose, "v:erbose")) {
        } else {
            PrintUsageError("invalid option \"%s\"", arg);
        }
    }
    return rval;
}   /* ParseOptions() */

/* ------------------------------------------------------------------------- */
/**
 * @return 1 if @a line of length @a line_length matches the given letter @a
 * pattern.
 *
 * @todo Optimize the pattern searching by precalculating all the lookups.
 */
int PatternMatches(const char* line, size_t line_length, const char* letters) {
    int rval = 1;
    PrintVerbose("letters=\"%s\" line=\"%s\"", letters, line);
    if (strlen(letters) < line_length) {
        return 0;
    }
    char* dup = strdup(letters);
    if (NULL == dup) {
        fprintf(stderr, "out of memory\n");
        return 0;
    }
    int dlen = strlen(dup);
    size_t i = 0;
    for (i = 0; i < line_length; ++i) {
        char* found = strchr(dup, line[i]);
        if (NULL == found) {
            found = strchr(dup, '.');
        }
        if (NULL == found) {
            found = strchr(dup, '?');
        }
        if (NULL == found) {
            rval = 0;
            break;
        }
        // Remove by putting the last char in this spot.
        dlen--;
        *found = dup[dlen];
        dup[dlen] = 0;
    }
    free(dup);
    return rval;
}   /* PatternMatches() */

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
    SetProgramName(argv[0]);
    argc = ParseOptions(argc, argv);  /* Remove options; leave program name and arguments. */
    if (argc < 2) {
        Usage(stderr, 1);
    }
    int any_pattern_found = 0;
    char line[0x0400];
    while (NULL != fgets(line, sizeof(line), stdin)) {
        size_t wlen = strlen(line);
        if (wlen > 0) {
            if ((line[wlen-1] == '\n') || (line[wlen-1] == '\r')) {
                line[wlen-1] = 0;
                --wlen;
            }
        }
        if (wlen > 0) {
            int pattern_i = 1;
            int pattern_found = 0;
            for (pattern_i = 1; !pattern_found && (pattern_i < argc); ++pattern_i) {
                char* pattern = argv[pattern_i];
                size_t plen = strlen(pattern);
                if (plen < wlen) {
                    ; // PrintVerbose("Not enough letters in \"%s\" for \"%s\".", pattern, line);
                } else {
                    // PrintVerbose("Checking if \"%s\" can be composed of \"%s\".", pattern);
                    if (PatternMatches(line, wlen, pattern)) {
                        printf("%s\n", line);
                        pattern_found = 1;
                        any_pattern_found = 1;
                    }
                }
            }
        }
    }
    return any_pattern_found ? 0 : 5;
}   /* main() */

