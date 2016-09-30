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
            "    %s [options] pattern...\n"
            , g_program);
    fprintf(file,
            "\n"
            "DESCRIPTION\n"
            "    %s reads lines from stdin and echoes them only if they match <pattern>.\n"
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
            "    -[no-]s:hift                Whether to check all shifted versions of pattern. [%s-shift]\n"
            , kDefaultShift ? "" : "-no");
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
int PatternMatches(const char* line, size_t line_length, const char* pattern) {
    PrintVerbose("pattern=\"%s\" line=\"%s\"", pattern, line);
    if (line_length != strlen(pattern)) {
        return 0;
    }
    unsigned char line_char_for_pattern[0x100] = "";
    unsigned char pattern_for_line_char[0x100] = "";
    size_t i = 0;
    for (i = 0; i < line_length; ++i) {
        unsigned char pch = pattern[i];
        unsigned char lch = line[i];
        if ((0 == line_char_for_pattern[pch]) &&
            (0 == pattern_for_line_char[lch])) {
            line_char_for_pattern[pch] = lch;
            pattern_for_line_char[lch] = pch;
        } else if ((line_char_for_pattern[pch] != lch) ||
                   (pattern_for_line_char[lch] != pch)) {
            return 0;
        }
    }
    return 1;
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
    char line[0x0400];
    while (NULL != fgets(line, sizeof(line), stdin)) {
        size_t wlen = strlen(line);
        if (wlen > 0) {
            if (line[wlen-1] == '\n') {
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
                if (g_shift) {
                    size_t shift_i = 0;
                    for (shift_i = 0; !pattern_found && (shift_i < plen); ++shift_i) {
                        PrintVerbose("Checking pattern \"%s\".", pattern);
                        if (PatternMatches(line, wlen, pattern)) {
                            printf("%s\n", line);
                            pattern_found = 1;
                        }
                        CircularShiftLeft(pattern, plen, 1);
                    }
                } else {
                    PrintVerbose("Checking pattern \"%s\".", pattern);
                    if (PatternMatches(line, wlen, pattern)) {
                        printf("%s\n", line);
                        pattern_found = 1;
                    }
                }
            }
        }
    }
    return 0;
}   /* main() */

