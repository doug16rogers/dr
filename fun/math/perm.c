/* Copyright (c) 2016-2019 Doug Rogers under the Zero Clause BSD License. */
/* You are free to do whatever you want with this software. See LICENSE.txt. */

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
 * Default name of this program.
 */
#define kProgram "perm"

/**
 * Name of this program; this may be modified by argv[0] in main().
 */
const char* g_program = kProgram;

/**
 * Default value for verbose setting.
 */
#define kDefaultVerbose 0

/**
 * Whether or not to emit verbose messages.
 */
int g_verbose = 0;

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
            "    %s [options] args...\n"
            , g_program);
    fprintf(file,
            "\n"
            "DESCRIPTION\n"
            "    %s print all permutations of the commant line arguments.\n"
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
            "    -[no-]v:erbose              Print verbose (debug) messages. [%s-verbose]\n"
            , kDefaultVerbose ? "" : "-no");
    exit(exit_code);
}   /* Usage() */

/* ------------------------------------------------------------------------- */
/**
 * Print an error message to stderr then exit the program with exit code 1.
 */
void PrintUsageError(const char* format, ...) {
    char text[0x0100] = "";
    va_list va;
    va_start(va, format);
    vsnprintf(text, sizeof(text), format, va);
    va_end(va);
    fprintf(stderr, "%s: %s\n", g_program, text);
    fprintf(stderr, "%s: Use '%s --help' for usage information.\n", g_program, g_program);
    exit(1);
}   /* PrintUsageError() */

/* ------------------------------------------------------------------------- */
/**
 * Find and return a pointer to the file name portion of @a path.
 *
 * @param path - a path whose name is desired. Typically this is argv[0] from
 * main().
 *
 * @return a pointer the first character after the last directory delimiter
 * (forward or back slash) in @a path, or @a path if none is found.
 */
const char* NamePartOfPath(const char* path) {
    const char* rval = path;
    if (NULL != path) {
        for (; 0 != *path; ++path) {
            if ((('/' == path[0]) || ('\\' == path[0])) &&
                !((0 == path[1]) || ('/' == path[1]) || ('\\' == path[1]))) {
                rval = &path[1];
            }
        }
    }
    return rval;
}   /* NamePartOfPath() */

/* ------------------------------------------------------------------------- */
/**
 * Look for an option of the form "[-[-]]option[=value]".
 *
 * If @a input contains '=' then non-null @a *value_ptr is set to point
 * to the character after '=' (or is set to NULL if there is no argument).
 *
 * @a descriptor may contain ':' characters which indicate abbreviation
 * points for the option. For example, "o:pt:ion" will match "-o",
 * "-o=value", "-opt", "-opt=value", "-option" and "-option=value".
 *
 * @return 1 if @a input matches @a descriptor, 0 otherwise.
 */
int IsOption(const char* input, const char** value_ptr, const char* descriptor) {
    int rval = 0;
    int finished = 0;
    assert(NULL != input);
    assert(NULL != descriptor);
    if ('-' == *input) {
        ++input;
        if ('-' == *input) {
            ++input;
        }
    } else {
        finished = 1;
    }
    while (!finished) {
        if ((0 == *input) || ('=' == *input)) {
            finished = 1;
            rval = (0 == *descriptor) || (':' == *descriptor);
        } else if ((0 == *descriptor) || ((':' != *descriptor) && (*input != *descriptor))) {
            finished = 1;
        } else {
            if (':' != *descriptor) {
                ++input;
            }
            ++descriptor;
        }
    }
    if (NULL != value_ptr) {
        *value_ptr = (rval && ('=' == *input)) ? (input + 1) : NULL;
    }
    return rval;
}   /* IsOption() */

/* ------------------------------------------------------------------------- */
/**
 * Look for flag option of the form "-[-][no-]option".
 *
 * @a descriptor may contain ':' characters which indicate abbreviation
 * points for the option. See IsOption() for more information.
 *
 * If @a input matches the descriptor then the value of @a *flag_value_ptr (if
 * not NULL) will be set to 1. If @a input matches the descriptor with "no-"
 * prefixed then @a *flag_value_ptr will be set to 0. If @a input does not
 * match @a descriptor, @a *flag_value_ptr is not modified.
 *
 * @return 1 if @a input matches @a descriptor with or without a "no-" prefix,
 * 0 otherwise.
 */
int IsFlagOption(const char* input, int* flag_value_ptr, const char* descriptor) {
    int flag_value = 1;
    int rval = 0;
    assert(NULL != input);
    assert(NULL != descriptor);
    if ('-' == *input) {
        rval = IsOption(input, NULL, descriptor);
        if (!rval) {
            flag_value = 0;
            const int k = ('-' == input[1]) ? 1 : 0;
            if (('n' == input[k+1]) && ('o' == input[k+2]) && ('-' == input[k+3])) {
                rval = IsOption(&input[k+3], NULL, descriptor);
            }
        }
    }
    if (rval && (NULL != flag_value_ptr)) {
        *flag_value_ptr = flag_value;
    }
    return rval;
}   /* IsFlagOption() */

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
        } else if (IsFlagOption(arg, &g_verbose, "v:erbose")) {
        } else {
            PrintUsageError("invalid option \"%s\"", arg);
        }
    }
    return rval;
}   /* ParseOptions() */

/* ------------------------------------------------------------------------- */
void print_two(const char* l, const char* r) {
    printf("%s%s\n", l, r);
}   /* print_two() */

/* ------------------------------------------------------------------------- */
void print_perm(int (*printer)(const char*), char* l, int llen, char* r, int rlen) {
    if (0 == rlen) {
        printer(l);
    } else {
        l[llen] = r[0];
        l[llen+1] = 0;
        print_perm(printer, l, llen + 1, r + 1, rlen - 1);
        for (int ri = 1; ri < rlen; ++ri) {
            char tmp = l[llen];
            l[llen] = r[ri];
            r[ri] = tmp;
            print_perm(printer, l, llen + 1, r + 1, rlen - 1);
        }
        /* Restore original order for caller. */
        for (int ri = 0; ri < rlen - 1; ri++) {
            r[ri] = r[ri+1];
        }
        r[rlen - 1] = l[llen];
    }
}   /* print_perm() */

/*

remove x (x:ys) = ys
remove x (y:ys) = remove x ys

foreach xl 
perm2 [] [] = []
perm2 _ [] = [x]
perm2 xl (y:ys) = perm2 (xl:y) ys

perm [] = []
perm (x:xs) = map 

 */

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
    g_program = NamePartOfPath(argv[0]);
    argc = ParseOptions(argc, argv);  /* Remove options; leave program name and arguments. */
    if (argc < 1) {
        Usage(stderr, 1);
    }
    for (int i = 1; i < argc; ++i) {
        char* arg = argv[i];
        int len = strlen(arg);
        char* left = calloc(1, len + 1);
        if (NULL == left) {
            perror("out of memory");
            return 2;
        }
        *left = 0;
        print_perm(puts, left, 0, arg, len);
#if 0
        char* arg = argv[i];
        int len = strlen(arg);
        char* out = calloc(1, len + 1);
        out[0] = arg[0];
        for (k = 1; k < len; ++k) {
            out[0] = arg[k];
            permute(&arg[1], len - 1, &out[1]);
            printf("%s\n", out);
        }
#endif
    }
    return 0;
}   /* main() */

