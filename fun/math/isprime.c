/* Copyright (c) 2016 FireEye Incorporated. All rights reserved. */

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>

#include <gmp.h>

/**
 * Default name of this program.
 */
#define kProgram "primesto"

/**
 * Name of this program; this may be modified by argv[0] in main().
 */
const char* g_program = kProgram;

/**
 * Whether to show composites.
 */
#define kDefaultShowComposite 0
int g_show_composite = 0;

/**
 * Whether to show primes.
 */
#define kDefaultShowPrime 0
int g_show_prime = 0;

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
            "    %s determines if all its decimal arguments are prime. If any are composite\n"
            "    then the exit code will be 1, otherwise the exit code will be 0.\n"
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
            "    -[no-][show-]c:omposite:s   Show arguments that are composite. [%s-show-composites]\n"
            , kDefaultShowComposite ? "" : "-no");
    fprintf(file,
            "\n"
            "    -[no-][show-]p:rime:s       Show arguments that are primes. [%s-show-primes]\n"
            , kDefaultShowPrime ? "" : "-no");
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
        } else if (IsFlagOption(arg, &g_show_composite, "c:omposite:s")) {
        } else if (IsFlagOption(arg, &g_show_composite, "show-c:omposite:s")) {
        } else if (IsFlagOption(arg, &g_show_prime, "p:rime:s")) {
        } else if (IsFlagOption(arg, &g_show_prime, "show-p:rime:s")) {
        } else if (IsFlagOption(arg, &g_verbose, "v:erbose")) {
        } else {
            PrintUsageError("invalid option \"%s\"", arg);
        }
    }
    return rval;
}   /* ParseOptions() */

/* ------------------------------------------------------------------------- */
/* int IntIsPositivePrime(int n) { */
/*     if (n < 2) */
/*         return 0; */
/*     if (2 == n) */
/*         return 1; */
/*     if (0 == (n % 2)) */
/*         return 0; */
/*     for (int k = 3; (k * k) <= n; k += 2) { */
/*         if (0 == (n % k)) */
/*             return 0; */
/*     } */
/*     return 1; */
/* }   /\* IntIsPositivePrime() *\/ */

/* ------------------------------------------------------------------------- */
/**
 * Test for primality using GMP's probable prime function.
 */
int IsPrime(const mpz_t n) {
    /*
     * mpz_probab_prime_p() says that the probability of the number being a
     * prime is P = 4^(-reps), or 1/(4^reps). I'm not sure whether the
     * documentation means that the probability for this particular number is
     * P or whether P applies to all numbers of that size. If it's the latter
     * then I need to bump up reps based on the size of the number so that P
     * times digits-base-4 is very small.
     */
    int tetrads = mpz_sizeinbase(n, 4);
    tetrads = (tetrads < 5) ? 5 : tetrads;      /* Minimum 50 reps. */
    int result = 0;
    int reps = 4 * tetrads;
    const int kMaxReps = 4096;
    reps = (reps < 0) ? kMaxReps : reps;
    if (g_verbose) {
        char* n_text = mpz_get_str(NULL, 10, n);
        printf("testing primality of n=%s\n", n_text);
        free(n_text);
    }
    while (1) {
        result = mpz_probab_prime_p(n, reps);
        if (g_verbose) {
            printf("  mpz_probab_prime_p(n, reps=%d) -> %d\n", reps, result);
        }
        switch (result) {
        case 0: return 0;       /* Definitely composite. */
        case 2: return 1;       /* Definitely prime. */
        case 1:
            if (reps >= kMaxReps) {
                return 1;       /* Best guess and we're not gonna try any more. */
            }
            reps = 2 * reps;
            if ((reps < 0) || (reps > kMaxReps)) {
                reps = kMaxReps;
            }
            break;
        }
    }   /* while getting a 'probably prime' result. */
}   /* IsPrime() */

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
    if (argc < 2) {
        Usage(stderr, 1);
    }
    int is_prime = 1;
    for (int i = 1; i < argc; i++) {
        char* arg = argv[i];
        mpz_t n;
        mpz_init(n);
        if (0 != mpz_set_str(n, arg, 10)) {
            PrintUsageError("invalid decimal number \"%s\".", arg);
        }
        if (IsPrime(n)) {
            if (g_verbose) {
                printf("%s is prime\n", arg);
            }
            if (g_show_prime) {
                printf("%s\n", arg);
            }
        } else {
            is_prime = 0;
            if (g_verbose) {
                printf("%s is composite\n", arg);
            }
            if (g_show_composite) {
                printf("%s\n", arg);
            }
        }
        mpz_clear(n);
    }
    return is_prime ? 0 : 1;
}   /* main() */
