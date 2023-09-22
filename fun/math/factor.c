/* Copyright (c) 2016-2019 Doug Rogers under the Zero Clause BSD License. */
/* You are free to do whatever you want with this software. See LICENSE.txt. */

#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <gmp.h>

#include "sieve.h"

/**
 * Default name of this program.
 */
#define kProgram "factor"

/**
 * Name of this program; this may be modified by argv[0] in main().
 */
const char* g_program = kProgram;

/**
 * Base to use for conversion. 0 means that the value can contain 0x/0X
 * (hexadecimal), 0b (binary) or leading 0 (octal).
 */
#define kDefaultBase 0
int g_base = kDefaultBase;

#define kBaseMin 2
#define kBaseMax 62;

/**
 * Default value for verbose setting.
 */
#define kDefaultVerbose 0

/**
 * Whether or not to emit verbose messages.
 */
int g_verbose = 0;

/**
 * Maximum value supported by sieve package - uint64_t.
 */
unsigned long kMaxSieveNumber = 0xFFFFFFFFFFFFFFFFull;

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
            "    -base=<base>                Use <base>; 0 = automatic 0/0x/0b [%d]\n"
            , kDefaultBase);
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
 * Print a message to stderr if g_verbose is set.
 */
void PrintVerbose(int level, const char* format, ...) {
    if (g_verbose >= level) {
        char text[0x0100] = "";
        va_list va;
        va_start(va, format);
        vsnprintf(text, sizeof(text), format, va);
        va_end(va);
        fprintf(stderr, "%s: %s\n", g_program, text);
    }
}   /* PrintVerbose() */

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
int IsIntOption(const char* input, int* int_value_ptr, const char* descriptor, int default_value) {
    int int_value = default_value;
    int rval = 0;
    const char* opt_value = NULL;
    assert(NULL != input);
    assert(NULL != descriptor);
    if ('-' == *input) {
        rval = IsOption(input, &opt_value, descriptor);
        if (rval && (NULL != opt_value) && (0 != opt_value[0])) {
            rval = (1 == sscanf(opt_value, "%d", &int_value));
        }
    }
    if (rval && (NULL != int_value_ptr)) {
        *int_value_ptr = int_value;
    }
    return rval;
}   /* IsIntOption() */

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
        } else if (IsIntOption(arg, &g_base, "b:ase", kDefaultBase)) {
        } else if (IsOption(arg, NULL, "v:erbose")) {
            g_verbose++;
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
int ProbablyPrime(const mpz_t n) {
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
        PrintVerbose(2, "  testing primality of n=%s with mpz_probab_prime_p()", n_text);
        free(n_text);
    }
    while (1) {
        result = mpz_probab_prime_p(n, reps);
        PrintVerbose(2, "  mpz_probab_prime_p(n, reps=%d) -> %d", reps, result);
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
}   /* ProbablyPrime() */

/* ------------------------------------------------------------------------- */
void handle_ui(unsigned long un) {
    uint64_t uq = 0;
    unsigned long factor_count = 0;
    PrintVerbose(1, "  handling %lu as uint64_t", un);
    switch (un) {
    case 0:
    case 1:
    case 2:
    case 3:
        printf("%lu", (unsigned long) un);
        return;
    default:
        break;
    }
    while (un != 1) {
        factor_count++;
        uq = sieve_least_prime_factor(un);
        printf("%s%lu", (factor_count == 1) ? "" : " ", (unsigned long) uq);
        un /= uq;
        if (un != 1) {
            PrintVerbose(1, "  factor %lu; now n=%lu", uq, un);
        }
    }
}   /* handle_ui() */

/* ------------------------------------------------------------------------- */
void handle_number(const char* s) {
    mpz_t n;
    mpz_t q;
    mpz_t r;
    unsigned long d = 0;
    unsigned long factor_count = 0;
    assert(sizeof(unsigned long) >= 8);

    mpz_init(n);
    mpz_init(q);
    mpz_init(r);
    if (0 != mpz_set_str(n, s, g_base)) {
        PrintUsageError("invalid integer number \"%s\".", s);
    }
    if (g_verbose >= 1) {
        fprintf(stderr, "loaded decimal n=");
        mpz_out_str(stderr, 10, n);
        fprintf(stderr, "\n");
    }
    
    if (mpz_sgn(n) < 0) {
        printf("-1 ");
        mpz_neg(r, n);
        mpz_set(n, r);
        mpz_clear(r);
    }

    if (mpz_cmp_ui(n, kMaxSieveNumber) <= 0) {
        handle_ui(mpz_get_ui(n));
        goto done;
    }
    
    /* If the value is a uint64_t, then just use sieve.h services. */
    /* Try 2 first. */
    for (d = 2; (d < 0x100000000ull) && (mpz_cmp_ui(n, d * d) >= 0); d += 2) {
        factor_count++;
        if (mpz_cmp_ui(n, kMaxSieveNumber) <= 0) {
            handle_ui(mpz_get_ui(n));
            goto done;
        }
        if (sieve_is_prime(d)) {
            PrintVerbose(2, "  trying small prime d=%lu", d);
            unsigned long ur = mpz_tdiv_q_ui(q, n, d);
            if (g_verbose >= 2) {
                fprintf(stderr, "  q=");
                mpz_out_str(stderr, 10, q);
                fprintf(stderr, ", ur=%lu\n", ur);
            }
            if (ur == 0) {
                printf("%s%lu", (factor_count == 1) ? "" : " ", d);
                if (mpz_cmp_ui(q, 1) == 0) {
                    break;
                }
                mpz_swap(n, q);
                if (g_verbose >= 1) {
                    fprintf(stderr, "  factor %lu; now n=", d);
                    mpz_out_str(stderr, 10, n);
                    fprintf(stderr, "\n");
                }
                d -= 2;     /* Try this prime again; works for both 2 and odd. */
            }
//            mpz_clear(q);
        }
        if (2 == d) {
            d = 1;         /* Get into the odd numbers now. */
        }
    }
    if (mpz_cmp_ui(n, 1) != 0) {
        mpz_out_str(stdout, g_base, n);
        if (!ProbablyPrime(n)) {
            printf("?");
        }
    }
done:
    mpz_clear(n);
    mpz_clear(q);
    mpz_clear(r);
    printf("\n");
}   /* handle_number() */

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
        for (;;) {
            char* line = NULL;
            size_t len = 0;
            if (getline(&line, &len, stdin) <= 0) {
                break;
            }
            if ((len > 0) && (line[len - 1] == '\n')) {
                line[len - 1] = 0;
            }
            handle_number(line);
            free(line);
        }
    } else {
        for (int i = 1; i < argc; i++) {
            handle_number(argv[i]);
        }
    }
    return 0;
}   /* main() */
