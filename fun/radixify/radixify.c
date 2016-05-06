/* Copyright (c) 2016 FireEye Incorporated. All rights reserved. */

#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <gmp.h>

#include "str.h"

/**
 * Default name of this program.
 */
#define kProgram "radixify"

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

/**
 * The list of input digits.
 */
const char* g_input_digits = NULL;

/**
 * The radix from the input digits.
 */
int g_input_radix = 0;

/**
 * The list of output digits.
 */
const char* g_output_digits = NULL;

/**
 * The radix from the output digits.
 */
int g_output_radix = 0;

/**
 * Whether to check the case of input characters for matches against letters
 * in g_input_digits. This will be set automatically.
 */
int g_case_sensitive = 0;

/**
 * Mapping for special digits lists.
 */
typedef struct SpecialDigits_s {
    char short_cut;
    const char* digits;
    const char* description;
} SpecialDigits;

/**
 * List of supported special digits lists.
 */
const SpecialDigits kSpecials[] = {
    { 'u', "1",                "unary sequence of 1s" },
    { 'b', "01",               "binary" },
    { 'o', "01234567",         "octal" },
    { 'd', "0123456789",       "decimal" },
    { 'x', "0123456789abcdef", "hexadecimal lower case" },
    { 'X', "0123456789ABCDEF", "hexadecimal upper case" },
    { 'k', "!@#$%^&*()~`_-+={[}]|\\:;”’<,>.?/", "karnamian" },
};

/**
 * Count of kSpecials.
 */
#define kSpecialsCount  (sizeof(kSpecials) / sizeof(kSpecials[0]))

/**
 * Conversion from input (unsigned) character to index.
 */
int g_input_digit_value[0x100] = {0};

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
    int i = 0;
    fprintf(file,
            "\n"
            "USAGE\n"
            "    %s [options] input-digits output-digits [numbers...]\n"
            , g_program);
    fprintf(file,
            "\n"
            "DESCRIPTION\n"
            "    %s converts numbers of any radix (base) to any other radix.\n"
            "\n"
            "    'input-digits' is a list of digits used to represent the input numbers and\n"
            "    'output-digits' is a similar list for the output numbers. If extra\n"
            "    arguments are provided then they are converted from the input radix to the\n"
            "    output radix, otherwise the numbers to be converted are read from stdin.\n"
            "\n"
            "    If 'input-digits' contains both upper and lower case of a particular letter\n"
            "    then input parsing will be case-sensitive, otherwise it will be not be case-\n"
            "    sensitive.\n"
            "\n"
            "    The numbers to be converted will be delimited by any non-'input-digits'\n"
            "    characters.\n"
            "\n"
            "    Some standard radix representations have single-character indicators that\n"
            "    may be used for 'input-digits' or 'output-digits':\n"
            "\n"
            , g_program);
    for (i = 0; i < kSpecialsCount; ++i) {
        const SpecialDigits* sd = &kSpecials[i];
        fprintf(file, "        %c - %s (%s)\n", sd->short_cut, sd->description, sd->digits);
    }
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
 * Print an error message to stderr.
 */
void PrintError(const char* format, ...) {
    char text[0x0100] = "";
    va_list va;
    va_start(va, format);
    vsnprintf(text, sizeof(text), format, va);
    va_end(va);
    fprintf(stderr, "%s: %s\n", g_program, text);
}   /* PrintError() */

/* ------------------------------------------------------------------------- */
/**
 * Print a verbose message to stdout if g_verbose is set.
 */
void PrintVerbose(const char* format, ...) {
    if (g_verbose) {
        char text[0x0100] = "";
        va_list va;
        va_start(va, format);
        vsnprintf(text, sizeof(text), format, va);
        va_end(va);
        fprintf(stdout, "%s: %s\n", g_program, text);
    }
}   /* PrintVerbose() */

/* ------------------------------------------------------------------------- */
/**
 * Process the output number.
 */
void HandleOutputNumber(mpz_t number) {
    mpz_t n;
    mpz_init_set(n, number);
    str_t str = str_new();
    if (NULL == str) {
        PrintError("out of memory");
        return;
    }
    if (1 == g_output_radix) {  /* Unary. */
        while (mpz_cmp_ui(n, 0) > 0) {
            str_append_char(str, g_output_digits[0]);
            mpz_sub_ui(n, n, 1);
        }
    } else {
        mpz_t q;        /* quotient */
        mpz_init(q);
        while (mpz_cmp_ui(n, 0) > 0) {
            int value = mpz_tdiv_q_ui(q, n, g_output_radix);    /* Returns remainder. */
            str_append_char(str, g_output_digits[value]);
            mpz_swap(q, n);
        }
        mpz_clear(q);
        str_reverse(str);
    }
    mpz_clear(n);
    printf("%s\n", str_data(str));
    str_delete(str);
}   /* HandleOutputNumber() */

/* ------------------------------------------------------------------------- */
/**
 * Process the output number.
 */
void HandleInputString(const char* s) {
    int in_number = 0;
    mpz_t n;
    for (; *s; ++s) {
        int value = g_input_digit_value[((uint8_t) *s)];
        if (value < 0) {        /* If not a valid digit... */
            if (in_number) {    /* If this is the end of a number... */
                HandleOutputNumber(n);
                mpz_clear(n);
                in_number = 0;
            }
        } else {
            if (!in_number) {
                mpz_init(n);
                in_number = 1;
            }
            if (1 == g_input_radix) {   /* If unary. */
                mpz_add_ui(n, n, 1);
            } else {
                mpz_mul_ui(n, n, g_input_radix);
                mpz_add_ui(n, n, value);
            }
        }
    }

    if (in_number) {    /* If this is the end of a number... */
        HandleOutputNumber(n);
        mpz_clear(n);
    }
}   /* HandleInputString() */

/* ------------------------------------------------------------------------- */
/**
 * Handles input characters from a file.
 *
 * @param file - FILE* stream from which to read characters.
 */
void HandleInputFromFile(FILE* file) {
    /*
     * TODO(dr) Handle unary separately using fgetch().
     */
    char line[0x4000] = "";
    while (fgets(line, sizeof(line), file)) {
        HandleInputString(line);
    }
}   /* HandleInputFromFile() */

/* ------------------------------------------------------------------------- */
typedef enum InputOutputDigits_e {
    kInputDigits,
    kOutputDigits
} InputOutputDigits;

/* ------------------------------------------------------------------------- */
/**
 * Check the set of digits as a set suitable for using for radix conversion.
 */
void SetDigitsOrExit(InputOutputDigits which, const char* digits) {
    int i = 0;
    int j = 0;
    int radix = 0;
    assert(NULL != digits);
    radix = strlen(digits);
    if (1 == radix) {   /* Check for short-cut: */
        for (i = 0; i < kSpecialsCount; ++i) {
            const SpecialDigits* sd = &kSpecials[i];
            if (*digits == sd->short_cut) {
                digits = sd->digits;
                radix = strlen(digits);
                break;
            }
        }
        if (kSpecialsCount == i) {
            PrintUsageError("Unknown special digits short-cut '%c'.", *digits);
        }
    } else {    /* Verify validity of digit set. */
        for (i = 1; i < radix; ++i) {
            for (j = 0; j < i; ++j) {
                if (digits[i] == digits[j]) {
                    PrintUsageError("Digit '%c' is repeated in %s digit set.", digits[i],
                                    (kInputDigits == which) ? "input" : "output");
                } else if ((kInputDigits == which) && (tolower(digits[i]) == tolower(digits[i]))) {
                    g_case_sensitive = 1;       /* Only useful for input. */
                }
            }
        }
    }
    if (kInputDigits == which) {
        g_input_digits = digits;
        g_input_radix = radix;
        for (i= 0; i < 0x100; ++i) {
            g_input_digit_value[i] = -1;
        }
        for (i= 0; i < radix; ++i) {
            const uint8_t b = (uint8_t) digits[i];
            if (!g_case_sensitive && isalpha(b)) {
                g_input_digit_value[tolower(b)] = i;
                g_input_digit_value[toupper(b)] = i;
            } else {
                g_input_digit_value[b] = i;
            }
        }
    } else {
        g_output_digits = digits;
        g_output_radix = radix;
    }
}   /* SetDigitsOrExit() */

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
            PrintUsageError("invalid argument \"%s\" (unrecognized option and both digit sets have been seen)", arg);
        }
    }
    return rval;
}   /* ParseOptions() */

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
    int i = 0;
    g_program = NamePartOfPath(argv[0]);
    argc = ParseOptions(argc, argv);  /* Remove options; leave program name and arguments. */
    if (argc < 2) {
        Usage(stderr, 1);
    }
    SetDigitsOrExit(kInputDigits, argv[1]);
    if (argc >= 3) {
        SetDigitsOrExit(kOutputDigits, argv[2]);
        PrintVerbose("Set output digits to \"%s\".", g_output_digits);
    } else {    /* Default output is decimal: */
        SetDigitsOrExit(kOutputDigits, "0123456789");
        PrintVerbose("Set output digits to default \"%s\".", g_output_digits);
    }
    if (3 == argc) {
        PrintVerbose("Reading digits from stdin.");
        HandleInputFromFile(stdin);
    } else {
        for (i = 3; i < argc; ++i) {
            PrintVerbose("Reading digits from command line argument \"%s\".", argv[i]);
            HandleInputString(argv[i]);
        }
    }
    
    return 0;
}   /* main() */

