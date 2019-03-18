/* Copyright (c) 2012-2019 Doug Rogers under the Zero Clause BSD License. */
/* You are free to do whatever you want with this software. See LICENSE.txt. */

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
 * The number of binary digits to read at a time. If 0 then read as text
 * using fgets().
 */
#define INPUT_CHUNK_SIZE_DEFAULT 0
#define INPUT_CHUNK_SIZE_MAX 0x4000
size_t g_input_chunk_size = INPUT_CHUNK_SIZE_DEFAULT;

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
    char short_cut;             /**< Single-character short-cut for digit set. */
    int radix;                  /**< 0 -> Use strlen(). */
    const char* digits;         /**< List of characters to use as digits. */
    const char* description;    /**< Description of this radix. */
} SpecialDigits;

/**
 * ASCII 8-bit (just raw bytes).
 */
const char kAscii8[0x100] = {
    0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,0x08,0x09,0x0A,0x0B,0x0C,0x0D,0x0E,0x0F,
    0x10,0x11,0x12,0x13,0x14,0x15,0x16,0x17,0x18,0x19,0x1A,0x1B,0x1C,0x1D,0x1E,0x1F,
    0x20,0x21,0x22,0x23,0x24,0x25,0x26,0x27,0x28,0x29,0x2A,0x2B,0x2C,0x2D,0x2E,0x2F,
    0x30,0x31,0x32,0x33,0x34,0x35,0x36,0x37,0x38,0x39,0x3A,0x3B,0x3C,0x3D,0x3E,0x3F,
    0x40,0x41,0x42,0x43,0x44,0x45,0x46,0x47,0x48,0x49,0x4A,0x4B,0x4C,0x4D,0x4E,0x4F,
    0x50,0x51,0x52,0x53,0x54,0x55,0x56,0x57,0x58,0x59,0x5A,0x5B,0x5C,0x5D,0x5E,0x5F,
    0x60,0x61,0x62,0x63,0x64,0x65,0x66,0x67,0x68,0x69,0x6A,0x6B,0x6C,0x6D,0x6E,0x6F,
    0x70,0x71,0x72,0x73,0x74,0x75,0x76,0x77,0x78,0x79,0x7A,0x7B,0x7C,0x7D,0x7E,0x7F,
    0x80,0x81,0x82,0x83,0x84,0x85,0x86,0x87,0x88,0x89,0x8A,0x8B,0x8C,0x8D,0x8E,0x8F,
    0x90,0x91,0x92,0x93,0x94,0x95,0x96,0x97,0x98,0x99,0x9A,0x9B,0x9C,0x9D,0x9E,0x9F,
    0xA0,0xA1,0xA2,0xA3,0xA4,0xA5,0xA6,0xA7,0xA8,0xA9,0xAA,0xAB,0xAC,0xAD,0xAE,0xAF,
    0xB0,0xB1,0xB2,0xB3,0xB4,0xB5,0xB6,0xB7,0xB8,0xB9,0xBA,0xBB,0xBC,0xBD,0xBE,0xBF,
    0xC0,0xC1,0xC2,0xC3,0xC4,0xC5,0xC6,0xC7,0xC8,0xC9,0xCA,0xCB,0xCC,0xCD,0xCE,0xCF,
    0xD0,0xD1,0xD2,0xD3,0xD4,0xD5,0xD6,0xD7,0xD8,0xD9,0xDA,0xDB,0xDC,0xDD,0xDE,0xDF,
    0xE0,0xE1,0xE2,0xE3,0xE4,0xE5,0xE6,0xE7,0xE8,0xE9,0xEA,0xEB,0xEC,0xED,0xEE,0xEF,
    0xF0,0xF1,0xF2,0xF3,0xF4,0xF5,0xF6,0xF7,0xF8,0xF9,0xFA,0xFB,0xFC,0xFD,0xFE,0xFF,
};

/**
 * List of supported special digits lists.
 */
const SpecialDigits kSpecials[] = {
    { 'u', 0, "1",                "Unary sequence of 1s" },
    { 'b', 0, "01",               "Binary" },
    { 'o', 0, "01234567",         "Octal" },
    { 'd', 0, "0123456789",       "Decimal" },
    { 'x', 0, "0123456789abcdef", "Hexadecimal (lower)" },
    { 'X', 0, "0123456789ABCDEF", "Hexadecimal (upper)" },
    { 'k', 0, "!@#$%^&*()~`_-+={[}]|\\:;”’<,>.?/", "karnamian" },
    /* @todo(dr) Provide Unicode support. */
    { 'F', 0, "/E@A:SIU+DRJNFCKTZLWHYPQOBG\"MXV#", "Ferranti Mark 1 [MSB first, +=\xC2\xBD/#=\xC2\xA3]" },
    { '6', 64, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/",
      "Base64 plain (AB...Zab..z01..9+/)" },
    { 'U', 64, "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_",
      "Base64 URL (AB...Zab..z01..9-_)" },
    { '7', 128, kAscii8, "ASCII 7-bit (raw bytes)" },
    { '8', 256, kAscii8, "ASCII 8-bit (raw bytes)" },
    { 'g', 0, "TCAG", "Genetic DNA codons" },
    { 'G', 0, "UCAG", "Genetic RNA codons" },
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
        if (sd->radix > 0) {
            fprintf(file, "        %c - %s\n", sd->short_cut, sd->description);
        } else {
            fprintf(file, "        %c - %s (%s)\n", sd->short_cut, sd->description, sd->digits);
        }
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
            "    -b:nary:-size=<num>         Number of bytes to read at a time (0=text) [%d].\n",
            INPUT_CHUNK_SIZE_DEFAULT);
    fprintf(file,
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
        while ((mpz_cmp_ui(n, 0) > 0) && !str_error(str)) {
            str_append_char(str, g_output_digits[0]);
            mpz_sub_ui(n, n, 1);
        }
    } else {
        mpz_t q;        /* quotient */
        mpz_init(q);
        while ((mpz_cmp_ui(n, 0) > 0) && !str_error(str)) {
            int value = mpz_tdiv_q_ui(q, n, g_output_radix);    /* Returns remainder. */
            str_append_char(str, g_output_digits[value]);
            mpz_swap(q, n);
        }
        mpz_clear(q);
        str_reverse(str);
    }
    mpz_clear(n);
    fwrite(str_data(str), 1, str_len(str), stdout);
    fputc('\n', stdout);
    str_delete(str);
}   /* HandleOutputNumber() */

/* ------------------------------------------------------------------------- */
/**
 * Process the output number.
 */
void HandleInputString(const char* s, size_t len) {
    int in_number = 0;
    size_t i;
    mpz_t n;
    if (len == 0) {
        len = strlen(s);
    }
    for (i = 0; i < len; ++s, ++i) {
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
    char line[INPUT_CHUNK_SIZE_MAX + 1] = "";
    size_t nelem = 0;
    while (!feof(file)) {
        line[0] = 0;
        if (g_input_chunk_size > 0) {
            nelem = fread(line, 1, g_input_chunk_size, file);
            if (0 == nelem) {
                break;
            }
        } else {
            if (NULL == fgets(line, sizeof(line), file)) {
                break;
            }
            nelem = strlen(line);
        }
        PrintVerbose("Read %d bytes from stdin.", (int) nelem);
        HandleInputString(line, nelem);
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
                radix = (sd->radix == 0) ? strlen(digits) : sd->radix;
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
                }
            }
        }
    }
    if (kInputDigits == which) {
        for (i = 1; i < radix; ++i) {
            for (j = 0; j < i; ++j) {
                if ((kInputDigits == which) &&
                    !g_case_sensitive &&
                    (tolower(digits[j]) == tolower(digits[i]))) {
                    PrintVerbose("Setting case sensitivity because %s digit [%d]='%c' and [%d]='%c'.",
                                 (kInputDigits == which) ? "input" : "output",
                                 j, digits[j], i, digits[i]);
                    g_case_sensitive = 1;       /* Only useful for input. */
                }
            }
        }
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
        const char* val = NULL;
        if (end_of_options || ('-' != *arg)) {
            non_option_argument_list[rval++] = arg;
        } else if (('-' == arg[1]) && (0 == arg[2])) {
            end_of_options = 1;
        } else if (IsOption(arg, &val, "b:inary:-size")) {
            int size = 0;
            if (NULL == val) {
                PrintUsageError("no binary chunk size provided in \"%s\"", arg);
            } else if ((sscanf(val, "%d", &size) != 1) || (size < 0) || (size > INPUT_CHUNK_SIZE_MAX)) {
                PrintUsageError("invalid binary chunk size provided in \"%s\"", arg);
            }
            g_input_chunk_size = size;
            PrintVerbose("Set input chunk size to %d.", (int) g_input_chunk_size);
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
            HandleInputString(argv[i], 0);
        }
    }
    
    return 0;
}   /* main() */

