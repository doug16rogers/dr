/* Copyright (c) 2012-2019 Doug Rogers under the Zero Clause BSD License. */
/* You are free to do whatever you want with this software. See LICENSE.txt. */

/**
 * @file
 *
 * This program emits UTF-8 byte sequences for the Unicode values
 * specified in hexadecimal on the commandline.
 */
#include <stdio.h>
#include <strings.h>
#include <stdlib.h>
#include <unistd.h>

/**
 * This program's name.
 */
#define PROGRAM "echo-utf8"

/**
 * The maximum number of bytes in a single UTF8 character encoding.
 */
#define MAX_UTF8_ENCODED_LENGTH  6

/**
 * Whether to print verbose (debugging) messages.
 */
int g_verbose = 0;

/* ------------------------------------------------------------------------- */
/**
 * Prints usage information then exits with the given @a exit_code.
 *
 * @param exit_code - exit code for the program. 0 means no error.
 */
void usage(FILE* out, int exit_code) __attribute__((noreturn));
void usage(FILE* out, int exit_code) {
    fprintf(out, "\n");
    fprintf(out, "Usage: %s [options] <hex-unicode-value>...\n", PROGRAM);
    fprintf(out, "\n");
    fprintf(out, "    %s converts hexadecimal full Unicode code-points into UTF-8 and\n", PROGRAM);
    fprintf(out, "    prints them to standard output. If no arguments are given on the command\n");
    fprintf(out, "    line then the hexadecimal values are read from standard input.\n");
    fprintf(out, "\n");
    fprintf(out, "Options:\n");
    fprintf(out, "\n");
    fprintf(out, "    -h      Print this usage information\n");
    fprintf(out, "    -v      Print verbose messages\n");
    fprintf(out, "\n");
    exit(exit_code);
}   /* usage() */

/* ------------------------------------------------------------------------- */
/**
 * Converts a Unicode character value into a sequence of bytes conformant
 * with UTF-8. See http://www.cl.cam.ac.uk/~mgk25/unicode.html.
 *
 * @param target  - buffer to hold UTF-8 characters.
 * @param unicode - 31-bit Unicode value.
 *
 * @return the number of UTF-8 bytes placed in the buffer, or 0 on error.
 */
unsigned int unicode2utf8(unsigned char* target, unsigned long unicode) {
    if ((unicode & ~0x0000007Ful) == 0ul) {
        *target = (unsigned char) (unicode & 0x7Ful);
        return 1;
    }
    if ((unicode & ~0x0000007FFul) == 0ul) {
        *target++ = 0xC0 | ((unsigned char) (unicode >> 6) & 0x1F);
        *target   = 0x80 | ((unsigned char) unicode & 0x3F);
        return 2;
    }
    if ((unicode & ~0x00000FFFFul) == 0ul) {
        *target++ = 0xE0 | ((unsigned char) (unicode >> 12) & 0x0F);
        *target++ = 0x80 | ((unsigned char) (unicode >>  6) & 0x3F);
        *target   = 0x80 | ((unsigned char) unicode & 0x3F);
        return 3;
    }
    if ((unicode & ~0x0001FFFFFul) == 0ul) {
        *target++ = 0xF0 | ((unsigned char) (unicode >> 18) & 0x07);
        *target++ = 0x80 | ((unsigned char) (unicode >> 12) & 0x3F);
        *target++ = 0x80 | ((unsigned char) (unicode >>  6) & 0x3F);
        *target   = 0x80 | ((unsigned char) unicode & 0x3F);
        return 4;
    }
    if ((unicode & ~0x003FFFFFFul) == 0ul) {
        *target++ = 0xF8 | ((unsigned char) (unicode >> 24) & 0x03);
        *target++ = 0x80 | ((unsigned char) (unicode >> 18) & 0x3F);
        *target++ = 0x80 | ((unsigned char) (unicode >> 12) & 0x3F);
        *target++ = 0x80 | ((unsigned char) (unicode >>  6) & 0x3F);
        *target   = 0x80 | ((unsigned char) unicode & 0x3F);
        return 5;
    }
    if ((unicode & ~0x7FFFFFFFul) == 0ul) {
        *target++ = 0xFC | ((unsigned char) (unicode >> 30) & 0x01);
        *target++ = 0x80 | ((unsigned char) (unicode >> 24) & 0x3F);
        *target++ = 0x80 | ((unsigned char) (unicode >> 18) & 0x3F);
        *target++ = 0x80 | ((unsigned char) (unicode >> 12) & 0x3F);
        *target++ = 0x80 | ((unsigned char) (unicode >>  6) & 0x3F);
        *target   = 0x80 | ((unsigned char) unicode & 0x3F);
        return 6;
    }
    return 0;
}   /* unicode2utf8() */

/* ------------------------------------------------------------------------- */
int handle_unicode(unsigned char* buffer, size_t buffer_size, unsigned int* buffer_count, unsigned long unicode) {
    if (*buffer_count + MAX_UTF8_ENCODED_LENGTH >= buffer_size) {
        fprintf(stderr, "%s: buffer overflow\n", PROGRAM);
        return 0;
    }
    unsigned char* utf8 = &buffer[*buffer_count];
    unsigned long bytes = unicode2utf8(utf8, unicode);
    if (g_verbose) {
        switch (bytes) {
        case 0:
            fprintf(stderr, "%s: U+%08lX => %lu bytes (invalid)\n", PROGRAM, unicode, bytes);
            break;
        case 1:
            fprintf(stderr, "%s: U+%08lX => %lu byte (ASCII) => %02X\n", PROGRAM, unicode, bytes, utf8[0]);
            break;
        default:
            fprintf(stderr, "%s: U+%08lX => %lu bytes =>", PROGRAM, unicode, bytes);
            for (unsigned i = 0; i < bytes; ++i) {
                fprintf(stderr, " %02X", utf8[i]);
            }
            fprintf(stderr, "\n");
            break;
        }
    }
    if (bytes == 0) {
        return 0;
    } else {
        *buffer_count += bytes;
        return 1;
    }
}   /* handle_unicode() */

/* ------------------------------------------------------------------------- */
int parse_arguments(int argc, char* argv[]) {
    while (1) {
        int c = getopt(argc, argv, "hv");
        if (-1 == c) {
            break;
        }
        switch (c) {
        case 'h':
            usage(stdout, 0);
            break;
        case 'v':
            g_verbose = 1;
            break;
        case '?':
            fprintf(stderr, "%s: invalid option '%c'; use -h for usage information\n", PROGRAM, optopt);
            exit(1);
        }
    }
    for (int i = 0; i < (argc - optind); ++i) {
        argv[1+i] = argv[optind+i];
    }
    return 1 + argc - optind;
}   /* parse_arguments() */

/* ------------------------------------------------------------------------- */
/**
 * Main program. Goes through each argument attempting to read it as a
 * hexadecimal number, then prints the UTF-8 byte sequence corresponding to
 * that Unicode character.
 *
 * @param argc - number of arguments on commandline, including program name.
 * @param argv - list of pointers to the arguments.
 *
 * @return 0 on success, something else if an argument could not be read as
 * a hexadecimal value.
 */
int main(int argc, char* argv[]) {
    unsigned long        unicode = 0;
    int                  i = 0;
    int                  return_code = 0;
    static unsigned char utf8_buffer[0x2000] = {0};
    unsigned int         total_bytes = 0;

    argc = parse_arguments(argc, argv);
    if (1 == argc) {
        while (1 == fscanf(stdin, "%lx", &unicode)) {
            if (!handle_unicode(utf8_buffer, sizeof(utf8_buffer), &total_bytes, unicode)) {
                return_code = 1;
                break;
            }
        }
    } else {
        for (i = 1; i < argc; i++) {
            if (sscanf(argv[i], "%lx", &unicode) != 1) {
                fprintf(stderr, "%s: invalid hexadecimal unicode value '%s' (arg %u)\n", PROGRAM, argv[i], i);
                usage(stderr, 2);
            } else if (!handle_unicode(utf8_buffer, sizeof(utf8_buffer), &total_bytes, unicode)) {
                return_code = 1;
                break;
            }
        }   /* Else not reading from stdin. */
    }    /* For each argument. */
    fwrite(utf8_buffer, 1, total_bytes, stdout);
    fwrite("\n", 1, 1, stdout);
    fflush(stdout);
    return return_code;
}   /* main() */
