/* Copyright (c) 2018 Doug Rogers under the terms of the MIT License. */
/* See http://www.opensource.org/licenses/mit-license.html.. */

#include <ctype.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

uint64_t g_count[0x100] = {0};
char g_include_char[0x100] = {0};
char* g_output[0x100] = {0};

typedef enum {
    FORMAT_DECIMAL,
    FORMAT_OCTAL,
    FORMAT_HEX_UPPER,
    FORMAT_HEX_LOWER,
} format_t;

typedef enum {
    CASE_CHANGE_NONE,
    CASE_CHANGE_TO_LOWER,
    CASE_CHANGE_TO_UPPER,
} case_change_t;

const char* g_program = "ccount";
const char* g_oct_prefix = "0";
const char* g_hex_prefix = "0x";
const char* g_zero_replacement = " ";
format_t g_format = FORMAT_DECIMAL;
case_change_t g_case_change = CASE_CHANGE_NONE;
char g_print_header = 0;
char g_print_as_list = 0;
char g_fancy_list = 0;
char g_print_total = 0;
char g_print_total_only = 0;
char g_print_zero_counts = 1;
char g_patterns = 0;
int g_min_included_char = 0;
int g_max_included_char = 0xFF;

/* ------------------------------------------------------------------------- */
void include_range(int start, int end, int include) {
    for (int i = start; i <= end; i++) {
        g_include_char[i] = include;
    }
}   /* include_range() */

/* ------------------------------------------------------------------------- */
static inline uint8_t hexval(char c) {
    if (('0' <= c) && (c <= '9')) return c - '0';
    if (('A' <= c) && (c <= 'F')) return c - 'A' + 10;
    if (('a' <= c) && (c <= 'f')) return c - 'a' + 10;
    return 0;
}   /* hexval() */

/* ------------------------------------------------------------------------- */
int unescape_char(const char** s_ptr, uint8_t* ch_ptr) {
    const char* s = *s_ptr;
    uint8_t ch;
    if (0 == *s)
        return 0;
    if (*s != '\\') {
        ch = *s++;
    } else {
        s++;
        ch = *s++;
        switch (ch) {
        case '\\': break;
        case '^': ch = '^'; break;
        case 't': ch = '\t'; break;
        case 'n': ch = '\n'; break;
        case 'r': ch = '\r'; break;
        case 'x':
            if (!isxdigit(*s)) {
                break;
            } else {
                ch = hexval(*s++);
                if (isxdigit(*s)) {
                    ch = 16 * ch + hexval(*s++);
                }
            }
            break;
        default:
            if (('0' <= ch) && (ch <= '7')) {
                ch -= '0';
                if (('0' <= *s) && (*s <= '7')) {
                    ch = ch * 8 + *s++ - '0';
                    if (('0' <= *s) && (*s <= '7')) {
                        ch = ch * 8 + *s++ - '0';
                    }
                }
            } else {
                ch = '\\';
                s--;
            }
        }
    }
    *s_ptr = s;
    *ch_ptr = ch;
    return 1;
}   /* unescape_char() */

/* ------------------------------------------------------------------------- */
void include_chars(const char* pattern, int include) {
    uint8_t* ch = calloc(1, strlen(pattern)+1);
    if (NULL == ch) {
        abort();
    }
    unsigned count = 0;
    if (0 == g_patterns) {
        memset(g_include_char, !include, sizeof(g_include_char));
        g_patterns++;
    }
    while (unescape_char(&pattern, &ch[count])) {
        count++;
    }
    for (unsigned i = 0; i < count; ++i) {
        if (((i+2) < count) && (ch[i+1] == '-')) {
            include_range(ch[i+0], ch[i+2], include);
            i += 2;
        } else if (((i+1) < count) && (ch[i] == '%')) {
            /* Handle character classes. */
            i++;
            switch (ch[i]) {
            case '%':
                g_include_char['%'] = include;
                break;
            case '.':
                include_range(0x00, 0x7F, include);
                break;
            case 'a':
                include_range('A', 'Z', include);
                include_range('a', 'z', include);
                break;
            case 'l':
                include_range('a', 'z', include);
                break;
            case 'u':
                include_range('A', 'Z', include);
                break;
            case 'w':
                include_range('0', '9', include);
                include_range('A', 'Z', include);
                include_range('a', 'z', include);
                break;
            case 'd':
                include_range('0', '9', include);
                break;
            case 'o':
                include_range('0', '9', include);
                break;
            case 'x':
                include_range('0', '9', include);
                include_range('A', 'F', include);
                include_range('a', 'f', include);
                break;
            case 's':
                g_include_char[' '] = include;
                g_include_char['\t'] = include;
                g_include_char['\n'] = include;
                g_include_char['\r'] = include;
                break;
            case 'p':
                include_range('!', '/', include);
                include_range(':', '@', include);
                include_range('[', '`', include);
                include_range('{', '~', include);
                break;
            case 'c':
                include_range(0x00, 0x1F, include);
                g_include_char[0x7F] = include;
                break;
            case 'z':
                g_include_char[0x00] = include;
                break;
            default:
                g_include_char['%'] = include;
                i--;
            }
        } else {
            g_include_char[ch[i]] = include;
        }
    }
    free(ch);
}   /* include_chars() */

/* ------------------------------------------------------------------------- */
uint64_t character_count(FILE* f) {
    uint64_t bytes = 0;
    int ch;
    while ((ch = fgetc(f)) >= 0) {
        if ((CASE_CHANGE_TO_UPPER == g_case_change) && ('a' <= ch) && (ch <= 'z'))
            ch -= ('a' - 'A');
        else if ((CASE_CHANGE_TO_LOWER == g_case_change) && ('A' <= ch) && (ch <= 'Z'))
            ch += ('a' - 'A');
        if (!g_include_char[ch]) {
            continue;
        }
        g_count[ch]++;
        bytes++;
    }
    return bytes;
}   /* character_count() */

/* ------------------------------------------------------------------------- */
char* print_count(uint64_t count) {
    char text[0x100] = "";
    if ((0ull == count) && !g_print_zero_counts) {
        snprintf(text, sizeof(text), "%s", g_zero_replacement);
    } else {
        switch (g_format) {
        case FORMAT_OCTAL: {
            const char* prefix = (count <= 7ull) ? "" : g_oct_prefix;
            snprintf(text, sizeof(text), "%s%llo", prefix, (long long) count);
            break;
        }
        case FORMAT_HEX_LOWER:
            snprintf(text, sizeof(text), "%s%llx", g_hex_prefix, (long long) count);
            break;
        case FORMAT_HEX_UPPER:
            snprintf(text, sizeof(text), "%s%llX", g_hex_prefix, (long long) count);
            break;
        case FORMAT_DECIMAL:
        default:
            snprintf(text, sizeof(text), "%llu", (long long) count);
            break;
        }
    }
    return strdup(text);
}   /* print_count() */

/* ------------------------------------------------------------------------- */
void usage(FILE* f, int exit_code) {
    fprintf(f, "USAGE\n");
    fprintf(f, "    %s [options] [filenames...]\n", g_program);
    fprintf(f, "\n");
    fprintf(f, "    Count and display the frequency of characters in a set of files.\n");
    fprintf(f, "    If no filenames are given, use stdin.\n");
    fprintf(f, "\n");
    fprintf(f, "OPTIONS\n");
    fprintf(f, "    -h         Print this help.\n");
    fprintf(f, "    -o         Print counts in octal.\n");
    fprintf(f, "    -x, -X     Print counts in hexadecimal.\n");
    fprintf(f, "    -N         Do not print '0' prefix for octal or '0x' for hexadecimal.\n");
    fprintf(f, "    -H         Include heading line and heading intro for each line.\n");
    fprintf(f, "    -i         Convert upper to lower case before counting.\n");
    fprintf(f, "    -I         Convert lower to upper case before counting.\n");
    fprintf(f, "    -a         ASCII; same as '-c %%.'.\n");
    fprintf(f, "    -b         Print blanks for zeroes; same as \"-Z ' '\"\n");
    fprintf(f, "    -c [^]STR  Include (exclude with '^') characters in STR. See below.\n");
    fprintf(f, "    -l         Print counts one-per-line and include zero counts.\n");
    fprintf(f, "    -L         Print one-per-line with format \"['<char>']=<count>\".\n");
    fprintf(f, "    -t         Print total character count as last output line.\n");
    fprintf(f, "    -T         Print only the total character count.\n");
    fprintf(f, "    -W NUM     Width of each count. [4]\n");
    fprintf(f, "    -z         Suppress printing of zero counts.\n");
    fprintf(f, "    -Z STR     Print STR for zero counts.\n");
    fprintf(f, "\n");
    fprintf(f, "INCLUDING AND EXCLUDING CHARACTERS\n");
    fprintf(f, "    Characters to be included (see '-c' above) or excluded (if STR is prefixed\n");
    fprintf(f, "    with '^') may be specified in three ways: (1) as a character 'x' itself,\n");
    fprintf(f, "    possibly escaped with '\\', (2) as a range of characters 'x-y', or (3) as\n");
    fprintf(f, "    a character class introduced with a percent, like '%%x'. See below.\n");
    fprintf(f, "\n");
    fprintf(f, "    To include/exclude '-', list it first in STR.\n");
    fprintf(f, "\n");
    fprintf(f, "    The following are recognized as escape sequences:\n");
    fprintf(f, "        \\\\          Backslash '\\'.\n");
    fprintf(f, "        \\^          Literal caret (not exclusion indicator), '^'.\n");
    fprintf(f, "        \\t          Tab, '\\x09'.\n");
    fprintf(f, "        \\n          Newline, '\\x0A'.\n");
    fprintf(f, "        \\r          Carriage return, '\\x0D'.\n");
    fprintf(f, "        \\#[#[#]]    Up to three octal digits.\n");
    fprintf(f, "        \\x#[#]      Up to two hexadecimal digits.\n");
    fprintf(f, "    Unrecognized escape sequences will not be treated as escape sequences; for\n");
    fprintf(f, "    example, '\\k' will be treated the same as '\\\\k'.\n");
    fprintf(f, "\n");
    fprintf(f, "    The following character classes are recognized:\n");
    fprintf(f, "        %%%%  The percent character.\n");
    fprintf(f, "        %%.  All ASCII characters; '\\x00-\\x7F'.\n");
    fprintf(f, "        %%a  All letters; 'A-Za-z'.\n");
    fprintf(f, "        %%l  Lower case letters; 'a-z'.\n");
    fprintf(f, "        %%u  Upper case letters; 'A-Z'.\n");
    fprintf(f, "        %%w  Alphanumeric characters; '0-9A-Za-z'.\n");
    fprintf(f, "        %%d  Decimal digits; '0-9'.\n");
    fprintf(f, "        %%o  Octal digits; '0-7'.\n");
    fprintf(f, "        %%x  Hexadecimal digits; '0-9A-Fa-f'.\n");
    fprintf(f, "        %%s  Space characters; ' \\t\\n\\r'.\n");
    fprintf(f, "        %%p  Punctuation characters; '!-/:-@[-`{-~'.\n");
    fprintf(f, "        %%c  Control characters; '\\x00-\\x1F\\0x7F'.\n");
    fprintf(f, "        %%z  NUL character, '\\0'.\n");
    exit(exit_code);
}   /* usage() */

/* ------------------------------------------------------------------------- */
void parse_args(int argc, char* argv[]) {
    int ch;
    while ((ch = getopt(argc, argv, "hdoxXiIHc:abNtTlLzZ:")) != -1) {
        switch (ch) {
        case 'h': usage(stdout, 0); break;
        case 'd': g_format = FORMAT_DECIMAL; break;
        case 'o': g_format = FORMAT_OCTAL; break;
        case 'x': g_format = FORMAT_HEX_LOWER; break;
        case 'X': g_format = FORMAT_HEX_UPPER; break;
        case 'i': g_case_change = CASE_CHANGE_TO_LOWER; break;
        case 'I': g_case_change = CASE_CHANGE_TO_UPPER; break;
        case 'H': g_print_header = 1; break;
        case 'c':
            if (*optarg == '^')
                include_chars(optarg + 1, 0);
            else
                include_chars(optarg, 1);
            break;
        case 'a': include_chars("%.", 1); break;
        case 'b': g_zero_replacement = " "; break;
        case 'N':
            g_oct_prefix = "";
            g_hex_prefix = "";
            break;
        case 't': g_print_total = 1; break;
        case 'T': g_print_total = 1; g_print_total_only = 1; break;
        case 'l': g_print_as_list = 1; g_fancy_list = 0; g_print_zero_counts = 1; break;
        case 'L': g_print_as_list = 1; g_fancy_list = 1; break;
        case 'z': g_print_zero_counts = 0; break;
        case 'Z': g_print_zero_counts = 0; g_zero_replacement = optarg; break;
        case '?':
        default:
            fprintf(stderr, "%s: use '-h' for help\n", g_program);
            exit(1);
        }
    }
}   /* parse_args() */

/* ------------------------------------------------------------------------- */
int main(int argc, char* argv[]) {
    uint64_t total_bytes = 0;
    g_program = argv[0];
    include_range(0, 0xFF, 1);
    parse_args(argc, argv);
    argc -= optind;
    argv += optind;
    if (argc == 0) {
        total_bytes += character_count(stdin);
    } else {
        int i = 0;
        for (i = 0; i < argc; i++) {
            FILE* f = fopen(argv[i], "rb");
            if (NULL == f) {
                fprintf(stderr, "%s: could not open \"%s\": %s\n", g_program, argv[i], strerror(errno));
                exit(1);
            } else {
                /* fprintf(stderr, "%s: reading \"%s\"...\n", PROGRAM, argv[i]); */
                total_bytes += character_count(f);
                fclose(f);
            }
        }
    }

    if (g_print_total_only)
        goto PrintTotal;

    int min_included = -1;
    int max_included = -1;
    int max_width = -1;
    for (int i = 0; i <= 0xFF; i++) {
        if (g_include_char[i]) {
            max_included = i;
            min_included = (-1 == min_included) ? i : min_included;
            g_output[i] = print_count(g_count[i]);
            int len = strlen(g_output[i]);
            max_width = (len > max_width) ? len : max_width;
        }
    }
    if (g_print_as_list) {
        for (int i = min_included; i <= max_included; ++i) {
            if (!g_include_char[i])
                continue;
            if ((0 == g_count[i]) && !g_print_zero_counts)
                continue;
            if (g_fancy_list) {
                printf("['");
                switch (i) {
                case '\n': printf("\\n"); break;
                case '\r': printf("\\r"); break;
                case '\t': printf("\\t"); break;
                default:
                    if (i < 8)
                        printf("\\%u", i);
                    else if ((' ' <= i) && (i <= '~'))
                        printf("%c", i);
                    else
                        printf("\\x%02x", i);
                }
                printf("']=");
            }
            printf("%s\n", g_output[i]);
        }
    } else {
        const int MIN_WIDTH = 1;
        max_width = (max_width < MIN_WIDTH) ? MIN_WIDTH : max_width;
        /* @todo: This won't work well if min/max are not 0/15 % 16. */
        if (g_print_header) {
            printf("   ");
            if (1 == max_width) {
                for (int i = 0; i < 0x10; ++i) printf(" %X", i);
            } else {
                for (int i = 0; i < 0x10; ++i) printf(" %*s_%X", max_width - 2, "", i);
            }
            printf("\n");
        }
        for (int i = (min_included & 0xF0); i <= (max_included | 0x0F); ++i) {
            if (((i % 16) == 0) && g_print_header)
                printf("%X_:", i / 16);
            if (g_include_char[i]) {
                printf(" %*s", max_width, g_output[i]);
            } else {
                printf(" %*s", max_width, "");
            }
            if ((i % 16) == 15)
                printf("\n");
        }
    }

PrintTotal:
    if (g_print_total) {
        char* total = print_count(total_bytes);
        printf("%s\n", total);
        free(total);
    }
    for (int i = 0; i <= 0xFF; ++i) {
        if (NULL != g_output[i]) {
            free(g_output[i]);
            g_output[i] = NULL;
        }
    }
    return 0;
}   /* main() */
