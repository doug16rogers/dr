#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "text_canvas.h"

/**
 * Name of this program. This will be replaced with argv[0].
 */
const char* g_program = "morse-chart";

/**
 * Whether or not to print a chart. If the chart is printed then no input or
 * command line options are converted.
 */
int g_print_chart = 0;

/**
 * Text to use to represent dit characters on output. Internally they are
 * coded with ".".
 */
#define kDefaultDit "."
const char* g_dit = kDefaultDit;

/**
 * Text to use to represent dah characters on output. Internally they are
 * coded with "-".
 */
#define kDefaultDah "-"
const char* g_dah = kDefaultDah;

/**
 * Text to use to represent inter-letter space on output. Internally they are
 * coded with " ".
 */
#define kDefaultSpace " "
const char* g_space = kDefaultSpace;

/**
 * Whether to restrict usage to just ITU-defined characters and terms.
 */
#define kDefaultItuOnly 0
int g_itu_only = kDefaultItuOnly;

/**
 * One option for dit is the middle dot charater U+00B7, '·'.
 */
const char* kMiddleDotUtf8 = "\xC2\xB7";

/**
 * Another option is to use the bullet operator U+2022, '•'.  
 */
const char* kBulletOperatorUtf8 = "\xE2\x80\xA2";

/**
 * Yet another option is to use the dot operator U+22C5, '⋅'.  
 */
const char* kDotOperatorUtf8 = "\xE2\x8B\x85";

/**
 * Among others we could try the Katakana middle dot U+30FB, '・'. But that's
 * a wide character.
 */
const char* kKatakanaMiddleDotUtf8 = "\xE3\x83\xBB";

/**
 * Information about a character or special Morse code.
 */
typedef struct morse_info_s {
    int is_itu;                 /**< Whether code is defined in ITU 1677. */
    const char* name;           /**< Character or special code. */
    uint8_t ascii;              /**< ASCII value, or -1 (0xFF) if none. */
    const char* morse;          /**< Morse Code representation of character using '.' and '-'. */
    const char* description;    /**< Description of code or NULL. */
} morse_info_t;

/**
 * The Morse code table as described by ITU 1677. The order of the codes is
 * mostly in agreement with the order provided in ITU-R M.1677 updated in
 * 2009. There are additional codes and the special codes (multi-character)
 * appear in a separate list.
 */
const morse_info_t kMorseCode[] = {
    /* 1.1.1 Letters */
    { 1, "A",  0x41, ".-",     NULL },
    { 1, "B",  0x42, "-...",   NULL },
    { 1, "C",  0x43, "-.-.",   NULL },
    { 1, "D",  0x44, "-..",    NULL },
    { 1, "E",  0x45, ".",      NULL },
//    { 1, "E'",   -1, "..-..",      "Accented E" },
    { 1, "F",  0x46, "..-.",   NULL },
    { 1, "G",  0x47, "--.",    NULL },
    { 1, "H",  0x48, "....",   NULL },
    { 1, "I",  0x49, "..",     NULL },
    { 1, "J",  0x4A, ".---",   NULL },
    { 1, "K",  0x4B, "-.-",    NULL },
    { 1, "L",  0x4C, ".-..",   NULL },
    { 1, "M",  0x4D, "--",     NULL },
    { 1, "N",  0x4E, "-.",     NULL },
    { 1, "O",  0x4F, "---",    NULL },
    { 1, "P",  0x50, ".--.",   NULL },
    { 1, "Q",  0x51, "--.-",   NULL },
    { 1, "R",  0x52, ".-.",    NULL },
    { 1, "S",  0x53, "...",    NULL },
    { 1, "T",  0x54, "-",      NULL },
    { 1, "U",  0x55, "..-",    NULL },
    { 1, "V",  0x56, "...-",   NULL },
    { 1, "W",  0x57, ".--",    NULL },
    { 1, "X",  0x58, "-..-",   NULL },
    { 1, "Y",  0x59, "-.--",   NULL },
    { 1, "Z",  0x5A, "--..",   NULL },

    /* 1.1.2 Figures */
    { 1, "0",  0x30, "-----",  NULL },
    { 1, "1",  0x31, ".----",  NULL },
    { 1, "2",  0x32, "..---",  NULL },
    { 1, "3",  0x33, "...--",  NULL },
    { 1, "4",  0x34, "....-",  NULL },
    { 1, "5",  0x35, ".....",  NULL },
    { 1, "6",  0x36, "-....",  NULL },
    { 1, "7",  0x37, "--...",  NULL },
    { 1, "8",  0x38, "---..",  NULL },
    { 1, "9",  0x39, "----.",  NULL },

    /* 1.1.3 Punctuation marks and miscellaneous signs */
    { 1, ".",  0x2E, ".-.-.-", "Full stop (period)" },
    { 1, ",",  0x2C, "--..--", "Comma" },
    { 1, ":",  0x3A, "---...", "Colon or division sign" },
    { 1, "?",  0x3F, "..--..", "Question mark or retransmission request" },
    { 1, "'",  0x27, ".----.", "Apostrophe" },
    { 1, "-",  0x2D, "-....-", "Hyphen or dash or subtraction sign" },
    { 1, "/",  0x2F, "-..-.",  "Fraction bar or division sign" },
    { 1, "(",  0x28, "-.--.",  "Left-hand bracket (parenthesis)" },
    { 1, ")",  0x29, "-.--.-", "Right-hand bracket (parenthesis)" },
    { 1, "\"", 0x22, ".-..-.", "Inverted commas (quotation marks)" },
    { 1, "=",  0x3D, "....-",  "Double hyphen" },
    { 1, "+",  0x2B, ".-.-.",  "Cross or addition sign" },
    { 1, "@",  0x40, ".--.-.", "Commercial at" },

    /* Non-ITU-1677 or other recognized signs: */
    { 0, "!",  0x21, "-.-.--", NULL },
    { 0, ";",  0x3B, "-.-.-.", NULL },
    { 0, "_",  0x5F, "..--.-", NULL },
//    { 0, "&",  0x26, ".-...",  NULL },
};   /* kMorseCode[] */

const morse_info_t kMorseSpecial[] = {
    /* ITU designated special codes */
    { 1, "<SN>", -1, "...-. ",     "Understood" },
    { 1, "<HH>", -1, "........",   "Error (eight dots)" },
    { 1, "<K>",  -1, "-.- ",       "Invitation to transmit" },
    { 1, "<AS>", -1, ".-...",      "Wait (&)" },
    { 1, "<SK>", -1, "...-.-",     "End of work" },
    { 1, "<CT>", -1, "-.-.-",      "Starting signal" /* " (to precede every transmission)" */ },

    /* Non-ITU special codes */
    { 0, "<AA>", -1, ".-.-",       "New line" },
    { 0, "<AR>", -1, ".-.-.",      "New page" },
    { 0, "<BT>", -1, "-...-",      "New paragraph" },
    { 0, "<KN>", -1, "-.--. ",     "Named station reply" },
    { 0, "<SOS>",-1, "...---...",  "Distress " },
    { 0, "<BK>", -1, "-... -.-",   "Break" },
    { 0, "<CL>", -1, "-.-. .-..",  "Closing" },
};   /* kMorseSpecial[] */

#define ARRAY_LEN(_a)  (sizeof(_a) / sizeof(_a[0]))

/* ------------------------------------------------------------------------- */
static void write_morse_chart(FILE* out) {
    const int kCanvasWidth = 89;
    const int kCanvasHeight = 13;
    const int kCodeColumnWidth = 12;
    text_canvas_t* tc = text_canvas_new(kCanvasWidth, kCanvasHeight);
    int i;
    int k = 0;
    int col = 0;
    text_canvas_move_to(tc, 0, TEXT_CANVAS_MAX);
    for (i = 0; i < ARRAY_LEN(kMorseCode); i++) {
        const morse_info_t* mi = &kMorseCode[i];
        if (!(g_itu_only && !mi->is_itu)) {
            /* @TODO: Use g_dit and g_dah. */
            tc_printf(tc, "%s %s\n", mi->name, mi->morse);
            k++;
            if ((k % kCanvasHeight) == 0) {
                col++;
                text_canvas_move_to(tc, kCodeColumnWidth * col, TEXT_CANVAS_MAX);
            }
        }
    }

    /*
     * Put special sequences on next column - if not already at start of next
     * column.
     */
    if ((k % kCanvasHeight) != 0) {
        k += kCanvasHeight - (k % kCanvasHeight);
        col++;
        text_canvas_move_to(tc, kCodeColumnWidth * col, TEXT_CANVAS_MAX);
    }

    for (i = 0; i < ARRAY_LEN(kMorseSpecial); i++) {
        const morse_info_t* mi = &kMorseSpecial[i];
        if (!(g_itu_only && !mi->is_itu)) {
            /* @TODO: Use g_dit and g_dah. */
            text_canvas_printf(tc, "%-5s %-12s %s\n", mi->name, mi->morse, mi->description);
            k++;
            if ((k % kCanvasHeight) == 0) {
                col += 2;
                text_canvas_move_to(tc, kCodeColumnWidth * col, TEXT_CANVAS_MAX);
            }
        }
    }
    text_canvas_fprint_canvas(tc, out);
    text_canvas_del(tc);
}   /* write_morse_chart() */

/* ------------------------------------------------------------------------- */
static const char* morse_for_char(int ch) {
    int i;
    ch = toupper(ch);
    for (i = 0; i < ARRAY_LEN(kMorseCode); ++i) {
        const morse_info_t* mi = &kMorseCode[i];
        if (g_itu_only && !mi->is_itu) {
            continue;
        }
        if (strlen(mi->name) != 1) {
            continue;
        }
        if (mi->name[0] == ch) {
            return mi->morse;
        }
    }
    return NULL;
}   /* morse_for_char() */

/* ------------------------------------------------------------------------- */
static void write_string_to_morse(const char* s, FILE* out) {
    int word_space_written = 0;
    for (; *s; ++s) {
        const char* m = morse_for_char(*s);
//        fprintf(out, "<'%c'='%s'>", *s, m ? m : "");
        if (isspace(*s) || (NULL == m)) {
            if (!word_space_written) {
                fprintf(out, "%s%s", g_space, g_space);   /* 1 already written by letter */
                word_space_written = 1;
            }
        } else {
            const char* dd = NULL;
            word_space_written = 0;
            for (dd = m; *dd != 0; dd++) {
                fprintf(out, "%s", ('.' == *dd) ? g_dit : g_dah);
            }
            fprintf(out, "%s", g_space);
        }
        if (*s == '\n') {
            fprintf(out, "\n");
        }
    }
    fflush(out);
}   /* write_string_to_morse() */

/* ------------------------------------------------------------------------- */
static void write_file_to_morse(FILE* in, FILE* out) {
    char line[0x100] = "";
    while (NULL != fgets(line, sizeof(line), in)) {
        /* @TODO: Handle special sequences. */
        /* @TODO: Handle special sequences split by fgets(). */
        write_string_to_morse(line, out);
    }
}   /* write_string_to_morse() */

/* ------------------------------------------------------------------------- */
static const char* prog_basename(const char* prog) {
    const char* rval = prog;
    for (; *prog; ++prog) {
        if ((*prog == '/') || (*prog == '\\')) {
            rval = prog + 1;
        }
    }
    return rval;
}   /* prog_basename() */

/* ------------------------------------------------------------------------- */
static void usage(FILE* out, int exit_code) __attribute__((noreturn));
static void usage(FILE* out, int exit_code) {
    fprintf(out,
            "\n"
            "NAME"
            "    %s\n"
            "\n"
            "SYNOPSIS\n"
            "    %s [OPTIONS] -c\n"
            "        Print a Morse Code chart.\n"
            "    %s [OPTIONS] <text>...\n"
            "        Convert <text>... to Morse Code.\n"
            "    %s [OPTIONS]\n"
            "        Convert text from stdin to Morse Code.\n"
            "\n"
            , g_program, g_program, g_program, g_program);
    fprintf(out,
            "DESCRIPTION\n"
            "    %s prints a Morse Code (ITU-R M.1677) chart or converts\n"
            "    text to Morse Code.\n"
            "\n"
            "    By default only single characters shown in the chart are converted. Use -e\n"
            "    to enable converting the special character sequences.\n"
            "\n"
            , g_program);
    fprintf(out,
            "OPTIONS\n"
            "    -h      Print this usage information.\n"
            "    -c      Print Morse Code chart.\n"
            "    -I      Use only items defined by ITU-R M.1677.\n"
            "    -d STR  Use STR as the dit (dot) character. [%s]\n"
            "    -D STR  Use STR as the dah (dash) character. [%s]\n"
            "    -u HEX  Use Unicode (UTF-8) code point HEX as the dit character.\n"
            "    -U HEX  Use Unicode (UTF-8) code point HEX as the dah character.\n"
            "    -s STR  Use STR as inter-letter space (tripled for inter-word). [%s]\n"
//            "    -e      Recognize special \"<XX>\" codes in input (see chart).\n"
            "\n"
//            , kDefaultItuOnly ? "yes" : "no"
            , kDefaultDit, kDefaultDah, kDefaultSpace
//            , kDefaultEscape ? "yes" : "no"
        );
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
/**
 * Allocates the output string on the heap.
 */
char* utf8(const char* hex){
    char* tgt = NULL;
    unsigned long unicode = 0;
    if (sscanf(hex, "%lx", &unicode) == 1) {
        tgt = malloc(8);
        if (NULL != tgt) {
            unsigned int n = unicode2utf8((unsigned char*) tgt, unicode);
            tgt[n] = 0;
        }
    }
    return tgt;
}   /* utf8() */

/* ------------------------------------------------------------------------- */
static void parse_args(int argc, char* argv[]) {
    int opt = 0;
    char* tmp = NULL;
    while ((opt = getopt(argc, argv, "hcId:D:u:U:s:e")) != -1) {
        switch (opt) {
        case 'h': usage(stdout, 0); break;
        case 'c': g_print_chart = 1; break;
        case 'I': g_itu_only = 1; break;
        case 'd': g_dit = optarg; break;
        case 'D': g_dah = optarg; break;
        case 'u': tmp = utf8(optarg); g_dit = (tmp != NULL) ? tmp : g_dit; break;
        case 'U': tmp = utf8(optarg); g_dah = (tmp != NULL) ? tmp : g_dah; break;
        case 's': g_space = optarg; break;
        case 'e':
            fprintf(stderr, "%s: '-e' not yet supported.\n", g_program);
            break;
        default:
            fprintf(stderr, "%s: Unknown option '-%c'; use '%s -h' for usage information.\n",
                    g_program, opt, g_program);
            exit(1);
        }
    }
}   /* parse_args() */

/* ------------------------------------------------------------------------- */
int main(int argc, char* argv[]) {
    g_program = prog_basename(argv[0]);
    parse_args(argc, argv);
    if (g_print_chart) {
        write_morse_chart(stdout);
    } else if (optind >= argc) {
        write_file_to_morse(stdin, stdout);
    } else {
        int i;
        for (i = optind; i < argc; i++) {
            if (i > optind) {
                write_string_to_morse(" ", stdout);
            }
            write_string_to_morse(argv[i], stdout);
        }
    }
    return 0;
}   /* main() */
