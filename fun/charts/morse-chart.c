#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/**
 * For dit I chose the middle dot charater U+00B7, '·'.
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
 * The Morse code table - ITU 1677.
 */
char kMorseChart[] =
    "  A · =       N = ·       1 · = = = =   . · = · = · =    <AA> · = · =           New line            \n"
    "  B = · · ·   O = = =     2 · · = = =   ? · · = = · ·    <AR> · = · = ·         New page            \n"
    "  C = · = ·   P · = = ·   3 · · · = =   ; = · = · = ·    <AS> · = · · ·         Wait (&)            \n"
    "  D = · ·     Q = = · =   4 · · · · =   ! = · = · = =    <BT> = · · · =         New paragraph       \n"
    "  E ·         R · = ·     5 · · · · ·   , = = · · = =    <CT> = · = · =         Attention           \n"
    "  F · · = ·   S · · ·     6 = · · · ·   : = = = · · ·    <HH> · · · · · · · ·   Error/????          \n"
    "  G = = ·     T =         7 = = · · ·   \" · = · · = ·     <K> = · =             Any station reply   \n"
    "  H · · · ·   U · · =     8 = = = · ·   ' · = = = = ·    <KN> = · = = ·         Named station reply \n"
    "  I · ·       V · · · =   9 = = = = ·   @ · = = · = ·    <SK> · · · = · =       End contact         \n"
    "  J · = = =   W · = =     0 = = = = =   _ · · = = · =    <SN> · · · = ·         Understood          \n"
    "  K = · =     X = · · =   / = · · = ·   + · = · = ·     <SOS> · · · = = = · · · Distress            \n"
    "  L · = · ·   Y = · = =   = = · · · =   - = · · · · =    <BK> = · · ·   = · =   Break               \n"
    "  M = =       Z = = · ·   ( = . = = .   ) = · = = · =    <CL> = · = ·   · = · · Closing             \n";

/* ------------------------------------------------------------------------- */
const char* morse_for_char(int ch) {
    switch (ch) {
    case '/': return "= · · = ·";
    case '=': return "· · · · =";
    case '(': return "= · = = ·";
    case '.': return "· = · = · =";
    case '?': return "· · = = · ·";
    case ';': return "= · = · = ·";
    case '!': return "= · = · = =";
    case ',': return "= = · · = =";
    case ':': return "= = = · · ·";
    case '"': return "· = · · = ·";
    case '\'': return "· = = = = ·";
    case '@': return "· = = · = ·";
    case '+': return "· = · = ·";
    case '-': return "= · · · · =";
    case ')': return "= · = = · =";

    case '1': return "· = = = =";
    case '2': return "· · = = =";
    case '3': return "· · · = =";
    case '4': return "· · · · =";
    case '5': return "· · · · ·";
    case '6': return "= · · · ·";
    case '7': return "= = · · ·";
    case '8': return "= = = · ·";
    case '9': return "= = = = ·";
    case '0': return "= = = = =";

    case 'a': case 'A': return "· =";
    case 'b': case 'B': return "= · · ·";
    case 'c': case 'C': return "= · = ·";
    case 'd': case 'D': return "= · ·";
    case 'e': case 'E': return "·";
    case 'f': case 'F': return "· · = ·";
    case 'g': case 'G': return "= = ·";
    case 'h': case 'H': return "· · · ·";
    case 'i': case 'I': return "· ·";
    case 'j': case 'J': return "· = = =";
    case 'k': case 'K': return "= · =";
    case 'l': case 'L': return "· = · ·";
    case 'm': case 'M': return "= =";
    case 'n': case 'N': return "= ·";
    case 'o': case 'O': return "= = =";
    case 'p': case 'P': return "· = = ·";
    case 'q': case 'Q': return "= = · =";
    case 'r': case 'R': return "· = ·";
    case 's': case 'S': return "· · ·";
    case 't': case 'T': return "=";
    case 'u': case 'U': return "· · =";
    case 'v': case 'V': return "· · · =";
    case 'w': case 'W': return "= · = =";
    case 'x': case 'X': return "= · · =";
    case 'y': case 'Y': return "= · = =";
    case 'z': case 'Z': return "= = · ·";
    default:
        return NULL;
    }
}   /* morse_for_char() */

/* ------------------------------------------------------------------------- */
void morse_print(FILE* out, const char* s) {
    for (; *s; ++s) {
        const char* m = morse_for_char(*s);
        if (NULL != m) {
            fprintf(out, " %s  ", m);
        } else {
            fputc(*s, out);
        }
    }
}   /* morse_print() */

/* ------------------------------------------------------------------------- */
int main(int argc, char* argv[]) {
    int ch = 0;
    int result = 1;     /* Kluge to make it work with write() return. */
    if (1 == argc) {
        result = write(1, kMorseChart, sizeof(kMorseChart) - 1);
    } else if ((2 == argc) && ((0 == strcmp("-h", argv[1])) || (0 == strcmp("--help", argv[1])))) {
        printf("\n"
               "    Usage: morse-chart [- | -h | --help | <string-to-convert>... ]\n"
               "\n"
               "    With no arguments, print a Morse chart.\n"
               "    With '-h' or '--help', print this usage information.\n"
               "    With a single '-' argument, read characters to convert from stdin.\n"
               "    Otherwise, print the Morse code for each argument on its own line.\n"
               "\n");
    } else if ((2 == argc) && (0 == strcmp("-", argv[1]))) {
        while ((ch = getchar()) != EOF) {
            const char* m = morse_for_char(ch);
            if (NULL != m) {
                printf(" %s  ", m);
            } else {
                putchar(ch);
            }
        }
        printf("\n");
    } else {
        int i =0;
        for (i = 1; i < argc; ++i) {
            morse_print(stdout, argv[i]);
            printf("\n");
        }
    }
    return (result > 0) ? 0 : 1;
}   /* main() */
