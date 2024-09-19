#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define streq(_ITEM_TO_COMPARE,_LITERAL) \
    (0 == strcmp((_ITEM_TO_COMPARE), _LITERAL))
//  (0 == strncmp((_ITEM_TO_COMPARE), _LITERAL, (sizeof(_LITERAL) - 1)))

#define DEFAULT_COLUMNS 32

int main(int argc, char *argv[]) {
    const char hex[] = "0123456789abcdef";
    int ch = 0;
    int col = 0;
    int columns = DEFAULT_COLUMNS;

    if (argc > 1) {
        if (streq(argv[1], "-h") || streq(argv[1], "help") || streq(argv[1], "--help")) {
            printf("USAGE\n");
            printf("\n");
            printf("     b2x <option>\n");
            printf("\n");
            printf("OPTIONS\n");
            printf("\n");
            printf("     help, -h, --help   Show this usage information\n");
            printf("     <number>           Number of bytes per line (0=no newlines) [%u]\n", DEFAULT_COLUMNS);
            printf("\n");
            return 0;
        } else {
            if (sscanf(argv[1], "%u", &columns) != 1) {
                fprintf(stderr, "b2x: bad number of columns '%s'; use '-h' for help\n", argv[1]);
                exit(1);
            }
        }
    }

    while ((ch = getchar()) != EOF) {
        putchar(hex[ch >> 4]);
        putchar(hex[ch & 15]);
        col++;
        if ((columns > 0) && (col >= columns)) {
            putchar('\n');
            col = 0;
        }
    }
    if ((columns > 0) && (col > 0)) {
        putchar('\n');
    }
    return 0;
}
