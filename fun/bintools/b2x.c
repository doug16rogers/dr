#include <stdio.h>

int main(int argc, char *argv[]) {
    (void) argc;
    (void) argv;
    const char hex[] = "0123456789abcdef";
    int ch = 0;
    int col = 0;
    while ((ch = getchar()) != EOF) {
        putchar(hex[ch >> 4]);
        putchar(hex[ch & 15]);
        if (++col == 32) {
            putchar('\n');
            col = 0;
        }
    }
    if (col > 0) {
        putchar('\n');
    }
    return 0;
}
