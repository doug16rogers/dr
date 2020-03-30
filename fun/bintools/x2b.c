#include <stdio.h>

int hexval(int c) {
    if (('0' <= c) && (c <= '9')) return c - '0';
    if (('a' <= c) && (c <= 'f')) return c - 'a' + 10;
    if (('A' <= c) && (c <= 'F')) return c - 'A' + 10;
    return -1;
}
int main(int argc, char *argv[]) {
    int digit = 0;
    int digits = 0;
    int byte = 0;
    int ch = 0;
    (void) argc;
    (void) argv;
    while ((ch = getchar()) != EOF) {
        digit = hexval(ch);
        if (-1 == digit) {
            if (digits > 0) {
                digits = 2;
            }
        } else {
            byte = (byte << 4) + digit;
            digits++;
        }
        if (digits >= 2) {
            putchar(byte);
            digits = 0;
            byte = 0;
        }
    }
    return 0;
}
