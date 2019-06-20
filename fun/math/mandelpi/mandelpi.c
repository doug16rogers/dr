#include <inttypes.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>

/* typedef double real_t; */
typedef long double real_t;

/*
 * Since this derivation does not require any imaginary components, we can
 * stick with real numbers rather than dealing with complex numbers,
 * magnitudes, etc.
 *
 * Note: This doesn't work so well with floating point numbers. Errors begin
 *       to spoil the calculation when power10 exceeds 12. Need to switch to
 *       using rationals.
 */

static real_t inline mandit(real_t z, real_t c) {
    return z * z + c;
}

int main(int argc, char *argv[]) {
    uint64_t n = 0;
    int power10 = 0;
    if ((argc != 2) ||
        (1 != sscanf(argv[1], "%d", &power10)) || (power10 <= 0) || (power10 > 40)) {
        fprintf(stderr, "usage: 'mandelpi <power10>', where <power10> is a positive power of ten that's not big.\n");
        return 1;
    }
    real_t c = ((real_t) 0.25) + powl(10.0, -power10);
    real_t z = 0;
    if (sizeof(real_t) > sizeof(double)) {
        printf("c=%28.24La\n", (long double) c);
    } else {
        printf("c=%20.14a\n", (double) c);
    }
    while (z <= 2.0){
        n++;
        z = mandit(z, c);
    }
    char text[0x40] = "";

//    printf("sizeof(real_t)=%zu\n", sizeof(real_t));
    snprintf(text, sizeof(text), "%"  PRIu64, n);
    printf("%c.%s\n", text[0], &text[1]);
    return 0;
}
