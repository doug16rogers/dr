/* Copyright (c) 2017 Doug Rogers under the terms of the MIT License. */
/* See http://www.opensource.org/licenses/mit-license.html.. */
/* $Id$ */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

/* ------------------------------------------------------------------------- */
uint64_t fib3_with(uint64_t a0, uint64_t a1, uint64_t a2, uint64_t n) {
    if (0 == n) {
        return a0;
    } else if (1 == n) {
        return a1;
    } else if (2 == n) {
        return a2;
    } else if ((0 == a0) && (0 == a1) && (0 == a2)) {
        return 0;
    } else {
        for (uint64_t i = 3; i <= n; ++i) {
            uint64_t an = a0 + a1 + a2;
            if (an < a2) {
                fprintf(stderr, "fib3: overflow in iteration %llu/%llu; last value = %llu; aborting\n",
                        i, n, a2);
                abort();
            }
            a0 = a1;
            a1 = a2;
            a2 = an;
        }
        return a2;
    }
}   /* fib3_with() */

/* ------------------------------------------------------------------------- */
int main(int argc, char* argv[]) {
    if (argc != 5) {
        fprintf(stderr, "usage: fib3 a0 a1 a2 n\n");
        return 1;
    }
    uint64_t a0 = atoi(argv[1]);
    uint64_t a1 = atoi(argv[2]);
    uint64_t a2 = atoi(argv[3]);
    uint64_t n = atoi(argv[4]);
    /* printf("[%llu,%llu,%llu]-fib3(%llu) = %llu\n", a0, a1, a2, n, fib3_with(a0, a1, a2, n)); */
    printf("%llu\n", fib3_with(a0, a1, a2, n));
    return 0;
}
