/* Copyright (c) 2017 Doug Rogers under the terms of the MIT License. */
/* See http://www.opensource.org/licenses/mit-license.html.. */
/* $Id$ */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

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
        n -= 2;
        while (n--) {
            uint64_t an = a0 + a1 + a2;
            if (an < a2) {
                break;          /* eh. what to do on overflow? */
            }
            a0 = a1;
            a1 = a2;
            a2 = an;
        }
        return a2;
    }
}   /* fib3_with() */

/* ------------------------------------------------------------------------- */
int hi_bit(int n) {
    while (1) {
        int n_with_lowest_one_cleared = n & (n - 1);
        if (n_with_lowest_one_cleared == 0) {
            break;
        }
        n = n_with_lowest_one_cleared;
    }
    return n;
}   /* hi_bit() */

/* ------------------------------------------------------------------------- */
uint64_t u64pow(uint64_t base, int n) {
    uint64_t p = 1;
    int b = hi_bit(n);
    while (b) {
        p *= p;
        if (n & b) {
            p *= base;
        }
        n &= ~b;
        b >>= 1;
    }
    return p;
}

/* ------------------------------------------------------------------------- */
int main(int argc, char* argv[]) {
    if (argc != 3) {
        fprintf(stderr, "usage: fib3-scanner <digits> <target>\n");
        return 1;
    }
    int verbose = 0;
    int digits = atoi(argv[1]);
    uint64_t target = atoi(argv[2]);
    if (digits < 0) {
        verbose = 1;
        digits = -digits;
    }
    if ((digits < 4) || (digits > 16)) {
        fprintf(stderr, "invalid digit count %d outside of range 4..16\n", digits);
        return 1;
    }
    const uint64_t MAX_PATTERN = u64pow(10, digits);
    printf("PATTERN=%llu\n", MAX_PATTERN-1);
    for (uint64_t pattern = 0; pattern < MAX_PATTERN; ++pattern) {
        if ((pattern % 10000) == 0) {
            printf("pattern=%0*llu\r", digits, pattern);
            fflush(stdout);
        }
        /*
         * For each pattern, try each partition into a0, a1, a2 and n.
         */
        for (int da0 = 1; da0 <= (digits - 3); ++da0) {
            for (int da1 = 1; da1 <= (digits - da0 - 2); ++da1) {
                for (int da2 = 1; da2 <= (digits - (da0 + da1) - 1); ++da2) {
                    int dn = digits - (da0 + da1 + da2);
                    uint64_t pat = pattern;
                    uint64_t pn = u64pow(10, dn);
                    uint64_t n = pat % pn;
                    pat /= pn;
                    pn = u64pow(10, da2);
                    uint64_t a2 = pat % pn;
                    pat /= pn;
                    pn = u64pow(10, da1);
                    uint64_t a1 = pat % pn;
                    uint64_t a0 = pat / pn;
                    if (verbose) {
                        printf("pattern=%0*llu  a0=%-5llu a1=%-5llu a2=%-5llu n=%-5llu\n",
                               digits, pattern, a0, a1, a2, n);
                    }
                    if (fib3_with(a0, a1, a2, n) == target) {
                        printf("fib3 %llu %llu %llu %llu = %llu\n", a0, a1, a2, n, target);
                    }
                }
            }
        }
    }
    printf("\n");
    return 0;
}   /* main() */
