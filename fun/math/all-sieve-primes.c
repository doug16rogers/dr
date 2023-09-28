/* Copyright (c) 2016-2019 Doug Rogers under the Zero Clause BSD License. */
/* You are free to do whatever you want with this software. See LICENSE.txt. */

/*
 * Print all the primes identified in the selected sieve table, one per line.
 */
#include <stdio.h>

#include "sieve.h"

int main(int argc, char **argv) {
    uint64_t max = 0;
    uint64_t n = 0;
    if ((argc != 2) || (sscanf(argv[1], "%lu", &max) != 1)) {
        fprintf(stderr, "Usage: all-sieve-primes <up-to-max>\n");
        return 1;
    }
    if (max > kSieveTableMax) {
        fprintf(stderr, "all-sieve-primes: max value %lu > %lu kSieveTableMax\n", max, kSieveTableMax);
        return 1;
    }
    for (n = 2; n < max; n++) {
        if (sieve_is_prime(n)) {
            printf("%lu\n", n);
        }
    }
    return 0;
}
