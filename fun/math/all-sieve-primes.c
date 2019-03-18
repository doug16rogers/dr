/* Copyright (c) 2016-2019 Doug Rogers under the Zero Clause BSD License. */
/* You are free to do whatever you want with this software. See LICENSE.txt. */

/*
 * Print all the primes identified in the selected sieve table, one per line.
 */
#include <stdio.h>

#include "sieve.h"

int main(void) {
    uint64_t n;
    for (n = 2; n < kSieveTableMax; n++) {
        if (sieve_is_prime(n)) {
            printf("%llu\n", (unsigned long long) n);
        }
    }
    return 0;
}
