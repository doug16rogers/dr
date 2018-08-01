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
