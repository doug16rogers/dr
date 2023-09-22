/* Copyright (c) 2016-2019 Doug Rogers under the Zero Clause BSD License. */
/* You are free to do whatever you want with this software. See LICENSE.txt. */

#include "sieve.h"

/**
 * Determine if @p n is prime.
 *
 * This is O(n) for 32-bit @p n otherwise it is O(log(n)).
 *
 * If @p n is in the sieve lookup table then the result will be immediately
 * known, otherwise an exhaustive search will be performed up to @c sqrt(n)
 * for divisibility by the primes in the sieve lookup table.
 *
 * @return true if @p n is prime, false otherwise.
 */
bool sieve_is_prime(uint64_t n) {
    /* If 32-bit then do a simple lookup first. */
    if (n < kSieveTableMax) {
        switch (n) {
        case 0: return false;
        case 1: return false;
        case 2: return true;
        default: break;
        }
        if ((n & 1ull) == 0) {
            return false;
        }
        n >>= 1;        /* Lookup the odd number. */
        if (kSieveTable[n / 64ull] & (1ull << (n % 64ull))) {
            return false;
        }
        return true;
    }
    /* Test for evenness is performed in sieve_least_prime_factor(). */
    return sieve_least_prime_factor(n) == n;
}   /* sieve_is_prime() */

/* ------------------------------------------------------------------------- */
/**
 * @return the least prime factor of @p n.
 */
uint64_t sieve_least_prime_factor(uint64_t n) {
    switch (n) {
    case 0: return 0;
    case 1: return 1;
    case 2: return 2;
    default:
        break;
    }
    if ((n & 1ull) == 0) {
        return 2;
    }
    if ((n < kSieveTableMax) && sieve_is_prime(n)) {
        return n;
    }
    for (uint64_t p = 3; (p < kSieveTableMax) && ((p * p) <= n); p += 2) {
        if (!sieve_is_prime(p)) {
            continue;
        }
        if ((n % p) == 0) {
            return p;
        }
    }
    return n;
}   /* sieve_least_prime_factor() */
