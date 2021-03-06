/* Copyright (c) 2016-2019 Doug Rogers under the Zero Clause BSD License. */
/* You are free to do whatever you want with this software. See LICENSE.txt. */

#ifndef MATH_SIEVE_H_
#define MATH_SIEVE_H_

#include <stdbool.h>
#include <stdint.h>

extern const uint64_t kSieveTableMax;   /* Actually max testable value + 1. */
extern const uint64_t kSieveTable[];    /* Bitmask for odd numbers, LSB first. */

/*
 * If you've built with the full 4G sieve space, these will work for all
 * 64-bit numbers. Otherwise they're only good up to kSieveTableMax^2 - 1.
 */
bool sieve_is_prime(uint64_t n);        /* O(1) if 32-bit else O(log(n)). */
uint64_t sieve_least_prime_factor(uint64_t n);

#endif  // MATH_SIEVE_H_
