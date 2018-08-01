/* Copyright (c) 2018 Doug Rogers under the terms of the MIT License. */
/* See http://www.opensource.org/licenses/mit-license.html.. */
/* $Id$ */

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
