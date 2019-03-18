/* Copyright (c) 2016-2019 Doug Rogers under the Zero Clause BSD License. */
/* You are free to do whatever you want with this software. See LICENSE.txt. */

#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/**
 * Default name of this program.
 */
#define kProgram "mkprimemask"

#define WORD_BITS 64

#if (WORD_BITS == 8)
typedef uint8_t word_t;
#elif (WORD_BITS == 16)
typedef uint16_t word_t;
#elif (WORD_BITS == 32)
typedef uint32_t word_t;
#elif (WORD_BITS == 64)
typedef uint64_t word_t;
#else
#error "Invalid WORD_BITS"
#endif

/**
 * Actually one more than max N supported by sieve.
 */
uint64_t max_n = 1024;

/**
 * Sieve of odd numbers. If a bit is set then that number is composite.
 */
word_t* sieve = NULL;

/**
 * Only valid after sieve is created.
 */
bool is_prime(word_t n) {
    switch (n) {
    case 0: return false;
    case 1: return false;
    case 2: return true;
    default: break;
    }
    if ((n & 1) == 0) {
        return false;
    }
    n >>= 1;
    if (sieve[n / WORD_BITS] & (((word_t) 1) << (n % WORD_BITS))) {
        return false;
    }
    return true;
}

int main(int argc, char* argv[]) {
    uint64_t i = 0;
    uint64_t divisor = 1;
    uint64_t words = 0;
    if (argc > 1) {
        if ((1 != sscanf(argv[1], "%" SCNu64, &max_n)) || (max_n < 8ULL) || (max_n > 0x100000000ULL)) {
            fprintf(stderr, "%s: invalid max N \"%s\"\n", kProgram, argv[2]);
            return 1;
        }
    }
    words = (max_n + WORD_BITS - 1) / WORD_BITS / 2;            /* Odds only. */
    sieve = calloc(WORD_BITS / 8, words);
    if (NULL == sieve) {
        fprintf(stderr, "%s: could not allocate %" PRIx64 "%u bytes for sieve\n", kProgram, words, WORD_BITS);
        return 1;
    }

    sieve[0] = 0x0001ULL;    /* Set 1 to be composite. */
    for (divisor = 3; (divisor * divisor) < max_n; divisor += 2) {
        uint64_t step;
        if (!is_prime(divisor)) {
            continue;
        }
        /* Here's a new prime. Mark all odd multiples as composite. */
        for (step = 3 * divisor; step < max_n; step += 2 * divisor) {
            uint64_t step_2 = step >> 1;
            sieve[step_2 / WORD_BITS] |= 1ULL << (step_2 % WORD_BITS);
        }
    }

    /* Now print the is_prime() code. */
    printf("#include <stdint.h>\n");
    printf("const uint64_t kSieveTableMax = %llu;    /* Actually max + 1. */\n", (unsigned long long) max_n);
    printf("const uint64_t kSieveTable[0x%08llX] = {\n", (unsigned long long) words);
#define WORDS_PER_LINE 4
    for (i = 0; i < words; i++) {
        printf("0x%" PRIX64 ",", sieve[i]);
        if ((i % WORDS_PER_LINE) == (WORDS_PER_LINE - 1)) {
            printf("\n");
        }
    }
    if ((i % WORDS_PER_LINE) != 0) {
        printf("\n");
    }
    printf("};\n");
    return 0;
}
