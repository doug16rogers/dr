#!/usr/bin/env python3

# It was proposed by Christian Goldbach that every odd composite number can
# be written as the sum of a prime and twice a square.

#    9 =  7 + 2 * 1^2
#   15 =  7 + 2 * 2^2
#   21 =  3 + 2 * 3^2
#   25 =  7 + 2 * 3^2
#   27 = 19 + 2 * 2^2
#   33 = 31 + 2 * 1^2

# It turns out that the conjecture was false.

# What is the smallest odd composite that cannot be written as the sum of a
# prime and twice a square?

odd_is_prime = { 3: True, 5: True, 7: True }
odd_primes = [3, 5, 7]

def dprint(s):
    pass
    # print(s)

n = 7
while True:
    n += 2
    # Determine if prime.
    n_is_prime = True
    for p in odd_primes:
        if p * p >= n:
            break
        if n % p == 0:
            n_is_prime = False
            break
    if n_is_prime:
        dprint(f'{n}: prime')
        odd_is_prime[n] = True
        odd_primes.append(n)
        continue

    odd_is_prime[n] = False

    # Check if lesser Goldbach conjecture holds.
    conjecture_holds = False
    k = 1
    two_square = 2 * k * k
    while two_square < n:
        if odd_is_prime[n - two_square]:
            conjecture_holds = True
            dprint(f'{n}: {n-two_square} + 2 * {k}^2')
            break
        k += 1
        two_square = 2 * k * k

    if not conjecture_holds:
        print(n)
        break
