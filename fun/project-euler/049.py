#!/usr/bin/env python3

# The arithmetic sequence, 1487, 4817, 8147, in which each of the terms
# increases by 3330, is unusual in two ways: (i) each of the three terms are
# prime, and, (ii) each of the 4-digit numbers are permutations of one
# another.

# There are no arithmetic sequences made up of three 1-, 2-, or 3-digit
# primes, exhibiting this property, but there is one other 4-digit increasing
# sequence.

# What 12-digit number do you form by concatenating the three terms in this
# sequence?

primes = [2, 3, 5, 7, 11, 13, 17, 23, 29]

def primes_below(n):
    if n <= 2:
        return []
    if n == 3:
        return [2]
    pl = [2]
    k = 3
    while k < n:
        is_prime = True
        for p in pl:
            if p * p > k:
                break
            if k % p == 0:
                is_prime = False
                break
        if is_prime:
            pl.append(k)
        k += 1
    return pl

MIN =  1000
MAX = 10000
primes = primes_below(MAX)
primes = [x for x in primes if x > MIN]

is_prime = {}
for i in range(MIN, MAX):
    is_prime[i] = False
for i in range(len(primes)):
    is_prime[primes[i]] = True

digits = []
for n in range(MAX):
    digits.append(''.join(sorted(str(n))))

seqs = []   # list of (p0, p1, p2) meeting criteria
j = 0
while (j < (len(primes) - 2)): # and (len(seqs) < 2):
    p0 = primes[j]
    k = j
    while k < (len(primes) - 1):
        k += 1
        p1 = primes[k]
        if digits[p0] != digits[p1]:
            continue
        p2 = p1 + (p1 - p0)
        if p2 >= MAX:
            break
        if is_prime[p2]:
            if digits[p0] == digits[p2]:
                seqs.append((p0, p1, p2))
    j += 1

# print(f'seqs={seqs}')

for seq in seqs:
    if seq[0] == 1487:
        continue
    print(f'{seq[0]:04d}{seq[1]:04d}{seq[2]:04d}')
