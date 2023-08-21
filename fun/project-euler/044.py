#!/usr/bin/env python3

import math
import sys


def pentagonal(n):
    return n * (3 * n - 1) // 2


# Eh, I don't see a good way to determine if a given number is pentagonal
# without doing a square-root operation:

#  p = n(3n-1)/2 -> 3n^2 - n - 2p = 0
#  n = [1 + sqrt(1 + 24p)]/6


def isqrt(n):
    x = n
    y = (x + 1) // 2
    while y < x:
        x = y
        y = (x + n // x) // 2
    return x


def inverse_pentagonal(p):
    d = 1 + 24 * p
    s = math.isqrt(d)    # From Python 3.8.
    if s * s != d:
        return None
    if s % 6 != 5:
        return None
    return (1 + s) // 6


# P[n] = n(3n - 1) / 2 = (3n^2 - n) / 2
# P[n+1] = (n+1)*(3(n+1) - 1)/2 = (n+1)*(3n+2)/2 = (3n^2+5n+2)/2
#        = [(3n^2 - n) + (6n + 2)]/2
#        = P[n] + (3n + 1)
#        = P[n] + 3(n+1) - 2

n = 1
p = [0, 1]   # Allow indexes to match problem.
ip = { 1: 1 }
d = []
s = []

if True:
    # Brute force
    while True:
        p.append(p[n] + 3 * n + 1)
        ip[p[n]] = n
        n += 1
        if n < 2500:
            continue
        sys.stdout.write(f'\rP[{n}]={p[n]}  ')
        for j in range(1, n):
            for k in range(j+1, n+1):
                s = p[k] + p[j]
                if s > p[n]:
                    break
                d = p[k] - p[j]
                if s in p and d in p:
                    print(f'\np[{j}]={p[j]}, p[{k}]={p[k]}, s=p[{ip[s]}]={s}, d=p[{ip[d]}]={d}')
# Brute force above, starting search at p[10000], though p[2500] suffices.
# $ ./044.py
# P[10000]=149995000
# p[1020]=1560090, p[2167]=7042750, s=p[2395]=8602840 d=p[1912]=5482660
# P[10001]=150025001  ^C

    exit(0)

# That takes too long (though I did get an answer), so do some analysis...
#    P[k] + P[j] = k(3k-1)/2 + j(3j-1)/2 = P[s] = s(3s-1)/2
#    P[k] - P[j] = k(3k-1)/2 - j(3j-1)/2 = P[d] = d(3d-1)/2
#     (a)   3s^2-s = 3k^2-k + 3j^j-j
#     (b)   3d^d-d = 3k^2-k - 3j^j-j
#    (a+b)  3s^2-s + 3d^d-d = 6k^2-2k
#    (a-b)  3s^2-s - 3d^d-d = 6j^2-2j

# So 24p+1 must be a perfect square and its square root must be 5 mod 6. For
# example, p=2 does yield the perfect scquare 49, but its square root is 1
# mod 6, not 5 mod 6.

# One optimization is to run up the list of P's and see if it is a difference,
# D. The check need run only up to the point where 3n+1 is greater than D. But
# that's still quadratic. But this would guarantee that we have minimized the
# value of D. We can start at P[4] since the earlier ones cannot be differences
# because they are too small.

