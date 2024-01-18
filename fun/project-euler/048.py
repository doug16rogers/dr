#!/usr/bin/env python3

# The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

# Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.

def exp_mod(n, e, m):
    r = 1
    bit = 0x0400   # Max 1000 needed.
    while bit > 0:
        r = (r * r) % m
        if e & bit:
            r = (r * n) % m
        bit >>= 1
    return r

assert exp_mod(1, 0, 100) == 1
assert exp_mod(1, 1, 100) == 1
assert exp_mod(1, 2, 100) == 1
assert exp_mod(1, 3, 100) == 1
assert exp_mod(2, 0, 100) == 1
assert exp_mod(2, 1, 100) == 2
assert exp_mod(2, 2, 100) == 4
assert exp_mod(2, 13, 100) == 92  # 8192

MOD = 10 ** 10
sum = 0
for i in range(1000):
    sum += exp_mod(i+1, i+1, MOD)
sum %= MOD
print(f'{sum:10d}')
