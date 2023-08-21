#!/usr/bin/env python3

import itertools

pandigital = []

def three(a, b, c):
    return c + 10 * (b + 10 * a)

# d6 (from the puzzle) must be 5, so permute the rest.
for d in itertools.permutations([0, 1, 2, 3, 4, 6, 7, 8, 9]):
    if d[3] % 2 != 0:
        continue
    if three(d[2], d[3], d[4]) % 3 != 0:
        continue
    if three(d[4], 5, d[5]) % 7 != 0:
        continue
    if three(5, d[5], d[6]) % 11 != 0:
        continue
    if three(d[5], d[6], d[7]) % 13 != 0:
        continue
    if three(d[6], d[7], d[8]) % 17 != 0:
        continue
    s = f'{d[0]}{d[1]}{d[2]}{d[3]}{d[4]}5{d[5]}{d[6]}{d[7]}{d[8]}'
    pandigital.append(int(s))
    # print(s)

print(sum(pandigital))
