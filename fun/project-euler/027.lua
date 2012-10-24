-- Project Euler Problem 27.

-- Find a quadratic formula that produces the maximum number of primes for
-- consecutive values of n.

local nt = require 'nt'

print('Generating primes...')
local prime, is_prime = nt.primes_by_sieve(1e6)
print('Solving problem...')

-- Use MIN_B, MAX_B = -999, 1601 to see it find the larger example.
local MIN_A, MAX_A = -999, 999
local MIN_B, MAX_B = MIN_A, MAX_A

-- Brute force:

local max_n, max_a, max_b = 0, 0, 0

for a = MIN_A, MAX_A do
   for b = MIN_B, MAX_B do
      local n = 0

      while is_prime[n*n + a*n + b] do
         n = n + 1
      end

      if n > max_n then
         max_n = n
         max_a = a
         max_b = b
         print(a, b, n)
      end
   end   -- for each b value
end   -- for each a value

print(max_a, max_b, max_n, max_a * max_b)
-- -61	971	71	-59231
