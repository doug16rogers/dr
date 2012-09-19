-- Project Euler Problem 37.

-- The number 3797 has an interesting property. Being prime itself, it is
-- possible to continuously remove digits from left to right, and remain
-- prime at each stage: 3797, 797, 97, and 7. Similarly we can work from
-- right to left: 3797, 379, 37, and 3.

-- Find the sum of the only eleven primes that are both truncatable from left
-- to right and right to left.

-- NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

-- Notes:

-- I could play games with "growing" primes rather than trimming them.  Start
-- with a 2, 3, 5, and 7, then add digits to generate a left-list and a
-- right-list. But I think it's easier just to brute force it.

require 'nt'

local n = 0

print('Generating primes up to 1e6.')

local primes, is_prime = nt.primes_by_mod(1e6)

local sum = 0

local function left_prime(p)
   if p < 10 then
      return is_prime[p]
   end

   p = (p - (p % 10)) / 10
   return is_prime[p] and left_prime(p)
end

local floor = math.floor

local function right_prime(p)
   if p < 10 then
      return is_prime[p]
   end

   p = tonumber(tostring(p):sub(2))

   return is_prime[p] and right_prime(p)
end

print('Starting search...')

for i, p in ipairs(primes) do
   if (p >= 10) and
        left_prime(p) and
        right_prime(p) then
      n = n + 1
      print(p)
      sum = sum + p

      if n >= 11 then
         break
      end
   end
end

print(n, sum)
