-- Project Euler Problem 7.

local sqrt = math.sqrt
local primes = { 2 }

local n = 3

while #primes < 10001 do
   for i, p in ipairs(primes) do
      if p * p > n then
         primes[#primes+1] = n
--       if #primes < 20 or #primes % 100 == 0 then print(#primes, n) end
         break
      end

      if n % p == 0 then
         break
      end
   end

   n = n + 2
end

print(#primes, primes[#primes])
-- 10001	104743
