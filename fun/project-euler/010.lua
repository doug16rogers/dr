-- Project Euler Problem 10.

-- This is a modified 007.lua.

local sqrt = math.sqrt
local primes = { 2 }

local n = 3
local sum = 2

while n < 2e6 do
   for i, p in ipairs(primes) do
      if p * p > n then
         primes[#primes+1] = n
         sum = sum + n
--       if #primes < 20 or #primes % 100 == 0 then print(#primes, n) end
         break
      end

      if n % p == 0 then
         break
      end
   end

   n = n + 2
end

print(#primes, primes[#primes], sum)
-- 148933	1999993	142913828922
