-- Project Euler Problem 50.

-- There are a couple ways to go about this.

-- I keep track the starting index (i, max_i), current maximum length
-- (max_n), and the maximum sum (max_sum). I then let an index run through
-- the set of primes starting at i + max_n (no need to start with a smaller
-- length than max_n+1), carefully modifying the sum as I go.

local floor = math.floor
local sqrt = math.sqrt

function printf(fmt, ...) io.stdout:write(string.format(fmt, ...)) end

local LIMIT = 1e6

-------------------------------------------------------------------------------
function primes_by_sieve(limit)
   local primes = { 2 }
   local is_prime = { [2] = true }

   for n = 3, limit, 2 do
      is_prime[n] = true
   end

   for n = 3, limit, 2 do
      if is_prime[n] then
         for j = n + n, limit, n do
            is_prime[j] = nil
         end

         primes[#primes + 1] = n
      end
   end   -- while incrementing through sieve

   return primes, is_prime
end   -- primes_by_sieve()

-------------------------------------------------------------------------------
function primes_by_mod(limit)
   local primes = { 2 }
   local is_prime = { [2] = true }

   for n = 3, limit, 2 do
      local limit = floor(sqrt(n))

      for _, p in ipairs(primes) do
         if n % p == 0 then
            break
         end

         if p >= limit then
            is_prime[n] = true
            primes[#primes + 1] = n
            break
         end
      end   -- for each prime already collected, check for divisibility.
   end   -- for each n

   return primes, is_prime
end

-- The sieve was more than twice as fast.
-- print(os.date())
-- local primes, is_prime = primes_by_sieve(LIMIT)
-- printf('%u primes below %u.\n', #primes, LIMIT)
-- print(os.date())
-- local primes, is_prime = primes_by_mod(LIMIT)
-- printf('%u primes below %u.\n', #primes, LIMIT)
-- print(os.date())

local primes, is_prime = primes_by_sieve(LIMIT)
printf('%u total primes below %u.\n', #primes, LIMIT)

local p_i = 1

for n = 1, LIMIT do
   if is_prime[n] then
      if n ~= primes[p_i] then
         printf("%u is_prime, but isn't %u!\n", n, primes[p_i])
      end
      p_i = p_i + 1
   end
end
      

local sum = 5
local max_sum = 5
local max_i = 1
local max_n = 2

local i = 1

while (i + max_n - 1) <= #primes do
   local initial_sum = sum
   -- print(i, max_n, sum, 'trying next set')

   if is_prime[sum] and sum > max_sum then
      max_i = i
      max_sum = sum
      -- same max_n right now
      -- print(max_i, max_n, max_sum, 'bigger sum at same max_n')
   end

   -- Now expand, trying to find a new max_n
   for j = i + max_n, #primes do
      sum = sum + primes[j]
      -- print(i, j - i + 1, sum)

      if sum > LIMIT then
         break
      end
      
      -- max_n is guaranteed to be larger here:

      if is_prime[sum] then
         max_i = i
         max_n = j - i + 1
         max_sum = sum
         initial_sum = sum
         -- print(max_i, max_n, max_sum, 'new bigger max_n')
      end
   end

   -- Now remove the previous starting prime from the max sum, add in the
   -- next prime, and try again.

   sum = initial_sum - primes[i] + (primes[i + max_n] or 0)

   -- printf('old sum %u - %u primes[%u] + %u primes[%u] = %u new sum\n',
   --        initial_sum, primes[i], i, (primes[i + max_n] or 0), i + max_n, sum)
   i = i + 1
end

printf('Maximum consecutive primes that add to another prime below %u:\n',
       LIMIT)
printf("%u primes from %u + %u + .. + %u = %u.\n",
       max_n, primes[max_i], primes[max_i+1], primes[max_i+max_n-1], max_sum)
