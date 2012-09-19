-- Project Euler Problem 35.

local floor = math.floor
local sqrt = math.sqrt

function printf(fmt, ...) io.stdout:write(string.format(fmt, ...)) end

local LIMIT = 1e6

-------------------------------------------------------------------------------
-- This was stolen from 050.lua.
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

local primes, is_prime = primes_by_sieve(LIMIT)
printf('%u total primes below %u.\n', #primes, LIMIT)

-------------------------------------------------------------------------------
function decimal_digits(n)
   local digits = 0

   while n >= 1 do
      n = n / 10
      digits = digits + 1
   end

   return digits
end   -- decimal_digits()

-------------------------------------------------------------------------------
local circs = {}
local is_circ = {}

-------------------------------------------------------------------------------
function add_circ(p)
   if not is_circ[p] then
      is_circ[p] = true
      circs[#circs+1] = p
   end
end   -- add_circ()

-------------------------------------------------------------------------------
function check_circular(p, digits)
   local rotations = { p }

   for i = 1, digits - 1 do
      local lsd = p % 10
      p = (p - lsd) / 10
      p = p + (lsd * (10 ^ (digits - 1)))
      rotations[#rotations + 1] = p

      if not is_prime[p] then
         return false
      end
   end   -- checking each rotation

   print(rotations[1])

   for _, r in ipairs(rotations) do
      add_circ(r)
   end

   return true
end   -- check_circular()

for _, p in ipairs(primes) do
   if not is_circ[p] then
      check_circular(p, decimal_digits(p))
   end
end   -- for each prime


printf("%u circular primes less than %u.\n", #circs, LIMIT)
