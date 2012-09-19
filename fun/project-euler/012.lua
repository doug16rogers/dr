-- Project Euler Problem 12.

-- This problem could explode, so I use the fact that triangle number N
-- is N * (N+1) / 2 and that the number of divisors of a number whose
-- unique factorization is Product(p_i ^ e_i) is Product(e_i).

-- Note that N and N + 1 are relatively prime so they share no factors.  That
-- means the product of their individual prime factorization exponents is the
-- the same as the product of their combined factorization exponents.

function factors(n)
   local s = {}
   local count = {}

   if n == 0 then
      return {}
   end

   if n == 1 then
      return { 1 }
   end

   if n < 0 then
      n = -n
      s[#s + 1] = -1
      count[-1] = 1
   end

   if n == 1 then
      if #s > 0 then
         return s, count
      else
         return { 1 }, { [1] = 1 }
      end
   end

   local last_p = 0
   local p = 2

   while (n % p) == 0 do
      s[#s + 1] = p
      n = n / p
      count[p] = (count[p] or 0) + 1
      last_p = p
   end

   p = 3

   -- Cannot check for p*p > n because n is reduced.
   -- But need to find way for p*p > n check when not reduced along the way.

   while (p * p) <= n do
      while (n % p) == 0 do
         s[#s + 1] = p
         count[p] = (count[p] or 0) + 1
         n = n / p
      end

      p = p + 2
   end

   if n > 1 then
      s[#s + 1] = n
      count[n] = (count[n] or 0) + 1
   end

   return s, count
end   -- factors()

local n = 2
local _, cn0 = factors(n)
local fc0 = 1

cn0[2] = cn0[2] - 1   -- Divided by 2.

for _, e in pairs(cn0) do
   fc0 = fc0 * (e + 1)
end

local factor_count = 0

repeat
   local _, cn1 = factors(n+1)
   local fc1

   -- This is the / 2 in N * (N + 1) / 2.
   if cn1[2] then
      cn1[2] = cn1[2] - 1
   end

   fc1 = 1

   for _, e in pairs(cn1) do
      fc1 = fc1 * (e + 1)
   end

   factor_count = fc0 * fc1
--   if n % 100 == 0 then print(n, n*(n+1)/2, fc0, fc1, factor_count) end

   cn0 = cn1
   fc0 = fc1
   n = n + 1
until factor_count > 500

print(n-1, n*(n-1)/2, factor_count)
-- 12375	76576500	576
