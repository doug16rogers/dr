-- Project Euler Problem 21

-------------------------------------------------------------------------------
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

-------------------------------------------------------------------------------
-- function allfactors(count)
--    local f = {}
-- end   -- allfactors()

-------------------------------------------------------------------------------
function sum(t)
   local s = 0
   for _,v in ipairs(t) do
      s = s + v
   end
   return s
end   -- sum()

local sums = {}

-- This is definitely the slow way, but it works:

sums[1] = 1

for n = 2, 9999 do
   sums[n] = 1
   for i = 2, math.floor(n / 2) do
      if n % i == 0 then
         sums[n] = sums[n] + i
      end
   end
end

-- This is the faster way:
-- sums[1] = 1

-- for n = 2, 9999 do
--    sums = sum(allfactors(select(2, factors(n))))
-- end

local amicable = {}

-- print(220, sums[220])
-- print(284, sums[284])

for i = 1, 9999 do
   local other = sums[i]
   if sums[other] == i and other ~= i then
      amicable[#amicable + 1] = i
   end
end

print(#amicable, sum(amicable))
-- 10	31626
