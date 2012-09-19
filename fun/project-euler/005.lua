-- Project Euler Problem 5.

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
end

local factor_count = {}

for i = 2, 20 do
   local facs, count = factors(i)
   for p, c in pairs(count) do
      if c > (factor_count[p] or 0) then
         factor_count[p] = c
      end
   end
end

local n = 1

for p, c in pairs(factor_count) do
   n = n * (p^c)
   print(p, c, n)
end
