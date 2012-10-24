-- Project Euler Problem 32.

-- The product 7254 is unusual, as the identity, 39 186 = 7254, containing
-- multiplicand, multiplier, and product is 1 through 9 pandigital.

-- Find the sum of all products whose multiplicand/multiplier/product identity
-- can be written as a 1 through 9 pandigital.

-- HINT: Some products can be obtained in more than one way so be sure to only
-- include it once in your sum.

-- NOTES:

-- There are 362,880 different permutations of 9 digits. A brute force method
-- could place the multiplication operator and the equals sign in each
-- reasonable location then test the result. Any positives would be added to
-- a hash list.

-- Problem 24 has notes on how to generate the Nth permutation of a set of
-- numbers. I'll add that to nt.lua.

require'nt'

-- This is slow, but it works.

-- local N = 6
local N = 9
local prods = {}
local sum = 0

for p in nt.permutations(N) do
   local s = table.concat(p)
   for times_i = 2, N / 2 do
      for equals_i = times_i + 1, N - 2 do
         local a = s:sub(1, times_i - 1)
         local b = s:sub(times_i, equals_i - 1)
         local c = s:sub(equals_i)
         local func = 'return ' .. a .. '*' .. b .. ' == ' .. c

         if loadstring(func)() then
            if not prods[c] then
               prods[c] = true
               prods[#prods+1] = c
               sum = sum + tonumber(c)
               print('+++', func)
            else
               print('---', func)
            end
         end
      end
   end
end

print(#prods, sum)
-- 7	45228
