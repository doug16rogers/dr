-- Project Euler Problem 41.
-- $Id: 041.lua 71 2008-10-24 10:13:33Z rogers $

-- We shall say that an n-digit number is pandigital if it makes use of all
-- the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital
-- and is also prime.

-- What is the largest n-digit pandigital prime that exists?

-------------------------------------------------------------------------------
-- Notes:

-- Well, if there can't be a 9-digit or 8-digit solution because they're
-- always divisible by 9 (and 3). So start with 7.

require'nt'

for k = 7, 4, -1 do
   for p in nt.reverse_permutations(k) do
      local n = tonumber(table.concat(p))
      local factors = nt.factors(n)

--      print(n, nt.array_to_string(factors))

      if factors[1] == n then
         print(n)
         -- 7652413
         -- break
      end   -- if
   end -- for
end   -- for each number of digits
