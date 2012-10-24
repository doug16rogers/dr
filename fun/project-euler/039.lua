-- Project Euler Problem 39.
-- $Id: 039.lua 71 2008-10-24 10:13:33Z rogers $

-- If p is the perimeter of a right angle triangle with integral length
-- sides, {a,b,c}, there are exactly three solutions for p = 120.

--        {20,48,52}, {24,45,51}, {30,40,50}

-- For which value of p 1000, is the number of solutions maximised?

-------------------------------------------------------------------------------
-- Notes:

-- Need to find way to generate pythagorean triples. Had this as a problem
-- in number theory. Just need to re-derive. Then for each one, accumulate
-- multiples in a table.

require'nt'

local LIMIT = 999
local list = {}

for trip in nt.pythagorean_triples() do
   local trip_sum = nt.sum(trip)

   if trip_sum > LIMIT then
      break
   end

   for k = 1, LIMIT / trip_sum do
      local sum = k * trip_sum

      if not list[sum] then
         list[sum] = {}
      end

      list[sum][#list[sum]+1] = { k * trip[1], k * trip[2], k * trip[3] }
      print(sum, nt.array_to_string(list[sum][#list[sum]]))
   end
end   -- for each pythagorean triple
