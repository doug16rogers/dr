-- Project Euler Problem 39.
-- $Id: 039.lua 71 2008-10-24 10:13:33Z rogers $

-- If p is the perimeter of a right angle triangle with integral length
-- sides, {a,b,c}, there are exactly three solutions for p = 120.

--        {20,48,52}, {24,45,51}, {30,40,50}

-- For which value of p <= 1000, is the number of solutions maximised?

-------------------------------------------------------------------------------
-- Notes:

-- Need to find way to generate pythagorean triples. Had this as a problem
-- in number theory. Just need to re-derive. Then for each one, accumulate
-- multiples in a table.

require'nt'

args = { ... }

LIMIT = 1000

if #args >= 1 then
   LIMIT = tonumber(args[1])
end

function printf(fmt,...)
   local s = string.format(fmt, ...)
   print(s)
   return s
end

local list = {}
local max_count = 0
local perimeter_at_max = 0

function array2_to_string(aa)
   t = {}
   for _, a in ipairs(aa) do
      t[#t+1] = nt.array_to_string(a)
   end
   return table.concat(t, ', ')
end

for triple in nt.pythagorean_triples(false, 'perimeter') do
   local perimeter = nt.sum(triple)

   if perimeter > LIMIT then
      break
   end

   if not list[perimeter] then
      list[perimeter] = {}
   end

   perimeter_list = list[perimeter]
   perimeter_list[#perimeter_list + 1] = triple
   if #perimeter_list > max_count then
      max_count = #perimeter_list
      perimeter_at_max = perimeter
      print(max_count, perimeter_at_max, array2_to_string(perimeter_list))
   end
end   -- for each pythagorean triple
   