-- Project Euler Problem 31.

-- In England the currency is made up of pound, £, and pence, p, and there
-- are eight coins in general circulation:

--     1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).

-- It is possible to make £2 in the following way:

--     1£1 + 150p + 220p + 15p + 12p + 31p

-- How many different ways can £2 be made using any number of coins?

local nt = require 'nt'

local floor  = math.floor
local printa = nt.print_array
local printf = function (fmt, ...) print(string.format(fmt, ...)) end
local atos   = nt.array_to_string

local TOTAL, NUMBERS = 200, { 200, 100, 50, 20, 10, 5, 2, 1 }
-- local TOTAL, NUMBERS = 10, { 10, 5, 2, 1 }
-- local TOTAL, NUMBERS = 5, { 5, 2, 1 }

printf('Sum to %u using %s...', TOTAL, atos(NUMBERS))

local count = 0

local function findsumeq(sum, set, coeff, sum_so_far, index)
   if sum_so_far >= sum or index > #set then
      return
   end

   local number = set[index]
   local max_of_index = floor(sum / number)

   for i = max_of_index, 0, -1 do
      coeff[index] = i

      if sum_so_far + (i * number) == sum then
         count = count + 1
         -- printf('%7u %s', count, atos(coeff))
      end

      findsumeq(sum, set, coeff, sum_so_far + (i * number), index + 1)
   end   -- for each at the index
end   -- findsumeq()

local function zero_array(n)
   local a = {}
   for i = 1, n do a[i] = 0 end
   return a
end   -- zero_array()

findsumeq(TOTAL, NUMBERS, zero_array(#NUMBERS), 0, 1)
print(count)
-- 73682
