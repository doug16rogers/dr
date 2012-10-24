-- Project Euler Problem 23.

local nt = require'nt'
local is_abundant = nt.is_abundant

local abundants = {}
local LIMIT = 28124

for n = 12, LIMIT - 12 do
   if is_abundant(n) then
      abundants[#abundants+1] = n
   end
end

print(#abundants, '#abundants')

local sum_of_two = {}

for i = 1, LIMIT - 1 do
   sum_of_two[i] = false
end

for _, a1 in ipairs(abundants) do
   for _, a2 in ipairs(abundants) do
      local sum = a1 + a2
      if sum < LIMIT then
         sum_of_two[sum] = true
      else
         break
      end
   end
end

local sum = 0

for n, is_sum in ipairs(sum_of_two) do
   if not is_sum then
      -- print(n)
      sum = sum + n
   end
end

print(sum)
-- 4179871

