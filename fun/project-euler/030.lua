-- Project Euler Problem 30

-- Surprisingly there are only three numbers that can be written as the sum
-- of fourth powers of their digits:

--    1634 = 1^4 + 6^4 + 3^4 + 4^4
--    8208 = 8^4 + 2^4 + 0^4 + 8^4
--    9474 = 9^4 + 4^4 + 7^4 + 4^4

-- As 1 = 1^4 is not a sum it is not included.

-- The sum of these numbers is 1634 + 8208 + 9474 = 19316.

-- Find the sum of all the numbers that can be written as the sum of fifth
-- powers of their digits.

nt = require 'nt'

local ceil   = math.ceil
local log    = math.log
local divmod = nt.divmod

local log10 = function (x) return log(x) / log(10) end
local N = 5
local num_power = { [0] = 0, 1, 2^N, 3^N, 4^N, 5^N, 6^N, 7^N, 8^N, 9^N }

-------------------------------------------------------------------------------
local function power_sum(n)
   if n < 10 then
      return num_power[n]
   end

   local left, right = divmod(n, 10)
   return num_power[right] + power_sum(left)
end   -- power_sum()

-- A quick estimate of how far we need to search... The maximum value for
-- power_sum(10^digits - 1) will be digits * 9^5 = digits * 59049. Clearly at
-- least 6 digits are needed, and 7 digits are too many since 10^7 > 7*59049.

-- But in the general case we need to find the minimal digits such that
-- 10^digits > digits * 9^N. Taking the log (base 10) of both sides yields:

--   digits > log(digits) + N * log(9)

-- Since log(digits) will be at least 5, a good first guess is digits =
-- ceil(0.699 + N * log(9)) = ceil(0.699 + 5 * 0.954) = 6. Of course for
-- much larger values of N, the value must computed.

local MAX_DIGITS = 1
local Nlog9 = N * log10(9)

while true do
   local digits_for_9N = ceil(log10(MAX_DIGITS) + Nlog9)
   -- print(MAX_DIGITS, digits_for_9N)
   if MAX_DIGITS >= digits_for_9N then break end
   MAX_DIGITS = MAX_DIGITS + 1
end

local LIMIT = 10^MAX_DIGITS - 1
print('LIMIT', LIMIT)

local sum = 0

for n = 10, LIMIT do
   local ps = power_sum(n)
   if n == ps then
      sum = sum + n
      print(n, sum)
   end
end   -- for

print(sum)
-- 443839
