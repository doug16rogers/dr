-- Project Euler Problem 36.

-- The decimal number, 585 = 1001001001 (binary), is palindromic in both
-- bases.

-- Find the sum of all numbers, less than one million, which are palindromic
-- in base 10 and base 2.

-- (Please note that the palindromic number, in either base, may not include
-- leading zeros.)

-- Notes: Added is_palindrome() to nt.

require 'nt'

local LIMIT = 999999
-- local LIMIT = 1000

local sum = 0

for i = 1, LIMIT do
   if nt.is_palindrome(tostring(i)) then
      local binary = nt.tobase(i, 2)

      if nt.is_palindrome(nt.tobase(i, 2)) then
         print(i, binary)
         sum = sum + i
      end
   end
end

print(sum)
-- 872187
