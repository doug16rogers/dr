-- Project Euler Problem 34.

-- 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.

-- Find the sum of all numbers which are equal to the sum of the factorial of
-- their digits.

-- Note: as 1! = 1 and 2! = 2 are not sums they are not included.

-- NOTES

-- Eventually, log10(n) * 9! < n, so there is a highest number to check.
-- So n / log10(n) > 9!, or log10(n) - log10(log10(n)) > log10(362880)
-- So log10(n) - log10(log10(n)) ~= 5.560. Call it 7 and we have to check
-- up to 10^7 = 9,999,999. Not too bad. 7 * 362,880 = 2,540,160 < 9,999,999,
-- but 6 * 362,880 = 2,177,280 > 999,999, so I definitely need to check some
-- 7-digit numbers.

local math_floor = math.floor

require'nt'

local fact = {
   [0] = 1, 1, 2, 6, 24,
   120, 720, 5040, 40320, 362880
}

local function factorial_sum(n)
   local m = n
   local s = 0

   while m > 0 do
      s = s + fact[m % 10]
      m = math_floor(m / 10)
   end   -- while

   return s
end   -- factorial_sum()

local function is_factorial_sum(n)
   local m = n
   local s = 0

   while (m > 0) and (s <= n) do
      s = s + fact[m % 10]
      m = math_floor(m / 10)
   end   -- while

   return s == n
end   -- is_factorial_sum()

local sum = 0

for n = 10, 9999999 do
   if is_factorial_sum(n) then
      sum = sum + n
      print(n, sum)
   end
end

-- 145	145
-- 40585	40730
