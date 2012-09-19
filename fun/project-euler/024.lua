-- Project Euler Problem 24.

-- Zero-based is the way to go, so the millionth permutation is permutation
-- number 999,999.

-- Start off with a list of the digits from 0 .. 9. The first digit of the
-- Nth permutation will be floor(N / 9!) since the rest of the digits can be
-- permuted in 9! ways. Similarly down the list. The only difficulty is in
-- managing the list of digits, but table.remove() makes that easy. Remember
-- that Lua arrays start at 1.

local permutation_number = 1e6   -- 1e6

local function factorial(n)
   if n <= 1 then
      return 1
   end

   return n * factorial(n - 1)
end   -- factorial(n)

local digits_in  = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }
local digits_out = {}

local value = permutation_number - 1

for i = 9, 1, -1 do
   local fact = factorial(i)
   local next_value = value % fact
   local index = ((value - next_value) / fact) + 1
   -- print(index, #digits_in)
   digits_out[#digits_out+1] = digits_in[index]
   table.remove(digits_in, index)
   value = next_value
end

digits_out[#digits_out+1] = digits_in[1]   -- grab the last digit.
table.remove(digits_in, 1)

for _, d in ipairs(digits_out) do
   io.write(tostring(d))
end

io.write('\n')
-- 2783915460
