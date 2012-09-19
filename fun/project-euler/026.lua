-- Project Euler Problem 26.

-- Find the value of d < 1000 for which 1/d contains the longest recurring
-- cycle.

require'nt'

local LIMIT = 999

-------------------------------------------------------------------------------
-- Returns num / den, num % den
--
-- It would be nice to have direct access to the hardware here since quotient
-- and remainder are usually available from the same instruction.
local function divmod(num, den)
   local mod = num % den
   return (num - mod) / den, mod
end   -- divmod()

-------------------------------------------------------------------------------
-- Returns sequence length, table of digits to end of sequence
local function sequence_length(n)
   local i = 0
   local num = 1
   local dig = 0
   local digits = {}
   local first_numerator_index = { }

   while true do
      dig, num = divmod(10 * num, n)

      if first_numerator_index[num] then
         break
      end

      i = i + 1
      digits[i] = dig
      first_numerator_index[num] = i
   end

   if num == 0 then
      return 0, digits
   end

   return i - first_numerator_index[num] + 1, digits
end   -- sequence_length()

-------------------------------------------------------------------------------
local max = 0
local max_n = 0

for n = 2, LIMIT do
   local seqlen, digits, numerators = sequence_length(n)

--    nt.print_array(digits,
--                   'digits 1/' .. tostring(n) ..
--                      ' skip=' .. tostring(#digits - seqlen) ..
--                      ' len=' .. tostring(seqlen))

   if seqlen > max then
      nt.print_array(digits,
                     'digits 1/' .. tostring(n) ..
                        ' skip=' .. tostring(#digits - seqlen) ..
                        ' len=' .. tostring(seqlen))
      max = seqlen
      max_n = n
   end
end   -- for

print(max, max_n)

