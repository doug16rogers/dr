-- Project Euler Problem 17.

-- I could try to figure out which words occur how many times - like the word
-- "one" appears 100 times at the end of a number and 100 times for each of
-- the numbers from 100-199. Then the word "and" occurs 890 times. The word
-- "hundred" occurs 900 times.

-- But it's probably easier just to write a program that produces the numbers
-- in word form.

local floor = math.floor

local word = {
   [1] = 'one', [2] = 'two',   [3] = 'three', [4] = 'four', [5] = 'five',
   [6] = 'six', [7] = 'seven', [8] = 'eight', [9] = 'nine', [10] = 'ten',
   [11] = 'eleven', [12] = 'twelve', [13] = 'thirteen', [14] = 'fourteen',
   [15] = 'fifteen', [16] = 'sixteen', [17] = 'seventeen', [18] = 'eighteen',
   [19] = 'nineteen',
   [20] = 'twenty', [30] = 'thirty', [40] = 'forty', [50] = 'fifty',
   [60] = 'sixty', [70] = 'seventy', [80] = 'eighty', [90] = 'ninety',
   [100] = 'hundred',
   [1000] = 'thousand'
}

local numbers = {}

function number_word(n)
   if n ~= 100 and word[n] then
      return word[n]
   end

   local t = {}

   if n >= 100 then
      t[#t+1] = word[floor(n / 100)]
      t[#t+1] = word[100]
      n = n % 100
      if n == 0 then
         return table.concat(t)
      end
      t[#t+1] = 'and'
   end

   if n < 20 then
      t[#t+1] = word[n]
   else
      t[#t+1] = word[n - (n % 10)]
      n = n % 10
      if n > 0 then
         t[#t+1] = word[n]
      end
   end

   return table.concat(t)
end   -- number_word()

local sum = 0

for i = 1, 999 do
   numbers[i] = number_word(i)
   sum = sum + #numbers[i]
   print(i, #numbers[i], sum, numbers[i])
end

local i = 1000
numbers[i] = 'onethousand'
sum = sum + #numbers[i]
print(i, #numbers[i], sum, numbers[i])

print('letters:', #(table.concat(numbers)))

   
