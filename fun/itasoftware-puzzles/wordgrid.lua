args = { ... }

local WORD_FILE = 'word.lst'

if #args > 0 then
   WORD_FILE = args[1]
end

function word_list_read(filename)
   local f = assert(io.open(filename, 'r'))
   local list = {}
   local list_by_len = {}

   for word in f:lines() do
      list[#list + 1] = word

      local len = word:len()

      if not list_by_len[len] then
         list_by_len[len] = {}
      end

      local by_len = list_by_len[len]
      by_len[#by_len + 1] = word
   end

   f:close()
   return list, list_by_len
end   -- word_list_read()

function array_copy(a)
   local c = {}
   for i, v in ipairs(a) do c[i] = v end
   return c
end   -- array_copy()

local alphabet = {}
local alphabet_text = "abcdefghijklmnopqrstuvwxyz"
for i = 1, #alphabet_text do
   local c = alphabet_text:sub(i, i)
   alphabet[i] = c
   alphabet[c] = i
end

function word_list_letter_frequency(wordlist)
   local freq = {}
   local total = 0

   for i, word in ipairs(wordlist) do
      for j = 1, #word do
         local c = word:sub(j, j)
         freq[c] = (freq[c] or 0) + 1
         total = total + 1
      end
   end

   return freq, total
end   -- word_list_letter_frequencty()

function word_list_length_counts(wordlist)
   local count = {}
   local max_len = 0

   for i, word in ipairs(wordlist) do
      local len = word:len()

      if len > max_len then
         max_len = len
      end

      count[len] = (count[len] or 0) + 1
   end

   return count, max_len
end   -- word_list_length_counts()

function word_list_find_grid(len, list)
   if not list then
      return nil, ('no word of length %u'):format(len)
   end

-- This assertion is not correct since it may be possible for words with
-- multiple letters to be used multiple times.
--    if #list < (2 * len) then
--       return nil, ('not enough words (%u < %u) of length %u'):format(
--                      #list, 2 * len, len)
--    end

   local grid = {}
   for i = 1, len do
      grid[i] = list[((i - 1 + len) % #list) + 1]
   end

   return grid
end   -- word_list_find_grid(len, list)

function grid_print(grid)
   print(("Grid of length %u:"):format(#grid))
   for i, word in ipairs(grid) do
      print(word)
   end
end   -- grid_print()

list,bylen = word_list_read(WORD_FILE)
freq,ftot = word_list_letter_frequency(list)
leng,lmax = word_list_length_counts(list)

print('Letter  Count   % Freq')
for i, c in ipairs(alphabet) do  -- Otherwise not in alphabetical order.
   local n = freq[c]
   print(("%4s   %6u  %7.4f"):format(c, n, 100 * n / ftot))
end

print("Length  Words")
for i,n in pairs(leng) do
   print(("%4u   %6u"):format(i, n))
end

for len = lmax, 1, -1 do
   local g, err = word_list_find_grid(len, bylen[len])

   if not g then
      print(err)
   else
      grid_print(g)
   end
end
