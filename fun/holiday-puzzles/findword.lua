#!/usr/bin/env lua
-- Finds a word in word.lst with the same letters as the one given on the
-- command line.

-- args = { ... }

-- if #args == 0 then
--    print('usage: lua findword.lua <letters...>')
--    os.exit(1)
-- end

local string_byte = string.byte
local string_char = string.char
local table_concat = table.concat
local table_sort = table.sort

function sorted_lower(s)
   local t = {}
   for i = 1, #s do
      local b = string_byte(s, i)

      if b >= 0x61 and b <= 0x7a then
         t[#t+1] = string_char(b)
      elseif b >= 0x41 and b <= 0x5A then
         t[#t+1] = string_char(b+0x20)
      end
   end

   table_sort(t)
   return table_concat(t)
end   -- sorted_lower()

function load_words(w)
   w = w or {}
   f=assert(io.open("../itasoftware-puzzles/word.lst", "rt"))

   for line in f:lines() do
      w[#w+1] = line
   end

   f:close()
   return w
end   -- load_words()

function anagram_list(list)
   local a = {}
   for i, word in ipairs(list) do
      local lower = sorted_lower(word)
      local alist = a[lower]
      if alist then
         alist[#alist+1] = word
      else
         a[lower] = { word }
      end
   end

   return a
end   -- anagram_list

word = load_words()
anagrams = anagram_list(words)

function print_anagram(word)
   local lower = sorted_lower(word)
   local list = anagrams[lower]

   if not list then
      print('No anagrams for "' .. word .. '".')
   else
      print('Anagrams for "' .. word .. '":')
      for i, w in ipairs(list) do
         print('  ' .. w)
      end
   end
end   -- print_anagram()



