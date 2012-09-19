#!/usr/bin/env /usr/bin/lua

args = { ... }

local CIPHER_WORD = 'edifice'
local WORD_FILE = 'word.lst'

if #args > 0 then
   CIPHER_WORD = args[1]
end

if #args > 1 then
   WORD_FILE = args[2]
end

local printf = function (...) print(string.format(...)) end

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

local wlen = #CIPHER_WORD

list, bylen = word_list_read(WORD_FILE)

list = bylen[wlen]

if not list then
   printf("There are no words of length %d in '%s' for '%s'.", wlen, WORD_FILE, CIPHER_WORD)
   return 1
end

function string_matches_substitution_cipher(s, key)
   local mapping = {}

   for i = 1, #s do
      local c = s:byte(i)
      local k = key:byte(i)

      if mapping[k] then
         if mapping[k] ~= c then
            return false
         end
      else
         mapping[k] = c
      end
   end

   -- for i = 1, #s do
   --    print(i, s:byte(i), key:byte(i), mapping[key:byte(i)])
   -- end

   return true
end   -- string_matches_substitution_cipher()

string.matches_substitution_cipher = string_matches_substitution_cipher

for _, word in ipairs(list) do
   if word:matches_substitution_cipher(CIPHER_WORD) then
      print(word)
   end
end

return 0
