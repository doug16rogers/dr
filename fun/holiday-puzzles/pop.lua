#! /usr/bin/env lua
-- $Id: pop.lua 116 2009-11-27 18:47:13Z rogers $

-- Lua code to make acrostics easier.

-- Set this to use the first and last letters of each word for the author and
-- title. Otherwise just the first letters are used.
-- double_list = true

full_quote = [[
Possessing a clear and penetrating mind, a strong and sound
judgment, calmness and temper for deliberation, with invincible firmness and
perseverance in resolutions maturely formed; drawing information from all;
acting from himself, with incorruptible integrity and unvarying patriotism;
his own superiority ... alike marked him as the man
designed by Heaven...
]]
-- ... and the public confidence ...
-- ... to lead in the great political as well as military events
-- which have distinguished the era of his life.

double_list = false
full_author = "Henry Lee"
full_title = "Eulogy for George Washington"
matches_to_show = "" -- initial letters to be checked in word list

words = { --
   "HINT",
   "EAR PIERCING",
   "NICE",
   "ROGERS",
   "YIP",

   "LIFE",
   "ENLISTED",
   "EGG FU YUNG",

   "E",
   "U.S. MARINE CORPS",
   "LIGHT HORSE",
   "OIL OF OLAY",
   "GRANDFATHER",
   "YOUTH",

   "FALLEN AND I CAN'T GET UP",
   "OVERSEAS",
   "ROBERT PETER",

   "GIVING",
   "ELASTICITY",
   "OFF",
   "R",
   "GIVE IN",
   "END ",

   "WATERY",
   "AVIATION",
   "SOUTH CAROLINA",
   "HISTORICAL",
   "INITIATION",
   "N",
   "GUESS",
   "TEACHER",
   "ORANGE",
   "NEWMAN",

--   "HONOLULU",
--   "EVENTUAL",
--   "NAVAJO",
--   "RUSTLING",
--   "YESTERDAY",
--   "LITTLE TEA LEAF",
--   "IO",
--   "GR",
--   "HG",
--   "TE",
--   "HO",
--   "OR",
--   "RG",
--   "SE",
--   "EW",
--   "HA",
--   "AS",
--   "RH",
--   "RI",
--   "YN",
--   "LG",
--   "ET",
--   "EO",
--   "EN",
}

local toupper_table = {}
for i = 96, 96 + 26 - 1 do
   toupper_table[string.char(i)] = string.char(i - 32)
end

function string:upper()
   return (self:gsub('(%a)', toupper_table))
end

author = full_author:upper():gsub('(%A)', '')
quote  = full_quote:upper():gsub('(%A)', '')
title  = full_title:upper():gsub('(%A)', '')

function string:lettercount()
   local count = {}
   self:gsub('(%a)', function (c)
                        if not count[c] then
                           count[#count + 1] = c
                           count[c] = 1
                        else
                           count[c] = count[c] + 1
                        end
                     end)
   return count
end   -- string:lettercount()

count = quote:lettercount()

atitle = author .. title

print("Quote: ", quote)
print("Author+title: ", atitle)
print("quote length:",  #quote)
print("author+title length:", #atitle)

if double_list and #atitle % 2 ~= 0 then
   print("Author + title must be even number of letters.")
   os.exit(1)
end

local nwords

if double_list then
   nwords = (#atitle) / 2
else
   nwords = #atitle
end

-- Uncomment this to print the first and last letters to get started:
for i = 1, nwords do
   if double_list then
--      print(i, atitle:sub(i,i) .. "____________" ..
--         atitle:sub(nwords + i, nwords + i))
      print('  "' ..
            atitle:sub(i,i) .. "____________" ..
               atitle:sub(nwords + i, nwords + i) ..
            '",')
   else
--      print(i .. '.', atitle:sub(i,i) .. "____________")
      print('  "' .. atitle:sub(i,i) .. '",')
   end   -- if
end   -- for

if #words ~= nwords then
   print(("#words %d ~= %d nwords; letters/#words=%3.1f, letters/nwords=%3.1f"):
         format(#words, nwords, #quote / #words, #quote / nwords))
   os.exit(1)
end

for i, word in ipairs(words) do
   print(("%2d. %s"):format(i, word))
end

print()

local bag = quote

for i, word in ipairs(words) do
   for j = 1, #word do
      local c = word:sub(j,j)
      if c:find('%a') then
         if not count[c] then
            print(("%d. %s[%d], '%s', is not in quote."):format(
                  i, word, j, c))
         else
            if count[c] == 0 then
               print(("%d. %s[%d], no more '%s' in quote."):format(
                     i, word, j, c))
            else
               count[c] = count[c] - 1
            end
         end   -- if
      end   -- if this is a letter
   end   -- for
end   -- for

io.write("\nLetters left: ")

local left = 0

for i = 1, 26 do
   local c = string.char(64+i)
   if count[c] and (count[c] > 0) then
      io.write(c .. '=' .. count[c] .. ' ')
      left = left + count[c]
   end
end

io.write('\n')
print('Letters left to place: ' .. left)

-- @a count should be a table indexed by uppercase characters of an integer
-- count of each.
function print_words_matching(count, show)
   f=assert(io.open("../itasoftware-puzzles/word.lst", "rt"))

   local match_first = (type(show) == 'string')
   local nn = 0
   local nn_limit = 1e6
   local len = 0
   local max_len = 80
   local min_len = 4

   for line in f:lines() do
      if #line >= min_len then
         local LINE = line:upper()

         if not match_first or (show:find(LINE:sub(1,1))) then
            local match = true
            local cnt = LINE:lettercount()
            for _, c in ipairs(cnt, c) do
               if cnt[c] > (count[c] or 0) then
                  match = false
                  break
               end
            end

            if match then
               if len + #line + 1 >= max_len then
                  io.write('\n')
                  len = 0
               end

               io.write(line:upper() .. ' ')
               len = len + #line + 1

               nn = nn + 1
               if nn >= nn_limit then
                  break
               end
            end
         end   -- if the word either matches a first letter or doesn't need to
      end   -- if the word is at least the minimum length
   end   -- for each line in the word file

   if len > 0 then
      io.write('\n')
   end

   f:close()
end   -- print_words_matching()

if matches_to_show and #matches_to_show > 0 then
   print()
   print_words_matching(count, matches_to_show)
end

print(("letters/#words=%3.1f, letters/nwords=%3.1f"):
         format(#quote / #words, #quote / nwords))
