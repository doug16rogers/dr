#! /usr/bin/env lua
-- $Id: 20091126-acrostic.lua 128 2009-12-24 23:05:50Z rogers $

-- Lua code to make acrostics easier.

local F = string.format

-- Set this to use the first and last letters of each word for the author and
-- title. Otherwise just the first letters are used.
-- double_list = true

double_list = false
full_author = "Whittier"    -- John Greenleaf Whittier
full_title = "The Pumpkin"

full_quote = [[
Ah! on Thanksgiving day....
When the care-wearied man seeks his mother once more,
And the worn matron smiles where the girl smiled before.
What moistens the lips and what brightens the eye?
What calls back the past, like the rich pumpkin pie?
]]

lwords = {
   "Whitewash",    -- 1.
   "Home plate",    -- 2.
   "In the know",    -- 3.
   "Thighs and Legs",    -- 4.
   "Twitch",    -- 5.
   "Inhaler",    -- 6.
   "Embassy",    -- 7.
   "Remember the Alamo",    -- 8.
   "Terrible twos",    -- 9.
   "High heels",    -- 10.
   "Emphasis",    -- 11.
   "Patched",    -- 12.
   "Unwed",    -- 13.
   "Meet the Parents",    -- 14.
   "President",    -- 15.
   "Knock-knock Who's there",
   "Imaginary friend",    -- 17.
   "National Archives",    -- 18.
}
matches_to_show = "p" -- initial letters to be checked in word list

-- Convert words to upper case.
words = {}
for i, s in ipairs(lwords) do words[i] = s:upper() end

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
function print_first_letters()
   for i = 1, nwords do
      if double_list then
         print(F('   "%s",', atitle:sub(i,i) .. "____________" ..
                 atitle:sub(nwords + i, nwords + i)))
      else
         print(F('   "%s",', atitle:sub(i,i) .. "____________"))
      end   -- if
   end   -- for
end   -- print_first_letters()

-- print_first_letters()

if #words ~= nwords then
   print(("#words %d ~= %d nwords"):format(#words, nwords))
   os.exit(1)
end

for i, word in ipairs(words) do
   print(("%2d. %s"):format(i, word))
end

print()

function check_first_letters()
   for i = 1, nwords do
      local word = words[i]
      if double_list then
         local a = atitle:sub(i,i)
         if a ~= word:sub(1,1) then
            print(F('*** %d. %s first letter does not match author/title (%s)',
                    i, word, a))
         end
         a = atitle:sub(nwords+i,nwords+i)
         if a ~= word:sub(-1) then
            print(F('*** %d. %s last letter does not match author/title (%s)',
                    i, word, a))
         end
      else
         local a = atitle:sub(i,i)
         if a ~= word:sub(1,1) then
            print(F('*** %d. %s first letter does not match author/title (%s)',
                    i, word, a))
         end
      end   -- if
   end   -- for
end   -- check_first_letters()

check_first_letters()

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
   matches_to_show = matches_to_show:upper()
   print()
   print_words_matching(count, matches_to_show)
end
