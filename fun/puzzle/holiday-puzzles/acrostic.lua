#! /usr/bin/env lua
-- $Id: acrostic.lua 88 2008-11-27 00:50:37Z rogers $

-- Lua code to make acrostics easier.

-- Set this to use the first and last letters of each word for the author and
-- title. Otherwise just the first letters are used.
double_list = true

-- Found this at a better site. Notice the extra line at the end. Hmmm.
-- full_quote = [[
-- Turkey: n. A large bird whose flesh when eaten on certain religious
-- anniversaries has the peculiar property of attesting piety and gratitude.
-- Incidentally, it is pretty good eating.
-- ]]

full_quote = [[
Turkey: n. A large bird whose flesh when eaten on certain religious
anniversaries has the peculiar property of attesting piety and gratitude.
]]

-- double_list = true
-- full_author = "Bierce"
-- full_title = "Devil's Dictionary"
-- words = {
--    "BY THE WATERS",
--    "INFRA-RED",
--    "EPISODE I",
--    "REALISTIC",
--    "CRAWL OUT OF THE PIT",
--    "EPI",
--    "DRAINO",
--    "EIGHTEEN",
--    "VOLGA",
--    "INTERPRETER",
--    "LATE ENTRY"
-- }

double_list = false
full_author = ""
full_title = "The Devil's Dictionary"
matches_to_show = "" -- initial letters to be checked in word list

-- Works:
-- words = {
--    "THANK",          -- What we do before dinner.
--    "HORN OF PLENTY", -- Cornucopia.
--    "EAT PIE",        -- What we do after dinner.
--    "DIETERS",        -- What we become next?
--    "EGG",            -- Essence of quiche.
--    "VEGETABLES",     -- Herbivore's staple.
--    "ICING",          -- Cake wrapper, of sorts.
--    "LAWYER",         -- Attorney.
--    "SOUPIER",        -- More bisquish.
--    "DISH",           -- China; meal.
--    "INFEST",         -- Fill with bugs.
--    "CANTALOUPE",     -- Replacement item for the honeydew list?
--    "THREATEN",       -- Rattle one's saber.
--    "IRIS",           -- Band of eye color.
--    "OUTER",          -- NC Banks adjective.
--    "NARRATE",        -- Tell tales.
--    "ARID",           -- Desert description.
--    "RUSH",           -- Kind of delivery.
--    "YAWN",           -- What we do as the evening expires.
-- }

words = { --
   "THANKS",
   "HORN OF PLENTY",
   "EAT PIE",
   "DRIFT",
   "EDGE",
   "VEGETABLE",
   "ICING",
   "LAYER",
   "SOUPIER",
   "DISH WATER",
   "INGEST",
   "CANTALOUPE",
   "TREAT",
   "IRISH",
   "OUTER",
   "NEAR",
   "ARISEN",
   "RUSH",
   "YAWN",
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
-- for i = 1, nwords do
--    if double_list then
--       print(i .. '.',
--             atitle:sub(i,i) .. "____________" ..
--                atitle:sub(nwords + i, nwords + i))
--    else
--       print(i .. '.', atitle:sub(i,i) .. "____________")
--    end   -- if
-- end   -- for

if #words ~= nwords then
   print(("#words %d ~= %d nwords"):format(#words, nwords))
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
