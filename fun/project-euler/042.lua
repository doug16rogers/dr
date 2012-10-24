-- Project Euler Problem 42

-- Simple in Lua.

local function read_csv_list(filename)
   local f = assert(io.open(filename, 'rt'))
   local list = loadstring('return {' .. f:read('*a') .. '}')()
   f:close()
   return list
end   -- read_csv_list()

local words = read_csv_list('projecteuler.net/project/words.txt')

-- Build some lookup tables:

local is_triangular = { }
local sum = 0

for i = 1, 20 do
   sum = sum + i
   is_triangular[sum] = true
end

local char_val = {}

for i = 1, 26 do char_val[string.char(64+i)] = i end

-- The actual calculation:

local count = 0

for _, word in ipairs(words) do
   local sum = 0
   for i = 1, #word do
      sum = sum + char_val[word:sub(i, i)]
   end

   if is_triangular[sum] then
      -- print(word)
      count = count + 1
   end
end

print(count)
-- 162
