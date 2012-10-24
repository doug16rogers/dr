-- Project Euler Problem 22.

local f = assert(io.open('projecteuler.net/project/names.txt', 'rt'))
local names = loadstring('return { ' .. f:read('*a') .. '}')()
f:close()
table.sort(names)

-------------------------------------------------------------------------------
function score(s)
   local sum = 0
   for i = 1, #s do
      sum = sum + s:byte(i) - 64
   end
   return sum
end   -- score()

print(938, names[938], score(names[938]))

local sum = 0
for i, name in ipairs(names) do
   sum = sum + (i * score(names[i]))
end

print(sum)
-- 938	COLIN	53
-- 871198282
