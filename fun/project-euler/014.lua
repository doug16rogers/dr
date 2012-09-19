-- Project Euler Problem 14.

-- This is a classic problem. It might even be in the Lua book but I haven't
-- checked.

local length = { 1 }   -- Length of the chain at n.

-- Pre-seed the powers of two.
for i = 1, 22 do
   length[2^i] = i + 1
end

local n = 3
local best_n = 2^19
local best = 19

local function chain_length(n)
   if n % 2 == 0 then
      n = n/2
   else
      n = 3*n + 1
   end

   if length[n] then
      return 1 + length[n]
   end

   return 1 + chain_length(n)
end

while n < 1e6 do
   if not length[n] then
      length[n] = chain_length(n)

      if length[n] > best then
         best_n = n
         best = length[n]
         print(best_n, best)
      end
   end

   n = n + 1
end   -- while

print(best_n, best)
