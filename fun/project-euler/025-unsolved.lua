-- Project Euler Problem 25

-- If a = (1+sqrt(5))/2, then
-- Fn ~~ ((1+sqrt(5))/2) ^ n. So we want to raise

local ap = (1 + math.sqrt(5)) / 2
local am = (1 - math.sqrt(5)) / 2

local bp = 1
local bm = 1

for n = 1, 12 do
   bp = bp * ap
   bm = bm * am
   print(n, bp + bm)
end
