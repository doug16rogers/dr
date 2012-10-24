-- Project Euler Problem 28.

-- Starting with the number 1 and moving to the right in a clockwise
-- direction a 5 by 5 spiral is formed as follows:

--             43 44 45 46 47 48 49
--             42 21 22 23 24 25 26
--             41 20  7  8  9 10 27
--             40 19  6  1  2 11 28
--             39 18  5  4  3 12 29
--             38 17 16 15 14 13 30
--             37 36 35 34 33 32 31

-- It can be verified that the sum of both diagonals is 101.

-- What is the sum of both diagonals in a 1001 by 1001 spiral formed in the
-- same way?

--   1:    1  + (2 * ( 3 +  9 =  12) ...     12 + 26   26 + 16
--   3:   25  + (2 * (13 + 25 =  38) ...     38 + 42   42 + 16
--   5:  101  + (2 * (31 + 49 =  80) ...     80 + 58   58 + 16
--   7:  261  + (2 * (57 + 81 = 138) ..     138 + 74   74 + 16 ...
--
--  1:   1   +  24    + 52   + 32
--  3:  25   +  76    + 84
--  5: 101   + 160
--  7: 261
--
--  1
--  1+2,  1+2*2,  1+3*2,  1+4*2
--  9+4,  9+2*4,  9+3*4,  9+4*4
-- 25+6, 25+2*6, 25+3*6, 25+4*6
--  ...
--  a[1] = 1
--  a[n+2] = a[n] + (4 * n^2) + (10 * n)
--  n = n + 1

local LIMIT = 1001
local a = 1
local d = 1
local n = 1

while true do
   -- print(n, a)
   if n >= LIMIT then break end
   a = a + (4 * n^2) + (10 * (n + 1))
   n = n + 2
end

print(n, a)
-- 1001	669171001
