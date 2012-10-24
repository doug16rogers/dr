-- Project Euler Problem 6.

-- The sum of i   from 1 to n is n * (n + 1) / 2, squared is n^2 * (n+1)^2 / 4
-- The sum of i^2 from 1 to n is n * (n + 1) * (2n + 1) / 6 = 50*101*(201/3).
-- The difference is then:
--    n*n*(n+1)*(n+1)*3/12 - n*(n+1)*(2n+1)*2/12
--        = [n*(n+1)/12] * [3*n*(n+1) - 2*(2n+1)]
--        = [n*(n+1)/12] * [3n^2 + 3n - 4n - 2]
--        = [n*(n+1)/12] * [3n^2 - n - 2]
--        = 100 * 101 * [30000 - 100 - 2] / 12
--        = 10100 * 29898 / 12
--        =  5050 *  4983
--        =  1010 * 24915
--                24915
--        =       25164150
-- All by hand, but just to check:

local sum = 0
local sumsqr = 0
for i = 1, 100 do
   sum = sum + i
   sumsqr = sumsqr + (i*i)
   print(i, sum*sum, sumsqr, sum*sum - sumsqr)
end

   

