-- Project Euler Problem 15.

-- Looking at the diagonals of the intersections from lower left to upper
-- right reveals that the number of routes from each vertex to the bottom
-- right is the sum of the number below and to the right of it. Since the
-- initial numbers along the bottom and right are all ones, this yields
-- a Pascal's triangle.

-- The total for the upper left in an NxN square grid is the center term of
-- row 2N of Pascal's triangle. This is just the binomial coefficient, C(2N,
-- N). For the 20x20 grid the number is C(40,20).

-- I once worked out a closed formula for C(2N,N). I'm gonna go find it in
-- my notebook. Well, it's not quite closed, but it's:

--           C(2N,N) = Sum(n,0,N) C(N,n)^2

-- That should be easier to calculate.

function choose(n,k)
   if (k + k) == n then
      local sum = 0

      for i = 0, k do
         sum = sum + choose(k, i)^2
      end

      return sum
   end
      
   if k < (n / 2) then
      k = n - k
   end

   local prod = 1

   for i = k+1, n do
      prod = prod * i
   end

   for i = 2, n - k do
      prod = prod / i
   end

   return prod
end   -- choose()

-- for n = 3, 5 do
--    for k = 0, n do
--       print(n, k, choose(n, k))
--    end
-- end

local N = 20

print(2*N, N, choose(2*N, N))
-- 40	20	137846528820
