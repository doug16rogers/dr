-- Project Euler Problem 18.

-- The text of the problem warns that, while it is possible to brute force
-- the answer, problem 67 is the same but with 100 rows. But is it worth
-- spending the time to figure out a more clever method now?

local triangle = {
   { 75, },
   { 95, 64, },
   { 17, 47, 82, },
   { 18, 35, 87, 10, },
   { 20, 04, 82, 47, 65, },
   { 19, 01, 23, 75, 03, 34, },
   { 88, 02, 77, 73, 07, 63, 67, },
   { 99, 65, 04, 28, 06, 16, 70, 92, },
   { 41, 41, 26, 56, 83, 40, 80, 70, 33, },
   { 41, 48, 72, 33, 47, 32, 37, 16, 94, 29, },
   { 53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14, },
   { 70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57, },
   { 91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48, },
   { 63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31, },
   { 04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23 },
}

-- Here's the brute force method:

function max(a, b)
   if a > b then
      return a
   end

   return b
end   -- max()

function maxpath(tri, row, col)
   if row > #tri then
      return 0
   end

   return max(tri[row][col] + maxpath(tri, row+1, col+0),
              tri[row][col] + maxpath(tri, row+1, col+1))
end   -- maxpath()

print(maxpath(triangle, 1, 1))
-- 1074

-- That was the brute force method. This one should have better behavior:

function printf(fmt, ...)
   io.write(fmt:format(...))
end

function aprint(a)
   for i, v in ipairs(a) do io.write(printf('%4u ', v)) end
   io.write('\n')
end

function copy(a, len)
   local c = {}
   len = len or #a
   for i = 1, len do c[i] = a[i] end
   return c
end   -- copy()

local last = #triangle
local best = copy(triangle[last])

-- aprint(best)

for i = last - 1, 1, -1 do
   local row = triangle[i]
   local new_best = copy(best, #row)
   for j = 1, #row do
      if best[j] > best[j+1] then
         new_best[j] = row[j] + best[j]
      else
         new_best[j] = row[j] + best[j+1]
      end
   end
   best = new_best
   -- aprint(best)
end

print(best[1])
-- 1074

