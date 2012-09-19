-- Project Euler Problem 2.

local sum = 0
local last = 0

function fibnext(n, limit, f_1, f_2)
   if (f_1 >= limit) then
      return f_1
   end

   f_1, f_2 = f_1 + f_2, f_1

   if (f_1 % 2) == 0 then
      sum = sum + f_1
      print(n, f_1, "diff", f_1 - last, "sum", sum)
      last = f_1
   end

   return fibnext(n+1, limit, f_1, f_2)
end

function fibmax(limit)
   return fibnext(1, limit, 1, 0)
end

fibmax(4e6)
