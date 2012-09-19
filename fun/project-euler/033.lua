-- Project Euler Problem 33

local floor = math.floor
local num_prod = 1
local den_prod = 1

local function div(a, b) return floor(a / b) end

for n1 = 1, 9 do
   for n0 = 1, 9 do
      local num = n1 * 10 + n0
      for d1 = 1, 9 do
         for d0 = 1, 9 do
            local den = d1 * 10 + d0
            if (num < den) and
               (n0 ~= n1) and (d0 ~= d1) and
               (n0 ~= 0) and (n1 ~= 0) and
               (d0 ~= 0) and (d1 ~= 0) then
               if (n0 == d0) and ((num * d1) == (den * n1)) then
                  print(num, den)
               elseif (n0 == d1) and ((num * d0) == (den * n1)) then
                  print(num, den)
               elseif (n1 == d0) and ((num * d1) == (den * n0)) then
                  print(num, den)
               elseif (n1 == d1) and ((num * d0) == (den * n0)) then
                  print(num, den)
               end
            end
         end
      end
   end
end

-- 16	64 -> 1/4
-- 19	95 -> 1/5
-- 26	65 -> 2/5
-- 49	98 -> 1/2
--         -> 1/100
-- 4*5*5 = 100

