-- Project Euler Problem 4.

local p
local s
local max = 0

for n = 999, 800, -1 do
   for m = n, 700, -1 do
      p = m * n
      s = tostring(p)
      r = s:reverse()
      if s == r then
         if p > max then
            print(p)
            max = p
         end
      end
   end
end

