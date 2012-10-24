-- Project Euler Problem 9.

-- With some analysis this could be done more cleverly, but...

for a = 1, 997 do
   for b = a, 997 do
      local c = 1000 - a - b
      if a^2 + b^2 == c^2 then
         print(a, b, c, a*b*c)
      end
   end
end
