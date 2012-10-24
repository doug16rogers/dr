-- Project Euler Problem 45.

local LIMIT = 1e10

local t_i, t, t_d = 1, 1, 2
local p_i, p, p_d = 1, 1, 4
local h_i, h, h_d = 1, 1, 5

while t < LIMIT do
   while (p < t) do
      p_i = p_i + 1
      p   = p   + p_d
      p_d = p_d + 3
   end

   while (h < t) do
      h_i = h_i + 1
      h   = h   + h_d
      h_d = h_d + 4
   end

   if (t == p) and (t == h) then
      print(t, t_i, p_i, h_i)
   end

   t_i = t_i + 1
   t   = t   + t_d
   t_d = t_d + 1
end

