-- Project Euler Problem 19

local normal_first_days = {
   2, 5, 5, 1, 3, 6,
   1, 4, 0, 2, 5, 0
}

local leap_first_days = {
   2, 5, 6, 2, 4, 0,
   2, 5, 1, 3, 6, 1
}

local first = 0
local sundays = 0

for year = 1901, 2000 do
   local first_days = normal_first_days

   if year % 4 == 0 then
      first_days = leap_first_days
   end

   for month = 1, 12 do
      if (first_days[month] + first) % 7 == 0 then
         sundays = sundays + 1
      end
   end   -- for each month

   if year % 4 == 0 then
      first = first + 2
   else
      first = first + 1
   end
end   -- for each year

print(sundays)
