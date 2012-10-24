-- Project Euler Problem 40.

local LIMIT = 1e6
local d = {}

local n = 1

while #d <= LIMIT do
   local s = tostring(n)

   for i = 1, #s do
      d[#d + 1] = s:byte(i) - 48
   end

   n = n + 1
end

setmetatable(d, { __index = function(t, k) return 1 end })

local product = 1
local p10 = 1
while p10 <= LIMIT do
   product = product * d[p10]
   p10 = 10 * p10
end

print(product)

