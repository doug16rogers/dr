-- Poor man's bitlib.

local math_floor = math.floor

module('bit')

function band(...)
   local x = { ... }
   local r = 0
   local b = 1

   for i = 0, 31 do
       local all_1 = true

       for j = 1, #x do
          local bit = x[j] % 2
          x[j] = math_floor(x[j] / 2)

          if bit == 0 then
             all_1 = false
          end
       end

       if all_1 then
          r = r + b
       end

       b = 2 * b
   end

   return r
end   -- band()

function bxor(...)
   local x = { ... }
   local r = 0
   local b = 1

   for i = 0, 31 do
       local parity = false

       for j = 1, #x do
          local bit = x[j] % 2
          x[j] = math_floor(x[j] / 2)

          if bit ~= 0 then
             parity = not parity
          end
       end

       if parity then
          r = r + b
       end

       b = 2 * b
   end

   return r
end   -- bxor()

