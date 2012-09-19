-- Project Euler Problem 38.

-- Take the number 192 and multiply it by each of 1, 2, and 3:

--     192 * 1 = 192
--     192 * 2 = 384
--     192 * 3 = 576

-- By concatenating each product we get the 1 to 9 pandigital, 192384576. We
-- will call 192384576 the concatenated product of 192 and (1,2,3)

-- The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4,
-- and 5, giving the pandigital, 918273645, which is the concatenated product
-- of 9 and (1,2,3,4,5).

-- What is the largest 1 to 9 pandigital 9-digit number that can be formed as
-- the concatenated product of an integer with (1,2, ... , n) where n 1?

-------------------------------------------------------------------------------
-- Notes.

require'nt'

local function digits_are_duplicate(t, n, base)
   base = base or 10
   local orig_n = n

   while n > 0 do
      local d = n % base

      if d == 0 then
         return true
      end

      if t[d] then
         return true
      end

      t[d] = true
      t.count = (t.count or 0) + 1
      n = (n - d) / base
   end   -- while

   t.value = (t.value or '') .. tostring(orig_n)
   return false
end   -- digits_are_duplicate()

-- local is_pandigital = {}

-- print(os.time() % 1000, 'Filling pandigital container...')

-- for p in nt.permutations(9) do
--    local s = table.concat(p)
--    is_pandigital[s] = true
--    is_pandigital[tonumber(s)] = true
-- end   -- for each permutation

-- print(os.time() % 1000, 'Finding...')

local max = 0

for n = 1, math.sqrt(987654321) do
   local digs = {}

   if not digits_are_duplicate(digs, n) then
      for k = 2, 11 do
         if digits_are_duplicate(digs, n * k) then
            break
         end
         
         if digs.count == 9 then
            local val = tonumber(digs.value)

            print(n, k, val)

            if val > max then
               max = val
            end

            break
         end
      end   -- for k = 2...
   end   -- if works for k == 1
end   -- for n

print(max)
-- 932718654
