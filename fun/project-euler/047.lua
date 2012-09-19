-- Project Euler Problem 47.

local N = 4     -- Number of factors in a row to find.

-------------------------------------------------------------------------------
-- Returns a list of factors and list of the count of each.
-- So factors(360) would return { 2, 3, 5 }, { 3, 2, 1 } since
-- 360 = 2^3 * 3^2 * 5^1.

function factors(n)
   local prime = {}
   local count = {}

   local function addprime(p)
      if p == prime[#prime] then
         count[#prime] = count[#prime] + 1
      else
         prime[#prime+1] = p
         count[#prime]   = 1
      end
   end

   if n == 0 then
      return {}, {}
   end

   if n == 1 then
      addprime(1)
      return prime, count
   end

   if n < 0 then
      n = -n
      addprime(-1)

      if n == 1 then
         return prime, count   -- negative 1.
      end
   end

   local p = 2

   while (n % p) == 0 do
      addprime(p)
      n = n / p
   end

   p = 3

   -- Cannot check for p*p > n because n is reduced.
   -- But need to find way for p*p > n check when not reduced along the way.

   while (p * p) <= n do
      while (n % p) == 0 do
         addprime(p)
         n = n / p
      end

      p = p + 2
   end

   if n > 1 then
      addprime(n)
   end

   return prime, count
end   -- factors()

-------------------------------------------------------------------------------
function factortext(n)
   local f, e = factors(n)
   local t = {}

   for i = 1, #f do
      local s = tostring(f[i])

      if e[i] ~= 1 then
         s = s .. '^' .. tostring(e[i])
      end

      t[#t+1] = s
   end

   return table.concat(t, ' x ')
end   -- factortext()

-- for i = 350, 370 do
--    print(i, factortext(i))
-- end

local n = 0   -- Could short-cut this with N.
local consecutive = 0

while consecutive < N do
   n = n + 1

   if #(factors(n)) == N then
      consecutive = consecutive + 1
   else
      consecutive = 0
   end
end

for i = n - N + 1, n do
   print(i, factortext(i))
end
