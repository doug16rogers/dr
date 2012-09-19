-- Project Euler Problem 3.

function factors(n)
   local s = {}

   if n == 0 then
      return {}
   end

   if n == 1 then
      return { 1 }
   end

   if n < 0 then
      n = -n
      s[#s + 1] = -1
   end

   if n == 1 then
      return s
   end

   local p = 2

   while (n % p) == 0 do
      s[#s + 1] = p
      n = n / p
   end

   p = 3

   -- Cannot check for p*p > n because n is reduced.
   -- But need to find way for p*p > n check when not reduced along the way.

   while (p * p) <= n do
      while (n % p) == 0 do
         s[#s + 1] = p
         n = n / p
      end

      p = p + 2
   end

   if n > 1 then
      s[#s + 1] = n
   end

   return s
end

function printfactors(n)
   local f = factors(n)

   io.write(tostring(n) .. ': ')
   for _,v in ipairs(f) do
      io.write(' ' .. tostring(v))
   end
   io.write('\n')
   return f
end

printfactors(600851475143)
