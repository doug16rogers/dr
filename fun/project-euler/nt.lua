-- $Id: nt.lua 71 2008-10-24 10:13:33Z rogers $

-- Number theory services. Mostly related to primes and factorization.

local ipairs       = ipairs
local math_ceil    = math.ceil
local math_floor   = math.floor
local math_sqrt    = math.sqrt
local pairs        = pairs
local print        = print
local string_reverse = string.reverse
local table_concat = table.concat
local table_remove = table.remove
local table_sort   = table.sort
local tonumber     = tonumber
local tostring     = tostring
local type         = type

module(...)

-------------------------------------------------------------------------------
function divmod(k, n)   -- return floor(k / n), k % n
   local m = k % n
   return (k - m) / n, m
end   -- divmod()

-------------------------------------------------------------------------------
function memoize_1(f)   -- If f(x) returns nil then memoization is moot.
   local value = {}
   local fm = function (x)
                 if value[x] ~= nil then
                    return value[x]
                 else
                    value[x] = f(x)
                    return value[x]
                 end
              end
   return fm
end   -- memoize_1()

-------------------------------------------------------------------------------
function sum(a)
   local sum = 0
   for _, v in ipairs(a) do
      sum = sum + v
   end
   return sum
end   -- sum()

-------------------------------------------------------------------------------
function product(a)
   local product = 1
   for _, v in ipairs(a) do
      product = product * v
   end
   return product
end   -- product()

-------------------------------------------------------------------------------
function factorial_raw(n)
   if n <= 1 then
      return 1
   end

   return n * factorial(n - 1)
end   -- factorial_raw()

factorial = memoize_1(factorial_raw)

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
function array_to_string(a)
   return '{' .. table_concat(a, ',') .. '}'
end   -- array_to_string()

-------------------------------------------------------------------------------
function print_array(a, name)
   if name then
      name = name .. ' = '
   else
      name = ''
   end

   print(name .. array_to_string(a))
end   -- print_array()

-------------------------------------------------------------------------------
function cross_product(s1, s2)
   local cross = {}
   for _, v1 in ipairs(s1) do
      for _, v2 in ipairs(s2) do
         cross[#cross+1] = v1 * v2
      end
   end
   return cross
end   -- cross_product()

-------------------------------------------------------------------------------
function create_all_factors(factors, counts, index)
   if index > #counts then
      return { 1 }
   end

   local p = factors[index]
   local p_n = { 1 }

   for i = 1, counts[index] do
      p_n[#p_n+1] = p * p_n[#p_n]
   end

   return cross_product(p_n, create_all_factors(factors, counts, index+1))
end   -- create_all_factors()

-------------------------------------------------------------------------------
function all_factors(n)
   local negative = false

   if n == 0 then
      return {}
   elseif n == 1 then
      return { 1 }
   elseif n == -1 then
      return { -1 }
   elseif n < 0 then
      negative = true
      n = -n
   end

   local fac, exp = factors(n)
   -- print_array(fac, 'fac')
   -- print_array(exp, 'exp')
   local result = create_all_factors(fac, exp, 1)
   table_sort(result)
   if negative then
      result[1] = -1
   end
   return result
end   -- all_factors()

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

   return table_concat(t, ' x ')
end   -- factortext()

-------------------------------------------------------------------------------
function is_perfect_raw(n)
   return sum(all_factors(n)) == 2 * n
end   -- is_perfect_raw()

is_perfect = memoize_1(is_perfect_raw)

-------------------------------------------------------------------------------
function is_abundant_raw(n)
   return sum(all_factors(n)) > 2 * n
end   -- is_abundant_raw()

is_abundant = memoize_1(is_abundant_raw)

-------------------------------------------------------------------------------
function is_deficient_raw(n)
   return sum(all_factors(n)) < 2 * n
end   -- is_deficient_raw()

is_deficient = memoize_1(is_deficient_raw)

-------------------------------------------------------------------------------
function primes_by_sieve(limit)
   local primes = { 2 }
   local is_prime = { [2] = true }

   for n = 3, limit, 2 do
      is_prime[n] = true
   end

   for n = 3, limit, 2 do
      if is_prime[n] then
         for j = n + n, limit, n do
            is_prime[j] = nil
         end

         primes[#primes + 1] = n
      end
   end   -- while incrementing through sieve

   return primes, is_prime
end   -- primes_by_sieve()

-------------------------------------------------------------------------------
-- This is slower than the sieve.
function primes_by_mod(limit)
   local primes = { 2 }
   local is_prime = { [2] = true }

   for n = 3, limit, 2 do
      local limit = math_floor(math_sqrt(n))

      for _, p in ipairs(primes) do
         if n % p == 0 then
            break
         end

         if p >= limit then
            is_prime[n] = true
            primes[#primes + 1] = n
            break
         end
      end   -- for each prime already collected, check for divisibility.
   end   -- for each n

   return primes, is_prime
end

-------------------------------------------------------------------------------
-- The sieve was more than twice as fast.
primes = primes_by_sieve

-------------------------------------------------------------------------------
-- Returns the kth permutation of n numbers (from 1 to n).
--
function permutation(n, k)
   if (n < 0) or (k <= 0) then
      return nil, "invalid argument: n must be >= 0, k must be >= 1"
   end

   local digits_in  = {}
   local digits_out = {}

   for i = 1, n do digits_in[i] = i end

   if n <= 1 then
      return digits_in
   end

   k = (k - 1) % factorial(n)  -- Switch to 0-based

   for i = n - 1, 1, -1 do
      local fact = factorial(i)
      local next_k = k % fact
      local index = ((k - next_k) / fact) + 1
      digits_out[#digits_out+1] = digits_in[index]
      table_remove(digits_in, index)
      k = next_k
   end

   if #digits_in > 0 then
      digits_out[#digits_out+1] = digits_in[1]
   end

   return digits_out
end   -- permutation()

-------------------------------------------------------------------------------
-- Returns an iterator through the permutations of n.
--  Note: This could be optimized; it's too slow.
function permutations(n)
   local k = 0
   local last = factorial(n)
   return function ()
             if k == last then
                return nil
             else
                k = k + 1
                return permutation(n, k)
             end
          end
end   -- permutations()

-------------------------------------------------------------------------------
-- Returns an iterator through the permutations of n.
--  Note: This could be optimized; it's too slow.
function reverse_permutations(n)
   local k = factorial(n) - 1
   return function ()
             if k == 0 then
                return nil
             else
                k = k - 1
                return permutation(n, k)
             end
          end
end   -- reverse_permutations()

-------------------------------------------------------------------------------
function is_palindrome(s)
   if type(s) ~= 'string' then
      return false
   end

   local len = #s

   for i = 1, len / 2 do
      if s:byte(i) ~= s:byte(len - i + 1) then
         return false
      end
   end
   return true
end   -- is_palindrome()

-------------------------------------------------------------------------------
-- Return a string representation of n in base b.
--
local base_digits = {
   [0] = '0',
   '1', '2', '3', '4', '5', '6', '7', '8', '9',
   'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
   'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
}
function tobase(n, b)
   local minus = ''
   local digits = {}

   if (b < 2) or (b > 36) then
      error('invalid base for conversion')
   end

   if n == 0 then
      return '0'
   end

   if n < 0 then
      n = -n
      minus = '-'
   end

   while n > 0 do
      local d = n % b
      digits[#digits+1] = base_digits[d]
      n = (n - d) / b
   end

   return minus .. string_reverse(table_concat(digits))
end   -- tobase()

-------------------------------------------------------------------------------
function pythagorean_triples()
   local k = 0
   return function ()
             if k >= 2 then
                return nil
             end

             k = k + 1

             if k == 1 then
                return { 3, 4, 5 }
             else
                return { 5, 12, 13 }
             end
          end
end   -- pythagorean_triples()
