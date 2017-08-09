-- $Id: nt.lua 71 2008-10-24 10:13:33Z rogers $

-- Number theory services. Mostly related to primes and factorization.

local ipairs       = ipairs
local math_ceil    = math.ceil
local math_floor   = math.floor
local math_sqrt    = math.sqrt
local pairs        = pairs
local print        = print
local string_format = string.format
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
-- assert(a >= b)
-- assert(a >= 0)
local function _gcd(a, b)
   local r = a % b
   if r == 0 then
      return b
   else
      return _gcd(b, r)
   end
end   -- _gcd()

-------------------------------------------------------------------------------
function gcd(a, b)
   if a == 0 or b == 0 then
      return 0
   end
   if a < 0 then a = -a end
   if b < 0 then b = -b end
   if a < b then
      return _gcd(b, a)
   else
      return _gcd(a, b)
   end
end   -- gcd()

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
-- Generator for a list of Pythogorean triples.
--    a^2 + b^2 = c^2     a > 0, b > 0, c > 0
--    m > n > 0, gcd(m,n) = 1, k > 0
--    a = k(m^2-n^2), b=k2mn, c=k(m^2+n^2)
--    p = a + b + c = km^2 - kn^2 + 2kmn + km^2 + kn^2 = 2km(m + n)
-- Start with q = 4 and select k, n, and m as partitions of q. This results
-- in a full enumeration but I don't know if it will hit certain triples more
-- than once.
--
function pythagorean_triples_kmn()
   local q = 4
   local k = 1
   local n = 1
   local m = 2
   local function _next_index()
      m = m - 1
      n = n + 1
      if m < n + 1 then
         k = k + 1
         if k + 1 > q - 3 then
            q = q + 1
            k = 1
         end
         n = 1
         m = q - k - n
      end
      if gcd(m, n) ~= 1 then
         return _next_index()
      end
      return true
   end   -- _next_index()
   
   local generator = function ()
      local r = { k * (m*m - n*n), k*2*m*n, k * (m*m + n*n) }
      print(string_format("q=%-3d k=%-3d n=%-3d m=%-3d p=%-4d triple=%s",
                          q, k, n, m, sum(r), array_to_string(r)))
      _next_index()
      return r
   end
   return generator
end   -- pythagorean_triples_kmn()

-------------------------------------------------------------------------------
-- Generator for a list of Pythogorean triples.
--    a^2 + b^2 = c^2     a > 0, b > 0, c > 0
--    m > n > 0, m coprime n, k > 0
--    a = k(m^2-n^2), b=k2mn, c=k(m^2+n^2)
--    p = a + b + c = km^2 - kn^2 + 2kmn + km^2 + kn^2 = 2km(m + n)

-- One idea is to fix p (incrementing it by 2 as needed) and run through the
-- possible values of k, m and n for that perimeter such that gcd(m,n) = 1.
-- Or, for a fixed p, start incrementing q from 1 (or some algebraically
-- determinable minimum value of q for the current p) up to some
-- algebraically determinable maximum value of q for p, accepting only those
-- values of k, m, and n that result in the proper p.
function pythagorean_triples_kmn_by_perimeter()
   local p = 12         -- 2km(m+n)
   local p2 = p / 2     -- p/2 = km(m+n)
   -- Factor p2.
   return generator
end   -- pythagorean_triples_kmn_by_perimeter()

-- ----------------------------------------------------------------------------
-- This is a brute force approach to enumerate Pythagorean triples in order of
-- their perimeter.
--
-- The returned triples will always satisfy a^2 + b^2 = c^2.
-- Triple {a,b,c} will satisfy non-nil @a abc_predicate(a, b, c).
-- Triple {a[n],b[n],c[n]} will satisfy a[n]+b[n]+c[n] >= a[n-1]+b[n-1]+c[n-1].
--
-- @return a, b, c such that a^2 + b^2 = c^2 and abc_predicate(a, b, c)
--
function pythagorean_triples_brute_force_by_perimeter(abc_predicate)
   local p = 12
   local a = 2
   local b = 5          -- Force a 'next' calculation initially so a, b, c are always valid triples.
   local c = p - a - b

   local function _find_next_set()
      b = b + 1
      c = p - a - b
      if c <= b then
         a = a + 1
         b = a
         if 3 * a >= p then
            a = 1
            b = 1
            p = p + 2   -- The perimeter must be even. Easy to see that.
         end
         return _find_next_set()
      end
      if (a * a) + (b * b) ~= (c * c) then
         return _find_next_set()
      end
      if abc_predicate and not abc_predicate(a, b, c) then
         return _find_next_set()
      end
      return true
   end   -- _find_next_set()
   
   local generator = function ()
      _find_next_set()
      return { a, b, c }
   end
   return generator
end   -- pythagorean_triples_brute_force_by_perimeter()

-- ----------------------------------------------------------------------------
-- This is a brute force approach to enumerate Pythagorean triples in order of
-- their hypotenuse.
--
-- The returned triples will always satisfy a^2 + b^2 = c^2.
-- Triple {a,b,c} will satisfy non-nil @a abc_predicate(a, b, c).
-- Triple {a[n],b[n],c[n]} will satisfy c[n] >= c[n-1].
--
-- @return a, b, c such that a^2 + b^2 = c^2 and abc_predicate(a, b, c)
--
function pythagorean_triples_brute_force_by_hypotenuse(abc_predicate)
   local c = 5
   local c2 = c * c
   local a = 3
   local b = 3          -- Force a 'next' calculation initially so a, b, c are always valid triples.

   local function _find_next_set()
      b = b + 1
      local a2b2 = (a * a) + (b * b)
      if a2b2 > c2 then
         a = a + 1
         b = a + 1
         if a + b >= c then
            c = c + 1
            c2 = c * c
            a = 3
            b = 4
         end
         return _find_next_set()
      end
      if a2b2 < c2 then
         return _find_next_set()
      end
      if abc_predicate and not abc_predicate(a, b, c) then
         return _find_next_set()
      end
      return true
   end   -- _find_next_set()
   
   local generator = function ()
      _find_next_set()
      return { a, b, c }
   end
   return generator
end   -- pythagorean_triples_brute_force_by_hypotenuse()

-- ----------------------------------------------------------------------------
function pythagorean_triples(relatively_prime, sort_by)
   local predicate = nil
   if relatively_prime then
      predicate = function (a, b, c) return gcd(a, b) == 1 end
   end
   local sort_by_perimeter = true
   if sort_by then
      local first = (tostring(sort_by)):sub(1, 1):lower()
      if first == 'p' then
         sort_by_perimeter = true
      elseif first == 'h' then
         sort_by_perimeter = false
      else
         assert(false, "parameter 'sort_by' must be string starting 'p[erimeter]' or 'h[ypotenuse]'")
      end
   end
   if sort_by_perimeter then
      return pythagorean_triples_brute_force_by_perimeter(predicate)
   else
      return pythagorean_triples_brute_force_by_hypotenuse(predicate)
   end
end   -- pythagorean_triples()
