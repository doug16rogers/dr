-- $Id: nt_test.lua 66 2008-10-16 14:49:10Z rogers $

-- Tests for number theory module.

local string_format = string.format
local table_concat  = table.concat

nt = require'nt'

local function equal(a, b)
   if type(a) ~= type(b) then
      return false
   end

   if type(a) == 'table' then
      for k, v in pairs(a) do
         if not equal(v, b[k]) then
            return false
         end
      end

      for k, v in pairs(b) do
         if not equal(v, a[k]) then
            return false
         end
      end
   else
      return a == b
   end

   return true
end   -- equal()

local function stringify(x)
   if type(x) == 'table' then
      local t = {}
      for k, v in pairs(x) do
         t[#t+1] = '[' .. stringify(k) .. ']=' .. stringify(v)
      end
      return '{' .. table_concat(t, ',') .. '}'
   elseif type(x) == 'string' then
      return string_format('%q', x)
   else
      return tostring(x)
   end
end   -- stringify()

-------------------------------------------------------------------------------
-- Test equal().
assert(equal(1,1))
assert(equal('g','g'))
assert(equal({},{}))
assert(equal({'a'},{'a'}))
assert(not equal({'a'},{'b'}))
assert(not equal({},{'b'}))
assert(not equal({'a'},{}))
assert(not equal({},{b='b'}))
assert(not equal({a='a'},{}))
assert(equal({1,2,'three'},{1,2,'three'}))

-- Test stringify().
assert(equal('{}', stringify({})))
assert(equal('1', stringify(1)))
assert(equal('"a"', stringify('a')))
assert(equal('{[1]=5,[2]=4,[3]=3,[4]="howdy"}', stringify({5,4,3,'howdy'})))

-- Test nt.sum()
assert(nt.sum{} == 0)
assert(nt.sum{1} == 1)
assert(nt.sum{1,2,3,4,5} == 15)
assert(nt.sum{1,2,-3,4,5} == 9)

-- Test nt.product()
assert(nt.product{} == 1)
assert(nt.product{1} == 1)
assert(nt.product{1,2,3,4,5} == 120)
assert(nt.product{1,2,-3,4,5} == -120)

-- Test nt.factorial()
assert(nt.factorial(0) == 1)
assert(nt.factorial(1) == 1)
assert(nt.factorial(2) == 2)
assert(nt.factorial(3) == 6)
assert(nt.factorial(4) == 24)
assert(nt.factorial(5) == 120)

-- Test nt.factors()
local function assert_factors(n, f, e)
   local ff, ee = nt.factors(n)
   -- print(n, nt.factortext(n))
   assert(equal(f, ff))
   assert(equal(e, ee))
end

assert_factors(0, {}, {})
assert_factors(1, {1}, {1})
assert_factors(-1, {-1}, {1})
assert_factors(2, {2}, {1})
assert_factors(-2, {-1,2}, {1,1})
assert_factors(3, {3}, {1})
assert_factors(4, {2}, {2})
assert_factors(5, {5}, {1})
assert_factors(360, {2,3,5}, {3,2,1})
assert_factors(15e6, {2,3,5}, {6,1,7})
assert_factors(-360, {-1,2,3,5}, {1,3,2,1})

-- Test nt.allfactors()
local function assert_all_factors(n, f)
   -- print(n)
   local ff = nt.all_factors(n)
   -- nt.print_array(ff, 'all_factors')
   assert(equal(f, ff))
end

assert_all_factors(0, {})
assert_all_factors(1, {1})
assert_all_factors(4, {1,2,4})
assert_all_factors(10, {1,2,5,10})
assert_all_factors(12, {1,2,3,4,6,12})
assert_all_factors(360,
                   {1,2,3,4,5,6,8,9,10,12,15,18,20,24,30,36,40,45,
                      60,72,90,120,180,360})

assert_all_factors(-1, {-1})
assert_all_factors(-2, {-1,2})
assert_all_factors(-4, {-1,2,4})
assert_all_factors(-12, {-1,2,3,4,6,12})

-- Test nt.is_perfect(), is_abundant(), is_deficient().

assert(nt.is_deficient(2))
assert(nt.is_deficient(3))
assert(nt.is_deficient(4))
assert(nt.is_deficient(5))
assert(nt.is_perfect(6))
assert(nt.is_deficient(7))
assert(nt.is_deficient(8))
assert(nt.is_deficient(9))
assert(nt.is_deficient(10))
assert(nt.is_deficient(11))
assert(nt.is_abundant(12))
assert(nt.is_deficient(13))
assert(nt.is_deficient(14))
assert(nt.is_deficient(15))
assert(nt.is_deficient(16))
assert(nt.is_deficient(17))
assert(nt.is_abundant(18))
assert(nt.is_deficient(19))
assert(nt.is_abundant(20))
assert(nt.is_deficient(21))
assert(nt.is_deficient(22))
assert(nt.is_abundant(24))
assert(nt.is_deficient(25))
assert(nt.is_deficient(26))
assert(nt.is_deficient(27))
assert(nt.is_perfect(28))

-- Test nt.permutation()
assert(equal(nt.permutation(0,1),{}))
assert(equal(nt.permutation(0,2),{}))
assert(equal(nt.permutation(1,1),{1}))
assert(equal(nt.permutation(1,2),{1}))
assert(equal(nt.permutation(2,1),{1,2}))
assert(equal(nt.permutation(2,2),{2,1}))
assert(equal(nt.permutation(3,1),{1,2,3}))
assert(equal(nt.permutation(3,2),{1,3,2}))
assert(equal(nt.permutation(3,3),{2,1,3}))
assert(equal(nt.permutation(3,4),{2,3,1}))
assert(equal(nt.permutation(3,5),{3,1,2}))
assert(equal(nt.permutation(3,6),{3,2,1}))
assert(equal(nt.permutation(3,7),{1,2,3}))
assert(equal(nt.permutation(3,8),{1,3,2}))
assert(equal(nt.permutation(4, 1),{1,2,3,4}))
assert(equal(nt.permutation(4,12),{2,4,3,1}))
assert(equal(nt.permutation(4,13),{3,1,2,4}))
assert(equal(nt.permutation(4,24),{4,3,2,1}))

local perm = nt.permutations(3)
assert(equal(perm(),{1,2,3}))
assert(equal(perm(),{1,3,2}))
assert(equal(perm(),{2,1,3}))
assert(equal(perm(),{2,3,1}))
assert(equal(perm(),{3,1,2}))
assert(equal(perm(),{3,2,1}))
assert(equal(perm(),nil))

-- Test is_palindrome().
assert(not nt.is_palindrome(nil))
assert(    nt.is_palindrome(''))
assert(    nt.is_palindrome('1'))
assert(not nt.is_palindrome('12'))
assert(not nt.is_palindrome('123'))
assert(not nt.is_palindrome('1232'))
assert(    nt.is_palindrome('12321'))

-- Test tobase().
assert(nt.tobase(0,2) == '0')
assert(nt.tobase(1,2) == '1')
assert(nt.tobase(2,2) == '10')
assert(nt.tobase(-2,2) == '-10')
assert(nt.tobase(122,2) == '1111010')
assert(nt.tobase(122,16) == '7A')
assert(nt.tobase(122,36) == '3E')

print('ok')
