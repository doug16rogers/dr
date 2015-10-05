require('bit')

function errorf(...)
   error(string.format(...), 2)
end

function assert_int(proper, actual)
   if actual ~= proper then
      errorf("Proper: %u (0x%08X); Actual: %u (0x%08X).", proper, proper, actual, actual)
   end
end

assert_int(0xFFFFFFFF, bit.band())
assert_int(5, bit.band(5))
assert_int(5, bit.band(5, 7))
assert_int(1, bit.band(5, 7, 3))
assert_int(0, bit.band(5, 7, 3, 2))

assert_int(0, bit.bxor())
assert_int(5, bit.bxor(5))
assert_int(2, bit.bxor(5, 7))
assert_int(1, bit.bxor(5, 7, 3))
assert_int(3, bit.bxor(5, 7, 3, 2))
assert_int(0x4E, bit.bxor(0x35, 0x7B))

print('ok')
