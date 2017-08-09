require'nt'

args = { ... }

limit = 40
relatively_prime = false
sort_by = 'perimeter'   -- or 'hypotenuse'

if #args >= 1 then
   limit = tonumber(args[1])
end

if #args >= 2 then
   relatively_prime = (tonumber(args[2]) ~= 0)
end

if #args >= 3 then
end

n = 0
for triple in nt.pythagorean_triples(relatively_prime, sort_by) do
-- for triple in nt.pythagorean_triples_kmn() do
   n = n + 1
   print(string.format("%4d: %5d %s", n, nt.sum(triple), nt.array_to_string(triple)))
   if n >= limit then
      break
   end
end
