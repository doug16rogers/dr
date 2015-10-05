-- $Id: 20081225-mark-charlene.lua 128 2009-12-24 23:05:50Z rogers $

function xref_add(t, key, val)
   if t[key] == nil then
      t[#t + 1] = key
      t[key] = val or true
   end

   return t
end   -- xref_add()

function xref_add_letters(t, s)
   for i = 1, #s do
      xref_add(t, s:sub(i, i))
   end

   return t
end   -- xref_add_letters()

function make_grid(rows, cols, initial_value)
   initial_value = initial_value or true
   local g = { rows = rows, cols = cols }

   for r = 1, rows do
      g[r] = {}
      for c = 1, cols do
         g[r][c] = initial_value
      end
   end
   return g
end   -- make_grid()

function print_grid(g)
   for r = 1, g.rows do
      io.write("  ")
      for c = 1, g.cols do
         io.write(" " .. tostring(g[r][c]))
      end
      io.write("\n")
   end
   return g
end   -- print_grid()

alpha = xref_add_letters({}, "MARKCHARLENE")
print(#alpha)
grid = make_grid(#alpha, #alpha, "x")
print(#grid)
--print(#grid[1])
print_grid(grid)

