#! /usr/bin/env lua
-- $Id: carter-puzzle.lua 80 2008-11-14 00:56:41Z rogers $

-- Lua code to create Jeff Carter puzzles - the ones where the letters of
-- phrases are put in alphabetical order.

local args = { ... }

if #args == 0 then
   print 'Usage: carter-puzzle.lua <source-text-file>'
   os.exit(1)
end   -- if

local f = assert(io.open(args[1], "rt"))

for line in f:lines() do
   local tab = {}
   line:gsub('(%S)', function (c) tab[#tab+1] = c end)
   table.sort(tab)
   print(table.concat(tab))
end

f:close()

