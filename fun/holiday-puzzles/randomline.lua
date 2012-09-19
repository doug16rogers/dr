#! /usr/bin/env lua
-- $Id: randomline.lua 89 2008-11-28 05:27:48Z rogers $

-- Reads lines from stdin or a file, then randomizes the list and prints the
-- result.

lines = {}

filenames = { ... }

if #filenames > 0 then
   print("Filenames not yet supported.")
   os.exit(1)
end

for line in io.lines() do
   lines[#lines + 1] = line
end

math.randomseed(os.time())

for i = 1, #lines do
   local ri = math.random(#lines)
   lines[ri], lines[i] = lines[i], lines[ri]   -- Swap places.
end

for _, line in ipairs(lines) do
   print(line)
end

