#!/bin/env /usr/bin/lua

args = { ... }

if (#args ~= 1) then
   print("Usage: scrabblegram.lua <letters>")
   os.exit(1)
end

letters = args[1]

function execf(...)
   local cmd = string.format(...)
   print(cmd)
   return os.execute(cmd)
end

regex = string.rep("[" .. letters .. "]", #letters)

greps = {
   string.format('grep "^%s$" word.lst', regex)
}

for i = 1, #letters do
   greps[#greps+1] = string.format('| grep "%s"', letters:sub(i,i))
end

cmd = table.concat(greps, ' ')
print(cmd)
os.execute(cmd)

