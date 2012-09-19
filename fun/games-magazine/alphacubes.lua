
require 'bit'

-- Intended to be loaded by a script that defines 'words' and 'N' for
-- the word list and the number of dice.

if not words or not N then
	error("Should be called by another that defines 'words' and 'N'.")
end

local printf = function(...) print(string.format(...)) end

local alphabet = "abcdefghijklmnoprstuvwyz"
local MASKS = { 0x01, 0x03, 0x07, 0x0F, 0x1F, 0x3F, 0x7F, 0xFF }
local MASK = MASKS[N]
local one_bit_count = { [0]=0,1,1,2, 1,2,2,3, 1,2,2,3, 2,3,3,4 }
local one_bit_set = {
    [0]=false,true,true,false, true,false,false,false,
	true,false,false,false, false,false,false,false
}

local which_one_set = { [0]=0,1,2,0, 3,0,0,0, 4,0,0,0, 0,0,0,0 }

local die = {}

for i = 1, N do
	die[i] = { n = i }
end

local alpha = {}

for i = 1, #alphabet do
    alpha[i] = {}
	alpha[i].char = alphabet:sub(i,i)
	alpha[i].mask = MASK
	alpha[i].die = false
        alpha[i].used = false
	alpha[alphabet:sub(i,i)] = alpha[i]  -- Allow indexing by letter.
end

local NUM_USED = 0

for _, word in ipairs(words) do
   for j = 1, #word do
      local a = alpha[word:sub(j,j)]
      if not a.used then
         a.used = true
         NUM_USED = NUM_USED + 1
      end
   end
end

for _, a in ipairs(alpha) do
   if NUM_USED >= (6 * N) then
      break
   end

   if not a.used then
      a.used = true
      NUM_USED = NUM_USED + 1
   end
end

-- local used_letters = ''
-- for _, a in ipairs(alpha) do
--    if a.used then
--       used_letters = used_letters .. a.char
--    end
-- end

-- print('used', #used_letters, used_letters)

--
--

local function print_status()
	for i, d in ipairs(die) do
		io.write(tostring(i), ': ')
		for j = 1, #d do io.write(d[j]) end
		io.write('\n')
	end
	for i, a in ipairs(alpha) do
		io.write(a.char, tostring(a.mask))
	end
	io.write('\n')
end

-- print('Initial:')
-- print_status()

-- Preload first word.

for i = 1, N do
	local c = words[1]:sub(i,i)
	alpha[c].die = i
	alpha[c].mask = 2 ^ (i - 1)
	die[i][1] = c
end

printf('After "%s":', words[1])
print_status()

local function try_word(w)
	for i = 1, #w do
		local c = w:sub(i,i)
		local a = alpha[c]
		-- printf("Outer %s (%s)", c, w)
		if a.die then
			-- printf("Outer %s die %d", c, a.die)
			for j = 1, #w do
				if i ~= j then
					local c2 = w:sub(j,j)
					local a2 = alpha[c2]
					if not a2.die then
						local b = 2 ^ (a.die - 1)
						-- printf("Inner %s removing %d from %d", c2, b, a2.mask)
						a2.mask = bit.band(a2.mask, bit.bxor(MASK, b))
						if one_bit_set[a2.mask] then
							a2.die = which_one_set[a2.mask]
							local d = die[a2.die]
							-- printf("Adding %s to die %d", c2, a2.die)
							d[#d+1] = c2
						end
					end
				end
			end
		end
	end

        -- Now find the case where one letter is not found (alphacubes_201103_2.lua):
        local num_not_found = 0
        local last_not_found = 1

        for i, a in ipairs(alpha) do
           if a.used and not a.die then
              num_not_found = num_not_found + 1
              last_not_found = i
           end
        end

--printf('num_not_found=%u last_not_found=%u', num_not_found, last_not_found)

        -- If exactly one letter has not been placed, put it at the
        -- end of the die that is short.

        if num_not_found == 1 then
           for i, d in ipairs(die) do
              if #d < 6 then
                 alpha[last_not_found].die = i
                 d[#d+1] = alpha[last_not_found].char
                 break
              end
           end
        end
end

for m = 1, 8 do
	for k = 2, #words do
		local word = words[k]
		try_word(word)
	end
	printf('After round #%d:', m)
	print_status()

        local finished = true

        for _, d in ipairs(die) do
           if #d ~= 6 then
              finished = false
              break
           end
        end

        if finished then
           break
        end
end
