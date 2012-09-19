function strip(s)
   return s:gsub('\n', ' '):gsub('%p','')
end

quote = strip[[
Possessing a clear and penetrating mind, a strong and sound
judgment, calmness and temper for deliberation, with invincible firmness and
perseverance in resolutions maturely formed; drawing information from all;
acting from himself, with incorruptible integrity and unvarying patriotism;
his own superiority and the public confidence alike marked him as the man
designed by Heaven.
]]

print(#quote)

function try(n)
   local t = {}
   local x = {}
   print()

   for i = 1, #quote / n do
      t[#t+1] = quote:sub((i - 1) * n + 1, i * n):lower()
      print(t[#t])
   end

   for i = 1, n do
      for j = 1, #quote / n do
         x[i] = (x[i] or '') .. t[j]:sub(i, i)
      end
   end

   for i = 1, n do
      if not anagrams[sorted_lower(x[i])] then
         print('No anagrams for "' .. x[i] .. '".')
      end
   end

   return t, x
end


-- ... ...
-- ... to lead in the great political as well as military events
-- which have distinguished the era of his life.
