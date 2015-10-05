-- dr.lua
-- printt(indexInc({1,2,3},5))
if string == nil then
   dofile("memo:lua5.lua")
end

dr = dr or {}

dr.w,dr.h,dr.d,dr.hascolor=pmode()

function dr.graph(...)
	local w,h=pmode()
	pmoveto(0,0)
	plineto(w,h)
	pevent()
end

function dr.hexal()
   local x,y = 10,10
   local fg=prgb(255,0,255)
   local bg=prgb(255,255,255)
   pcolor(fg,bg)
   pclear()
   pbox(x,y,x+20,y+40, fg)
   x,y = 25,17
   pbox(x,y,x+20,y+40, fg)
   x,y = 5,37
   pbox(x,y,x+20,y+40, fg)
   pevent()
end

function dr.tohex(s)
   return string.gsub(s, ".",
         function (c)
            return string.format("\\%0X", string.byte(c))
         end)
end

function dr.copyfuncs(from, to)
   for k,v in from do
      if type(v) == "function" then
         to[k] = v
      end
   end
end

function dr.printt(t)
   if type(t) ~= 'table' then
      print('not a table')
   else
      for k,v in t do
         print(k,v)
      end
   end
end

function dr.printa(t)
   if type(t) ~= 'table' then
      print('not a table')
   else
      for i=1,table.getn(t) do
         write(' ' .. t[i])
      end
      write('\n')
   end
end

function dr.strrev(s)
   local t = ""
   for i = 1, string.len(s) do
      t = t .. string.sub(s, -i, -i)
   end
   return t
end   -- dr.strrev()

-- for n=10000,10999 do if dr.ispalfactor(n) then print(n) end end; print('done')
-- 1089, 2178, 10989
function dr.ispalfactor(n)
   if math.mod(n, 10) == 0 then
      return false
   end
   local s = "" .. n
   local rs = dr.strrev(s)
   local rn = 0 + rs
   if n == rn then
      return false
   end
   if string.len(s) ~= string.len(rs) then
      return false
   end
   if n > rn then
      return (math.mod(n, rn) == 0)
   end
   return (math.mod(rn, n) == 0)
end   -- dr.ispalfactor()

-- print(string.xor('10110011', '1101'))
function string.xor(s1, s2)
   local b
   local n = string.len(s1)
   if n < string.len(s2) then
      n = string.len(s2)
   end

   local result = ''
   for i = 1, n do
      b = string.sub(s1, i, i) or '0'
      b = b .. (string.sub(s2, i, i) or '0')
      if (b == '10') or (b == '01') then
         result = result .. '1'
      else
         result = result .. '0'
      end
   end
   return result
end   -- string.xor(s1, s2)

function string.crc(s, poly, shifter)
   local dbit, sbit
   local plen = string.len(poly)
   shifter = shifter or ''
   shifter =  string.rep('0', plen) .. shifter
   shifter = string.sub(shifter, -plen)

   for i = 1, string.len(s) do
      dbit = string.sub(s, i, i)
      sbit = string.sub(shifter, 1, 1)
      if bs.op.bxor[dbit .. sbit] == '1' then
         shifter = string.xor(shifter, poly)
      end
      shifter = string.sub(shifter, 2) .. '0'
   end   -- loop
   return shifter
end   -- string.crc()

function indexInc(self, max)
   local n = table.getn(self)
   local j = n
   while true do
      self[j] = self[j] + 1
      if self[j] <= max - n + j then
         break
      end
      j = j - 1
      if j < 1 then
         return nil
      end
   end
   for k = j + 1, n do
      self[k] = self[k - 1] + 1
   end
   return self
end   -- indexInc()

printt = dr.printt
printa = dr.printa
