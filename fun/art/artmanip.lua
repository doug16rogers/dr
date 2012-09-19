-- $Id: artmanip.lua 75 2008-10-24 14:42:25Z rogers $

require'bstring'

-------------------------------------------------------------------------------
function image_masks(list)
   local masks = {}

   for i = 1, #list do
      masks[i] = list[i]:bxor(list[1])
   end

   return masks
end   -- image_masks()

-------------------------------------------------------------------------------
function load_monochrome(filename)
   local file = assert(io.open(filename or 'ascii_women.txt', 'r'))

   local next = file:lines()
   local line
   local images = {}

   while true do
      line = next()

      if not line then
         break
      end

      if line:sub(1,5) == '64x52' then
         local s = ''
         for i = 1, 52 do
            s = s .. next():gsub('-', '0'):gsub('#', '1')
         end
         images[#images + 1] = bstring.newtext(s)

      end
   end

   file:close()
   
   return images, image_masks(images)
end   -- load_monochrome()

-------------------------------------------------------------------------------
function bstring:display(replace_01)
   local i = 0
   replace_01 = replace_01 or ' #'

   if replace_01:len() < 2 then
      replace_01 = replace_01 .. '#'
   end

   local repl = { ['0'] = replace_01:sub(1,1), ['1'] = replace_01:sub(2,2) }

   while i < self:len() do
      print((self:sub(i+1, i+64):tostring():gsub('[01]',repl)))
      i = i + 64
   end

   return self
end

-------------------------------------------------------------------------------
function bstring:display_hex(name)
   local ROWS = 13
   local COLS = 64

   name = name or 'dummy'
   io.write('static const char* ', name, ' =\n')

   local i = 1

   for row = 1, ROWS do
      io.write('  "')

      for col = 1, COLS do
         io.write(string.format('%X', self:word(i, i+3)))
         i = i + 4
      end

      io.write('"')

      if row == ROWS then
         io.write(';')
      end

      io.write('\n')
   end

   return self
end   -- display_hex()

-------------------------------------------------------------------------------
-- Based on ascii_women.txt on 20081024T101916 (Fri).
function load_edc()
   local img, msk = load_monochrome()
   local msg = img[1]
   local fec = img[1]:bxor(img[2])
   local tdc = img[1]:bxor(img[3])
   local scr = img[3]:bxor(img[4])
   return msg, fec, tdc, scr
end   -- load_edc()

-------------------------------------------------------------------------------
function print_edc()
   local msg, fec, tdc, scr = load_edc()
   msg:display_hex('message_text')
   fec:display_hex('fec_mask_text')
   tdc:display_hex('tdc_mask_text')
   scr:display_hex('scr_mask_text')
   return msg, fec, tdc, scr
end   -- print_edc()

-------------------------------------------------------------------------------
function display_edc_combinations(replace_01)
   local msg, fec, tdc, scr = load_edc()

   print('\n-------------------------------------------------')
   print('MS188220_EDC_MODE_NOFEC_NOTDC_NOSCRAMBLING:')
   msg:display(replace_01)

   print('\n-------------------------------------------------')
   print('MS188220_EDC_MODE_NOFEC_NOTDC_SCRAMBLING:')
   msg                  :bxor(scr):display(replace_01)

   print('\n-------------------------------------------------')
   print('MS188220_EDC_MODE_FEC_NOTDC_NOSCRAMBLING:')
   msg:bor(fec)                   :display(replace_01)

   print('\n-------------------------------------------------')
   print('MS188220_EDC_MODE_FEC_NOTDC_SCRAMBLING:')
   msg:bor(fec)         :bxor(scr):display(replace_01)

   print('\n-------------------------------------------------')
   print('MS188220_EDC_MODE_FEC_TDC_NOSCRAMBLING:')
   msg:bor(fec):bor(tdc)          :display(replace_01)

   print('\n-------------------------------------------------')
   print('MS188220_EDC_MODE_FEC_TDC_SCRAMBLING:')
   msg:bor(fec):bor(tdc):bxor(scr):display(replace_01)

   return msg, fec, tdc, scr
end   -- display_edc_combinations()
