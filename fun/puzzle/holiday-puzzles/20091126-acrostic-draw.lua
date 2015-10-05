-------------------------------------------------------------------------------
full_author = "Whittier"    -- John Greenleaf Whittier
full_title = "The Pumpkin"

full_quote = [[
Ah! on Thanksgiving day....
When the care-wearied man seeks his mother once more,
And the worn matron smiles where the girl smiled before.
What moistens the lips and what brightens the eye?
What calls back the past, like the rich pumpkin pie?
]]

words_clues = {
   { "Whitewash",          "Tom Sawyer's fencing job" },
   { "Home plate",         "Pentagon on a diamond" },
   { "In the know",        "Party to the secret" },
   { "Thighs and Legs",    "Dark lover's desire" },
   { "Twitch",             "Tic" },
   { "Inhaler",            "Asthmatic's relief" },
   { "Embassy",            "Chancery" }, -- "Place for a consulate" },
   { "Remember the Alamo", "Battle cry in San Jacinto" },
   { "Terrible twos",      "Tantrum-prone era" },
   { "High heels",         "Stilettos" },
   { "Emphasis",           "Stress" },
   { "Patched",            "Repaired" },
   { "Unwed",              "Single" },
   { "Meet the Parents",   "2000 Stiller / De Niro comedy" }, -- "Cat milking movie" },
   { "President",          "Garfield, for one" },
   { "Knock-knock Who's there", "Kid's joke repartee" },
   { "Imaginary friend",   "Buddy of mind" },
   { "National Archives",  "Home of the Constitution" },
}

filename = "200911-acrostic.ps"
-- include_punctuation = true   -- Eventually allow punctuation.
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- NO NEED TO MODIFY STUFF BELOW HERE....
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------

math.randomseed(0x12345)

local F = string.format

quote = full_quote:gsub('\n',' '):gsub('[^%a ]',''):upper()

function lines_in_string(s)
   local count = 0
   local max_len = 0
   for line in s:gmatch("[^\n]+") do
      count = count + 1
      if #line > max_len then
         max_len = #line
      end
   end
   return count, max_len
end

qrows, qcols = lines_in_string(full_quote)

-- Generate list of locations in quote for each different letter.

qinfo = {}
qloc = {}

loc = 1

for c in quote:gmatch('%a') do
   local cloc = qloc[c]

   if not cloc then
      cloc = { next = 1, letter = c }   -- Used later to apply to words.
      qloc[c] = cloc
   end

   cloc[#cloc+1] = loc
   qinfo[loc] = { letter = c, cloc = cloc }
   loc = loc + 1
end

-- Randomize the locations.

for _, box in ipairs(qinfo) do
   local cloc = box.cloc

   for i = 1, #cloc do
      local r = math.random(#cloc)
      local swap = cloc[i]
      cloc[i] = cloc[r]
      cloc[r] = swap
   end
end

-- Cross-reference locations of each answer's letter within quote.

for i, wc in ipairs(words_clues) do
   local word = wc[1]:upper():gsub('[^%a ]','')
   local box_list = {}
   for c in word:gmatch('%a') do
      local box = {}
      local cloc = qloc[c]
      box.letter = c
      box.loc = cloc[cloc.next]
      cloc.next = cloc.next + 1
      qinfo[box.loc].box = box
      qinfo[box.loc].word = i
      box_list[#box_list+1] = box
   end
   wc.box = box_list
end

-- Print quote with location of each letter in words.

function letterize(n)
   local letter = string.char(string.byte('A') + n  - 1)
   if n > 26 then
      return letter .. letter
   end
   return letter
end   -- letterize()

-------------------------------------------------------------------------------
PS = require('PostScript')

ps = PS.new()

--p0 = { x = 72 * 1.0, y = 72 *  1.0 }
--p1 = { x = 72 * 7.5, y = 72 * 10.0 }

X_MIN =  0.5 * 72
X_MAX =  8.0 * 72
Y_MAX = 10.5 * 72
Y_MIN =  0.5 * 72
COLUMNS = 30     -- Boxes across the page for the quotation.
BOX_WIDTH = (X_MAX - X_MIN) / COLUMNS
BOX_FONT_FACTOR = 3.5

QUOTE_Y_MAX = Y_MAX - 1.0 * 72    -- Leave room for instructions.

FONT = "Arial"
font_size = 11
ps:setfont(FONT.."-Italic", font_size)
ps:move(X_MIN,Y_MAX-font_size):show("This puzzle consists of a quotation and a set of clues to the letters contained in the quotation.")
ps:movedy(-font_size):         show("Determine the answer for each clue then substitute the letters into the quotation at the location")
ps:movedy(-font_size):         show("indicated for that letter. Work back and forth between the clues and the quotation to solve the puzzle.")
ps:movedy(-font_size):         show("When the puzzle is complete, the answers' first letters will spell the quotation's author and source.")
ps:setfont(FONT.."-Italic-Bold", font_size)
ps:movedy(-font_size):         show("Tricky tricky! All non-space punctuation has been removed from the quotation and answers.")

-- The lower left corner is returned.

function box_position(r, c)
   local x = X_MIN + ((c - 1) * BOX_WIDTH)
   local y = QUOTE_Y_MAX - ((r - 0) * BOX_WIDTH)
   return x, y
end

-- Prints a box to the PostScript object. If number is less than 1 then the box
-- is drawn shaded. If word_index is less than 1, then it is not included in
-- the text being drawn.

function print_box(r, c, number, word_index)
   -- This is the bottom left:
   local x, y = box_position(r, c)
   local w = BOX_WIDTH

   if number <= 0 then
      ps:move(x,y):linedx(w):linedy(w):linedx(-w):linedy(-w):fill(0.5)
   else
      local text = tostring(number)

      if word_index > 0 then
         text = text .. '-' .. letterize(word_index)
      end

      ps:setfont(FONT, w/BOX_FONT_FACTOR)
      ps:move(x+w/10,y+w-w/BOX_FONT_FACTOR):show(text):close()
   end

   -- Draw outline last so it stands out.
   ps:setgray(0):move(x,y):linedx(w):linedy(w):linedx(-w):linedy(-w):stroke()
end   -- print_box()

row = 1
col = 1
loc = 1

for c in quote:gmatch('[%a ]') do
   if c == ' ' then
      print_box(row,col,-1,-1)
   else
      print_box(row,col,loc,qinfo[loc].word)
      loc = loc + 1
   end

   col = col + 1

   if col > COLUMNS then
      col = 1
      row = row + 1
   end
end

if col ~= 1 then
   while col <= COLUMNS do
      print_box(row,col,-1,-1)
      col = col + 1
   end
   row = row + 1
end

row = row + 1
ANSWER_START_COL = 9

-- Print clues with words after.

for i, wc in ipairs(words_clues) do
   local word, clue = wc[1]:upper():gsub('[^%a ]',''), wc[2]
   local alpha = 1

   for col = 1, #word do
      if word:sub(col,col) == ' ' then
         print_box(row, ANSWER_START_COL + col, 0, 0)
      else
         print_box(row, ANSWER_START_COL + col, wc.box[alpha].loc, 0)
         alpha = alpha + 1
      end
   end

   local x, y = box_position(row, 1)

   ps:moveto(x,y+BOX_WIDTH/5):setfont(FONT, BOX_WIDTH*3/5)
   ps:show(letterize(i) .. ". " .. clue)

   row = row + 1.25
end

ps:setfont(FONT.."-Italic", 2*font_size)
ps:move(X_MIN+15*font_size,Y_MIN+5*font_size):show("Happy Thanksgiving!")
ps:setfont(FONT.."", font_size)
ps:move(X_MIN+10*font_size,Y_MIN+3*font_size):show("Have a lot of fun - and get your mind out of the gutter on clue D!")

ps:setfont(FONT, 6):moveto(X_MIN,Y_MIN):show("2009-11-26 Thanksgiving Puzzle")
ps:showpage()

filname = filename or "PostScriptTest.ps"
ps:writefile(filename)
os.execute('evince ' .. filename)

