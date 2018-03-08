local error = error
local ipairs = ipairs
local os = os
local pairs = pairs
local print = print
local setmetatable = setmetatable
local string = string
local table = table

-- local MS = require('matchstick')

module('matchstick_grid')

local MSG = {}
local mt = { __index = MSG }

-- ----------------------------------------------------------------------------
local function printf(...)
    print(string.format(...))
end   -- printf()

-- ----------------------------------------------------------------------------
local function errorf(...)
   error(string.format(...))
end   -- errorf()

args = { ... }
stick_list = {}

-- Maximum size of grid supported.
kMaxSize = 10
kMaxRows = kMaxSize
kMaxCols = kMaxSize
kRowFirst = 49       -- '1'
kColFirst = 97       -- 'a'
kRowLast = kRowFirst + kMaxRows - 1
kColLast = kColFirst + kMaxCols - 1

-- Direction codes (from head to tail).
kNorth = 1
kSouth = 2
kEast = 3
kWest = 4
kDirectionInit = { "N", "S", "E", "W" }
kDirectionName = { "North", "South", "East", "West" }

-- Stick segment types
kHead = 1
kStem = 2
kTail = 3

-- ----------------------------------------------------------------------------
local function pos_to_string(p)
   return string.char(kRowFirst + p[1] - 1) .. string.char(kColFirst + p[2] - 1)
end   -- pos_to_string()

-- ----------------------------------------------------------------------------
-- Rows are '1' .. '0' (for 10).
-- Cols are 'a' .. 'j'.
local function convert_coord_byte(b)
   is_row = (kRowFirst <= b) and (b <= kRowLast) 
   is_col = (kColFirst <= b) and (b <= kColLast) 
   if not (is_row or is_col) then
      printf_exit("invalid row or col character '%s'", string.char(b))
   end
   loc = is_row and (b - kRowFirst + 1) or (b - kColFirst + 1)
   return is_row, loc
end   -- convert_coord_byte()

-- ----------------------------------------------------------------------------
local function parse_stick(s)
    if #s ~= 3 then
        printf_exit("invalid length of stick description '%s'", s)
    end
    local is_row, row = convert_coord_byte(string.byte(s, 1))
    local head_is_row, head = convert_coord_byte(string.byte(s, 2))
    local tail_is_row, tail = convert_coord_byte(string.byte(s, 3))
    if not ((is_row and (not head_is_row) and (not tail_is_row)) or
            ((not is_row) and head_is_row and tail_is_row)) then
       printf_exit("invalid mix of rows and columns in stick description '%s'", s)
    end
    if tail == head then
       printf_exit("invalid tail and head (same) in stick description '%s'", s)
    end
    local step = (head < tail) and 1 or -1
    local pos = {}
    for loc = head, tail, step do
       local position = is_row and { row, loc } or { loc, row }
       if loc == head then
          position.segment = kHead
       elseif loc == tail then
          position.segment = kTail
       else
          position.segment = kStem
       end
       pos[#pos + 1] = position
    end
    stick = {}
    stick.description = s
    stick.pos = pos
    stick.head = pos[1]
    stick.tail = pos[#pos]
    stick.length = (step * (tail - head)) + 1
    if is_row then
       stick.direction = (head > tail) and kEast or kWest
    else
       stick.direction = (head > tail) and kSouth or kNorth
    end
    return stick
end   -- parse_stick()

-- ----------------------------------------------------------------------------
local function stick_to_string(s)
   local t = {}
   t[#t+1] = s.description
   for _, pos in ipairs(s.pos) do
      t[#t+1] = string.format("(%d,%d)", pos[1], pos[2])
   end
   return table.concat(t, ' ')
end   -- stick_to_string(s)

-- ----------------------------------------------------------------------------
-- Entrypoint into module. This will create a new matchstick_grid object.
--
function new(stick_descriptions, row_burnts, col_burnts)
    local max_row = 0
    local max_col = 0
    local sticks = {}
    for stick_description in stick_descriptions:gmatch('%w%w%w') do
       local stick = parse_stick(stick_description)
       sticks[#sticks+1] = stick
    end
    for _, stick in ipairs(sticks) do
       for _, pos in ipairs(stick.pos) do
          max_row = (pos[1] > max_row) and pos[1] or max_row
          max_col = (pos[2] > max_col) and pos[2] or max_col
       end
    end
    local g = {}
    for row = 1, max_row do
       g[row] = {}
       for col = 1, max_col do
          g[row][col] = false
       end
    end
    for _, stick in ipairs(sticks) do
       for _, pos in ipairs(stick.pos) do
          local cell = g[pos[1]][pos[2]]
          if cell then
             printf_exit("stick '%s' intersects with stick '%s'", stick.description, cell.stick.description)
          end
          cell = { stick = stick, burnt = false, row = pos[1], col = pos[2], segment = pos.segment }
          g[pos[1]][pos[2]] = cell
       end
    end
    for row = 1, max_row do
       for col = 1, max_col do
          if not g[row][col] then
             printf_exit("no stick covers location '%s'", pos_to_string({row,col}))
          end
       end
    end
    g.sticks = sticks
    g.rows = max_row
    g.cols = max_col
    return setmetatable(g, mt)
end   -- new()

-- ----------------------------------------------------------------------------
-- These are in the order North, South, East, West (for the direction the
-- match is pointing if the head is an arrowhead.
kIconRows =  7    -- These include both sides; so they'll overlap.
kIconCols = 13
kHeadOpenIcons = { {
    "+-----------+",
    "|   _____   |",
    "|  /     \\  |",
    "| |       | |",
    "|  \\     /  |",
    "|   |   |   |",
    "+---|   |---+",
}, {
    "+---|   |---+",
    "|   |   |   |",
    "|  /     \\  |",
    "| |       | |",
    "|  \\_____/  |",
    "|           |",
    "+-----------+",
}, {
    "+-----------+",
    "|   _____   |",
    "---/     \\  |",
    "          | |",
    "---\\_____/  |",
    "|           |",
    "+-----------+",
}, {
    "+-----------+",
    "|   _____   |",
    "|  /     \\---",
    "| |          ",
    "|  \\_____/---",
    "|           |",
    "+-----------+",
} }   -- kHeadOpenIcons

kStemOpenIcons = { {
    "+---|   |---+",
    "|   |   |   |",
    "|   |   |   |",
    "|   |   |   |",
    "|   |   |   |",
    "|   |   |   |",
    "+---|   |---+",
}, {
    "+---|   |---+",
    "|   |   |   |",
    "|   |   |   |",
    "|   |   |   |",
    "|   |   |   |",
    "|   |   |   |",
    "+---|   |---+",
}, {
    "+-----------+",
    "|           |",
    "-------------",
    "             ",
    "-------------",
    "|           |",
    "+-----------+",
}, {
    "+-----------+",
    "|           |",
    "-------------",
    "             ",
    "-------------",
    "|           |",
    "+-----------+",
   } };   -- kStemOpenIcons

kTailOpenIcons = { {
    "+---|   |---+",
    "|   |   |   |",
    "|   |   |   |",
    "|   |   |   |",
    "|   |___|   |",
    "|           |",
    "+-----------+"
}, {
    "+-----------+",
    "|    ___    |",
    "|   |   |   |",
    "|   |   |   |",
    "|   |   |   |",
    "|   |   |   |",
    "+---|   |---+"
}, {
    "+-----------+",
    "|           |",
    "| +----------",
    "| |          ",
    "| +----------",
    "|           |",
    "+-----------+",
}, {
    "+-----------+",
    "|           |",
    "----------+ |",
    "          | |",
    "----------+ |",
    "|           |",
    "+-----------+",
} }   -- kTailOpenIcons

kHeadBurnIcons = { {
    "+-----------+",
    "|   _____   |",
    "|  /#####\\  |",
    "| |#######| |",
    "|  \\#####/  |",
    "|   |###|   |",
    "+---|###|---+",
}, {
    "+---|###|---+",
    "|   |###|   |",
    "|  /#####\\  |",
    "| |#######| |",
    "|  \\#####/  |",
    "|           |",
    "+-----------+",
}, {
    "+-----------+",
    "|   _____   |",
    "---/#####\\  |",
    "##########| |",
    "---\\#####/  |",
    "|           |",
    "+-----------+",
}, {
    "+-----------+",
    "|   _____   |",
    "|  /#####\\---",
    "| |##########",
    "|  \\#####/---",
    "|           |",
    "+-----------+",
} }   -- kHeadBurnIcons

kStemBurnIcons = { {
    "+---|###|---+",
    "|   |###|   |",
    "|   |###|   |",
    "|   |###|   |",
    "|   |###|   |",
    "|   |###|   |",
    "+---|###|---+",
}, {
    "+---|###|---+",
    "|   |###|   |",
    "|   |###|   |",
    "|   |###|   |",
    "|   |###|   |",
    "|   |###|   |",
    "+---|###|---+",
}, {
    "+-----------+",
    "|           |",
    "-------------",
    "#############",
    "-------------",
    "|           |",
    "+-----------+",
}, {
    "+-----------+",
    "|           |",
    "-------------",
    "#############",
    "-------------",
    "|           |",
    "+-----------+",
   } };   -- kStemBurnIcons

kTailBurnIcons = { {
    "+---|###|---+",
    "|   |###|   |",
    "|   |###|   |",
    "|   |###|   |",
    "|   |###|   |",
    "|           |",
    "+-----------+"
}, {
    "+-----------+",
    "|    ___    |",
    "|   |###|   |",
    "|   |###|   |",
    "|   |###|   |",
    "|   |###|   |",
    "+---|###|---+"
}, {
    "+-----------+",
    "|           |",
    "| +----------",
    "| |##########",
    "| +----------",
    "|           |",
    "+-----------+",
}, {
    "+-----------+",
    "|           |",
    "----------+ |",
    "##########| |",
    "----------+ |",
    "|           |",
    "+-----------+",
} }   -- kTailBurnIcons

-- This is all the icons indexed by segment type:
kOpenIcons = { kHeadOpenIcons, kStemOpenIcons, kTailOpenIcons }
kBurnIcons = { kHeadBurnIcons, kStemBurnIcons, kTailBurnIcons }
kIcons = { kOpenIcons, kBurnIcons }

-- ----------------------------------------------------------------------------
function MSG:display()
   local kCanvasRows = 1 + ((kIconRows - 1) * self.cols)
   local kCanvasCols = 1 + ((kIconCols - 1) * self.rows)
   local canvas = {}
   for r = 1, kCanvasRows do
      canvas[r] = {}
      for c = 1, kCanvasCols do
         canvas[r][c] = ' '
      end
   end
   for row = 1, self.rows do
      for col = 1, self.cols do
         -- This should really be put in another function. Then I will need
         -- to move the canvas and its sizes directly into the object.
         local cell = self[row][col]
         local icon = kIcons[cell.burnt and 2 or 1][cell.segment][cell.stick.direction];
         local canvas_row= (kIconRows - 1) * (row - 1)    -- zero-based
         local canvas_col =(kIconCols - 1) * (col - 1)
         for r = 1, kIconRows do
            for c = 1, kIconCols do
               canvas[canvas_row+r][canvas_col+c] = icon[r]:sub(c, c)
            end
         end
      end
   end
   for r = 1, kCanvasRows do
      print('    ' .. table.concat(canvas[r]))
   end
   return self
end   -- MSG:display()

-- ----------------------------------------------------------------------------
function MSG:burn_matchstick_segments(stick_number, segments_to_burn)
   local stick = self.sticks[stick_number]
   if not stick then
      errorf("stick_number %d > %d number of matchsticks in grid", stick_number, #self.sticks)
   end
   if segments_to_burn > #stick.pos then
      errorf("segments_to_burn %d > %d number of segments in matchstick %d",
             segments_to_burn, #stick.pos, stick_number)
   end
   for i = 1, segments_to_burn do
      local pos = stick.pos[i]
      self[pos[1]][pos[2]].burnt = true
   end
   return self
end   -- MSG:burn_matchstick_segments()
