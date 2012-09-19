-- Dexudoku.lua
-- m=Dexudoku:new() m:print()

if string == nil then
   dofile("memo:lua5.lua")
end

-- local Dexudoku
Dexudoku = {}

Dexudoku.mt = { __index = Dexudoku }

Dexudoku.symbols = {}
Dexudoku.symbols[4] = { [false] = ' ', '1', '2', '3', '4' }
Dexudoku.symbols[9] =
{
  [false] = ' ',
  '1', '2', '3', '4', '5', '6', '7', '8', '9'
}

Dexudoku.symbols[16] =
{
  [false] = ' ',
  '0', '1', '2', '3',
  '4', '5', '6', '7',
  '8', '9', 'A', 'B',
  'C', 'D', 'E', 'F'
}
Dexudoku.symbols[25] =
{
  [false] = ' ',
  'A', 'B', 'C', 'D', 'E',
  'F', 'G', 'H', 'I', 'J',
  'K', 'L', 'M', 'N', 'O',
  'P', 'Q', 'R', 'S', 'T',
  'U', 'V', 'W', 'X', 'Y'
}

function Dexudoku:new(N)
   local grid = {}
   local row = {}
   local col = {}
   local box = {}

   grid.N = N or 16
   grid.S = math.sqrt(grid.N)

   if grid.N ~= (grid.S * grid.S) then
      return nil, "N must be a perfect square"
   end

   grid.row = row
   grid.col = col
   grid.box = box

   -- First create each group array.
   for j = 1, grid.N do
      row[j] = {}
      col[j] = {}
      box[j] = {}
   end   -- for each group's index

   for j = 1, grid.N do
      for k = 1, grid.N do
-- This work for zero-based indexes:
--       local n = (grid.S * math.mod(j, grid.S)) + math.floor(j / grid.S) + k
--       n = math.mod(n, grid.N)
         n = false
         local cell = { val = n }
         row[j][k] = cell
         col[k][j] = cell
         cell.rowi = j
         cell.row = row[cell.rowi]
         cell.coli = k
         cell.col = col[cell.coli]
         cell.boxi = 1 + (grid.S * math.floor((j-1) / grid.S)) +
            math.floor((k-1) / grid.S)
         cell.box = box[cell.boxi]
         local boxc = 1 + (grid.S * math.mod((j-1), grid.S)) +
            math.mod((k-1), grid.S)
         box[cell.boxi][boxc] = cell
         cell.grid = grid
      end
   end

   setmetatable(grid, self.mt)

   if self.symbols[grid.N] then
      grid:setSymbols(self.symbols[grid.N])
   else
      grid:setSymbols(self:numericSymbols(N))
   end

   return grid
end   -- new()

function Dexudoku:numericSymbols(n)
   if self.symbols[n] then
      return self.symbols[n]
   end

   local last = string.format('%d', n)
   local symbols = {}

   symbols[false] = string.rep(' ', string.len(last))

   local format='%' .. string.len(last) .. 'd'

   for i = 1, n do
      symbols[i] = string.format(format, i)
   end

   self.symbols[n] = symbols
   return symbols
end   -- numericSymbols()

function Dexudoku:set(row,col,num)
   if type(num) == 'string' then
      if num == self.symbol[false] then
         num = false
      else
         for i = 1, self.N do
            if self.symbol[i] == num then
               num = i
               break
            end
         end

         if type(num) == 'string' then
            error("invalid symbol '" .. num .. "'")
         end
      end
   end

   if not self.symbol[num] then
      error("invalid value '" .. num .. "'")
   end

   local cell = self.row[row][col]
   cell.val = num
   return self
end   -- set()

function Dexudoku:setSymbols(symbols)
   if not symbols[false] then
      error("symbol set must have a value for false")
   end

   for i = 1, self.N do
      if not symbols[false] then
         error("symbol set must have a value for " .. i)
      end
   end

   self.symbol = symbols
   return self
end   -- setSymbols()

function Dexudoku:printGrid(with_bars)
   local s = ''
   local symbol_width = string.len(self.symbol[false])
   local box_divider
   local cell_divider

   box_divider = string.rep('##' .. string.rep('#', symbol_width) .. '#',
                            self.N) .. '#'
   local cell_line = '-' .. string.rep('-', symbol_width) .. '-'
   local box_line = '#' .. cell_line .. string.rep('+' .. cell_line, self.S-1)
   cell_divider = string.rep(box_line, self.S) .. '#'
   print(box_divider)

   for j = 1, self.N do
      s = '#'

      for k = 1, self.N do
         local cell = self.row[j][k]

         s = s .. ' ' .. self.symbol[cell.val]

         if math.mod((k-1), self.S) == (self.S - 1) then
            s = s .. ' #'
         else
            s = s .. ' |'
         end
      end

      print(s)

      if math.mod ((j-1), self.S) == (self.S - 1) then
         print(box_divider)
      else
         print(cell_divider)
      end
   end   -- for each row

   return self
end   -- printGrid()

function Dexudoku:print(with_bars)
   local s = ''

   for j = 1, self.N do
      s = ''

      for k = 1, self.N do
         local cell = self.row[j][k]
         s = s .. self.symbol[cell.val]
      end

      print(s)
   end

   return self
end   -- print()

return Dexudoku
