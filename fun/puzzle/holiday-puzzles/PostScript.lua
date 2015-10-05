-- This module provides a PostScript object.

-- The commands are stored in a table of strings that may then be written to
-- a file, etc.

local assert        = assert
local io_open       = io.open
local setmetatable  = setmetatable
local string_format = string.format
local table_concat  = table.concat
local type          = type

module(...)

local method = {}
local mt = { __index = method }

-------------------------------------------------------------------------------
-- Creates a new PostScript document object.
--
-- @param info - table holding document information.
-- @return a new PostScript object.

function new(info)
   info = info or {}    -- Make sure the info table exists.

   local ps = {}
   ps._line = {}
   ps._page = 0
   ps._new_page = true
   ps._coord_fmt1 = "%4.2f "
   ps._coord_fmt2 = ps._coord_fmt1 .. ps._coord_fmt1
   return setmetatable(ps, mt)
end   -- new()

-------------------------------------------------------------------------------
function method:add(s)
   local line = self._line

   -- Only write the new page setup stuff out when something is emitted AFTER
   -- a 'showpage'.

   if self._new_page then
      self._page = self._page + 1
      line[#line + 1] =
         string_format("%%%%Page: %d %d", self._page, self._page)
      line[#line + 1] = "%%BeginPageSetup"
      line[#line + 1] = "%%EndPageSetup"
      self._new_page = false
   end

   -- Now add the line.
   line[#line + 1] = s;
   return self
end   -- add()

-------------------------------------------------------------------------------
function method:closepath()
   self:add("closepath")
   self._x = nil
   self._y = nil
   return self
end   -- closepath()

-------------------------------------------------------------------------------
function method:fill(option)
   if type(option) == 'number' then
      self:setgray(option)
   end

   if self:pathopen() then
      self:closepath()
   end

   return self:add("fill")
end   -- fill()

-------------------------------------------------------------------------------
function method:lineto(x, y)
   -- assert(type(x) == 'number', 'x must be a number')
   -- assert(type(y) == 'number', 'y must be a number')
   assert(self:pathopen(), 'no path open; must call moveto() before lineto()')
   self._x = x
   self._y = y
   return self:add(string_format(self._coord_fmt2, x, y) .. "lineto")
end   -- lineto()

-------------------------------------------------------------------------------
function method:linedx(dx)
   return self:lineto(self._x + dx, self._y)
end   -- linedx()

-------------------------------------------------------------------------------
function method:linedy(dy)
   return self:lineto(self._x, self._y + dy)
end   -- linedy()

-------------------------------------------------------------------------------
function method:linex(x)
   return self:lineto(x, self._y)
end   -- linex()

-------------------------------------------------------------------------------
function method:liney(y)
   return self:lineto(self._x, y)
end   -- liney()

-------------------------------------------------------------------------------
function method:moveto(x, y)
   -- assert(type(x) == 'number', 'x must be a number')
   -- assert(type(y) == 'number', 'y must be a number')

   if not self:pathopen() then
      self:newpath()
   end

   self._x = x
   self._y = y
   return self:add(string_format(self._coord_fmt2, x, y) .. "moveto")
end   -- moveto()

-------------------------------------------------------------------------------
function method:movedx(dx)
   return self:moveto(self._x + dx, self._y)
end   -- movedx()

-------------------------------------------------------------------------------
function method:movedy(dy)
   return self:moveto(self._x, self._y + dy)
end   -- movedy()

-------------------------------------------------------------------------------
function method:movex(x)
   return self:moveto(x, self._y)
end   -- movex()

-------------------------------------------------------------------------------
function method:movey(y)
   return self:moveto(self._x, y)
end   -- movey()

-------------------------------------------------------------------------------
function method:newpath()
   self:add("newpath")
   return self
end   -- newpath()

-------------------------------------------------------------------------------
function method:pathopen()
   return self._x ~= nil
end   -- pathopen()

-------------------------------------------------------------------------------
function method:rlineto(x, y)
   return self:lineto(self._x + x, self._y + y)
end   -- rlineto()

-------------------------------------------------------------------------------
function method:rmoveto(x, y)
   return self:moveto(self._x + x, self._y + y)
end   -- rmoveto()

-------------------------------------------------------------------------------
function method:setfont(name, option)
   local command = '/' .. name .. ' findfont'

   if type(option) == 'number' then
      command = command .. string_format(' %4.2f scalefont', option)
   elseif type(option) == 'table' then
      if option.scale then
         command = command .. string_format(' %4.2f scalefont', option.scale)
      end
   end

   return self:add(command .. ' setfont')
end   -- setfont()

-------------------------------------------------------------------------------
function method:setgray(frac)
   return self:add(string_format("%5.3f setgray", frac))
end   -- setgray()

-------------------------------------------------------------------------------
function method:show(s)
   return self:add(string_format("(%s) show", s:gsub("([()])", "\\%1")))
end   -- show()

-------------------------------------------------------------------------------
function method:showpage()
   if self:pathopen() then
      self:closepath()
   end

   self:add("showpage")
   self._new_page = true
   return self
end   -- moveto()

-------------------------------------------------------------------------------
function method:stroke(option)
   if type(option) == 'number' then
      self:setgray(option)
   end

   if self:pathopen() then
      self:closepath()
   end

   return self:add("stroke")
end   -- stroke()

-------------------------------------------------------------------------------
function method:writefile(file)
   local close_file = false

   if type(file) == 'string' then
      file = assert(io_open(file, 'w'))
      close_file = true
   end

   file:write("%!PS-Adobe-3.0\n");
   file:write("%%DocumentData: Clean8Bit\n");
   file:write("%%DocumentPaperSizes: Letter\n");
   file:write("%%Orientation: Portrait\n");
   file:write(("%%%%Pages %d\n"):format(self._page))
   file:write("%%PageOrder: Ascend\n");
   file:write("%%EndComments\n");
   file:write("\n");
   file:write("%%BeginProlog\n");
   file:write("%%EndProlog\n");
   file:write("\n");
   file:write(table_concat(self._line, "\n"))
   file:write("\n")

   if close_file then
      file:close()
   end
end   -- writefile()

-- Some useful shortcuts:
method.close = method.closepath
method.dx    = method.movedx
method.dy    = method.movedy
method.line  = method.lineto
method.move  = method.moveto
method.rline = method.rlineto
method.rmove = method.rmoveto
