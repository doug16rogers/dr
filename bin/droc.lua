
-- Lua script to process droc markups in source code.

args = { ... }

TABSTOP_INCREMENT = 4
PROGRAM_COMMENT = "//"
MAX_COMMENT_WIDTH = 24
HEADER_LINE_TEXT_LEN = 100

local printf = function(...) print(string.format(...)) end
local max    = function(a, b) return (a > b) and a or b end
local min    = function(a, b) return (a < b) and a or b end

files = {}
max_filename_len = 0
max_line_number_len = 0
max_line_text_len = 0
max_comment_len = 0

-- Replace tabs with the appropriate number of spaces.
-- Make this a string method.
function string:untabify(inc)
   inc = inc or 4
   local t = {}

   for i = 1, #self do
      local c = self:sub(i,i)

      if c == '\t' then
         t[#t+1] = ' '

	 while (#t % inc) ~= 0 do
            t[#t+1] = ' '
         end
      else
         t[#t+1] = c
      end
   end

   return table.concat(t)
end   -- string.untabify()

-- Trims all whitespsect at the ends.
function string:trim()
   return self:match("^%s*([%S].*[%S])%s*$") or ""
end   -- string:trim()

-- Removes leading path from filename.
function string:basename()
   local base = self:match('^.*[/\\]([^/\\]+)$')
   return base or self
end   -- string:basename()

local function handle_file(filename)
   local line_info = {}
   local f = assert(io.popen(string.format('grep -n "%s" "%s"', PROGRAM_COMMENT .. "@", filename)))

   for line in f:lines() do
      local line_number, untab = line:match("^([%d]+):(.*)$")
      line_number = line_number:trim()
      untab = untab:untabify(TABSTOP_INCREMENT)
      local comment = untab:match(PROGRAM_COMMENT .. "@{([^}]*)}")
      comment = comment or untab:match(PROGRAM_COMMENT .. "@(.*)$") or ""
      comment = comment:trim()
      -- printf("comment='%s' for '%s'", tostring(comment), untab)
      -- Allow (and remove) "//@{...}", "//@{" and "//@" as markings.
      local line_text = untab:gsub(PROGRAM_COMMENT .. "@{[^}]*}", "")
      line_text = line_text:gsub(PROGRAM_COMMENT .. "@{", "")
      line_text = line_text:gsub(PROGRAM_COMMENT .. "@.*$",  "")

      max_line_number_len = max(max_line_number_len, #line_number)
      max_comment_len = max(max_comment_len, #comment)
      max_line_text_len = max(max_line_text_len, #line_text)
      line_info[#line_info+1] = {
         line_number = line_number,  -- Note: This is a string.
         line_text = line_text,
         comment = comment,
      }
   end

   f:close()

   if #line_info > 0 then
      local basename = filename:basename()
      files[#files+1] = basename
      files[basename] = line_info
      max_filename_len = max(max_filename_len, #basename)
   end
end

for _, fn in ipairs(args) do
   handle_file(fn)
end

local rep = string.rep

local function print_filename(filename, max_comment, max_number)
   if max_comment == 0 then
      printf("+%s+%s", rep('=', max_number), rep('=', HEADER_LINE_TEXT_LEN))
      printf("|%s|%s", rep('#', max_number), filename)
      printf("+%s+%s", rep('-', max_number), rep('-', HEADER_LINE_TEXT_LEN))
   else
      max_comment = min(max_comment, MAX_COMMENT_WIDTH)
      printf("+%s+%s+%s", rep('=', max_comment), rep('=', max_number), rep('=', HEADER_LINE_TEXT_LEN))
      printf("|%s|%s|%s", rep(' ', max_comment), rep('#', max_number), filename)
      printf("+%s+%s+%s", rep('-', max_comment), rep('-', max_number), rep('-', HEADER_LINE_TEXT_LEN))
   end
end   -- print_filename()

local function print_line_info(info, max_comment, max_number)
   if max_comment == 0 then
      local fmt = string.format('|%%%us|%%s', max_number)
      printf(fmt, info.line_number, info.line_text)
   else
      max_comment = min(max_comment, MAX_COMMENT_WIDTH)
      local fmt = string.format('|%%-%us|%%%us|%%s', max_comment, max_number)
      local comment = info.comment:sub(1, max_comment)
      printf(fmt, comment, info.line_number, info.line_text)

      local len_printed = max_comment

      while len_printed < #info.comment do
         printf(fmt, info.comment:sub(len_printed + 1, len_printed + max_comment), "", "")
         len_printed = len_printed + max_comment
      end
   end
end   -- print_line_info()

for _, fn in ipairs(files) do
   print_filename(fn, max_comment_len, max_line_number_len)

   for _, info in ipairs(files[fn]) do
      print_line_info(info, max_comment_len, max_line_number_len)
   end
end
