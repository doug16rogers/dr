
-- Lua script to process droc markups in source code.

args = { ... }

TABSTOP_INCREMENT = 4
PROGRAM_COMMENT = "//"
MAX_COMMENT_WIDTH = 30

files = {}
max_filename_len = 0
max_line_len = 0
max_comment_len = 0

local function printf(...)
   local s = string.format(...)
   print(s)
   return s
end   -- printf()

local function max(a, b)
   if a > b then
      return a
   end

   return b
end   -- max()

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
   local f = assert(io.popen(string.format('grep "%s" "%s"', PROGRAM_COMMENT .. "@", filename)))

   for line in f:lines() do
      local untab = line:untabify(TABSTOP_INCREMENT)
      local comment = untab:match(PROGRAM_COMMENT .. "@{([^}]*)}")
      comment = comment or untab:match(PROGRAM_COMMENT .. "@(.*)$") or ""
      comment = comment:trim()
      -- printf("comment='%s' for '%s'", tostring(comment), untab)
      -- Allow (and remove) "//@{...}", "//@{" and "//@" as markings.
      local codeline = untab:gsub(PROGRAM_COMMENT .. "@{[^}]*}", "")
      codeline = codeline:gsub(PROGRAM_COMMENT .. "@{", "")
      codeline = codeline:gsub(PROGRAM_COMMENT .. "@.*$",  "")

      max_comment_len = max(max_comment_len, #comment)
      max_line_len    = max(max_line_len, #codeline)
      line_info[#line_info+1] = { line = codeline, comment = comment }
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

local print_comment = false
local fmt = string.format("%%%us|%%s", max_filename_len)

if max_comment_len > 0 then
   print_comment = true
   local comment_len = max_comment_len
   if comment_len > MAX_COMMENT_WIDTH then comment_len = MAX_COMMENT_WIDTH end
   fmt = string.format("%%%us|%%-%us|%%s", max_filename_len, comment_len)
end

for _, fn in ipairs(files) do
   for _, info in ipairs(files[fn]) do
      if print_comment then
         local comment = info.comment
         if #comment > MAX_COMMENT_WIDTH then comment = comment:sub(1, MAX_COMMENT_WIDTH) end
         printf(fmt, fn, comment, info.line)
         local comment_chars_printed = MAX_COMMENT_WIDTH
         while comment_chars_printed < #info.comment do
            printf(fmt, "", info.comment:sub(comment_chars_printed + 1, comment_chars_printed + MAX_COMMENT_WIDTH), "")
            comment_chars_printed = comment_chars_printed + MAX_COMMENT_WIDTH
         end
      else
         printf(fmt, fn, info.line)
      end
   end
end

