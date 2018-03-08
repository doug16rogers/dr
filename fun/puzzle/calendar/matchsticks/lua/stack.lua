local assert = assert
local setmetatable = setmetatable

module('stack')

local stack = {}
local mt = { __index = stack }

function new()
   local self = {}
   self.stack = {}
   return setmetatable(self, mt)
end   -- new()

function stack:count()
   return #self.stack
end   -- stack:count()

function stack:push(v)
   assert(v ~= nil)
   self.stack[#self.stack+1] = v
   return self
end   -- stack:push()

function stack:pop()
   local v = self.stack[#self.stack]
   if #self.stack > 0 then
      self.stack[#self.stack] = nil
   end
   return v
end   -- stack:pop()

