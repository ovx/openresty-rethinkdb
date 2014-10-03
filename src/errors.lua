local util = require('./util')

-- Import some names to this namespace for convienience
local is_instance = util.is_instance
local class = util.class

local ReQLDriverError, ReQLServerError, ReQLRuntimeError, ReQLCompileError
local ReQLClientError, ReQLQueryPrinter

ReQLDriverError = class(
  'ReQLDriverError',
  function(self, msg)
    self.msg = msg
    self.message = self.__class.__name .. ' ' .. msg
  end
)

ReQLServerError = class(
  'ReQLServerError',
  function(self, msg, term, frames)
    self.msg = msg
    if term then
      local printer = ReQLQueryPrinter(term, frames)
      self.message = self.__class.__name .. ' ' .. msg .. ' in:\n' .. printer:print_query() .. '\n' .. printer:print_carrots()
    else
      self.message = self.__class.__name .. ' ' .. msg
    end
  end
)

ReQLRuntimeError = class('ReQLRuntimeError', ReQLServerError, {})
ReQLCompileError = class('ReQLCompileError', ReQLServerError, {})
ReQLClientError = class('ReQLClientError', ReQLServerError, {})

ReQLQueryPrinter = class(
  'ReQLQueryPrinter',
  {
    __init = function(self, term, frames)
      self.term = term
      self.frames = frames
    end,
    print_query = function(self)
      return self:join_tree(self:compose_term(self.term))
    end,
    print_carrots = function(self)
      local tree
      if #self.frames == 0 then
        tree = {
          self:carrotify(self:compose_term(self.term))
        }
      else
        tree = self:compose_carrots(self.term, self.frames)
      end
      return self:join_tree(tree):gsub('[^%^]', ' ')
    end,
    compose_term = function(self, term)
      if type(term) ~= 'table' then return '' .. term end
      local args = {}
      for i, arg in ipairs(term.args) do
        args[i] = self:compose_term(arg)
      end
      local optargs = { }
      for key, arg in ipairs(term.optargs) do
        optargs[key] = self:compose_term(arg)
      end
      return term:compose(args, optargs)
    end,
    compose_carrots = function(self, term, frames)
      local frame = table.remove(frames, 1)
      local args = {}
      for arg, i in ipairs(term.args) do
        if frame == i then
          args[i] = self:compose_carrots(arg, frames)
        else
          args[i] = self:compose_term(arg)
        end
      end
      local optargs = { }
      for key, arg in ipairs(term.optargs) do
        if frame == key then
          optargs[key] = self:compose_carrots(arg, frames)
        else
          optargs[key] = self:compose_term(arg)
        end
      end
      if frame then
        return term.compose(args, optargs)
      end
      return self:carrotify(term.compose(args, optargs))
    end,
    carrot_marker = { },
    carrotify = function(self, tree)
      return {carrot_marker, tree}
    end,
    join_tree = function(self, tree)
      local str = ''
      for _, term in ipairs(tree) do
        if type(term) == 'table' then
          if #term == 2 and term[0] == self.carrot_marker then
            str = str .. self:join_tree(term[1]):gsub('.', '^')
          else
            str = str .. self:join_tree(term)
          end
        else
          str = str .. term
        end
      end
      return str
    end
  }
)

return {
  ReQLDriverError = ReQLDriverError,
  ReQLRuntimeError = ReQLRuntimeError,
  ReQLCompileError = ReQLCompileError,
  ReQLClientError = ReQLClientError
}
