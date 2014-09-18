local ReQLDriverError, ReQLServerError, ReQLRuntimeError, ReQLCompileError
local ReQLClientError, ReQLQueryPrinter
do
  local _base_0 = { }
  _base_0.__index = _base_0
  local _class_0 = setmetatable({
    __init = function(self, msg)
      self.msg = msg
      self.message = msg
    end,
    __base = _base_0,
    __name = "ReQLDriverError"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  ReQLDriverError = _class_0
end
do
  local _base_0 = { }
  _base_0.__index = _base_0
  local _class_0 = setmetatable({
    __init = function(self, msg, term, frames)
      self.msg = msg
      self.frames = frames
      self.printer = ReQLQueryPrinter(term, frames)
      if term then
        self.message = " in:\n" .. self.printer:printQuery() .. "\n" .. self.printer:printCarrots()
        if msg[-1] == '.' then
          self.message = msg:sub(1, -2) .. self.message
        else
          self.message = msg .. self.message
        end
      else
        self.message = msg
      end
    end,
    __base = _base_0,
    __name = "ReQLServerError"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  ReQLServerError = _class_0
end
do
  local _parent_0 = ReQLServerError
  local _base_0 = { }
  _base_0.__index = _base_0
  setmetatable(_base_0, _parent_0.__base)
  local _class_0 = setmetatable({
    __init = function(self, ...)
      return _parent_0.__init(self, ...)
    end,
    __base = _base_0,
    __name = "ReQLRuntimeError",
    __parent = _parent_0
  }, {
    __index = function(cls, name)
      local val = rawget(_base_0, name)
      if val == nil then
        return _parent_0[name]
      else
        return val
      end
    end,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  if _parent_0.__inherited then
    _parent_0.__inherited(_parent_0, _class_0)
  end
  ReQLRuntimeError = _class_0
end
do
  local _parent_0 = ReQLServerError
  local _base_0 = { }
  _base_0.__index = _base_0
  setmetatable(_base_0, _parent_0.__base)
  local _class_0 = setmetatable({
    __init = function(self, ...)
      return _parent_0.__init(self, ...)
    end,
    __base = _base_0,
    __name = "ReQLCompileError",
    __parent = _parent_0
  }, {
    __index = function(cls, name)
      local val = rawget(_base_0, name)
      if val == nil then
        return _parent_0[name]
      else
        return val
      end
    end,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  if _parent_0.__inherited then
    _parent_0.__inherited(_parent_0, _class_0)
  end
  ReQLCompileError = _class_0
end
do
  local _parent_0 = ReQLServerError
  local _base_0 = { }
  _base_0.__index = _base_0
  setmetatable(_base_0, _parent_0.__base)
  local _class_0 = setmetatable({
    __init = function(self, ...)
      return _parent_0.__init(self, ...)
    end,
    __base = _base_0,
    __name = "ReQLClientError",
    __parent = _parent_0
  }, {
    __index = function(cls, name)
      local val = rawget(_base_0, name)
      if val == nil then
        return _parent_0[name]
      else
        return val
      end
    end,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  if _parent_0.__inherited then
    _parent_0.__inherited(_parent_0, _class_0)
  end
  ReQLClientError = _class_0
end
do
  local _base_0 = {
    printQuery = function(self)
      return self:joinTree(self:composeTerm(self.term))
    end,
    printCarrots = function(self)
      local tree
      if self.frames.length == 0 then
        tree = {
          self:carrotify(self:composeTerm(self.term))
        }
      else
        tree = self:composeCarrots(self.term, self.frames)
      end
      return self:joinTree(tree).gsub('[^\^]', ' ')
    end,
    composeTerm = function(self, term)
      local args = {}
      for i, arg in ipairs(term.args) do
        args[i] = self:composeTerm(arg)
      end
      local optargs = { }
      for key, arg in ipairs(term.optargs) do
        optargs[key] = self:composeTerm(arg)
      end
      return term:compose(args, optargs)
    end,
    composeCarrots = function(self, term, frames)
      local frame = frames.shift()
      local args = {}
      for arg, i in ipairs(term.args) do
        if frame == i then
          args[i] = self:composeCarrots(arg, frames)
        else
          args[i] = self:composeTerm(arg)
        end
      end
      local optargs = { }
      for key, arg in ipairs(term.optargs) do
        if frame == key then
          optargs[key] = self:composeCarrots(arg, frames)
        else
          optargs[key] = self:composeTerm(arg)
        end
      end
      if frame then
        return term.compose(args, optargs)
      end
      return self:carrotify(term.compose(args, optargs))
    end,
    carrotMarker = { },
    carrotify = function(self, tree)
      return {carrotMarker, tree}
    end,
    joinTree = function(self, tree)
      local str = ''
      for _, term in ipairs(tree) do
        if Array.isArray(term) then
          if term.length == 2 and term[0] == self.carrotMarker then
            str = str .. self:joinTree(term[1]).gsub('.', '^')
          else
            str = str .. self:joinTree(term)
          end
        else
          str = str .. term
        end
      end
      return str
    end
  }
  _base_0.__index = _base_0
  local _class_0 = setmetatable({
    __init = function(self, term, frames)
      self.term = term
      self.frames = frames
    end,
    __base = _base_0,
    __name = "ReQLQueryPrinter"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  local self = _class_0
  ReQLQueryPrinter = _class_0
end
return {
  ReQLDriverError = ReQLDriverError,
  ReQLRuntimeError = ReQLRuntimeError,
  ReQLCompileError = ReQLCompileError,
  ReQLClientError = ReQLClientError,
}
