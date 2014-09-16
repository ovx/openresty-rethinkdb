local RqlDriverError, RqlServerError, RqlRuntimeError, RqlCompileError
local RqlClientError, RqlQueryPrinter
do
  local _base_0 = { }
  _base_0.__index = _base_0
  local _class_0 = setmetatable({
    __init = function(self, msg)
      self.name = self.__name
      self.msg = msg
      self.message = msg
    end,
    __base = _base_0,
    __name = "RqlDriverError"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  RqlDriverError = _class_0
end
do
  local _base_0 = { }
  _base_0.__index = _base_0
  local _class_0 = setmetatable({
    __init = function(self, msg, term, frames)
      self.name = self.constructor.name
      self.msg = msg
      self.frames = frames
      if term then
        if msg[msg.length - 1] == '.' then
          self.message = tostring(msg.slice(0, msg.length - 1)) .. " in:\n#{RqlQueryPrinter::printQuery(term)}\n#{RqlQueryPrinter::printCarrots(term, frames)}"
        else
          self.message = tostring(msg) .. " in:\n#{RqlQueryPrinter::printQuery(term)}\n#{RqlQueryPrinter::printCarrots(term, frames)}"
        end
      else
        self.message = tostring(msg)
      end
    end,
    __base = _base_0,
    __name = "RqlServerError"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  RqlServerError = _class_0
end
do
  local _parent_0 = RqlServerError
  local _base_0 = { }
  _base_0.__index = _base_0
  setmetatable(_base_0, _parent_0.__base)
  local _class_0 = setmetatable({
    __init = function(self, ...)
      return _parent_0.__init(self, ...)
    end,
    __base = _base_0,
    __name = "RqlRuntimeError",
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
  RqlRuntimeError = _class_0
end
do
  local _parent_0 = RqlServerError
  local _base_0 = { }
  _base_0.__index = _base_0
  setmetatable(_base_0, _parent_0.__base)
  local _class_0 = setmetatable({
    __init = function(self, ...)
      return _parent_0.__init(self, ...)
    end,
    __base = _base_0,
    __name = "RqlCompileError",
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
  RqlCompileError = _class_0
end
do
  local _parent_0 = RqlServerError
  local _base_0 = { }
  _base_0.__index = _base_0
  setmetatable(_base_0, _parent_0.__base)
  local _class_0 = setmetatable({
    __init = function(self, ...)
      return _parent_0.__init(self, ...)
    end,
    __base = _base_0,
    __name = "RqlClientError",
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
  RqlClientError = _class_0
end
do
  local composeTerm, composeCarrots, carrotMarker, carrotify, joinTree
  local _base_0 = {
    printQuery = function(term)
      local tree = composeTerm(term)
      return joinTree(tree)
    end,
    printCarrots = function(term, frames)
      if frames.length == 0 then
        local tree = {
          carrotify(composeTerm(term))
        }
      else
        local tree = composeCarrots(term, frames)
      end
      return joinTree(tree).gsub('[^\^]', ' ')
    end
  }
  _base_0.__index = _base_0
  local _class_0 = setmetatable({
    __init = function(self) end,
    __base = _base_0,
    __name = "RqlQueryPrinter"
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
  composeTerm = function(term)
    local args
    do
      local _accum_0 = { }
      local _len_0 = 1
      for i, arg in ipairs(term.args) do
        _accum_0[_len_0] = composeTerm(arg)
        _len_0 = _len_0 + 1
      end
      args = _accum_0
    end
    local optargs = { }
    for key, arg in ipairs(term.optargs) do
      optargs[key] = composeTerm(arg)
    end
    return term.compose(args, optargs)
  end
  composeCarrots = function(term, frames)
    local frame = frames.shift()
    local args
    do
      local _accum_0 = { }
      local _len_0 = 1
      for arg, i in ipairs(term.args) do
        if frame == i then
          _accum_0[_len_0] = composeCarrots(arg, frames)
        else
          _accum_0[_len_0] = composeTerm(arg)
        end
        _len_0 = _len_0 + 1
      end
      args = _accum_0
    end
    local optargs = { }
    for key, arg in ipairs(term.optargs) do
      if frame == key then
        optargs[key] = composeCarrots(arg, frames)
      else
        optargs[key] = composeTerm(arg)
      end
    end
    if frame then
      return term.compose(args, optargs)
    else
      return carrotify(term.compose(args, optargs))
    end
  end
  carrotMarker = { }
  carrotify = function(tree)
    return {
      carrotMarker,
      tree
    }
  end
  joinTree = function(tree)
    local str = ''
    for _, term in ipairs(tree) do
      if Array.isArray(term) then
        if term.length == 2 and term[0] == carrotMarker then
          str = str + joinTree(term[1]).gsub('.', '^')
        else
          str = str + joinTree(term)
        end
      else
        str = str + term
      end
    end
    return str
  end
  RqlQueryPrinter = _class_0
end
return {
  RqlDriverError = RqlDriverError,
  RqlRuntimeError = RqlRuntimeError,
  RqlCompileError = RqlCompileError,
  RqlClientError = RqlClientError,
  printQuery = RqlQueryPrinter.printQuery
}
