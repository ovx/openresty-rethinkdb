local ReQLDriverError, ReQLServerError, ReQLRuntimeError, ReQLCompileError
local ReQLClientError, ReQLQueryPrinter

local convert_pseudotype, recursively_convert_pseudotype, mk_atom, mk_seq
local is_array, is_instance

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
        self.message = " in:\n" .. self.printer:print_query() .. "\n" .. self.printer:print_carrots()
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
      return self:join_tree(tree).gsub('[^\^]', ' ')
    end,
    compose_term = function(self, term)
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
      local frame = frames.shift()
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
        if is_array(term) then
          if #term == 2 and term[0] == self.carrot_marker then
            str = str .. self:join_tree(term[1]).gsub('.', '^')
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

function convert_pseudotype(obj, opts)
  -- An R_OBJECT may be a regular object or a "pseudo-type" so we need a
  -- second layer of type switching here on the obfuscated field "$reql_type$"
  local _exp_0 = obj['$reql_type$']
  if 'TIME' == _exp_0 then
    local _exp_1 = opts.time_format
    if 'native' == _exp_1 or not _exp_1 then
      if not (obj['epoch_time']) then
        error(err.ReQLDriverError("pseudo-type TIME " .. tostring(obj) .. " object missing expected field 'epoch_time'."))
      end

      -- We ignore the timezone field of the pseudo-type TIME object. JS dates do not support timezones.
      -- By converting to a native date object we are intentionally throwing out timezone information.

      -- field "epoch_time" is in seconds but the Date constructor expects milliseconds
      return (Date(obj['epoch_time'] * 1000))
    elseif 'raw' == _exp_1 then
      -- Just return the raw (`{'$reql_type$'...}`) object
      return obj
    else
      error(err.ReQLDriverError("Unknown time_format run option " .. tostring(opts.time_format) .. "."))
    end
  elseif 'GROUPED_DATA' == _exp_0 then
    local _exp_1 = opts.group_format
    if 'native' == _exp_1 or not _exp_1 then
      -- Don't convert the data into a map, because the keys could be objects which doesn't work in JS
      -- Instead, we have the following format:
      -- [ { 'group': <group>, 'reduction': <value(s)> } }, ... ]
      res = {}
      j = 1
      for i, v in ipairs(obj['data']) do
        res[j] = {
          group = i,
          reduction = v
        }
        j = j + 1
      end
      obj = res
    elseif 'raw' == _exp_1 then
      return obj
    else
      error(err.ReQLDriverError("Unknown group_format run option " .. tostring(opts.group_format) .. "."))
    end
  elseif 'BINARY' == _exp_0 then
    local _exp_1 = opts.binary_format
    if 'native' == _exp_1 or not _exp_1 then
      if not (obj['data']) then
        error(err.ReQLDriverError("pseudo-type BINARY object missing expected field 'data'."))
      end
      return (Buffer(obj['data'], 'base64'))
    elseif 'raw' == _exp_1 then
      return obj
    else
      error(err.ReQLDriverError("Unknown binary_format run option " .. tostring(opts.binary_format) .. "."))
    end
  else
    -- Regular object or unknown pseudo type
    return obj
  end
end

function recursively_convert_pseudotype(obj, opts)
  if type(obj) == 'table' then
    for key, value in ipairs(obj) do
      obj[key] = recursively_convert_pseudotype(value, opts)
    end
    obj = convert_pseudotype(obj, opts)
  end
  return obj
end

function mk_atom(response, opts)
  return recursively_convert_pseudotype(response.r[0], opts)
end

function mk_seq(response, opts)
  return recursively_convert_pseudotype(response.r, opts)
end

function is_array(obj)
  if type(obj) ~= 'table' or obj:maxn() == 0 then return false end
  for k, v in pairs(obj) do
    if type(k) ~= 'number' then return false end
  end
  return true
end

function is_instance(class, obj)
  if type(obj) ~= 'table' then return false end
  local obj_cls = obj.__class
  while obj_cls do
    if obj_cls == class then
      return true
    end
    obj_cls = obj_cls.__parent
  end
  return false
end

return {
  ReQLDriverError = ReQLDriverError,
  ReQLRuntimeError = ReQLRuntimeError,
  ReQLCompileError = ReQLCompileError,
  ReQLClientError = ReQLClientError,
  mk_atom = mk_atom,
  mk_seq = mk_seq,
  is_array = is_array,
  is_instance = is_instance
}
