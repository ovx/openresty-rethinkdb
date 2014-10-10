local json = require('json')
local mime = require('mime')
local socket = require('socket')

-- r is both the main export table for the module
-- and a function that wraps a native Lua value in an ReQL datum
local r = {}

local Connection, Cursor
local DatumTerm, ReQLOp
--[[AstNames]]
local ReQLDriverError, ReQLServerError, ReQLRuntimeError, ReQLCompileError
local ReQLClientError, ReQLQueryPrinter, ReQLError

setmetatable(r, {
  __call = function(cls, val, nesting_depth)
    if nesting_depth == nil then
      nesting_depth = 20
    end
    if type(nesting_depth) ~= 'number' then
      error('Second argument to `r(val, nesting_depth)` must be a number.')
    end
    if nesting_depth <= 0 then
      error('Nesting depth limit exceeded')
    end
    if is_instance(val, ReQLOp) then
      return val
    end
    if type(val) == 'function' then
      return Func({}, val)
    end
    if type(val) == 'table' then
      local array = true
      for k, v in pairs(val) do
        if type(k) ~= 'number' then array = false end
        val[k] = r(v, nesting_depth - 1)
      end
      if array then
        return MakeArray({}, unpack(val))
      end
      return MakeObj(val)
    end
    return DatumTerm(val)
  end
})

function class(name, parent, base)
  local index, init

  if base == nil then
    base = parent
    parent = nil
  end

  if type(base) == 'function' then
    base = {__init = base}
  end

  if parent and parent.__base then
    setmetatable(base, parent.__base)
  else
    index = base
  end

  init = base.__init
  base.__init = nil
  base.__index = base

  local _class_0 = setmetatable({
    __name = name,
    __init = init,
    __base = base,
    __parent = parent
  }, {
    __index = index or function(cls, name)
      local val = rawget(base, name)
      if val == nil then
        return parent[name]
      else
        return val
      end
    end,
    __call = function(cls, ...)
      local self = setmetatable({}, cls.__base)
      cls.__init(self, ...)
      return self
    end
  })
  base.__class = _class_0

  if parent and parent.__inherited then
    parent:__inherited(_class_0)
  end

  return _class_0
end

function is_instance(obj, ...)
  local class_list = {...}

  for _, class in ipairs(class_list) do
    if type(class) == 'string' and type(obj) == class then
      return true
    else
      class = class.__name
    end

    if type(obj) == 'table' then
      local obj_cls = obj.__class
      while obj_cls do
        if obj_cls.__name == class then
          return true
        end
        obj_cls = obj_cls.__parent
      end
    end
  end

  return false
end

function intsp(seq)
  if seq[1] == nil then
    return {}
  end
  local res = {seq[1]}
  for i=2, #seq do
    table.insert(res, ', ')
    table.insert(res, seq[i])
  end
  return res
end

function kved(optargs)
  return {
    '{',
    intsp((function()
      local _accum_0 = {}
      local i = 1
      for k, v in pairs(optargs) do
        _accum_0[i] = {k, ': ', v}
        i = i + 1
      end
      return _accum_0
    end)()),
    '}'
  }
end

function intspallargs(args, optargs)
  local argrepr = {}
  if #args > 0 then
    table.insert(argrepr, intsp(args))
  end
  if optargs and #optargs > 0 then
    if #argrepr > 0 then
      table.insert(argrepr, ', ')
    end
    table.insert(argrepr, kved(optargs))
  end
  return argrepr
end

function should_wrap(arg)
  return is_instance(arg, DatumTerm, MakeArray, MakeObj)
end

function get_opts(...)
  local args = {...}
  local opt = {}
  local pos_opt = args[-1]
  if (type(pos_opt) == 'table') and (not is_instance(pos_opt, ReQLOp)) then
    opt = pos_opt
    args[-1] = nil
  end
  return opt, unpack(args)
end

function bytes_to_int(str)
  local t = {str:byte(1,-1)}
  local n = 0
  for k=1,#t do
    n = n + t[k] * 2 ^ ((k - 1) * 8)
  end
  return n
end

function div_mod(num, den)
  return math.floor(num / den), math.fmod(num, den)
end

function int_to_bytes(num, bytes)
  local res = {}
  local mul = 0
  for k=bytes,1,-1 do
    res[k], num = div_mod(num, 2 ^ (8 * (k - 1)))
  end
  return string.char(unpack(res))
end

function convert_pseudotype(obj, opts)
  -- An R_OBJECT may be a regular table or a 'pseudo-type' so we need a
  -- second layer of type switching here on the obfuscated field '$reql_type$'
  local reql_type = obj['$reql_type$']
  if 'TIME' == reql_type then
    local time_format = opts.time_format
    if 'native' == time_format or not time_format then
      if not (obj['epoch_time']) then
        error(ReQLDriverError('pseudo-type TIME ' .. obj .. ' table missing expected field `epoch_time`.'))
      end

      -- We ignore the timezone field of the pseudo-type TIME table. JS dates do not support timezones.
      -- By converting to a native date table we are intentionally throwing out timezone information.

      -- field 'epoch_time' is in seconds but the Date constructor expects milliseconds
      return obj['epoch_time']
    elseif 'raw' == time_format then
      return obj
    else
      error(ReQLDriverError('Unknown time_format run option ' .. opts.time_format .. '.'))
    end
  elseif 'GROUPED_DATA' == reql_type then
    local group_format = opts.group_format
    if 'native' == group_format or not group_format then
      -- Don't convert the data into a map, because the keys could be tables which doesn't work in JS
      -- Instead, we have the following format:
      -- [ { 'group': <group>, 'reduction': <value(s)> } }, ... ]
      res = {}
      for i, v in ipairs(obj['data']) do
        res[i] = {
          group = i,
          reduction = v
        }
      end
      obj = res
    elseif 'raw' == group_format then
      return obj
    else
      error(ReQLDriverError('Unknown group_format run option ' .. opts.group_format .. '.'))
    end
  elseif 'BINARY' == reql_type then
    local binary_format = opts.binary_format
    if 'native' == binary_format or not binary_format then
      if not obj.data then
        error(ReQLDriverError('pseudo-type BINARY table missing expected field `data`.'))
      end
      return mime.unb64(obj.data)
    elseif 'raw' == binary_format then
      return obj
    else
      error(ReQLDriverError('Unknown binary_format run option ' .. opts.binary_format .. '.'))
    end
  else
    -- Regular table or unknown pseudo type
    return obj
  end
end

function recursively_convert_pseudotype(obj, opts)
  if type(obj) == 'table' then
    for key, value in pairs(obj) do
      obj[key] = recursively_convert_pseudotype(value, opts)
    end
    obj = convert_pseudotype(obj, opts)
  end
  if obj == json.util.null then return nil end
  return obj
end

ReQLError = class(
  'ReQLError',
  function(self, msg, term, frames)
    self.msg = msg
    self.message = self.__class.__name .. ' ' .. msg
    if term then
      self.message = self.message .. ' in:\n' .. ReQLQueryPrinter(term, frames):print_query()
    end
  end
)

ReQLDriverError = class('ReQLDriverError', ReQLError, {})

ReQLServerError = class('ReQLServerError', ReQLError, {})

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
      local carrots
      if #self.frames == 0 then
        carrots = {self:carrotify(self:compose_term(self.term))}
      else
        carrots = self:compose_carrots(self.term, self.frames)
      end
      carrots = self:join_tree(carrots):gsub('[^%^]', '')
      return self:join_tree(self:compose_term(self.term)) .. '\n' .. carrots
    end,
    compose_term = function(self, term)
      if type(term) ~= 'table' then return '' .. term end
      local args = {}
      for i, arg in ipairs(term.args) do
        args[i] = self:compose_term(arg)
      end
      local optargs = {}
      for key, arg in pairs(term.optargs) do
        optargs[key] = self:compose_term(arg)
      end
      return term:compose(args, optargs)
    end,
    compose_carrots = function(self, term, frames)
      local frame = table.remove(frames, 1)
      local args = {}
      for i, arg in ipairs(term.args) do
        if frame == (i - 1) then
          args[i] = self:compose_carrots(arg, frames)
        else
          args[i] = self:compose_term(arg)
        end
      end
      local optargs = {}
      for key, arg in pairs(term.optargs) do
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
    carrot_marker = {},
    carrotify = function(self, tree)
      return {carrot_marker, tree}
    end,
    join_tree = function(self, tree)
      local str = ''
      for _, term in ipairs(tree) do
        if type(term) == 'table' then
          if #term == 2 and term[1] == self.carrot_marker then
            str = str .. self:join_tree(term[2]):gsub('.', '^')
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

ast_methods = {
  run = function(self, connection, options, callback)
    -- Valid syntaxes are
    -- connection
    -- connection, callback
    -- connection, options, callback
    -- connection, nil, callback

    -- Handle run(connection, callback)
    if type(options) == 'function' then
      if callback then
        return error('Second argument to `run` cannot be a function if a third argument is provided.')
      end
      callback = options
      options = {}
    end
    -- else we suppose that we have run(connection[, options][, callback])

    if not is_instance(connection, 'Connection') then
      if callback then
        return callback(ReQLDriverError('First argument to `run` must be a connection.'))
      end
      error('First argument to `run` must be a connection.')
    end

    return connection:_start(self, callback, options or {})
  end,
  --[[AstMethods]]
}

class_methods = {
  __init = function(self, optargs, ...)
    optargs = optargs or {}
    self.args = {...}
    local first = self.args[1]
    if self.tt == --[[Term.FUNC]] then
      local args = {}
      local arg_nums = {}
      for i=1, optargs.arity or 1 do
        table.insert(arg_nums, ReQLOp.next_var_id)
        table.insert(args, Var({}, ReQLOp.next_var_id))
        ReQLOp.next_var_id = ReQLOp.next_var_id + 1
      end
      first = first(unpack(args))
      if first == nil then
        error('Anonymous function returned `nil`. Did you forget a `return`?')
      end
      optargs.arity = nil
      self.args = {MakeArray({}, unpack(arg_nums)), r(first)}
    elseif self.tt == --[[Term.BINARY]] then
      if is_instance(first, ReQLOp) then
      elseif type(first) == 'string' then
        self.base64_data = mime.b64(first)
      else
        error('Parameter to `r.binary` must be a string or ReQL query.')
      end
    elseif self.tt == --[[Term.FUNCALL]] then
      self.args[-1] = Func({arity = args.n - 1}, self.args[-1])
      table.insert(self.args, table.remove(self.args, -1), 1)
      for i, a in ipairs(self.args) do
        self.args[i] = r(a)
      end
    elseif self.tt == --[[Term.REDUCE]] then
      self.args[-1] = Func({arity = 2}, self.args[-1])
      for i, a in ipairs(self.args) do
        self.args[i] = r(a)
      end
    else
      for i, a in ipairs(self.args) do
        self.args[i] = r(a)
      end
    end
    self.optargs = optargs
  end,
  build = function(self)
    if self.tt == --[[Term.BINARY]] and (not self.args[1]) then
      return {
        ['$reql_type$'] = 'BINARY',
        data = self.base64_data
      }
    end
    if self.tt == --[[Term.MAKE_OBJ]] then
      local res = {}
      for key, val in pairs(self.optargs) do
        res[key] = val:build()
      end
      return res
    end
    local args = {}
    for i, arg in ipairs(self.args) do
      args[i] = arg:build()
    end
    res = {self.tt, args}
    if #self.optargs > 0 then
      local opts = {}
      for key, val in pairs(self.optargs) do
        opts[key] = val:build()
      end
      table.insert(res, opts)
    end
    return res
  end,
  compose = function(self, args, optargs)
    if self.tt == --[[Term.MAKE_ARRAY]] then
      return {
        '{',
        intsp(args),
        '}'
      }
    end
    if self.tt == --[[Term.MAKE_OBJ]] then
      return kved(optargs)
    end
    if self.tt == --[[Term.VAR]] then
      if not args then return {} end
      for i, v in ipairs(args) do
        args[i] = 'var_' .. v
      end
      return args
    end
    if self.tt == --[[Term.BINARY]] then
      if self.args[1] then
        return {
          'r.binary(',
          intspallargs(args, optargs),
          ')'
        }
      else
        return 'r.binary(<data>)'
      end
    end
    if self.tt == --[[Term.IMPLICIT_VAR]] then
      return {
        'r.row'
      }
    end
    if self.tt == --[[Term.TABLE]] then
      if is_instance(self.args[1], Db) then
        return {
          args[1],
          ':table(',
          intspallargs((function()
            local _accum_0 = {}
            for _index_0 = 2, #args do
              _accum_0[_index_0 - 1] = args[_index_0]
            end
            return _accum_0
          end)(), optargs),
          ')'
        }
      else
        return {
          'r.table(',
          intspallargs(args, optargs),
          ')'
        }
      end
    end
    if self.tt == --[[Term.BRACKET]] then
      return {
        args[1],
        '(',
        args[2],
        ')'
      }
    end
    if self.tt == --[[Term.FUNC]] then
      if ivar_scan(self.args[2]) then
        return {
          args[2]
        }
      end
      return {
        'function(',
        intsp(args[1]),
        ') return ',
        args[2],
        ' end'
      }
    end
    if self.tt == --[[Term.FUNCALL]] then
      if #args > 2 then
        return {
          'r.do_(',
          intsp((function()
            local _accum_0 = {}
            for i = 2, #args do
              _accum_0[i - 1] = args[i]
            end
            return _accum_0
          end)()),
          ', ',
          args[1],
          ')'
        }
      end
      if should_wrap(self.args[1]) then
        args[1] = {
          'r(',
          args[1],
          ')'
        }
      end
      return {
        args[2],
        '.do_(',
        args[1],
        ')'
      }
    end
    if should_wrap(self.args[1]) then
      args[1] = {
        'r(',
        args[1],
        ')'
      }
    end
    return {
      args[1],
      ':',
      self.st,
      '(',
      intspallargs((function()
        local _accum_0 = {}
        for _index_0 = 2, #args do
          _accum_0[_index_0 - 1] = args[_index_0]
        end
        return _accum_0
      end)(), optargs),
      ')'
    }
  end,
  next_var_id = 0,
}

for name, meth in pairs(ast_methods) do
  class_methods[name] = meth
  r[name] = meth
end

-- AST classes

ReQLOp = class('ReQLOp', class_methods)


local meta = {
  __call = function(...)
    return Bracket({}, ...)
  end,
  __add = function(...)
    return Add({}, ...)
  end,
  __mul = function(...)
    return Mul({}, ...)
  end,
  __mod = function(...)
    return Mod({}, ...)
  end,
  __sub = function(...)
    return Sub({}, ...)
  end,
  __div = function(...)
    return Div({}, ...)
  end
}

function ast(name, base)
  for k, v in pairs(meta) do
    base[k] = v
  end
  return class(name, ReQLOp, base)
end

DatumTerm = ast(
  'DatumTerm',
  {
    __init = function(self, val)
      if type(val) == 'number' then
        if math.abs(val) == math.huge or val == math.huge * 0 then
          error('Illegal non-finite number `' .. val .. '`.')
        end
      end
      self.data = val
    end,
    args = {},
    optargs = {},
    compose = function(self)
      if type(self.data) == 'string' then
        return '"' .. self.data .. '"'
      end
      if self.data == nil then
        return 'nil'
      end
      return '' .. self.data
    end,
    build = function(self)
      if self.data == nil then return json.util.null end
      return self.data
    end
  }
)

--[[AstClasses]]

-- All top level exported functions

function r.js(...)
  return JavaScript(get_opts(...))
end
function r.http(...)
  return Http(get_opts(...))
end
function r.json(...)
  return Json({}, ...)
end
function r.error(...)
  return Error({}, ...)
end
function r.random(...)
  return Random(get_opts(...))
end
function r.binary(...)
  return Binary({}, ...)
end
function r.table(...)
  return Table(get_opts(...))
end
function r.db(...)
  return Db({}, ...)
end
function r.db_create(...)
  return DbCreate({}, ...)
end
function r.db_drop(...)
  return DbDrop({}, ...)
end
function r.db_list(...)
  return DbList({}, ...)
end
function r.table_create(...)
  return TableCreate(get_opts(...))
end
function r.table_drop(...)
  return TableDrop({}, ...)
end
function r.table_list(...)
  return TableList({}, ...)
end
function r.do_(...)
  return FunCall({}, ...)
end
function r.branch(...)
  return Branch({}, ...)
end
function r.asc(...)
  return Asc({}, ...)
end
function r.desc(...)
  return Desc({}, ...)
end
function r.eq(...)
  return Eq({}, ...)
end
function r.ne(...)
  return Ne({}, ...)
end
function r.lt(...)
  return Lt({}, ...)
end
function r.le(...)
  return Le({}, ...)
end
function r.gt(...)
  return Gt({}, ...)
end
function r.ge(...)
  return Ge({}, ...)
end
function r.any(...)
  return Any({}, ...)
end
function r.all(...)
  return All({}, ...)
end
function r.not_(...)
  return Not({}, ...)
end
function r.add(...)
  return Add({}, ...)
end
function r.sub(...)
  return Sub({}, ...)
end
function r.div(...)
  return Div({}, ...)
end
function r.mul(...)
  return Mul({}, ...)
end
function r.mod(...)
  return Mod({}, ...)
end
function r.type_of(...)
  return TypeOf({}, ...)
end
function r.info(...)
  return Info({}, ...)
end
function r.literal(...)
  return Literal({}, ...)
end
function r.iso8601(...)
  return ISO8601(get_opts(...))
end
function r.epoch_time(...)
  return EpochTime({}, ...)
end
function r.now(...)
  return Now({}, ...)
end
function r.time(...)
  return Time({}, ...)
end

r.monday = Monday()
r.tuesday = Tuesday()
r.wednesday = Wednesday()
r.thursday = Thursday()
r.friday = Friday()
r.saturday = Saturday()
r.sunday = Sunday()

r.january = January()
r.february = February()
r.march = March()
r.april = April()
r.may = May()
r.june = June()
r.july = July()
r.august = August()
r.september = September()
r.october = October()
r.november = November()
r.december = December()

function r.object(...)
  return Object({}, ...)
end
function r.args(...)
  return Args({}, ...)
end
function r.geo_json(...)
  return GeoJson({}, ...)
end
function r.point(...)
  return Point({}, ...)
end
function r.line(...)
  return Line({}, ...)
end
function r.polygon(...)
  return Polygon({}, ...)
end
function r.intersects(...)
  return Intersects({}, ...)
end
function r.distance(...)
  return Distance(get_opts(...))
end
function r.circle(...)
  return Circle(get_opts(...))
end
function r.uuid(...)
  return UUID({}, ...)
end

Cursor = class(
  'Cursor',
  {
    __init = function(self, conn, token, opts, root)
      self._conn = conn
      self._token = token
      self._opts = opts
      self._root = root -- current query
      self._responses = {}
      self._response_index = 1
      self._cont_flag = true
    end,
    _add_response = function(self, response)
      local t = response.t
      if not self._type then self._type = t end
      if response.r[1] or t == --[[Response.WAIT_COMPLETE]] then
        table.insert(self._responses, response)
      end
      if t ~= --[[Response.SUCCESS_PARTIAL]] and t ~= --[[Response.SUCCESS_FEED]] then
        -- We got an error, SUCCESS_SEQUENCE, WAIT_COMPLETE, or a SUCCESS_ATOM
        self._end_flag = true
        self._conn:_del_query(self._token)
      end
      self._cont_flag = false
    end,
    _prompt_cont = function(self)
      if self._end_flag then return end
      -- Let's ask the server for more data if we haven't already
      if not self._cont_flag then
        self._cont_flag = true
        self._conn:_continue_query(self._token)
      end
      self._conn:_get_response(self._token)
    end,
    -- Implement IterableResult
    next = function(self, callback)
      -- Try to get a row out of the responses
      while not self._responses[1] do
        if self._end_flag then
          return cb(ReQLDriverError('No more rows in the cursor.'))
        end
        self:_prompt_cont()
      end
      local response = self._responses[1]
      -- Behavior varies considerably based on response type
      -- Error responses are not discarded, and the error will be sent to all future callbacks
      local t = response.t
      if t == --[[Response.SUCCESS_ATOM]] or t == --[[Response.SUCCESS_PARTIAL]] or t == --[[Response.SUCCESS_FEED]] or t == --[[Response.SUCCESS_SEQUENCE]] then
        local row, err = pcall(
          recursively_convert_pseudotype,
          response.r[self._response_index],
          self._opts
        )
        if err then row = response.r[self._response_index] end
        self._response_index = self._response_index + 1

        -- If we're done with this response, discard it
        if not response.r[self._response_index] then
          table.remove(self._responses, 1)
          self._response_index = 1
        end

        return cb(err, row)
      elseif t == --[[Response.COMPILE_ERROR]] then
        return cb(ReQLCompileError(response.r[1], self._root, response.b))
      elseif t == --[[Response.CLIENT_ERROR]] then
        return cb(ReQLClientError(response.r[1], self._root, response.b))
      elseif t == --[[Response.RUNTIME_ERROR]] then
        return cb(ReQLRuntimeError(response.r[1], self._root, response.b))
      elseif t == --[[Response.WAIT_COMPLETE]] then
        return cb(nil, nil)
      end
      return cb(ReQLDriverError('Unknown response type ' .. t))
    end,
    close = function(self, callback)
      if not self._end_flag then
        self._conn:_end_query(self._token)
      end
      if cb then return cb() end
    end,
    each = function(self, callback, on_finished)
      if type(cb) ~= 'function' then
        error('First argument to each must be a function.')
      end
      if on_finished and type(on_finished) ~= 'function' then
        error('Optional second argument to each must be a function.')
      end
      function next_cb(err, data)
        if err then
          if err.message == 'ReQLDriverError No more rows in the cursor.' then
            err = nil
          end
          if on_finished then
            return on_finished(err)
          end
        else
          cb(data)
          return self:next(next_cb)
        end
      end
      return self:next(next_cb)
    end,
    to_array = function(self, callback)
      if not self._type then self:_prompt_cont() end
      if self._type == --[[Response.SUCCESS_FEED]] then
        return cb(ReQLDriverError('`to_array` is not available for feeds.'))
      end
      local arr = {}
      return self:each(
        function(row)
          table.insert(arr, row)
        end,
        function(err)
          return cb(err, arr)
        end
      )
    end,
  }
)

Connection = class(
  'Connection',
  {
    __init = function(self, host_or_callback, callback)
      local host = {}
      if type(host_or_callback) == 'function' then
        callback = host_or_callback
      else
        host = host_or_callback
      end
      if type(host) == 'string' then
        host = {
          host = host
        }
      end
      function cb(err, conn)
        if callback then
          local res = callback(err, conn)
          conn:close({noreply_wait = false})
          return res
        end
        return conn, err
      end
      self.host = host.host or self.DEFAULT_HOST
      self.port = host.port or self.DEFAULT_PORT
      self.db = host.db -- left nil if this is not set
      self.auth_key = host.auth_key or self.DEFAULT_AUTH_KEY
      self.timeout = host.timeout or self.DEFAULT_TIMEOUT
      self.outstanding_callbacks = {}
      self.next_token = 1
      self.open = false
      self.buffer = ''
      self._events = self._events or {}
      if self.raw_socket then
        self:close({
          noreply_wait = false
        })
      end
      self.raw_socket = socket.tcp()
      self.raw_socket:settimeout(self.timeout)
      local status, err = self.raw_socket:connect(self.host, self.port)
      if status then
        local buf, err, partial
        -- Initialize connection with magic number to validate version
        self.raw_socket:send(
          --[[Version]] ..
          int_to_bytes(self.auth_key:len(), 4) ..
          self.auth_key ..
          --[[Protocol]]
        )

        -- Now we have to wait for a response from the server
        -- acknowledging the connection
        while 1 do
          buf, err, partial = self.raw_socket:receive(8)
          buf = buf or partial
          if not buf then
            return cb(ReQLDriverError('Server dropped connection with message:  \'' .. status_str .. '\'\n' .. err))
          end
          self.buffer = self.buffer .. buf
          i, j = buf:find('\0')
          if i then
            local status_str = self.buffer:sub(1, i - 1)
            self.buffer = self.buffer:sub(i + 1)
            if status_str == 'SUCCESS' then
              -- We're good, finish setting up the connection
              self.open = true
              return cb(nil, self)
            end
            return cb(ReQLDriverError('Server dropped connection with message: \'' .. status_str .. '\''))
          end
        end
      end
      return cb(ReQLDriverError('Could not connect to ' .. self.host .. ':' .. self.port .. '.\n' .. err))
    end,
    DEFAULT_HOST = 'localhost',
    DEFAULT_PORT = 28015,
    DEFAULT_AUTH_KEY = '',
    DEFAULT_TIMEOUT = 20, -- In seconds
    _get_response = function(self, reqest_token)
      local response_length = 0
      local token = 0
      local buf, err, partial
      -- Buffer data, execute return results if need be
      while true do
        buf, err, partial = self.raw_socket:receive(
          math.max(12, response_length)
        )
        buf = buf or partial
        if (not buf) and err then
          return self:_process_response(
            {
              t = --[[Response.CLIENT_ERROR]],
              r = {'connection returned: ' .. err},
              b = {}
            },
            reqest_token
          )
        end
        self.buffer = self.buffer .. buf
        if response_length > 0 then
          if string.len(self.buffer) >= response_length then
            local response_buffer = string.sub(self.buffer, 1, response_length)
            self.buffer = string.sub(self.buffer, response_length + 1)
            response_length = 0
            self:_process_response(json.decode(response_buffer), token)
            if token == reqest_token then return end
          end
        else
          if string.len(self.buffer) >= 12 then
            token = bytes_to_int(self.buffer:sub(1, 8))
            response_length = bytes_to_int(self.buffer:sub(9, 12))
            self.buffer = self.buffer:sub(13)
          end
        end
      end
    end,
    _del_query = function(self, token)
      -- This query is done, delete this cursor
      self.outstanding_callbacks[token].cursor = nil
    end,
    _process_response = function(self, response, token)
      local cursor = self.outstanding_callbacks[token]
      if not cursor then
        -- Unexpected token
        error('Unexpected token ' .. token .. '.')
      end
      cursor = cursor.cursor
      if cursor then
        return cursor:_add_response(response)
      end
    end,
    close = function(self, opts_or_callback, callback)
      local opts = {}
      local cb
      if callback then
        if type(opts_or_callback) ~= 'table' then
          error('First argument to two-argument `close` must be a table.')
        end
        opts = opts_or_callback
        cb = callback
      elseif type(opts_or_callback) == 'table' then
          opts = opts_or_callback
      elseif type(opts_or_callback) == 'function' then
        cb = opts_or_callback
      end

      function wrapped_cb(err)
        self.open = false
        self.raw_socket:shutdown()
        self.raw_socket:close()
        if cb then
          return cb(err)
        end
        return nil, err
      end

      local noreply_wait = opts.noreply_wait and self.open

      if noreply_wait then
        return self:noreply_wait(wrapped_cb)
      end
      return wrapped_cb()
    end,
    noreply_wait = function(self, callback)
      if type(cb) ~= 'function' then
        cb = function(err) return nil, err end
      end
      function callback(err, cur)
        if cur then
          return cur.next(function(err) return cb(err) end)
        end
        return cb(err)
      end
      if not self.open then
        return callback(ReQLDriverError('Connection is closed.'))
      end

      -- Assign token
      local token = self.next_token
      self.next_token = self.next_token + 1

      -- Save cursor
      local cursor = Cursor(self, token, {})

      -- Save cursor
      self.outstanding_callbacks[token] = {cursor = cursor}

      -- Construct query
      self:_write_query(token, '[' .. --[[Query.NOREPLY_WAIT]] .. ']')

      return callback(nil, cursor)
    end,
    _write_query = function(self, token, data)
      self.raw_socket:send(
        int_to_bytes(token, 8) ..
        int_to_bytes(#data, 4) ..
        data
      )
    end,
    reconnect = function(self, opts_or_callback, callback)
      local opts
      if callback then
        opts = opts_or_callback
      elseif type(opts_or_callback) == 'function' then
        opts = {}
        callback = opts_or_callback
      else
        opts = opts_or_callback or {}
      end
      function cb(err, conn)
        if callback then
          local res = callback(err, conn)
          conn:close({noreply_wait = false})
          return res
        end
        return conn, err
      end
      return self:close(opts, function(err)
        if err then
          return cb(err)
        end
        return Connection(self, cb)
      end)
    end,
    use = function(self, db)
      self.db = db
    end,
    _start = function(self, term, cb, opts)
      if not (self.open) then
        cb(ReQLDriverError('Connection is closed.'))
      end

      -- Assign token
      local token = self.next_token
      self.next_token = self.next_token + 1

      for k, v in pairs(opts) do
        if k == 'use_outdated' or k == 'noreply' or k == 'profile' then
          v = not not v
        end
        opts[k] = r(v):build()
      end

      -- Set global options
      if self.db then
        opts.db = r.db(self.db):build()
      end

      -- Construct query
      local query = {--[[Query.START]], term:build(), opts}

      local cursor = Cursor(self, token, opts, term)

      -- Save cursor
      self.outstanding_callbacks[token] = {cursor = cursor}
      self:_send_query(token, query)
      local res
      if type(cb) == 'function' and not opts.noreply then
        res = cb(nil, cursor)
      end
      cursor:close()
      return res
    end,
    _continue_query = function(self, token)
      return self:_write_query(token, '[' .. --[[Query.CONTINUE]] .. ']')
    end,
    _end_query = function(self, token)
      self:_del_query(token)
      return self:_write_query(token, '[' .. --[[Query.STOP]] .. ']')
    end,
    _send_query = function(self, token, query)
      return self:_write_query(token, json.encode(query))
    end
  }
)

-- Add connect
r.connect = function(...)
  return Connection(...)
end

-- Export ReQL Errors
r.error = {
  ReQLError = ReQLError,
  ReQLDriverError = ReQLDriverError,
  ReQLServerError = ReQLServerError,
  ReQLRuntimeError = ReQLRuntimeError,
  ReQLCompileError = ReQLCompileError,
  ReQLClientError = ReQLClientError
}

-- Export class introspection
r.is_instance = is_instance

-- Export all names defined on r
return r
