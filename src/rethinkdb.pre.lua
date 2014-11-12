local json = require('json')
local mime = require('mime')
local socket = require('socket')

-- r is both the main export table for the module
-- and a function that wraps a native Lua value in a ReQL datum
local r = {}

local Connection, Cursor
local DatumTerm, ReQLOp
--[[AstNames]]
local ReQLDriverError, ReQLServerError, ReQLRuntimeError, ReQLCompileError
local ReQLClientError, ReQLQueryPrinter, ReQLError

function r.is_instance(obj, ...)
  local class_list = {...}

  for _, cls in ipairs(class_list) do
    if type(cls) == 'string' then
      if type(obj) == cls then
        return true
      end
    else
      cls = cls.__name
    end

    if type(obj) == 'table' then
      local obj_cls = obj.__class
      while obj_cls do
        if obj_cls.__name == cls then
          return true
        end
        obj_cls = obj_cls.__parent
      end
    end
  end

  return false
end

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
    if r.is_instance(val, 'ReQLOp') then
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

function intsp(seq)
  local res = {}
  local sep = ''
  for _, v in ipairs(seq) do
    table.insert(res, {sep, v})
    sep = ', '
  end
  return res
end

function kved(optargs)
  local res = {'{'}
  local sep = ''
  for k, v in pairs(optargs) do
    table.insert(res, {sep, k, ': ', v})
    sep = ', '
  end
  table.insert(res, '}')
  return res
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

function get_opts(...)
  local args = {...}
  local opt = {}
  local pos_opt = args[#args]
  if (type(pos_opt) == 'table') and (not r.is_instance(pos_opt, 'ReQLOp')) then
    opt = pos_opt
    args[#args] = nil
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

function int_to_bytes(num, bytes)
  local res = {}
  local mul = 0
  for k = bytes, 1, -1 do
    local den = 2 ^ (8 * (k - 1))
    res[k] = math.floor(num / den)
    num = math.fmod(num, den)
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

-- All top level exported functions

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

    if not is_instance(connection, 'Connection', 'Pool') then
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
    local args = {...}
    optargs = optargs or {}
    if self.tt == --[[Term.FUNC]] then
      local func = args[1]
      local anon_args = {}
      local arg_nums = {}
      if debug.getinfo then
        local func_info = debug.getinfo(func)
        if func_info.what == 'Lua' and func_info.nparams then
          optargs.arity = func_info.nparams
        end
      end
      for i=1, optargs.arity or 1 do
        table.insert(arg_nums, ReQLOp.next_var_id)
        table.insert(anon_args, Var({}, ReQLOp.next_var_id))
        ReQLOp.next_var_id = ReQLOp.next_var_id + 1
      end
      func = func(unpack(anon_args))
      if func == nil then
        error('Anonymous function returned `nil`. Did you forget a `return`?')
      end
      optargs.arity = nil
      args = {{unpack(arg_nums)}, func}
    elseif self.tt == --[[Term.BINARY]] then
      local data = args[1]
      if r.is_instance(data, 'ReQLOp') then
      elseif type(data) == 'string' then
        self.base64_data = mime.b64(table.remove(args, 1))
      else
        error('Parameter to `r.binary` must be a string or ReQL query.')
      end
    elseif self.tt == --[[Term.FUNCALL]] then
      local func = table.remove(args)
      if type(func) == 'function' then
        func = Func({arity = #args}, func)
      end
      table.insert(args, 1, func)
    elseif self.tt == --[[Term.REDUCE]] then
      args[#args] = Func({arity = 2}, args[#args])
    end
    self.args = {}
    self.optargs = {}
    for i, a in ipairs(args) do
      self.args[i] = r(a)
    end
    for k, v in pairs(optargs) do
      self.optargs[k] = r(v)
    end
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
      return {'var_' .. args[1]}
    end
    if self.tt == --[[Term.BINARY]] and not self.args[1] then
      return 'r.binary(<data>)'
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
      return {
        'function(',
        intsp((function()
          local _accum_0 = {}
          for i, v in ipairs(self.args[1]) do
            _accum_0[i] = 'var_' .. v
          end
          return _accum_0
        end)()),
        ') return ',
        args[2],
        ' end'
      }
    end
    if self.tt == --[[Term.FUNCALL]] then
      local func = table.remove(args, 1)
      if func then
        table.insert(args, func)
      end
    end
    if not self.args then
      return {
        type(self)
      }
    end
    return {
      'r.' .. self.st .. '(',
      intspallargs(args, optargs),
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
        if math.abs(val) == math.huge or val ~= val then
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
      else
        self._conn:_continue_query(self._token)
      end
      while (self._cb and self._responses[1]) do
        self:_run_cb(self._cb)
      end
    end,
    _run_cb = function(self, callback)
      local cb = function(err, row)
        return callback(err, row)
      end
      local response = self._responses[1]
      -- Behavior varies considerably based on response type
      -- Error responses are not discarded, and the error will be sent to all future callbacks
      local t = response.t
      if t == --[[Response.SUCCESS_ATOM]] or t == --[[Response.SUCCESS_PARTIAL]] or t == --[[Response.SUCCESS_FEED]] or t == --[[Response.SUCCESS_SEQUENCE]] then
        local err

        local status, row = pcall(
          recursively_convert_pseudotype,
          response.r[self._response_index],
          self._opts
        )
        if not status then
          err = row
          row = response.r[self._response_index]
        end

        self._response_index = self._response_index + 1

        -- If we're done with this response, discard it
        if not response.r[self._response_index] then
          table.remove(self._responses, 1)
          self._response_index = 1
        end

        return cb(err, row)
      end
      self:clear()
      if t == --[[Response.COMPILE_ERROR]] then
        return cb(ReQLCompileError(response.r[1], self._root, response.b))
      elseif t == --[[Response.CLIENT_ERROR]] then
        return cb(ReQLClientError(response.r[1], self._root, response.b))
      elseif t == --[[Response.RUNTIME_ERROR]] then
        return cb(ReQLRuntimeError(response.r[1], self._root, response.b))
      elseif t == --[[Response.WAIT_COMPLETE]] then
        return cb()
      end
      return cb(ReQLDriverError('Unknown response type ' .. t))
    end,
    set = function(self, callback)
      self._cb = callback
    end,
    clear = function(self)
      self._cb = nil
    end,
    -- Implement IterableResult
    next = function(self, callback)
      local cb = function(err, row)
        return callback(err, row)
      end
      if self._cb then
        return cb(ReQLDriverError('Use `cur:clear()` before `cur:next`.'))
      end
      -- Try to get a row out of the responses
      while not self._responses[1] do
        if self._end_flag then
          return cb(ReQLDriverError('No more rows in the cursor.'))
        end
        self._conn:_get_response(self._token)
      end
      return self:_run_cb(cb)
    end,
    close = function(self, callback)
      if not self._end_flag then
        self._conn:_end_query(self._token)
        self._end_flag = true
      end
      if callback then return callback() end
    end,
    each = function(self, callback, on_finished)
      if type(callback) ~= 'function' then
        error('First argument to each must be a function.')
      end
      if on_finished and type(on_finished) ~= 'function' then
        error('Optional second argument to each must be a function.')
      end
      local cb = function(row)
        return callback(row)
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
      if not self._type then self._conn:_get_response(self._token) end
      if self._type == --[[Response.SUCCESS_FEED]] then
        return cb(ReQLDriverError('`to_array` is not available for feeds.'))
      end
      local cb = function(err, arr)
        return callback(err, arr)
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
      elseif type(host_or_callback) == 'string' then
        host = {host = host_or_callback}
      elseif host_or_callback then
        host = host_or_callback
      end
      local cb = function(err, conn)
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
          int_to_bytes(#self.auth_key, 4) ..
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
          if #self.buffer >= response_length then
            local response_buffer = string.sub(self.buffer, 1, response_length)
            self.buffer = string.sub(self.buffer, response_length + 1)
            response_length = 0
            self:_process_response(json.decode(response_buffer), token)
            if token == reqest_token then return end
          end
        else
          if #self.buffer >= 12 then
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
      local cb = function(err, cur)
        if cur then
          return cur.next(function(err) return callback(err) end)
        end
        return callback(err)
      end
      if not self.open then
        return cb(ReQLDriverError('Connection is closed.'))
      end

      -- Assign token
      local token = self.next_token
      self.next_token = self.next_token + 1

      -- Save cursor
      local cursor = Cursor(self, token, {})

      -- Save cursor
      self.outstanding_callbacks[token] = {cursor = cursor}

      -- Construct query
      self:_send_query(token, {--[[Query.NOREPLY_WAIT]]})

      return cb(nil, cursor)
    end,
    reconnect = function(self, opts_or_callback, callback)
      local opts = {}
      if callback or not type(opts_or_callback) == 'function' then
        opts = opts_or_callback
      else
        callback = opts_or_callback
      end
      return self:close(opts, function()
        return Connection(self, callback)
      end)
    end,
    use = function(self, db)
      self.db = db
    end,
    _start = function(self, term, callback, opts)
      local cb = function(err, cur)
        local res
        if type(callback) == 'function' and not opts.noreply then
          res = callback(err, cur)
        else
          if err then
            error(err.message)
          end
        end
        cur:close()
        return res
      end
      if not self.open then
        cb(ReQLDriverError('Connection is closed.'))
      end

      -- Assign token
      local token = self.next_token
      self.next_token = self.next_token + 1

      -- Set global options
      local global_opts = {}

      for k, v in pairs(opts) do
        global_opts[k] = r(v):build()
      end

      if opts.db then
        global_opts.db = r.db(opts.db):build()
      elseif self.db then
        global_opts.db = r.db(self.db):build()
      end

      -- Construct query
      local query = {--[[Query.START]], term:build(), global_opts}

      local cursor = Cursor(self, token, opts, term)

      -- Save cursor
      self.outstanding_callbacks[token] = {cursor = cursor}
      self:_send_query(token, query)
      return cb(nil, cursor)
    end,
    _continue_query = function(self, token)
      self:_send_query(token, {--[[Query.CONTINUE]]})
    end,
    _end_query = function(self, token)
      self:_del_query(token)
      self:_send_query(token, {--[[Query.STOP]]})
    end,
    _send_query = function(self, token, query)
      local data = json.encode(query)
      self.raw_socket:send(
        int_to_bytes(token, 8) ..
        int_to_bytes(#data, 4) ..
        data
      )
    end
  }
)

Pool = class(
  'Pool',
  {
    __init = function(self, host, callback)
      local cb = function(err, pool)
        if callback then
          local res = callback(err, pool)
          pool:close({noreply_wait = false})
          return res
        end
        return pool, err
      end
      self.open = false
      conn, err = Connection(host)
      if err then return cb(err) end
      self.open = true
      self.pool = {conn}
      self.size = host.size or 12
      self.host = host
      for i=2, self.size do
        table.insert(self.pool, (Connection(host)))
      end
      return cb(nil, self)
    end,
    close = function(self, opts, callback)
      local cb = function(err)
        if err and callback then
          callback(err)
        end
      end
      for _, conn in pairs(self.pool) do
        conn:close(opts, cb)
      end
      self.open = false
      if callback then return callback() end
    end,
    _start = function(self, term, callback, opts)
      local wear = math.huge
      local good_conn
      for i=1, self.size do
        if not self.pool[i] then self.pool[i] = Connection(self.host) end
        local conn = self.pool[i]
        if not conn.open then conn:reconnect() end
        if conn.next_token < wear then
          good_conn = conn
          wear = conn.next_token
        end
      end
      return conn:_start(term, callback, opts)
    end
  }
)

-- Add connect
r.connect = Connection
r.pool = Pool

-- Export ReQL Errors
r.error = {
  ReQLError = ReQLError,
  ReQLDriverError = ReQLDriverError,
  ReQLServerError = ReQLServerError,
  ReQLRuntimeError = ReQLRuntimeError,
  ReQLCompileError = ReQLCompileError,
  ReQLClientError = ReQLClientError
}

-- Export all names defined on r
return r
