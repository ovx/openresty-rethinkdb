local socket = require('socket')
local json = require('json')

local errors = require('./errors')
local Cursor = require('./cursor')
local protodef = require('./proto')
local proto_version = protodef.Version.V0_3
local proto_protocol = protodef.Protocol.JSON
local proto_query_type = protodef.QueryType
local proto_response_type = protodef.ResponseType

-- Import some names to this namespace for convienience
local mk_atom = errors.mk_atom
local is_instance = errors.is_instance
local is_array = errors.is_array

local Connection
local bytes_to_int, int_to_bytes

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
    for k=bytes,1,-1 do
        mul = 2 ^ (8 * (k - 1))
        res[k] = math.floor(num / mul)
        num = math.fmod(num, mul)
    end
    return string.char(unpack(res))
end

do
  local _base_0 = {
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
        buf, err, partial = self.raw_socket:receive(1024)
        buf = buf or partial
        if (not buf) and err then
          return self.outstanding_callbacks[token].cb(
            errors.ReQLDriverError()
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
      self.outstanding_callbacks[token] = {}
    end,
    _process_response = function(self, response, token)
      local profile = response.p
      if self.outstanding_callbacks[token] then
        local root, cursor, opts
        do
          local _obj_0 = self.outstanding_callbacks[token]
          root, cursor, opts = _obj_0.root, _obj_0.cursor, _obj_0.opts
        end
        if cursor then
          cursor:_add_response(response)
          if cursor._end_flag and cursor._outstanding_requests == 0 then
            return self:_del_query(token)
          end
        end
      else
        -- Unexpected token
        return error(errors.ReQLDriverError("Unexpected token " .. token .. "."))
      end
    end,
    close = function(self, opts_or_callback, callback)
      local opts = {}
      local cb
      if callback then
        if type(opts_or_callback) ~= 'table' then
          error(errors.ReQLDriverError("First argument to two-argument `close` must be an object."))
        end
        opts = opts_or_callback
        cb = callback
      else
        if type(opts_or_callback) == 'table' then
          opts = opts_or_callback
        else
          if type(opts_or_callback) == "function" then
            cb = opts_or_callback
          end
        end
      end

      if cb and type(cb) ~= 'function' then
        error(errors.ReQLDriverError("First argument to two-argument `close` must be an object."))
      end

      local wrapped_cb = function(...)
        self.open = false
        self.raw_socket:shutdown()
        self.raw_socket:close()
        if cb then
          return cb(...)
        end
      end

      local noreply_wait = opts.noreply_wait and self.open

      if noreply_wait then
        return self:noreply_wait(wrapped_cb)
      end
      return wrapped_cb()
    end,
    noreply_wait = function(self, cb)
      callback = function(err, cur)
        if cur then
          cur.each(function() end)
        end
        cb(err)
      end
      if not (self.open) then
        return callback(errors.ReQLDriverError("Connection is closed."))
      end

      -- Assign token
      local token = self.next_token
      self.next_token = self.next_token + 1

      -- Construct query
      local query = { }
      query.type = proto_query_type.NOREPLY_WAIT
      query.token = token

      -- Save callback
      self.outstanding_callbacks[token] = {
        cb = callback,
        root = nil,
        opts = nil
      }
      return self:_send_query(query)
    end,
    _write_query = function(self, token, data)
      self.raw_socket:send(
        int_to_bytes(token, 8) ..
        int_to_bytes(#data, 4) ..
        data
      )
    end,
    cancel = function(self)
      self.raw_socket.destroy()
      self.outstanding_callbacks = { }
    end,
    reconnect = function(self, opts_or_callback, callback)
      if callback then
        local opts = opts_or_callback
        local cb = callback
      else
        if type(opts_or_callback) == "function" then
          local opts = { }
          local cb = opts_or_callback
        else
          if opts_or_callback then
            local opts = opts_or_callback
          else
            local opts = { }
          end
          local cb = callback
        end
      end
      local close_cb = function(self, err)
        if err then
          return cb(err)
        else
          local construct_cb = function(self)
            return self.constructor.call(self, {
              host = self.host,
              port = self.port
            }, cb)
          end
          return set_timeout(construct_cb, 0)
        end
      end
      return self:close(opts, close_cb)
    end,
    use = function(self, db)
      self.db = db
    end,
    _start = function(self, term, cb, opts)
      if not (self.open) then
        cb(errors.ReQLDriverError("Connection is closed."))
      end

      -- Assign token
      local token = self.next_token
      self.next_token = self.next_token + 1

      -- Construct query
      local query = { }
      query.global_optargs = opts
      query.type = proto_query_type.START
      query.query = term:build()
      query.token = token
      -- Set global options
      if self.db then
        query.global_optargs.db = r.db(self.db):build()
      end

      local cursor = Cursor(self, token, query.global_optargs, term)

      -- Save callback
      do
        local callback = {
          root = term,
          opts = opts,
          cursor = cursor
        }
        self.outstanding_callbacks[token] = callback
      end
      self:_send_query(query)
      if type(cb) == 'function' and not opts.noreply then
        local res = cb(nil, cursor)
        cursor:close()
        return res
      end
    end,
    _continue_query = function(self, token)
      return self:_write_query(token, json.encode({proto_query_type.CONTINUE}))
    end,
    _end_query = function(self, token)
      return self:_write_query(token, json.encode({proto_query_type.STOP}))
    end,
    _send_query = function(self, query)
      -- Serialize query to JSON
      local data = {query.type}
      if query.query then
        data[2] = query.query
        if #query.global_optargs > 0 then
          data[3] = query.global_optargs
        end
      end
      self:_write_query(query.token, json.encode(data))
    end
  }
  _base_0.__index = _base_0
  local _class_0 = setmetatable({
    __init = function(self, host, callback)
      if not (host) then
        host = { }
      else
        if type(host) == 'string' then
          host = {
            host = host
          }
        end
      end
      self.host = host.host or self.DEFAULT_HOST
      self.port = host.port or self.DEFAULT_PORT
      self.db = host.db -- left nil if this is not set
      self.auth_key = host.auth_key or self.DEFAULT_AUTH_KEY
      self.timeout = host.timeout or self.DEFAULT_TIMEOUT
      self.outstanding_callbacks = { }
      self.next_token = 1
      self.open = false
      self.buffer = ''
      self._events = self._events or { }
      local err_callback = function(self, e)
        self:remove_listener('connect', con_callback)
        if is_instance(errors.ReQLDriverError, e) then
          return callback(e)
        else
          return callback(errors.ReQLDriverError("Could not connect to " .. tostring(self.host) .. ":" .. tostring(self.port) .. ".\n" .. tostring(e.message)))
        end
      end
      if self.raw_socket then
        self:close({
          noreply_wait = false
        })
      end
      self.raw_socket = socket.tcp()
      self.raw_socket:settimeout(self.timeout)
      local status, err = self.raw_socket:connect(self.host, self.port)
      if status then
        -- Initialize connection with magic number to validate version
        self.raw_socket:send(
          int_to_bytes(proto_version, 4) ..
          int_to_bytes(self.auth_key:len(), 4) ..
          self.auth_key ..
          int_to_bytes(proto_protocol, 4)
        )

        -- Now we have to wait for a response from the server
        -- acknowledging the connection
        while 1 do
          buf, err, partial = self.raw_socket:receive(8)
          buf = buf or partial
          if buf then
            self.buffer = self.buffer .. buf
          else
            return callback(errors.ReQLDriverError("Could not connect to " .. tostring(self.host) .. ":" .. tostring(self.port) .. ".\n" .. tostring(e)))
          end
          i, j = buf:find("\0")
          if i then
            local status_str = self.buffer:sub(1, i - 1)
            self.buffer = self.buffer:sub(i + 1)
            if status_str == "SUCCESS" then
              -- We're good, finish setting up the connection
              self.open = true
              local res = callback(nil, self)
              self:close({noreply_wait = false})
              return res
            else
              return callback(errors.ReQLDriverError("Server dropped connection with message: \"" + status_str + "\""))
            end
          end
        end
      end
    end,
    __base = _base_0,
    __name = "Connection",
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
  Connection = _class_0
end

return {
  is_connection = function(connection)
    return is_instance(Connection, connection)
  end,

  -- The main function of this module
  connect = function(host_or_callback, callback)
    local host = {}
    if type(host_or_callback) == 'function' then
      callback = host_or_callback
    else
      host = host_or_callback
    end
    return Connection(host, callback)
  end
}
