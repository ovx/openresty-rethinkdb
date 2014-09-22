local socket = require('socket')
local err = require('./errors')
local Cursor = require('./cursor')
local protodef = require('./proto')
local proto_version = protodef.Version.V0_3
local proto_protocol = protodef.Protocol.JSON
local proto_query_type = protodef.QueryType
local proto_response_type = protodef.ResponseType
-- local r = require('./ast')

-- Import some names to this namespace for convienience
local mk_atom = err.mk_atom
local is_instance = err.is_instance
local is_array = err.is_array

local Connection
local bytes_to_int, int_to_bytes, to_json, from_json

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

function to_json(obj)
  if obj == nil then
    return 'null'
  end
  if type(obj) == 'boolean' then
    if obj then return 'true' end
    return 'false'
  end
  if type(obj) == 'string' then
    return obj
  end
  if type(obj) == 'number' then
    return tostring(obj)
  end
  if is_array(obj) then
    local res = '['
    local first = true
    for _, v in ipairs(obj) do
      if first then
        res = res .. to_json(v)
        first = false
      else
        res = res .. ',' .. to_json(v)
      end
    end
    return res .. ']'
  end
  if type(obj) == 'tree' then
    local res = '{'
    local first = true
    for k, v in pairs(obj) do
      if first then
        res = res .. k .. ':' .. to_json(obj[k])
        first = false
      else
        res = res .. ',' .. k .. ':' .. to_json(obj[k])
      end
    end
    return res .. '}'
  end
  return '{}'
end

do
  local _base_0 = {
    DEFAULT_HOST = 'localhost',
    DEFAULT_PORT = 28015,
    DEFAULT_AUTH_KEY = '',
    DEFAULT_TIMEOUT = 20, -- In seconds
    _data = function(self, buf)
      -- Buffer data, execute return results if need be
      self.buffer = self.buffer .. buf
      while strlen(self.buffer) >= 12 do
        local token = bytes_to_int(self.buffer:sub(1, 8))
        self.response_length = bytes_to_int(self.buffer:sub(9, 12))
        self.buffer = self.buffer:sub(12)
        if not (strlen(self.buffer) >= self.response_length) then
          break
        end
        local response_buffer = self.buffer:sub(1, self.response_length)
        self.buffer = self.buffer.sub(self.response_length)
        local response = JSON.parse(response_buffer)
        self:_process_response(response, token)
      end
    end,
    _del_query = function(self, token)
      -- This query is done, delete this cursor
      delete(self.outstanding_callbacks[token])
      if #self.outstanding_callbacks < 1 and not self.open then
        return self:cancel()
      end
    end,
    _process_response = function(self, response, token)
      local profile = response.p
      if self.outstanding_callbacks[token] then
        local cb, root, cursor, opts, feed
        do
          local _obj_0 = self.outstanding_callbacks[token]
          cb, root, cursor, opts, feed = _obj_0.cb, _obj_0.root, _obj_0.cursor, _obj_0.opts, _obj_0.feed
        end
        if cursor then
          cursor._add_response(response)
          if cursor._end_flag and cursor._outstanding_requests == 0 then
            return self:_del_query(token)
          end
        else
          if feed then
            feed._add_response(response)
            if feed._end_flag and feed._outstanding_requests == 0 then
              return self:_del_query(token)
            end
          else
            if cb then
              -- Behavior varies considerably based on response type
              local _exp_0 = response.t
              if proto_response_type.COMPILE_ERROR == _exp_0 then
                cb(err.ReQLCompileError(response, root))
                return self:_del_query(token)
              elseif proto_response_type.CLIENT_ERROR == _exp_0 then
                cb(err.ReQLClientError(response, root))
                return self:_del_query(token)
              elseif proto_response_type.RUNTIME_ERROR == _exp_0 then
                cb(err.ReQLRuntimeError(response, root))
                return self:_del_query(token)
              elseif proto_response_type.SUCCESS_ATOM == _exp_0 then
                response = {mk_atom(response, opts)}
                if profile then
                  response = {
                    profile = profile,
                    value = response
                  }
                end
                cb(nil, response)
                return self:_del_query(token)
              elseif proto_response_type.SUCCESS_PARTIAL == _exp_0 then
                cursor = Cursor(self, token, opts, root)
                self.outstanding_callbacks[token].cursor = cursor
                if profile then
                  return cb(nil, {
                    profile = profile,
                    value = cursor._add_response(response)
                  })
                else
                  return cb(nil, cursor._add_response(response))
                end
              elseif proto_response_type.SUCCESS_SEQUENCE == _exp_0 then
                cursor = Cursor(self, token, opts, root)
                self:_del_query(token)
                if profile then
                  return cb(nil, {
                    profile = profile,
                    value = cursor._add_response(response)
                  })
                else
                  return cb(nil, cursor._add_response(response))
                end
              elseif proto_response_type.SUCCESS_FEED == _exp_0 then
                feed = Cursor(self, token, opts, root)
                self.outstanding_callbacks[token].feed = feed
                if profile then
                  return cb(nil, {
                    profile = profile,
                    value = feed._add_response(response)
                  })
                else
                  return cb(nil, feed._add_response(response))
                end
              elseif proto_response_type.WAIT_COMPLETE == _exp_0 then
                self:_del_query(token)
                return cb(nil, nil)
              else
                return cb(err.ReQLDriverError("Unknown response type"))
              end
            end
          end
        end
      else
        -- Unexpected token
        return self:emit('error', err.ReQLDriverError("Unexpected token " .. tostring(token) .. "."))
      end
    end,
    close = function(self, opts_or_callback, callback)
      local opts, cb
      if callback then
        opts = opts_or_callback
        cb = callback
      else
        if type(opts_or_callback) == 'tree' then
          opts = opts_or_callback
          cb = nil
        else
          if type(opts_or_callback) == "function" then
            opts = { }
            cb = opts_or_callback
          else
            opts = { }
          end
        end
      end
      local wrapped_cb = function(self, ...)
        self.raw_socket["end"]()
        if cb then
          return cb(unpack(arg))
        end
      end

      callback = wrapped_cb

      if callback then
        local opts = opts_or_callback
        if not (type(opts) == 'tree') then
          error(err.ReQLDriverError("First argument to two-argument `close` must be an object."))
        end
        local cb = callback
      else
        if type(opts_or_callback) == 'tree' then
          local opts = opts_or_callback
          local cb = nil
        else
          if type(opts_or_callback) == 'function' then
            local opts = { }
            local cb = opts_or_callback
          else
            local opts = opts_or_callback
            local cb = nil
          end
        end
      end
      local noreply_wait = opts.noreply_wait and self.open
      local wrapped_cb = function(...)
        self.open = false
        if cb then
          return cb(...)
        end
      end
      if noreply_wait then
        return self:noreply_wait(wrapped_cb)
      else
        return wrapped_cb()
      end
    end,
    noreply_wait = function(self, cb)
      callback = function(err, cur)
        if cur then
          cur.each(function() end)
        end
        cb(err)
      end
      if not (self.open) then
        return callback(err.ReQLDriverError("Connection is closed."))
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
        error(err.ReQLDriverError("Connection is closed."))
      end

      -- Assign token
      local token = self.next_token
      self.next_token = self.next_token + 1

      -- Construct query
      local query = { }
      query.global_optargs = { }
      query.type = proto_query_type.START
      query.query = term:build()
      query.token = token
      -- Set global options
      if self.db then
        query.global_optargs['db'] = r.db(self.db):build()
      end
      if opts.use_outdated then
        query.global_optargs['use_outdated'] = r.expr(not not opts.use_outdated):build()
      end
      if opts.noreply then
        query.global_optargs['noreply'] = r.expr(not not opts.noreply):build()
      end
      if opts.profile then
        query.global_optargs['profile'] = r.expr(not not opts.profile):build()
      end
      if opts.durability then
        query.global_optargs['durability'] = r.expr(opts.durability):build()
      end
      if opts.batch_conf then
        query.global_optargs['batch_conf'] = r.expr(opts.batch_conf):build()
      end
      if opts.array_limit then
        query.global_optargs['array_limit'] = r.expr(opts.array_limit):build()
      end

      -- Save callback
      do
        local callback = {
          root = term,
          opts = opts,
          cursor = Cursor(
            self,
            token,
            query.global_optargs,
            term
          )
        }
        if not opts.noreply then callback.cb = cb end
        self.outstanding_callbacks[token] = callback
      end
      self:_send_query(query)
      if opts.noreply and type(cb) == 'function' then
        return cb(nil)
      end
    end,
    _continue_query = function(self, token)
      return self:_write_query(token, to_json(proto_query_type.CONTINUE))
    end,
    _end_query = function(self, token)
      return self:_write_query(token, to_json(proto_query_type.STOP))
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
      self:_write_query(query.token, to_json(data))
      local callback = self.outstanding_callbacks[query.token]
      local cursor = callback.cursor
      if not callback.opts.noreply then
        local cb = callback.cb
        if type(cb) == 'function' then
          cb(cursor)
        end
      end
      cursor:close()
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
        if is_instance(err.ReQLDriverError, e) then
          return callback(e)
        else
          return callback(err.ReQLDriverError("Could not connect to " .. tostring(self.host) .. ":" .. tostring(self.port) .. ".\n" .. tostring(e.message)))
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
          buf, e, partial = self.raw_socket:receive(8)
          if buf or err == 'timeout' then
            self.buffer = self.buffer .. (buf or partial)
          else
            return callback(err.ReQLDriverError("Could not connect to " .. tostring(self.host) .. ":" .. tostring(self.port) .. ".\n" .. tostring(e)))
          end
          i, j = buf:find("\0")
          if i then
            local status_str = self.buffer:sub(1, i - 1)
            self.buffer = self.buffer:sub(i + 1)
            if status_str == "SUCCESS" then
              -- We're good, finish setting up the connection
              self.open = true
              callback(nil, self)
              self:close({noreply_wait = false})
            else
              return callback(err.ReQLDriverError("Server dropped connection with message: \"" + status_str + "\""))
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
