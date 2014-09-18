local socket = require('socket')
local util = require('./util')
local err = require('./errors')
local cursors = require('./cursor')
local protodef = require('./proto-def')
local protoVersion = protodef.Version.V0_3
local protoProtocol = protodef.Protocol.JSON
local protoQueryType = protodef.QueryType
local protoResponseType = protodef.ResponseType
-- local r = require('./ast')

-- Import some names to this namespace for convienience
local mkAtom = util.mkAtom
local mkErr = util.mkErr
local isinstance = util.isinstance

local Connection

function bytes_to_int(str, endian, signed) -- use length of string to determine 8,16,32,64 bits
    local t = {str:byte(1,-1)}
    if endian == "big" then --reverse bytes
        local tt = {}
        for k=1,#t do
            tt[#t-k+1] = t[k]
        end
        t = tt
    end
    local n = 0
    for k=1,#t do
        n = n + t[k] * 2 ^ ((k - 1) * 8)
    end
    if signed then
        n = (n > 2 ^ (#t * 8 - 1) - 1) and (n - 2 ^ (#t * 8)) or n -- if last bit set, negative.
    end
    return n
end

function int_to_bytes(num, endian, signed)
    if num < 0 and not signed then num = -num print"warning, dropping sign from number converting to unsigned" end
    local res = {}
    local n = math.ceil(select(2, math.frexp(num)) / 8) -- number of bytes to be used.
    if signed and num < 0 then
        num = num + 2 ^ n
    end
    for k=n,1,-1 do -- 256 = 2^8 bits per char.
        local mul=2 ^ (8 * (k - 1))
        res[k] = math.floor(num/mul)
        num = num - res[k] * mul
    end
    assert(num==0)
    if endian == "big" then
        local t = {}
        for k=1,n do
            t[k] = res[n-k+1]
        end
        res = t
    end
    return (string.char(unpack(res)) .. "\0\0\0\0"):sub(1, 4)
end

do
  local _base_0 = {
    DEFAULT_HOST = 'localhost',
    DEFAULT_PORT = 28015,
    DEFAULT_AUTH_KEY = '',
    DEFAULT_TIMEOUT = 20, -- In seconds
    _data = function(self, buf)
      -- Buffer data, execute return results if need be
      self.buffer = Buffer.concat({
        self.buffer,
        buf
      })
      while self.buffer.length >= 12 do
        local token = self.buffer.readUInt32LE(0) + 0x100000000 * self.buffer.readUInt32LE(4)
        local responseLength = self.buffer.readUInt32LE(8)
        if not (self.buffer.length >= (12 + responseLength)) then
          break
        end
        local responseBuffer = self.buffer.slice(12, responseLength + 12)
        local response = JSON.parse(responseBuffer)
        self:_processResponse(response, token)
        self.buffer = self.buffer.slice(12 + responseLength)
      end
    end,
    _delQuery = function(self, token)
      -- This query is done, delete this cursor
      delete(self.outstandingCallbacks[token])
      if Object.keys(self.outstandingCallbacks).length < 1 and not self.open then
        return self:cancel()
      end
    end,
    _processResponse = function(self, response, token)
      local profile = response.p
      if self.outstandingCallbacks[token] then
        local cb, root, cursor, opts, feed
        do
          local _obj_0 = self.outstandingCallbacks[token]
          cb, root, cursor, opts, feed = _obj_0.cb, _obj_0.root, _obj_0.cursor, _obj_0.opts, _obj_0.feed
        end
        if cursor then
          cursor._addResponse(response)
          if cursor._endFlag and cursor._outstandingRequests == 0 then
            return self:_delQuery(token)
          end
        else
          if feed then
            feed._addResponse(response)
            if feed._endFlag and feed._outstandingRequests == 0 then
              return self:_delQuery(token)
            end
          else
            if cb then
              -- Behavior varies considerably based on response type
              local _exp_0 = response.t
              if protoResponseType.COMPILE_ERROR == _exp_0 then
                cb(mkErr(err.ReQLCompileError, response, root))
                return self:_delQuery(token)
              elseif protoResponseType.CLIENT_ERROR == _exp_0 then
                cb(mkErr(err.ReQLClientError, response, root))
                return self:_delQuery(token)
              elseif protoResponseType.RUNTIME_ERROR == _exp_0 then
                cb(mkErr(err.ReQLRuntimeError, response, root))
                return self:_delQuery(token)
              elseif protoResponseType.SUCCESS_ATOM == _exp_0 then
                response = mkAtom(response, opts)
                if Array.isArray(response) then
                  response = cursors.makeIterable(response)
                end
                if profile then
                  response = {
                    profile = profile,
                    value = response
                  }
                end
                cb(nil, response)
                return self:_delQuery(token)
              elseif protoResponseType.SUCCESS_PARTIAL == _exp_0 then
                cursor = cursors.Cursor(self, token, opts, root)
                self.outstandingCallbacks[token].cursor = cursor
                if profile then
                  return cb(nil, {
                    profile = profile,
                    value = cursor._addResponse(response)
                  })
                else
                  return cb(nil, cursor._addResponse(response))
                end
              elseif protoResponseType.SUCCESS_SEQUENCE == _exp_0 then
                cursor = cursors.Cursor(self, token, opts, root)
                self:_delQuery(token)
                if profile then
                  return cb(nil, {
                    profile = profile,
                    value = cursor._addResponse(response)
                  })
                else
                  return cb(nil, cursor._addResponse(response))
                end
              elseif protoResponseType.SUCCESS_FEED == _exp_0 then
                feed = cursors.Feed(self, token, opts, root)
                self.outstandingCallbacks[token].feed = feed
                if profile then
                  return cb(nil, {
                    profile = profile,
                    value = feed._addResponse(response)
                  })
                else
                  return cb(nil, feed._addResponse(response))
                end
              elseif protoResponseType.WAIT_COMPLETE == _exp_0 then
                self:_delQuery(token)
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
    close = function(self, optsOrCallback, callback)
      local opts, cb
      if callback then
        opts = optsOrCallback
        cb = callback
      else
        if type(optsOrCallback) == 'tree' then
          opts = optsOrCallback
          cb = nil
        else
          if type(optsOrCallback) == "function" then
            opts = { }
            cb = optsOrCallback
          else
            opts = { }
          end
        end
      end
      local wrappedCb = function(self, ...)
        self.rawSocket["end"]()
        if cb then
          return cb(unpack(arg))
        end
      end

      callback = wrappedCb

      if callback then
        local opts = optsOrCallback
        if not (type(opts) == 'tree') then
          error(err.ReQLDriverError("First argument to two-argument `close` must be an object."))
        end
        local cb = callback
      else
        if type(optsOrCallback) == 'tree' then
          local opts = optsOrCallback
          local cb = nil
        else
          if type(optsOrCallback) == 'function' then
            local opts = { }
            local cb = optsOrCallback
          else
            local opts = optsOrCallback
            local cb = nil
          end
        end
      end
      for key, _ in ipairs(opts) do
        if not (key == 'noreplyWait') then
          error(err.ReQLDriverError("First argument to two-argument `close` must be { noreplyWait: <bool> }."))
        end
      end
      local noreplyWait = ((not opts.noreplyWait) or opts.noreplyWait) and self.open
      local wrappedCb = function(self, ...)
        self.open = false
        if cb then
          return cb(unpack(arg))
        end
      end
      if noreplyWait then
        return self:noreplyWait(wrappedCb)
      else
        return wrappedCb()
      end
    end,
    noreplyWait = function(self, callback)
      if not (self.open) then
        return callback(err.ReQLDriverError("Connection is closed."))
      end

      -- Assign token
      local token = self.nextToken
      self.nextToken = self.nextToken + 1

      -- Construct query
      local query = { }
      query.type = protoQueryType.NOREPLY_WAIT
      query.token = token

      -- Save callback
      self.outstandingCallbacks[token] = {
        cb = callback,
        root = nil,
        opts = nil
      }
      return self:_sendQuery(query)
    end,
    _writeQuery = function(self, token, data)
      local tokenBuf = Buffer(8)
      tokenBuf.writeUInt32LE(token % 0xFFFFFFFF, 0)
      tokenBuf.writeUInt32LE(Math.floor(token / 0xFFFFFFFF), 4)
      self.rawSocket.write(tokenBuf)
      return self:write(Buffer(data))
    end,
    write = function(self, chunk)
      local lengthBuffer = Buffer(4)
      lengthBuffer.writeUInt32LE(chunk.length, 0)
      self.rawSocket.write(lengthBuffer)
      return self.rawSocket.write(chunk)
    end,
    cancel = function(self)
      self.rawSocket.destroy()
      self.outstandingCallbacks = { }
    end,
    reconnect = function(self, optsOrCallback, callback)
      if callback then
        local opts = optsOrCallback
        local cb = callback
      else
        if type(optsOrCallback) == "function" then
          local opts = { }
          local cb = optsOrCallback
        else
          if optsOrCallback then
            local opts = optsOrCallback
          else
            local opts = { }
          end
          local cb = callback
        end
      end
      local closeCb = function(self, err)
        if err then
          return cb(err)
        else
          local constructCb = function(self)
            return self.constructor.call(self, {
              host = self.host,
              port = self.port
            }, cb)
          end
          return setTimeout(constructCb, 0)
        end
      end
      return self:close(opts, closeCb)
    end,
    use = function(self, db)
      self.db = db
    end,
    _start = function(self, term, cb, opts)
      if not (self.open) then
        error(err.ReQLDriverError("Connection is closed."))
      end

      -- Assign token
      local token = self.nextToken
      self.nextToken = self.nextToken + 1

      -- Construct query
      local query = { }
      query.global_optargs = { }
      query.type = protoQueryType.START
      query.query = term:build()
      query.token = token
      -- Set global options
      if self.db then
        query.global_optargs['db'] = r.db(self.db):build()
      end
      if opts.useOutdated then
        query.global_optargs['use_outdated'] = r.expr(not not opts.useOutdated):build()
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
      if opts.batchConf then
        query.global_optargs['batch_conf'] = r.expr(opts.batchConf):build()
      end
      if opts.arrayLimit then
        query.global_optargs['array_limit'] = r.expr(opts.arrayLimit):build()
      end

      -- Save callback
      if not opts.noreply then
        self.outstandingCallbacks[token] = {
          cb = cb,
          root = term,
          opts = opts
        }
      end
      self:_sendQuery(query)
      if opts.noreply and type(cb) == 'function' then
        return cb(nil)
      end
    end,
    _continueQuery = function(self, token)
      local query = {
        type = protoQueryType.CONTINUE,
        token = token
      }
      return self:_sendQuery(query)
    end,
    _endQuery = function(self, token)
      local query = {
        type = protoQueryType.STOP,
        token = token
      }
      return self:_sendQuery(query)
    end,
    _sendQuery = function(self, query)
      -- Serialize query to JSON
      local data = {
        query.type
      }
      if query.query then
        data.push(query.query)
        if query.global_optargs and Object.keys(query.global_optargs).length > 0 then
          data.push(query.global_optargs)
        end
      end
      return self:_writeQuery(query.token, JSON.stringify(data))
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
      self.authKey = host.authKey or self.DEFAULT_AUTH_KEY
      self.timeout = host.timeout or self.DEFAULT_TIMEOUT
      self.outstandingCallbacks = { }
      self.nextToken = 1
      self.open = false
      self.buffer = ''
      self._events = self._events or { }
      local errCallback = function(self, e)
        self:removeListener('connect', conCallback)
        if isinstance(err.ReQLDriverError, e) then
          return callback(e)
        else
          return callback(err.ReQLDriverError("Could not connect to " .. tostring(self.host) .. ":" .. tostring(self.port) .. ".\n" .. tostring(e.message)))
        end
      end
      if self.rawSocket then
        self:close({
          noreplyWait = false
        })
      end
      self.rawSocket = socket.tcp()
      self.rawSocket:settimeout(self.timeout)
      local status, err = self.rawSocket:connect(self.host, self.port)
      if status then
        -- Initialize connection with magic number to validate version
        local version = int_to_bytes(protoVersion)
        local auth_length = int_to_bytes(self.authKey:len())
        local protocol = int_to_bytes(protoProtocol)

        self.rawSocket:send(
          version ..
          auth_length ..
          self.authKey ..
          protocol
        )

        -- Now we have to wait for a response from the server
        -- acknowledging the connection
        while 1 do
          buf, e, partial = self.rawSocket:receive(8)
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
              return callback(nil, self)
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
  isConnection = function(connection)
    return isinstance(Connection, connection)
  end,

  -- The main function of this module
  connect = function(hostOrCallback, callback)
    local host = {}
    if type(hostOrCallback) == 'function' then
      callback = hostOrCallback
    else
      host = hostOrCallback
    end
    return Connection(host, callback)
  end
}
