local net = require('net')
local events = require('events')
local util = require('./util')
local err = require('./errors')
local cursors = require('./cursor')
local protodef = require('./proto-def')
local protoVersion = protodef.Version.V0_3
local protoProtocol = protodef.Protocol.JSON
local protoQueryType = protodef.QueryType
local protoResponseType = protodef.ResponseType
local r = require('./ast')

-- Import some names to this namespace for convienience
local ar = util.ar
local varar = util.varar
local aropt = util.aropt
local mkAtom = util.mkAtom
local mkErr = util.mkErr
local Connection
do
  local _parent_0 = events.EventEmitter
  local _base_0 = {
    DEFAULT_HOST = 'localhost',
    DEFAULT_PORT = 28015,
    DEFAULT_AUTH_KEY = '',
    DEFAULT_TIMEOUT = 20, -- In seconds
    _data = function(buf)
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
    _delQuery = function(token)
      -- This query is done, delete this cursor
      delete(self.outstandingCallbacks[token])
      if Object.keys(self.outstandingCallbacks).length < 1 and not self.open then
        return self:cancel()
      end
    end,
    _processResponse = function(response, token)
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
                cb(mkErr(err.RqlCompileError, response, root))
                return self:_delQuery(token)
              elseif protoResponseType.CLIENT_ERROR == _exp_0 then
                cb(mkErr(err.RqlClientError, response, root))
                return self:_delQuery(token)
              elseif protoResponseType.RUNTIME_ERROR == _exp_0 then
                cb(mkErr(err.RqlRuntimeError, response, root))
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
                return cb(err.RqlDriverError("Unknown response type"))
              end
            end
          end
        end
      else
        -- Unexpected token
        return self:emit('error', err.RqlDriverError("Unexpected token " .. tostring(token) .. "."))
      end
    end,
    close = varar(0, 2, function(optsOrCallback, callback)
      if callback then
        local opts = optsOrCallback
        if not (type(opts) == 'tree') then
          error(err.RqlDriverError("First argument to two-argument `close` must be an object."))
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
      for key in opts do
        if not (key == 'noreplyWait') then
          error(err.RqlDriverError("First argument to two-argument `close` must be { noreplyWait: <bool> }."))
        end
      end
      local noreplyWait = ((not opts.noreplyWait) or opts.noreplyWait) and self.open
      local wrappedCb
      wrappedCb = function(self, ...)
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
    end),
    noreplyWait = varar(0, 1, function(callback)
      if not (self.open) then
        return callback(err.RqlDriverError("Connection is closed."))
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
    end),
    cancel = ar(function()
      self.outstandingCallbacks = { }
    end),
    reconnect = varar(0, 2, function(optsOrCallback, callback)
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
      local closeCb
      closeCb = function(self, err)
        if err then
          return cb(err)
        else
          local constructCb
          constructCb = function(self)
            return self.constructor.call(self, {
              host = self.host,
              port = self.port
            }, cb)
          end
          return setTimeout(constructCb, 0)
        end
      end
      return self:close(opts, closeCb)
    end),
    use = ar(function(db)
      self.db = db
    end),
    _start = function(term, cb, opts)
      if not (self.open) then
        error(err.RqlDriverError("Connection is closed."))
      end

      -- Assign token
      local token = self.nextToken
      self.nextToken = self.nextToken + 1

      -- Construct query
      local query = { }
      query.global_optargs = { }
      query.type = protoQueryType.START
      query.query = term.build()
      query.token = token
      -- Set global options
      if self.db then
        query.global_optargs['db'] = r.db(self.db).build()
      end
      if opts.useOutdated then
        query.global_optargs['use_outdated'] = r.expr(not not opts.useOutdated).build()
      end
      if opts.noreply then
        query.global_optargs['noreply'] = r.expr(not not opts.noreply).build()
      end
      if opts.profile then
        query.global_optargs['profile'] = r.expr(not not opts.profile).build()
      end
      if opts.durability then
        query.global_optargs['durability'] = r.expr(opts.durability).build()
      end
      if opts.batchConf then
        query.global_optargs['batch_conf'] = r.expr(opts.batchConf).build()
      end
      if opts.arrayLimit then
        query.global_optargs['array_limit'] = r.expr(opts.arrayLimit).build()
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
    _continueQuery = function(token)
      local query = {
        type = protoQueryType.CONTINUE,
        token = token
      }
      return self:_sendQuery(query)
    end,
    _endQuery = function(token)
      local query = {
        type = protoQueryType.STOP,
        token = token
      }
      return self:_sendQuery(query)
    end,
    _sendQuery = function(query)
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
  setmetatable(_base_0, _parent_0.__base)
  local _class_0 = setmetatable({
    __init = function(host, callback)
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
      self.buffer = Buffer(0)
      self._events = self._events or { }
      local errCallback
      errCallback = function(self, e)
        self:removeListener('connect', conCallback)
        if err.RqlDriverError.instanceof(e) then
          return callback(e)
        else
          return callback(err.RqlDriverError("Could not connect to " .. tostring(self.host) .. ":" .. tostring(self.port) .. ".\n" .. tostring(e.message)))
        end
      end
      self:once('error', errCallback)
      local conCallback
      conCallback = function(self)
        self:removeListener('error', errCallback)
        self.open = true
        return callback(nil, self)
      end
      return self:once('connect', conCallback)
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
  if _parent_0.__inherited then
    _parent_0.__inherited(_parent_0, _class_0)
  end
  Connection = _class_0
end
local TcpConnection
do
  local _parent_0 = Connection
  local _base_0 = {
    close = varar(0, 2, function(optsOrCallback, callback)
      if callback then
        local opts = optsOrCallback
        local cb = callback
      else
        if type(optsOrCallback) == 'tree' then
          local opts = optsOrCallback
          local cb = nil
        else
          if type(optsOrCallback) == "function" then
            local opts = { }
            local cb = optsOrCallback
          else
            local opts = { }
          end
        end
      end
      local wrappedCb
      wrappedCb = function(self, ...)
        self.rawSocket["end"]()
        if cb then
          return cb(unpack(arg))
        end
      end

      -- This would simply be super(opts, wrappedCb), if we were not in the varar
      -- anonymous function
      return TcpConnection.__super__.close.call(self, opts, wrappedCb)
    end),
    cancel = function()
      self.rawSocket.destroy()
      return _parent_0.cancel(self)
    end,
    _writeQuery = function(token, data)
      local tokenBuf = Buffer(8)
      tokenBuf.writeUInt32LE(token % 0xFFFFFFFF, 0)
      tokenBuf.writeUInt32LE(Math.floor(token / 0xFFFFFFFF), 4)
      self.rawSocket.write(tokenBuf)
      return self:write(Buffer(data))
    end,
    write = function(chunk)
      local lengthBuffer = Buffer(4)
      lengthBuffer.writeUInt32LE(chunk.length, 0)
      self.rawSocket.write(lengthBuffer)
      return self.rawSocket.write(chunk)
    end
  }
  _base_0.__index = _base_0
  setmetatable(_base_0, _parent_0.__base)
  local _class_0 = setmetatable({
    __init = function(host, callback)
      if not (TcpConnection.isAvailable()) then
        error(err.RqlDriverError("TCP sockets are not available in this environment"))
      end
      _parent_0.__init(self, host, callback)
      if self.rawSocket then
        self:close({
          noreplyWait = false
        })
      end
      self.rawSocket = net.connect(self.port, self.host)
      self.rawSocket.setNoDelay()
      local timeout = setTimeout((function(self)
        return self.rawSocket.destroy()(self:emit('error', err.RqlDriverError("Handshake timedout")))
      end), self.timeout * 1000)
      self.rawSocket.once('error', function(self)
        return clearTimeout(timeout)
      end)
      self.rawSocket.once('connect', function(self)
        -- Initialize connection with magic number to validate version
        local version = Buffer(4)
        version.writeUInt32LE(protoVersion, 0)
        local auth_buffer = Buffer(self.authKey, 'ascii')
        local auth_length = Buffer(4)
        auth_length.writeUInt32LE(auth_buffer.length, 0)
        local protocol = Buffer(4)

        -- Send the protocol type that we will be using to communicate with the server
        protocol.writeUInt32LE(protoProtocol, 0)
        self.rawSocket.write(Buffer.concat({
          version,
          auth_length,
          auth_buffer,
          protocol
        }))

        -- Now we have to wait for a response from the server
        -- acknowledging the connection
        local handshake_callback
        handshake_callback = function(self, buf)
          self.buffer = Buffer.concat({
            self.buffer,
            buf
          })
          for b, i in self.buffer do
            if b == 0 then
              self.rawSocket.removeListener('data', handshake_callback)
              local status_buf = self.buffer.slice(0, i)
              self.buffer = self.buffer.slice(i + 1)
              local status_str = status_buf.toString()
              clearTimeout(timeout)
              if status_str == "SUCCESS" then
                -- We're good, finish setting up the connection
                self.rawSocket.on('data', function(self, buf)
                  return self:_data(buf)
                end)
                self:emit('connect')
                return
              else
                self:emit('error', err.RqlDriverError("Server dropped connection with message: \"" + status_str.trim() + "\""))
                return
              end
            end
          end
        end
        return self.rawSocket.on('data', handshake_callback)
      end)
      self.rawSocket.on('error', function(self, ...)
        return self:emit('error', unpack(arg))
      end)
      self.rawSocket.on('close', function(self)
        self.open = false, self:emit('close', {
          noreplyWait = false
        })
      end)

      -- In case the raw socket timesout, we close it and re-emit the event for the user
      return self.rawSocket.on('timeout', function(self)
        self.open = false, self:emit('timeout')
      end)
    end,
    __base = _base_0,
    __name = "TcpConnection",
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
  local self = _class_0
  self.isAvailable = function()
    return not (process.browser)
  end
  if _parent_0.__inherited then
    _parent_0.__inherited(_parent_0, _class_0)
  end
  TcpConnection = _class_0
end
local HttpConnection
do
  local _parent_0 = Connection
  local _base_0 = {
    DEFAULT_PROTOCOL = 'http',
    cancel = function()
      self.xhr.abort()
      local xhr = XMLHttpRequest
      xhr.open("POST", tostring(self._url) .. "close-connection?conn_id=" .. tostring(self._connId), true)
      xhr.send()
      self._url = nil
      self._connId = nil
      return _parent_0.cancel(self)
    end,
    close = varar(0, 2, function(optsOrCallback, callback)
      if callback then
        local opts = optsOrCallback
        local cb = callback
      else
        if type(optsOrCallback) == 'tree' then
          local opts = optsOrCallback
          local cb = nil
        else
          local opts = { }
          local cb = optsOrCallback
        end
      end
      if not (not cb or type(cb) == 'function') then
        error(err.RqlDriverError("Final argument to `close` must be a callback function or object."))
      end
      local wrappedCb
      wrappedCb = function(self, ...)
        self:cancel()
        if cb then
          return cb(unpack(arg))
        end
      end

      -- This would simply be super(opts, wrappedCb), if we were not in the varar
      -- anonymous function
      return HttpConnection.__super__.close.call(this, opts, wrappedCb)
    end),
    _writeQuery = function(token, data)
      local buf = Buffer(encodeURI(data).length - 1 + 8)
      buf.writeUInt32LE(token % 0xFFFFFFFF, 0)
      buf.writeUInt32LE(Math.floor(token / 0xFFFFFFFF), 4)
      buf.write(data, 8)
      return self:write(buf)
    end,
    write = function(chunk)
      local xhr = XMLHttpRequest
      xhr.open("POST", tostring(self._url) .. "?conn_id=" .. tostring(self._connId), true)
      xhr.responseType = "arraybuffer"
      xhr.onreadystatechange = function(self, e)
        if xhr.readyState == 4 and xhr.status == 200 then
          -- Convert response from ArrayBuffer to node buffer

          local buf = Buffer((function()
            local _accum_0 = { }
            local _len_0 = 1
            for b in (Uint8Array(xhr.response)) do
              _accum_0[_len_0] = b
              _len_0 = _len_0 + 1
            end
            return _accum_0
          end)())
          return self:_data(buf)
        end
      end

      -- Convert the chunk from node buffer to ArrayBuffer
      local array = ArrayBuffer(chunk.length)
      local view = Uint8Array(array)
      local i = 0
      while i < chunk.length do
        view[i] = chunk[i]
        i = i + 1
      end
      xhr.send(array)
      self.xhr = xhr -- We allow only one query at a time per HTTP connection
    end
  }
  _base_0.__index = _base_0
  setmetatable(_base_0, _parent_0.__base)
  local _class_0 = setmetatable({
    __init = function(host, callback)
      if not (HttpConnection.isAvailable()) then
        error(err.RqlDriverError("XMLHttpRequest is not available in this environment"))
      end
      _parent_0.__init(self, host, callback)
      local protocol
      if host.protocol == 'https' then
        protocol = 'https'
      else
        protocol = self.DEFAULT_PROTOCOL
      end
      local url = tostring(protocol) .. "://" .. tostring(self.host) .. ":" .. tostring(self.port) .. tostring(host.pathname) .. "ajax/reql/"
      local xhr = XMLHttpRequest
      xhr.open("GET", url + "open-new-connection", true)
      xhr.responseType = "arraybuffer"
      xhr.onreadystatechange = function(self, e)
        if xhr.readyState == 4 then
          if xhr.status == 200 then
            self._url = url
            self._connId = (DataView(xhr.response)).getInt32(0, true)
            return self:emit('connect')
          else
            return self:emit('error', err.RqlDriverError("XHR error, http status " .. tostring(xhr.status) .. "."))
          end
        end
      end
      xhr.send()
      self.xhr = xhr -- We allow only one query at a time per HTTP connection
    end,
    __base = _base_0,
    __name = "HttpConnection",
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
  local self = _class_0
  self.isAvailable = function()
    return not not XMLHttpRequest
  end
  if _parent_0.__inherited then
    _parent_0.__inherited(_parent_0, _class_0)
  end
  HttpConnection = _class_0
end
return {
  isConnection = function(connection)
    return Connection.instanceof(connection)
  end,

  -- The main function of this module
  connect = varar(0, 2, function(hostOrCallback, callback)
    if type(hostOrCallback) == 'function' then
      local host = { }
      callback = hostOrCallback
    else
      local host = hostOrCallback
    end
    if TcpConnection.isAvailable() then
      return TcpConnection(host, callback)
    else
      if HttpConnection.isAvailable() then
        return HttpConnection(host, callback)
      else
        return error(err.RqlDriverError("Neither TCP nor HTTP avaiable in this environment"))
      end
    end
  end)
}
