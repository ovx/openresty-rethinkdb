local err = require('./errors')
local util = require('./util')
local protoResponseType = require('./proto-def').ResponseType
local EventEmitter = require('events').EventEmitter

-- Import some names to this namespace for convenience
local ar = util.ar
local varar = util.varar
local aropt = util.aropt
local mkErr = util.mkErr

-- setImmediate is not defined in some browsers (including Chrome)
if not setImmediate then
  local setImmediate = function(cb)
    return setTimeout(cb, 0)
  end
end
local IterableResult
do
  local _base_0 = {
    stackSize = 100,
    _addResponse = function(response)
      if response.t == self._type or response.t == protoResponseType.SUCCESS_SEQUENCE then
        -- We push a "ok" response only if it's not empty
        if response.r.length > 0 then
          self._responses.push(response)
        end
      else
        self._responses.push(response)
      end
      self._outstandingRequests = self._outstandingRequests - 1
      if response.t ~= self._type then
        -- We got an error or a SUCCESS_SEQUENCE
        self._endFlag = true
        if self._closeCb then
          local _exp_0 = response.t
          if protoResponseType.COMPILE_ERROR == _exp_0 then
            self:_closeCb(mkErr(err.RqlRuntimeError, response, self._root))
          elseif protoResponseType.CLIENT_ERROR == _exp_0 then
            self:_closeCb(mkErr(err.RqlRuntimeError, response, self._root))
          elseif protoResponseType.RUNTIME_ERROR == _exp_0 then
            self:_closeCb(mkErr(err.RqlRuntimeError, response, self._root))
          else
            self:_closeCb()
          end
        end
      end
      self._contFlag = false
      if self._closeAsap == false then
        self:_promptNext()
      else
        self:close(self._closeCb)
      end
      return self
    end,
    _getCallback = function()
      self._iterations = self._iterations + 1
      local cb = self._cbQueue.shift()
      if self._iterations % self.stackSize == self.stackSize - 1 then
        local immediateCb = (function(err, row)
          return setImmediate(function()
            return cb(err, row)
          end)
        end)
        return immediateCb
      else
        return cb
      end
    end,
    _handleRow = function()
      local response = self._responses[0]
      local row = util.recursivelyConvertPseudotype(response.r[self._responseIndex], self._opts)
      local cb = self:_getCallback()
      self._responseIndex = self._responseIndex + 1

      -- If we're done with this response, discard it
      if self._responseIndex == response.r.length then
        self._responses.shift()
        self._responseIndex = 0
      end
      return cb(nil, row)
    end,
    bufferEmpty = function()
      return self._responses.length == 0 or self._responses[0].r.length <= self._responseIndex
    end,
    _promptNext = function()
      -- If there are no more waiting callbacks, just wait until the next event
      while self._cbQueue[0] do
        if self:bufferEmpty() == true then
          -- We prefetch things here, set `is 0` to avoid prefectch
          if self._endFlag == true then
            local cb = self:_getCallback()
            cb(err.RqlDriverError("No more rows in the cursor."))
          else
            if self._responses.length <= 1 then
              self:_promptCont()
            end
          end
          return
        else

          -- Try to get a row out of the responses
          local response = self._responses[0]
          if self._responses.length == 1 then
            -- We're low on data, prebuffer
            self:_promptCont()
          end

          -- Error responses are not discarded, and the error will be sent to all future callbacks
          local _exp_0 = response.t
          if protoResponseType.SUCCESS_PARTIAL == _exp_0 then
            self:_handleRow()
          elseif protoResponseType.SUCCESS_FEED == _exp_0 then
            self:_handleRow()
          elseif protoResponseType.SUCCESS_SEQUENCE == _exp_0 then
            if response.r.length == 0 then
              self._responses.shift()
            else
              self:_handleRow()
            end
          elseif protoResponseType.COMPILE_ERROR == _exp_0 then
            self._responses.shift()
            local cb = self:_getCallback()
            cb(mkErr(err.RqlCompileError, response, self._root))
          elseif protoResponseType.CLIENT_ERROR == _exp_0 then
            self._responses.shift()
            local cb = self:_getCallback()
            cb(mkErr(err.RqlClientError, response, self._root))
          elseif protoResponseType.RUNTIME_ERROR == _exp_0 then
            self._responses.shift()
            local cb = self:_getCallback()
            cb(mkErr(err.RqlRuntimeError, response, self._root))
          else
            self._responses.shift()
            local cb = self:_getCallback()
            cb(err.RqlDriverError("Unknown response type for cursor"))
          end
        end
      end
    end,
    _promptCont = function()
      -- Let's ask the server for more data if we haven't already
      if not ((self._contFlag or self._endFlag)) then
        self._contFlag = true
        self._outstandingRequests = self._outstandingRequests + 1
        return self._conn._continueQuery(self._token)
      end
    end,
    -- Implement IterableResult
    hasNext = function()
      error(err.RqlDriverError("The `hasNext` command has been removed since 1.13. Use `next` instead."))
    end,
    _next = varar(0, 1, function(cb)
      local fn = function(self, cb)
        self._cbQueue.push(cb)
        return self:_promptNext()
      end
      return fn(cb)
    end),
    close = varar(0, 1, function(cb)
      if self._endFlag == true then
        return cb()
      else
        self._closeCb = cb
        if self._outstandingRequests > 0 then
          self._closeAsap = true
        else
          self._outstandingRequests = self._outstandingRequests + 1
          return self._conn._endQuery(self._token)
        end
      end
    end),
    _each = varar(1, 2, function(cb, onFinished)
      if not (type(cb) == 'function') then
        error(err.RqlDriverError("First argument to each must be a function."))
      end
      if onFinished and type(onFinished) ~= 'function' then
        error(err.RqlDriverError("Optional second argument to each must be a function."))
      end
      local stopFlag = false
      local self = self
      local nextCb = function(self, err, data)
        if stopFlag ~= true then
          if err then
            if err.message == 'No more rows in the cursor.' then
              if onFinished then
                return onFinished()
              end
            else
              return cb(err)
            end
          else
            stopFlag = cb(nil, data) == false
            return self:_next(nextCb)
          end
        else
          if onFinished then
            return onFinished()
          end
        end
      end
      return self:_next(nextCb)
    end),
    toArray = varar(0, 1, function(cb)
      local fn = function(self, cb)
        local arr = { }
        local eachCb = function(self, err, row)
          if err then
            return cb(err)
          else
            return arr.push(row)
          end
        end
        local onFinish = function(self, err, ar)
          return cb(nil, arr)
        end
        return self:each(eachCb, onFinish)
      end
      return fn(cb)
    end),
    _makeEmitter = function()
      self.emitter = EventEmitter
      self.each = function()
        error(err.RqlDriverError("You cannot use the cursor interface and the EventEmitter interface at the same time."))
      end
      self.next = function()
        error(err.RqlDriverError("You cannot use the cursor interface and the EventEmitter interface at the same time."))
      end
    end,
    addListener = function(...)
      if not self.emitter then
        self:_makeEmitter()
        setImmediate(function(self)
          return self:_each(self._eachCb)
        end)
      end
      return self.emitter.addListener(unpack(arg))
    end,
    on = function(...)
      if not self.emitter then
        self:_makeEmitter()
        setImmediate(function(self)
          return self:_each(self._eachCb)
        end)
      end
      return self.emitter.on(unpack(arg))
    end,
    once = function()
      if not self.emitter then
        self:_makeEmitter()
        setImmediate(function(self)
          return self:_each(self._eachCb)
        end)
      end
      return self.emitter.once()
    end,
    removeListener = function()
      if not self.emitter then
        self:_makeEmitter()
        setImmediate(function(self)
          return self:_each(self._eachCb)
        end)
      end
      return self.emitter.removeListener()
    end,
    removeAllListeners = function()
      if not self.emitter then
        self:_makeEmitter()
        setImmediate(function(self)
          return self:_each(self._eachCb)
        end)
      end
      return self.emitter.removeAllListeners()
    end,
    setMaxListeners = function()
      if not self.emitter then
        self:_makeEmitter()
        setImmediate(function(self)
          return self:_each(self._eachCb)
        end)
      end
      return self.emitter.setMaxListeners()
    end,
    listeners = function()
      if not self.emitter then
        self:_makeEmitter()
        setImmediate(function(self)
          return self:_each(self._eachCb)
        end)
      end
      return self.emitter.listeners()
    end,
    emit = function()
      if not self.emitter then
        self:_makeEmitter()
        setImmediate(function(self)
          return self:_each(self._eachCb)
        end)
      end
      return self.emitter.emit()
    end,
    _eachCb = function(self, err, data)
      if err then
        return self.emitter.emit('error', err)
      else
        return self.emitter.emit('data', data)
      end
    end
  }
  _base_0.__index = _base_0
  local _class_0 = setmetatable({
    __init = function(conn, token, opts, root)
      self._conn = conn
      self._token = token
      self._opts = opts
      self._root = root -- current query
      self._responses = { }
      self._responseIndex = 0
      self._outstandingRequests = 1 -- Because we haven't add the response yet
      self._iterations = 0
      self._endFlag = false
      self._contFlag = false
      self._closeAsap = false
      self._cont = nil
      self._cbQueue = { }
      self.next = self._next
      self.each = self._each
    end,
    __base = _base_0,
    __name = "IterableResult"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  IterableResult = _class_0
end
local Cursor
do
  local _parent_0 = IterableResult
  local _base_0 = {
    toString = ar(function()
      return "[object Cursor]"
    end)
  }
  _base_0.__index = _base_0
  setmetatable(_base_0, _parent_0.__base)
  local _class_0 = setmetatable({
    __init = function()
      self._type = protoResponseType.SUCCESS_PARTIAL
      return _parent_0
    end,
    __base = _base_0,
    __name = "Cursor",
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
  Cursor = _class_0
end
local Feed
do
  local _parent_0 = IterableResult
  local _base_0 = {
    hasNext = function()
      error(err.RqlDriverError("`hasNext` is not available for feeds."))
    end,
    toArray = function()
      error(err.RqlDriverError("`toArray` is not available for feeds."))
    end,
    toString = ar(function()
      return "[object Feed]"
    end)
  }
  _base_0.__index = _base_0
  setmetatable(_base_0, _parent_0.__base)
  local _class_0 = setmetatable({
    __init = function()
      self._type = protoResponseType.SUCCESS_FEED
      return _parent_0
    end,
    __base = _base_0,
    __name = "Feed",
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
  Feed = _class_0
end


-- Used to wrap array results so they support the same iterable result
-- API as cursors.

local ArrayResult
do
  local _parent_0 = IterableResult
  local _base_0 = {
    -- We store @__index as soon as the user starts using the cursor interface
    _hasNext = ar(function()
      if not self.__index then
        self.__index = 0
      end
      return self.__index < self.length
    end),
    _next = varar(0, 1, function(cb)
      local fn = function(self, cb)
        if self:_hasNext() == true then
          self = self
          if self.__index % self.stackSize == self.stackSize - 1 then
            return setImmediate(function()
              cb(nil, self[self.__index])
              self.__index = self.__index + 1
            end)
          else
            cb(nil, self[self.__index])
            self.__index = self.__index + 1
          end
        else
          return cb(err.RqlDriverError("No more rows in the cursor."))
        end
      end
      return fn(cb)
    end),
    toArray = varar(0, 1, function(cb)
      local fn = function(self, cb)
        -- IterableResult.toArray would create a copy
        if self.__index then
          return cb(nil, self.slice(self.__index, self.length))
        else
          return cb(nil, self)
        end
      end
      return fn(cb)
    end),
    close = function()
      return self
    end,
    makeIterable = function(response)
      response.__proto__ = { }
      for name, method in ArrayResult.prototype do
        if name ~= 'constructor' then
          if name == '_each' then
            response.__proto__['each'] = method
            response.__proto__['_each'] = method
          else
            if name == '_next' then
              response.__proto__['next'] = method
              response.__proto__['_next'] = method
            else
              response.__proto__[name] = method
            end
          end
        end
      end
      response.__proto__.__proto__ = { }
      return response
    end
  }
  _base_0.__index = _base_0
  setmetatable(_base_0, _parent_0.__base)
  local _class_0 = setmetatable({
    __init = function(self, ...)
      return _parent_0.__init(self, ...)
    end,
    __base = _base_0,
    __name = "ArrayResult",
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
  ArrayResult = _class_0
end
return {
  Cursor = Cursor,
  Feed = Feed,
  makeIterable = ArrayResult.makeIterable
}
