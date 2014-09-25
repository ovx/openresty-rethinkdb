local err = require('./errors')
local proto_response_type = require('./proto').ResponseType

local Cursor

do
  local _base_0 = {
    stack_size = 100,
    _add_response = function(self, response)
      if not self._type then self._type = response.t end
      if response.t == self._type or response.t == proto_response_type.SUCCESS_SEQUENCE then
        -- We push a "ok" response only if it's not empty
        if #response.r > 0 then
          table.insert(self._responses, response)
        end
      else
        table.insert(self._responses, response)
      end
      if response.t ~= self._type then
        -- We got an error or a SUCCESS_SEQUENCE
        self._end_flag = true
        if self._close_cb then
          local _exp_0 = response.t
          if proto_response_type.COMPILE_ERROR == _exp_0 then
            self._close_cb(err.ReQLRuntimeError(response, self._root))
          elseif proto_response_type.CLIENT_ERROR == _exp_0 then
            self._close_cb(err.ReQLRuntimeError(response, self._root))
          elseif proto_response_type.RUNTIME_ERROR == _exp_0 then
            self._close_cb(err.ReQLRuntimeError(response, self._root))
          else
            self._close_cb()
          end
        end
      end
      self._cont_flag = false
      if self._close_asap == false then
        self:_prompt_next()
      else
        self:close(self._close_cb)
      end
      return self
    end,
    _get_callback = function(self)
      self._iterations = self._iterations + 1
      local cb = self._cb_queue.shift()
      if self._iterations % self.stack_size == self.stack_size - 1 then
        local immediate_cb = (function(err, row)
          return set_immediate(function()
            return cb(err, row)
          end)
        end)
        return immediate_cb
      else
        return cb
      end
    end,
    _handle_row = function(self)
      local response = self._responses[0]
      local row = err.recursively_convert_pseudotype(response.r[self._response_index], self._opts)
      local cb = self:_get_callback()
      self._response_index = self._response_index + 1

      -- If we're done with this response, discard it
      if self._response_index == #response.r then
        self._responses.shift()
        self._response_index = 0
      end
      return cb(nil, row)
    end,
    buffer_empty = function(self)
      return #self._responses == 0 or #self._responses[0].r <= self._response_index
    end,
    _prompt_next = function(self)
      -- If there are no more waiting callbacks, just wait until the next event
      while self._cb_queue[0] do
        if self:buffer_empty() == true then
          -- We prefetch things here, set `is 0` to avoid prefectch
          if self._end_flag == true then
            local cb = self:_get_callback()
            cb(err.ReQLDriverError("No more rows in the cursor."))
          else
            if #self._responses <= 1 then
              self:_prompt_cont()
            end
          end
          return
        else

          -- Try to get a row out of the responses
          local response = self._responses[0]
          if #self._responses == 1 then
            -- We're low on data, prebuffer
            self:_prompt_cont()
          end

          -- Error responses are not discarded, and the error will be sent to all future callbacks
          local _exp_0 = response.t
          if proto_response_type.SUCCESS_PARTIAL == _exp_0 then
            self:_handle_row()
          elseif proto_response_type.SUCCESS_FEED == _exp_0 then
            self:_handle_row()
          elseif proto_response_type.SUCCESS_SEQUENCE == _exp_0 then
            if #response.r == 0 then
              self._responses.shift()
            else
              self:_handle_row()
            end
          elseif proto_response_type.COMPILE_ERROR == _exp_0 then
            self._responses.shift()
            local cb = self:_get_callback()
            cb(err.ReQLCompileError(response, self._root))
          elseif proto_response_type.CLIENT_ERROR == _exp_0 then
            self._responses.shift()
            local cb = self:_get_callback()
            cb(err.ReQLClientError(response, self._root))
          elseif proto_response_type.RUNTIME_ERROR == _exp_0 then
            self._responses.shift()
            local cb = self:_get_callback()
            cb(err.ReQLRuntimeError(response, self._root))
          else
            self._responses.shift()
            local cb = self:_get_callback()
            cb(err.ReQLDriverError("Unknown response type for cursor"))
          end
        end
      end
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
    next = function(self, cb)
      -- Try to get a row out of the responses
      if not self._responses[1] then
        -- We prefetch things here, set `is 0` to avoid prefectch
        if self._end_flag == true then
          return cb(err.ReQLDriverError("No more rows in the cursor."))
        end
        self:_prompt_cont()
      end
      local response = self._responses[1]
      -- Error responses are not discarded, and the error will be sent to all future callbacks
      local t = response.t
      if proto_response_type.SUCCESS_PARTIAL == t or proto_response_type.SUCCESS_FEED == t or proto_response_type.SUCCESS_SEQUENCE == t then
        local row = err.recursively_convert_pseudotype(response.r[self._response_index], self._opts)
        self._response_index = self._response_index + 1

        -- If we're done with this response, discard it
        if not response.r[self._response_index] then
          table.remove(self._responses, 1)
          self._response_index = 1
        end
        return cb(nil, row)
      elseif proto_response_type.COMPILE_ERROR == t then
        return cb(err.ReQLCompileError(response, self._root))
      elseif proto_response_type.CLIENT_ERROR == t then
        return cb(err.ReQLClientError(response, self._root))
      elseif proto_response_type.RUNTIME_ERROR == t then
        return cb(err.ReQLRuntimeError(response, self._root))
      end
      return cb(err.ReQLDriverError("Unknown response type for cursor"))
    end,
    close = function(self, cb)
      if not self._end_flag then
        self._conn:_end_query(self._token)
      end
      if cb then return cb() end
    end,
    each = function(self, cb, on_finished)
      if type(cb) ~= 'function' then
        error(err.ReQLDriverError("First argument to each must be a function."))
      end
      if on_finished and type(on_finished) ~= 'function' then
        error(err.ReQLDriverError("Optional second argument to each must be a function."))
      end
      function next_cb(err, data)
        if err then
          if err.message ~= 'No more rows in the cursor.' then
            return cb(err)
          end
          if on_finished then
            return on_finished()
          end
        else
          cb(nil, data)
          return self:next(next_cb)
        end
      end
      return self:next(next_cb)
    end,
    to_array = function(self, cb)
      if not self._type then self:_prompt_cont() end
      if self._type == proto_response_type.SUCCESS_FEED then
        return cb(err.ReQLDriverError("`to_array` is not available for feeds."))
      end
      local arr = {}
      return self:each(
        function(err, row)
          if err then
            return cb(err)
          end
          table.insert(arr, row)
        end,
        function()
          return cb(nil, arr)
        end
      )
    end,
  }
  _base_0.__index = _base_0
  local _class_0 = setmetatable({
    __init = function(self, conn, token, opts, root)
      self._conn = conn
      self._token = token
      self._opts = opts
      self._root = root -- current query
      self._responses = { }
      self._outstanding_requests = 1 -- Because we haven't add the response yet
      self._iterations = 0
      self._response_index = 1
      self._end_flag = false
      self._cont_flag = false
      self._close_asap = false
      self._cont = nil
      self._cb_queue = { }
    end,
    __base = _base_0,
    __name = "Cursor"
  }, {
    __index = _base_0,
    __call = function(cls, ...)
      local _self_0 = setmetatable({}, _base_0)
      cls.__init(_self_0, ...)
      return _self_0
    end
  })
  _base_0.__class = _class_0
  Cursor = _class_0
end

return Cursor
