local mime = require('mime')
local socket = ngx.socket
local cjson = require('cjson')

-- r is both the main export table for the module
-- and a function that wraps a native Lua value in a ReQL datum
local r = {
  json_parser = cjson,
  logger = function(err)
    if type(err) == 'string' then
      error(err)
    elseif type(err) == 'table' and err.msg then
      error(err.msg)
    else
      error('Unknown error type from driver')
    end
  end
}

function r._logger(err)
  if r.logger then
    r.logger(err)
  end
end

local DATUMTERM, ReQLOp
local ADD, AND, APPEND, APRIL, ARGS, ASC, AUGUST, AVG, BETWEEN
local BETWEEN_DEPRECATED, BINARY, BRACKET, BRANCH, CEIL, CHANGES, CHANGE_AT
local CIRCLE, COERCE_TO, CONCAT_MAP, CONFIG, CONTAINS, COUNT, DATE, DAY
local DAY_OF_WEEK, DAY_OF_YEAR, DB, DB_CREATE, DB_DROP, DB_LIST, DECEMBER
local DEFAULT, DELETE, DELETE_AT, DESC, DIFFERENCE, DISTANCE, DISTINCT, DIV
local DOWNCASE, DURING, EPOCH_TIME, EQ, EQ_JOIN, ERROR, FEBRUARY, FILL, FILTER
local FLOOR, FOR_EACH, FRIDAY, FUNC, FUNCALL, GE, GEOJSON, GET, GET_ALL
local GET_FIELD, GET_INTERSECTING, GET_NEAREST, GROUP, GT, HAS_FIELDS, HOURS
local HTTP, INCLUDES, INDEX_CREATE, INDEX_DROP, INDEX_LIST, INDEX_RENAME
local INDEX_STATUS, INDEX_WAIT, INFO, INNER_JOIN, INSERT, INSERT_AT
local INTERSECTS, IN_TIMEZONE, ISO8601, IS_EMPTY, JANUARY, JAVASCRIPT, JSON
local JULY, JUNE, KEYS, LE, LIMIT, LINE, LITERAL, LT, MAKE_ARRAY, MAKE_OBJ
local MAP, MARCH, MATCH, MAX, MAXVAL, MAY, MERGE, MIN, MINUTES, MINVAL, MOD
local MONDAY, MONTH, MUL, NE, NOT, NOVEMBER, NOW, NTH, OBJECT, OCTOBER
local OFFSETS_OF, OR, ORDER_BY, OUTER_JOIN, PLUCK, POINT, POLYGON, POLYGON_SUB
local PREPEND, RANDOM, RANGE, REBALANCE, RECONFIGURE, REDUCE, REPLACE, ROUND
local SAMPLE, SATURDAY, SECONDS, SEPTEMBER, SET_DIFFERENCE, SET_INSERT
local SET_INTERSECTION, SET_UNION, SKIP, SLICE, SPLICE_AT, SPLIT, STATUS, SUB
local SUM, SUNDAY, SYNC, TABLE, TABLE_CREATE, TABLE_DROP, TABLE_LIST, THURSDAY
local TIME, TIMEZONE, TIME_OF_DAY, TO_EPOCH_TIME, TO_GEOJSON, TO_ISO8601
local TO_JSON_STRING, TUESDAY, TYPE_OF, UNGROUP, UNION, UPCASE, UPDATE, UUID
local VAR, WAIT, WEDNESDAY, WITHOUT, WITH_FIELDS, YEAR, ZIP
local ReQLDriverError, ReQLServerError, ReQLRuntimeError, ReQLCompileError
local ReQLClientError, ReQLQueryPrinter, ReQLError

function r.is_instance(obj, cls, ...)
  if cls == nil then return false end

  if type(cls) == 'string' then
    if type(obj) == cls then
      return true
    end
  elseif type(cls) == 'table' then
    cls = cls.__name
  else
    return false
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

  return r.is_instance(obj, ...)
end

setmetatable(r, {
  __call = function(cls, val, nesting_depth)
    if nesting_depth == nil then
      nesting_depth = 20
    end
    if type(nesting_depth) ~= 'number' then
      return r._logger('Second argument to `r(val, nesting_depth)` must be a number.')
    end
    if nesting_depth <= 0 then
      return r._logger('Nesting depth limit exceeded')
    end
    if r.is_instance(val, 'ReQLOp') and type(val.build) == 'function' then
      return val
    end
    if type(val) == 'function' then
      return FUNC({}, val)
    end
    if type(val) == 'table' then
      local array = true
      for k, v in pairs(val) do
        if type(k) ~= 'number' then array = false end
        val[k] = r(v, nesting_depth - 1)
      end
      if array then
        return MAKE_ARRAY({}, unpack(val))
      end
      return MAKE_OBJ(val)
    end
    if type(val) == 'userdata' then
      val = pcall(tostring, val)
      r._logger('Found userdata inserting "' .. val .. '" into query')
      return DATUMTERM(val)
    end
    if type(val) == 'thread' then
      val = pcall(tostring, val)
      r._logger('Cannot insert thread object into query ' .. val)
      return nil
    end
    return DATUMTERM(val)
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
  if next(args) then
    table.insert(argrepr, intsp(args))
  end
  if optargs and next(optargs) then
    if next(argrepr) then
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
        return r._logger(ReQLDriverError('pseudo-type TIME ' .. obj .. ' table missing expected field `epoch_time`.'))
      end

      -- We ignore the timezone field of the pseudo-type TIME table. JS dates do not support timezones.
      -- By converting to a native date table we are intentionally throwing out timezone information.

      -- field 'epoch_time' is in seconds but the Date constructor expects milliseconds
      return obj['epoch_time']
    elseif 'raw' == time_format then
      return obj
    else
      return r._logger(ReQLDriverError('Unknown time_format run option ' .. opts.time_format .. '.'))
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
      return r._logger(ReQLDriverError('Unknown group_format run option ' .. opts.group_format .. '.'))
    end
  elseif 'BINARY' == reql_type then
    local binary_format = opts.binary_format
    if 'native' == binary_format or not binary_format then
      if not obj.data then
        return r._logger(ReQLDriverError('pseudo-type BINARY table missing expected field `data`.'))
      end
      return mime.unb64(obj.data)
    elseif 'raw' == binary_format then
      return obj
    else
      return r._logger(ReQLDriverError('Unknown binary_format run option ' .. opts.binary_format .. '.'))
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
  if r.json_parser.null then
    if obj == r.json_parser.null then return nil end
  elseif r.json_parser.util then
    if obj == r.json_parser.util.null then return nil end
  end
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
      if next(self.frames) then
        carrots = self:compose_carrots(self.term, self.frames)
      else
        carrots = {self:carrotify(self:compose_term(self.term))}
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
        return r._logger('Second argument to `run` cannot be a function if a third argument is provided.')
      end
      callback = options
      options = {}
    end
    -- else we suppose that we have run(connection[, options][, callback])

    if not r.is_instance(connection, 'Connection', 'Pool') then
      if r._pool then
        connection = r._pool
      else
        if callback then
          return callback(ReQLDriverError('First argument to `run` must be a connection.'))
        end
        return r._logger('First argument to `run` must be a connection.')
      end
    end

    return connection:_start(self, callback, options or {})
  end,
  add = function(...) return ADD({}, ...) end,
  and_ = function(...) return AND({}, ...) end,
  append = function(...) return APPEND({}, ...) end,
  april = function(...) return APRIL({}, ...) end,
  args = function(...) return ARGS({}, ...) end,
  asc = function(...) return ASC({}, ...) end,
  august = function(...) return AUGUST({}, ...) end,
  avg = function(...) return AVG({}, ...) end,
  between = function(arg0, arg1, arg2, opts) return BETWEEN(opts, arg0, arg1, arg2) end,
  between_deprecated = function(arg0, arg1, arg2, opts) return BETWEEN_DEPRECATED(opts, arg0, arg1, arg2) end,
  binary = function(...) return BINARY({}, ...) end,
  index = function(...) return BRACKET({}, ...) end,
  branch = function(...) return BRANCH({}, ...) end,
  ceil = function(...) return CEIL({}, ...) end,
  changes = function(...) return CHANGES({}, ...) end,
  change_at = function(...) return CHANGE_AT({}, ...) end,
  circle = function(...) return CIRCLE(get_opts(...)) end,
  coerce_to = function(...) return COERCE_TO({}, ...) end,
  concat_map = function(...) return CONCAT_MAP({}, ...) end,
  config = function(...) return CONFIG({}, ...) end,
  contains = function(...) return CONTAINS({}, ...) end,
  count = function(...) return COUNT({}, ...) end,
  date = function(...) return DATE({}, ...) end,
  day = function(...) return DAY({}, ...) end,
  day_of_week = function(...) return DAY_OF_WEEK({}, ...) end,
  day_of_year = function(...) return DAY_OF_YEAR({}, ...) end,
  db = function(...) return DB({}, ...) end,
  db_create = function(...) return DB_CREATE({}, ...) end,
  db_drop = function(...) return DB_DROP({}, ...) end,
  db_list = function(...) return DB_LIST({}, ...) end,
  december = function(...) return DECEMBER({}, ...) end,
  default = function(...) return DEFAULT({}, ...) end,
  delete = function(...) return DELETE(get_opts(...)) end,
  delete_at = function(...) return DELETE_AT({}, ...) end,
  desc = function(...) return DESC({}, ...) end,
  difference = function(...) return DIFFERENCE({}, ...) end,
  distance = function(arg0, arg1, opts) return DISTANCE(opts, arg0, arg1) end,
  distinct = function(...) return DISTINCT(get_opts(...)) end,
  div = function(...) return DIV({}, ...) end,
  downcase = function(...) return DOWNCASE({}, ...) end,
  during = function(arg0, arg1, arg2, opts) return DURING(opts, arg0, arg1, arg2) end,
  epoch_time = function(...) return EPOCH_TIME({}, ...) end,
  eq = function(...) return EQ({}, ...) end,
  eq_join = function(...) return EQ_JOIN(get_opts(...)) end,
  error_ = function(...) return ERROR({}, ...) end,
  february = function(...) return FEBRUARY({}, ...) end,
  fill = function(...) return FILL({}, ...) end,
  filter = function(arg0, arg1, opts) return FILTER(opts, arg0, arg1) end,
  floor = function(...) return FLOOR({}, ...) end,
  for_each = function(...) return FOR_EACH({}, ...) end,
  friday = function(...) return FRIDAY({}, ...) end,
  func = function(...) return FUNC({}, ...) end,
  do_ = function(...) return FUNCALL({}, ...) end,
  ge = function(...) return GE({}, ...) end,
  geojson = function(...) return GEOJSON({}, ...) end,
  get = function(...) return GET({}, ...) end,
  get_all = function(...) return GET_ALL(get_opts(...)) end,
  get_field = function(...) return GET_FIELD({}, ...) end,
  get_intersecting = function(...) return GET_INTERSECTING(get_opts(...)) end,
  get_nearest = function(...) return GET_NEAREST(get_opts(...)) end,
  group = function(...) return GROUP(get_opts(...)) end,
  gt = function(...) return GT({}, ...) end,
  has_fields = function(...) return HAS_FIELDS({}, ...) end,
  hours = function(...) return HOURS({}, ...) end,
  http = function(...) return HTTP(get_opts(...)) end,
  includes = function(...) return INCLUDES({}, ...) end,
  index_create = function(...) return INDEX_CREATE(get_opts(...)) end,
  index_drop = function(...) return INDEX_DROP({}, ...) end,
  index_list = function(...) return INDEX_LIST({}, ...) end,
  index_rename = function(...) return INDEX_RENAME(get_opts(...)) end,
  index_status = function(...) return INDEX_STATUS({}, ...) end,
  index_wait = function(...) return INDEX_WAIT({}, ...) end,
  info = function(...) return INFO({}, ...) end,
  inner_join = function(...) return INNER_JOIN({}, ...) end,
  insert = function(arg0, arg1, opts) return INSERT(opts, arg0, arg1) end,
  insert_at = function(...) return INSERT_AT({}, ...) end,
  intersects = function(...) return INTERSECTS({}, ...) end,
  in_timezone = function(...) return IN_TIMEZONE({}, ...) end,
  iso8601 = function(...) return ISO8601(get_opts(...)) end,
  is_empty = function(...) return IS_EMPTY({}, ...) end,
  january = function(...) return JANUARY({}, ...) end,
  js = function(...) return JAVASCRIPT(get_opts(...)) end,
  json = function(...) return JSON({}, ...) end,
  july = function(...) return JULY({}, ...) end,
  june = function(...) return JUNE({}, ...) end,
  keys = function(...) return KEYS({}, ...) end,
  le = function(...) return LE({}, ...) end,
  limit = function(...) return LIMIT({}, ...) end,
  line = function(...) return LINE({}, ...) end,
  literal = function(...) return LITERAL({}, ...) end,
  lt = function(...) return LT({}, ...) end,
  make_array = function(...) return MAKE_ARRAY({}, ...) end,
  make_obj = function(...) return MAKE_OBJ({}, ...) end,
  map = function(...) return MAP({}, ...) end,
  march = function(...) return MARCH({}, ...) end,
  match = function(...) return MATCH({}, ...) end,
  max = function(...) return MAX({}, ...) end,
  maxval = function(...) return MAXVAL({}, ...) end,
  may = function(...) return MAY({}, ...) end,
  merge = function(...) return MERGE({}, ...) end,
  min = function(...) return MIN({}, ...) end,
  minutes = function(...) return MINUTES({}, ...) end,
  minval = function(...) return MINVAL({}, ...) end,
  mod = function(...) return MOD({}, ...) end,
  monday = function(...) return MONDAY({}, ...) end,
  month = function(...) return MONTH({}, ...) end,
  mul = function(...) return MUL({}, ...) end,
  ne = function(...) return NE({}, ...) end,
  not_ = function(...) return NOT({}, ...) end,
  november = function(...) return NOVEMBER({}, ...) end,
  now = function(...) return NOW({}, ...) end,
  nth = function(...) return NTH({}, ...) end,
  object = function(...) return OBJECT({}, ...) end,
  october = function(...) return OCTOBER({}, ...) end,
  offsets_of = function(...) return OFFSETS_OF({}, ...) end,
  or_ = function(...) return OR({}, ...) end,
  order_by = function(...) return ORDER_BY(get_opts(...)) end,
  outer_join = function(...) return OUTER_JOIN({}, ...) end,
  pluck = function(...) return PLUCK({}, ...) end,
  point = function(...) return POINT({}, ...) end,
  polygon = function(...) return POLYGON({}, ...) end,
  polygon_sub = function(...) return POLYGON_SUB({}, ...) end,
  prepend = function(...) return PREPEND({}, ...) end,
  random = function(...) return RANDOM(get_opts(...)) end,
  range = function(...) return RANGE({}, ...) end,
  rebalance = function(...) return REBALANCE({}, ...) end,
  reconfigure = function(...) return RECONFIGURE({}, ...) end,
  reduce = function(...) return REDUCE({}, ...) end,
  replace = function(...) return REPLACE(get_opts(...)) end,
  round = function(...) return ROUND({}, ...) end,
  sample = function(...) return SAMPLE({}, ...) end,
  saturday = function(...) return SATURDAY({}, ...) end,
  seconds = function(...) return SECONDS({}, ...) end,
  september = function(...) return SEPTEMBER({}, ...) end,
  set_difference = function(...) return SET_DIFFERENCE({}, ...) end,
  set_insert = function(...) return SET_INSERT({}, ...) end,
  set_intersection = function(...) return SET_INTERSECTION({}, ...) end,
  set_union = function(...) return SET_UNION({}, ...) end,
  skip = function(...) return SKIP({}, ...) end,
  slice = function(...) return SLICE(get_opts(...)) end,
  splice_at = function(...) return SPLICE_AT({}, ...) end,
  split = function(...) return SPLIT({}, ...) end,
  status = function(...) return STATUS({}, ...) end,
  sub = function(...) return SUB({}, ...) end,
  sum = function(...) return SUM({}, ...) end,
  sunday = function(...) return SUNDAY({}, ...) end,
  sync = function(...) return SYNC({}, ...) end,
  table = function(...) return TABLE(get_opts(...)) end,
  table_create = function(...) return TABLE_CREATE(get_opts(...)) end,
  table_drop = function(...) return TABLE_DROP({}, ...) end,
  table_list = function(...) return TABLE_LIST({}, ...) end,
  thursday = function(...) return THURSDAY({}, ...) end,
  time = function(...) return TIME({}, ...) end,
  timezone = function(...) return TIMEZONE({}, ...) end,
  time_of_day = function(...) return TIME_OF_DAY({}, ...) end,
  to_epoch_time = function(...) return TO_EPOCH_TIME({}, ...) end,
  to_geojson = function(...) return TO_GEOJSON({}, ...) end,
  to_iso8601 = function(...) return TO_ISO8601({}, ...) end,
  to_json_string = function(...) return TO_JSON_STRING({}, ...) end,
  tuesday = function(...) return TUESDAY({}, ...) end,
  type_of = function(...) return TYPE_OF({}, ...) end,
  ungroup = function(...) return UNGROUP({}, ...) end,
  union = function(...) return UNION({}, ...) end,
  upcase = function(...) return UPCASE({}, ...) end,
  update = function(arg0, arg1, opts) return UPDATE(opts, arg0, arg1) end,
  uuid = function(...) return UUID({}, ...) end,
  var = function(...) return VAR({}, ...) end,
  wait = function(...) return WAIT({}, ...) end,
  wednesday = function(...) return WEDNESDAY({}, ...) end,
  without = function(...) return WITHOUT({}, ...) end,
  with_fields = function(...) return WITH_FIELDS({}, ...) end,
  year = function(...) return YEAR({}, ...) end,
  zip = function(...) return ZIP({}, ...) end
}

class_methods = {
  __init = function(self, optargs, ...)
    local args = {...}
    optargs = optargs or {}
    if self.tt == 69 then
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
        table.insert(anon_args, VAR({}, ReQLOp.next_var_id))
        ReQLOp.next_var_id = ReQLOp.next_var_id + 1
      end
      func = func(unpack(anon_args))
      if func == nil then
        return r._logger('Anonymous function returned `nil`. Did you forget a `return`?')
      end
      optargs.arity = nil
      args = {arg_nums, func}
    elseif self.tt == 155 then
      local data = args[1]
      if r.is_instance(data, 'ReQLOp') then
      elseif type(data) == 'string' then
        self.base64_data = mime.b64(table.remove(args, 1))
      else
        return r._logger('Parameter to `r.binary` must be a string or ReQL query.')
      end
    elseif self.tt == 64 then
      local func = table.remove(args)
      if type(func) == 'function' then
        func = FUNC({arity = #args}, func)
      end
      table.insert(args, 1, func)
    elseif self.tt == 37 then
      args[#args] = FUNC({arity = 2}, args[#args])
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
    if self.tt == 155 and (not self.args[1]) then
      return {
        ['$reql_type$'] = 'BINARY',
        data = self.base64_data
      }
    end
    if self.tt == 3 then
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
    if next(self.optargs) then
      local opts = {}
      for key, val in pairs(self.optargs) do
        opts[key] = val:build()
      end
      table.insert(res, opts)
    end
    return res
  end,
  compose = function(self, args, optargs)
    if self.tt == 2 then
      return {
        '{',
        intsp(args),
        '}'
      }
    end
    if self.tt == 3 then
      return kved(optargs)
    end
    if self.tt == 10 then
      return {'var_' .. args[1]}
    end
    if self.tt == 155 and not self.args[1] then
      return 'r.binary(<data>)'
    end
    if self.tt == 170 then
      return {
        args[1],
        '(',
        args[2],
        ')'
      }
    end
    if self.tt == 69 then
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
    if self.tt == 64 then
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
    return BRACKET({}, ...)
  end,
  __add = function(...)
    return ADD({}, ...)
  end,
  __mul = function(...)
    return MUL({}, ...)
  end,
  __mod = function(...)
    return MOD({}, ...)
  end,
  __sub = function(...)
    return SUB({}, ...)
  end,
  __div = function(...)
    return DIV({}, ...)
  end
}

function ast(name, base)
  for k, v in pairs(meta) do
    base[k] = v
  end
  return class(name, ReQLOp, base)
end

DATUMTERM = ast(
  'DATUMTERM',
  {
    __init = function(self, val)
      if type(val) == 'number' then
        if math.abs(val) == math.huge or val ~= val then
          return r._logger('Illegal non-finite number `' .. val .. '`.')
        end
      end
      self.data = val
    end,
    args = {},
    optargs = {},
    compose = function(self)
      if self.data == nil then
        return 'nil'
      end
      return r.json_parser.encode(self.data)
    end,
    build = function(self)
      if self.data == nil then
        if r.json_parser.null then
          return r.json_parser.null
        end
        if r.json_parser.util then
          return r.json_parser.util.null
        end
      end
      return self.data
    end
  }
)

ADD = ast('ADD', {tt = 24, st = 'add'})
AND = ast('AND', {tt = 67, st = 'and_'})
APPEND = ast('APPEND', {tt = 29, st = 'append'})
APRIL = ast('APRIL', {tt = 117, st = 'april'})
ARGS = ast('ARGS', {tt = 154, st = 'args'})
ASC = ast('ASC', {tt = 73, st = 'asc'})
AUGUST = ast('AUGUST', {tt = 121, st = 'august'})
AVG = ast('AVG', {tt = 146, st = 'avg'})
BETWEEN = ast('BETWEEN', {tt = 182, st = 'between'})
BETWEEN_DEPRECATED = ast('BETWEEN_DEPRECATED', {tt = 36, st = 'between_deprecated'})
BINARY = ast('BINARY', {tt = 155, st = 'binary'})
BRACKET = ast('BRACKET', {tt = 170, st = 'index'})
BRANCH = ast('BRANCH', {tt = 65, st = 'branch'})
CEIL = ast('CEIL', {tt = 184, st = 'ceil'})
CHANGES = ast('CHANGES', {tt = 152, st = 'changes'})
CHANGE_AT = ast('CHANGE_AT', {tt = 84, st = 'change_at'})
CIRCLE = ast('CIRCLE', {tt = 165, st = 'circle'})
COERCE_TO = ast('COERCE_TO', {tt = 51, st = 'coerce_to'})
CONCAT_MAP = ast('CONCAT_MAP', {tt = 40, st = 'concat_map'})
CONFIG = ast('CONFIG', {tt = 174, st = 'config'})
CONTAINS = ast('CONTAINS', {tt = 93, st = 'contains'})
COUNT = ast('COUNT', {tt = 43, st = 'count'})
DATE = ast('DATE', {tt = 106, st = 'date'})
DAY = ast('DAY', {tt = 130, st = 'day'})
DAY_OF_WEEK = ast('DAY_OF_WEEK', {tt = 131, st = 'day_of_week'})
DAY_OF_YEAR = ast('DAY_OF_YEAR', {tt = 132, st = 'day_of_year'})
DB = ast('DB', {tt = 14, st = 'db'})
DB_CREATE = ast('DB_CREATE', {tt = 57, st = 'db_create'})
DB_DROP = ast('DB_DROP', {tt = 58, st = 'db_drop'})
DB_LIST = ast('DB_LIST', {tt = 59, st = 'db_list'})
DECEMBER = ast('DECEMBER', {tt = 125, st = 'december'})
DEFAULT = ast('DEFAULT', {tt = 92, st = 'default'})
DELETE = ast('DELETE', {tt = 54, st = 'delete'})
DELETE_AT = ast('DELETE_AT', {tt = 83, st = 'delete_at'})
DESC = ast('DESC', {tt = 74, st = 'desc'})
DIFFERENCE = ast('DIFFERENCE', {tt = 95, st = 'difference'})
DISTANCE = ast('DISTANCE', {tt = 162, st = 'distance'})
DISTINCT = ast('DISTINCT', {tt = 42, st = 'distinct'})
DIV = ast('DIV', {tt = 27, st = 'div'})
DOWNCASE = ast('DOWNCASE', {tt = 142, st = 'downcase'})
DURING = ast('DURING', {tt = 105, st = 'during'})
EPOCH_TIME = ast('EPOCH_TIME', {tt = 101, st = 'epoch_time'})
EQ = ast('EQ', {tt = 17, st = 'eq'})
EQ_JOIN = ast('EQ_JOIN', {tt = 50, st = 'eq_join'})
ERROR = ast('ERROR', {tt = 12, st = 'error_'})
FEBRUARY = ast('FEBRUARY', {tt = 115, st = 'february'})
FILL = ast('FILL', {tt = 167, st = 'fill'})
FILTER = ast('FILTER', {tt = 39, st = 'filter'})
FLOOR = ast('FLOOR', {tt = 183, st = 'floor'})
FOR_EACH = ast('FOR_EACH', {tt = 68, st = 'for_each'})
FRIDAY = ast('FRIDAY', {tt = 111, st = 'friday'})
FUNC = ast('FUNC', {tt = 69, st = 'func'})
FUNCALL = ast('FUNCALL', {tt = 64, st = 'do_'})
GE = ast('GE', {tt = 22, st = 'ge'})
GEOJSON = ast('GEOJSON', {tt = 157, st = 'geojson'})
GET = ast('GET', {tt = 16, st = 'get'})
GET_ALL = ast('GET_ALL', {tt = 78, st = 'get_all'})
GET_FIELD = ast('GET_FIELD', {tt = 31, st = 'get_field'})
GET_INTERSECTING = ast('GET_INTERSECTING', {tt = 166, st = 'get_intersecting'})
GET_NEAREST = ast('GET_NEAREST', {tt = 168, st = 'get_nearest'})
GROUP = ast('GROUP', {tt = 144, st = 'group'})
GT = ast('GT', {tt = 21, st = 'gt'})
HAS_FIELDS = ast('HAS_FIELDS', {tt = 32, st = 'has_fields'})
HOURS = ast('HOURS', {tt = 133, st = 'hours'})
HTTP = ast('HTTP', {tt = 153, st = 'http'})
INCLUDES = ast('INCLUDES', {tt = 164, st = 'includes'})
INDEX_CREATE = ast('INDEX_CREATE', {tt = 75, st = 'index_create'})
INDEX_DROP = ast('INDEX_DROP', {tt = 76, st = 'index_drop'})
INDEX_LIST = ast('INDEX_LIST', {tt = 77, st = 'index_list'})
INDEX_RENAME = ast('INDEX_RENAME', {tt = 156, st = 'index_rename'})
INDEX_STATUS = ast('INDEX_STATUS', {tt = 139, st = 'index_status'})
INDEX_WAIT = ast('INDEX_WAIT', {tt = 140, st = 'index_wait'})
INFO = ast('INFO', {tt = 79, st = 'info'})
INNER_JOIN = ast('INNER_JOIN', {tt = 48, st = 'inner_join'})
INSERT = ast('INSERT', {tt = 56, st = 'insert'})
INSERT_AT = ast('INSERT_AT', {tt = 82, st = 'insert_at'})
INTERSECTS = ast('INTERSECTS', {tt = 163, st = 'intersects'})
IN_TIMEZONE = ast('IN_TIMEZONE', {tt = 104, st = 'in_timezone'})
ISO8601 = ast('ISO8601', {tt = 99, st = 'iso8601'})
IS_EMPTY = ast('IS_EMPTY', {tt = 86, st = 'is_empty'})
JANUARY = ast('JANUARY', {tt = 114, st = 'january'})
JAVASCRIPT = ast('JAVASCRIPT', {tt = 11, st = 'js'})
JSON = ast('JSON', {tt = 98, st = 'json'})
JULY = ast('JULY', {tt = 120, st = 'july'})
JUNE = ast('JUNE', {tt = 119, st = 'june'})
KEYS = ast('KEYS', {tt = 94, st = 'keys'})
LE = ast('LE', {tt = 20, st = 'le'})
LIMIT = ast('LIMIT', {tt = 71, st = 'limit'})
LINE = ast('LINE', {tt = 160, st = 'line'})
LITERAL = ast('LITERAL', {tt = 137, st = 'literal'})
LT = ast('LT', {tt = 19, st = 'lt'})
MAKE_ARRAY = ast('MAKE_ARRAY', {tt = 2, st = 'make_array'})
MAKE_OBJ = ast('MAKE_OBJ', {tt = 3, st = 'make_obj'})
MAP = ast('MAP', {tt = 38, st = 'map'})
MARCH = ast('MARCH', {tt = 116, st = 'march'})
MATCH = ast('MATCH', {tt = 97, st = 'match'})
MAX = ast('MAX', {tt = 148, st = 'max'})
MAXVAL = ast('MAXVAL', {tt = 181, st = 'maxval'})
MAY = ast('MAY', {tt = 118, st = 'may'})
MERGE = ast('MERGE', {tt = 35, st = 'merge'})
MIN = ast('MIN', {tt = 147, st = 'min'})
MINUTES = ast('MINUTES', {tt = 134, st = 'minutes'})
MINVAL = ast('MINVAL', {tt = 180, st = 'minval'})
MOD = ast('MOD', {tt = 28, st = 'mod'})
MONDAY = ast('MONDAY', {tt = 107, st = 'monday'})
MONTH = ast('MONTH', {tt = 129, st = 'month'})
MUL = ast('MUL', {tt = 26, st = 'mul'})
NE = ast('NE', {tt = 18, st = 'ne'})
NOT = ast('NOT', {tt = 23, st = 'not_'})
NOVEMBER = ast('NOVEMBER', {tt = 124, st = 'november'})
NOW = ast('NOW', {tt = 103, st = 'now'})
NTH = ast('NTH', {tt = 45, st = 'nth'})
OBJECT = ast('OBJECT', {tt = 143, st = 'object'})
OCTOBER = ast('OCTOBER', {tt = 123, st = 'october'})
OFFSETS_OF = ast('OFFSETS_OF', {tt = 87, st = 'offsets_of'})
OR = ast('OR', {tt = 66, st = 'or_'})
ORDER_BY = ast('ORDER_BY', {tt = 41, st = 'order_by'})
OUTER_JOIN = ast('OUTER_JOIN', {tt = 49, st = 'outer_join'})
PLUCK = ast('PLUCK', {tt = 33, st = 'pluck'})
POINT = ast('POINT', {tt = 159, st = 'point'})
POLYGON = ast('POLYGON', {tt = 161, st = 'polygon'})
POLYGON_SUB = ast('POLYGON_SUB', {tt = 171, st = 'polygon_sub'})
PREPEND = ast('PREPEND', {tt = 80, st = 'prepend'})
RANDOM = ast('RANDOM', {tt = 151, st = 'random'})
RANGE = ast('RANGE', {tt = 173, st = 'range'})
REBALANCE = ast('REBALANCE', {tt = 179, st = 'rebalance'})
RECONFIGURE = ast('RECONFIGURE', {tt = 176, st = 'reconfigure'})
REDUCE = ast('REDUCE', {tt = 37, st = 'reduce'})
REPLACE = ast('REPLACE', {tt = 55, st = 'replace'})
ROUND = ast('ROUND', {tt = 185, st = 'round'})
SAMPLE = ast('SAMPLE', {tt = 81, st = 'sample'})
SATURDAY = ast('SATURDAY', {tt = 112, st = 'saturday'})
SECONDS = ast('SECONDS', {tt = 135, st = 'seconds'})
SEPTEMBER = ast('SEPTEMBER', {tt = 122, st = 'september'})
SET_DIFFERENCE = ast('SET_DIFFERENCE', {tt = 91, st = 'set_difference'})
SET_INSERT = ast('SET_INSERT', {tt = 88, st = 'set_insert'})
SET_INTERSECTION = ast('SET_INTERSECTION', {tt = 89, st = 'set_intersection'})
SET_UNION = ast('SET_UNION', {tt = 90, st = 'set_union'})
SKIP = ast('SKIP', {tt = 70, st = 'skip'})
SLICE = ast('SLICE', {tt = 30, st = 'slice'})
SPLICE_AT = ast('SPLICE_AT', {tt = 85, st = 'splice_at'})
SPLIT = ast('SPLIT', {tt = 149, st = 'split'})
STATUS = ast('STATUS', {tt = 175, st = 'status'})
SUB = ast('SUB', {tt = 25, st = 'sub'})
SUM = ast('SUM', {tt = 145, st = 'sum'})
SUNDAY = ast('SUNDAY', {tt = 113, st = 'sunday'})
SYNC = ast('SYNC', {tt = 138, st = 'sync'})
TABLE = ast('TABLE', {tt = 15, st = 'table'})
TABLE_CREATE = ast('TABLE_CREATE', {tt = 60, st = 'table_create'})
TABLE_DROP = ast('TABLE_DROP', {tt = 61, st = 'table_drop'})
TABLE_LIST = ast('TABLE_LIST', {tt = 62, st = 'table_list'})
THURSDAY = ast('THURSDAY', {tt = 110, st = 'thursday'})
TIME = ast('TIME', {tt = 136, st = 'time'})
TIMEZONE = ast('TIMEZONE', {tt = 127, st = 'timezone'})
TIME_OF_DAY = ast('TIME_OF_DAY', {tt = 126, st = 'time_of_day'})
TO_EPOCH_TIME = ast('TO_EPOCH_TIME', {tt = 102, st = 'to_epoch_time'})
TO_GEOJSON = ast('TO_GEOJSON', {tt = 158, st = 'to_geojson'})
TO_ISO8601 = ast('TO_ISO8601', {tt = 100, st = 'to_iso8601'})
TO_JSON_STRING = ast('TO_JSON_STRING', {tt = 172, st = 'to_json_string'})
TUESDAY = ast('TUESDAY', {tt = 108, st = 'tuesday'})
TYPE_OF = ast('TYPE_OF', {tt = 52, st = 'type_of'})
UNGROUP = ast('UNGROUP', {tt = 150, st = 'ungroup'})
UNION = ast('UNION', {tt = 44, st = 'union'})
UPCASE = ast('UPCASE', {tt = 141, st = 'upcase'})
UPDATE = ast('UPDATE', {tt = 53, st = 'update'})
UUID = ast('UUID', {tt = 169, st = 'uuid'})
VAR = ast('VAR', {tt = 10, st = 'var'})
WAIT = ast('WAIT', {tt = 177, st = 'wait'})
WEDNESDAY = ast('WEDNESDAY', {tt = 109, st = 'wednesday'})
WITHOUT = ast('WITHOUT', {tt = 34, st = 'without'})
WITH_FIELDS = ast('WITH_FIELDS', {tt = 96, st = 'with_fields'})
YEAR = ast('YEAR', {tt = 128, st = 'year'})
ZIP = ast('ZIP', {tt = 72, st = 'zip'})

local Cursor = class(
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
      if not self._type then self._type = response.n or true end
      if response.r[1] or t == 4 then
        table.insert(self._responses, response)
      end
      if t ~= 3 then
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
      if t == 1 or t == 3 or t == 2 then
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
      if t == 17 then
        return cb(ReQLCompileError(response.r[1], self._root, response.b))
      elseif t == 16 then
        return cb(ReQLClientError(response.r[1], self._root, response.b))
      elseif t == 18 then
        return cb(ReQLRuntimeError(response.r[1], self._root, response.b))
      elseif t == 4 then
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
        return r._logger('First argument to each must be a function.')
      end
      if on_finished and type(on_finished) ~= 'function' then
        return r._logger('Optional second argument to each must be a function.')
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
      if type(self._type) == 'number' then
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

r.connect = class(
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
      self.weight = 0
      self.host = host.host or self.DEFAULT_HOST
      self.port = host.port or self.DEFAULT_PORT
      self.db = host.db -- left nil if this is not set
      self.auth_key = host.auth_key or self.DEFAULT_AUTH_KEY
      self.timeout = host.timeout or self.DEFAULT_TIMEOUT
      self.outstanding_callbacks = {}
      self.next_token = 1
      self.open = false
      self.buffer = ''
      self.raw_socket = socket.tcp()
      self.raw_socket:setkeepalive()
      self.raw_socket:settimeout(self.timeout)
      local status, err = self.raw_socket:connect(self.host, self.port)
      if status then
        local buf, err, partial
        -- Initialize connection with magic number to validate version
        self.raw_socket:send(
          '\62\232\117\95' ..
          int_to_bytes(#(self.auth_key), 4) ..
          self.auth_key ..
          '\199\112\105\126'
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
    DEFAULT_HOST = '127.0.0.1',
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
              t = 16,
              r = {'connection returned: ' .. err},
              b = {}
            },
            reqest_token
          )
        end
        self.buffer = self.buffer .. buf
        if response_length > 0 then
          if #(self.buffer) >= response_length then
            local response_buffer = string.sub(self.buffer, 1, response_length)
            self.buffer = string.sub(self.buffer, response_length + 1)
            response_length = 0
            self:_process_response(r.json_parser.decode(response_buffer), token)
            if token == reqest_token then return end
          end
        else
          if #(self.buffer) >= 12 then
            token = bytes_to_int(self.buffer:sub(1, 8))
            response_length = bytes_to_int(self.buffer:sub(9, 12))
            self.buffer = self.buffer:sub(13)
          end
        end
      end
    end,
    _del_query = function(self, token)
      -- This query is done, delete this cursor
      if self.outstanding_callbacks[token].cursor then
        self.weight = self.weight - 1
      end
      self.outstanding_callbacks[token].cursor = nil
    end,
    _process_response = function(self, response, token)
      local cursor = self.outstanding_callbacks[token]
      if not cursor then
        -- Unexpected token
        return r._logger('Unexpected token ' .. token .. '.')
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
          return r._logger('First argument to two-argument `close` must be a table.')
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
        self.weight = 0
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
      self:_send_query(token, {4})

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
        return r.connect(self, callback)
      end)
    end,
    use = function(self, db)
      self.db = db
    end,
    _start = function(self, term, callback, opts)
      local cb = function(err, cur)
        local res
        if type(callback) == 'function' then
          res = callback(err, cur)
        else
          if err then
            return r._logger(err.message)
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
      self.weight = self.weight + 1

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

      if type(callback) ~= 'function' then
        global_opts.noreply = true
      end

      -- Construct query
      local query = {1, term:build(), global_opts}

      local cursor = Cursor(self, token, opts, term)

      -- Save cursor
      self.outstanding_callbacks[token] = {cursor = cursor}
      self:_send_query(token, query)
      return cb(nil, cursor)
    end,
    _continue_query = function(self, token)
      self:_send_query(token, {2})
    end,
    _end_query = function(self, token)
      self:_del_query(token)
      self:_send_query(token, {3})
    end,
    _send_query = function(self, token, query)
      local data = r.json_parser.encode(query)
      self.raw_socket:send(
        int_to_bytes(token, 8) ..
        int_to_bytes(#data, 4) ..
        data
      )
    end
  }
)

r.pool = class(
  'Pool',
  {
    __init = function(self, host, callback)
      local cb = function(err, pool)
        if not r._pool then
          r._pool = pool
        end
        if callback then
          local res = callback(err, pool)
          pool:close({noreply_wait = false})
          return res
        end
        return pool, err
      end
      self.open = false
      conn, err = r.connect(host)
      if err then return cb(err) end
      self.open = true
      self.pool = {conn}
      self.size = host.size or 12
      self.host = host
      for i=2, self.size do
        table.insert(self.pool, (r.connect(host)))
      end
      return cb(nil, self)
    end,
    close = function(self, opts, callback)
      local err
      local cb = function(e)
        if e then
          err = e
        end
      end
      for _, conn in pairs(self.pool) do
        conn:close(opts, cb)
      end
      self.open = false
      if callback then return callback(err) end
    end,
    _start = function(self, term, callback, opts)
      local weight = math.huge
      local good_conn
      if opts.conn then
        weight = -1
        good_conn = self.pool[opts.conn]
      end
      for i=1, self.size do
        if not self.pool[i] then self.pool[i] = r.connect(self.host) end
        local conn = self.pool[i]
        if not conn.open then conn:reconnect() end
        if conn.weight < weight then
          good_conn = conn
          weight = conn.weight
        end
      end
      return good_conn:_start(term, callback, opts)
    end
  }
)

-- Export all names defined on r
return r
