local json = require('json')
local mime = require('mime')
local socket = require('socket')

-- r is both the main export table for the module
-- and a function that wraps a native Lua value in an ReQL datum
local r = {}

local Connection, Cursor
local DatumTerm, ReQLOp
local Add, All, Any, Append, April, Args, Asc, August, Avg, Between, Binary
local Bracket, Branch, ChangeAt, Changes, Circle, CoerceTo, ConcatMap
local Contains, Count, Date, Day, DayOfWeek, DayOfYear, Db, DbCreate, DbDrop
local DbList, December, Default, Delete, DeleteAt, Desc, Difference, Distance
local Distinct, Div, Do, Downcase, During, EpochTime, Eq, EqJoin, Error
local February, Fill, Filter, ForEach, Friday, Func, Ge, Geojson, Get, GetAll
local GetField, GetIntersecting, GetNearest, Group, Gt, HasFields, Hours, Http
local ISO8601, InTimezone, Includes, IndexCreate, IndexDrop, IndexList
local IndexRename, IndexStatus, IndexWait, IndexesOf, Info, InnerJoin, Insert
local InsertAt, Intersects, IsEmpty, January, JavaScript, Json, July, June
local Keys, Le, Limit, Line, Literal, Lt, MakeArray, MakeObj, Map, March
local Match, Max, May, Merge, Min, Minutes, Mod, Monday, Month, Mul, Ne, Not
local November, Now, Nth, Object, October, OrderBy, OuterJoin, Pluck, Point
local Polygon, PolygonSub, Prepend, Random, Range, Reduce, Replace, Sample
local Saturday, Seconds, September, SetDifference, SetInsert, SetIntersection
local SetUnion, Skip, Slice, SpliceAt, Split, Sub, Sum, Sunday, Sync, Table
local TableCreate, TableDrop, TableList, Thursday, Time, TimeOfDay, Timezone
local ToEpochTime, ToGeojson, ToISO8601, ToJsonString, Tuesday, TypeOf, UUID
local Ungroup, Union, Upcase, Update, Var, Wednesday, WithFields, Without
local Year, Zip
local ReQLDriverError, ReQLServerError, ReQLRuntimeError, ReQLCompileError
local ReQLClientError, ReQLQueryPrinter, ReQLError

function is_instance(obj, ...)
  local class_list = {...}

  for _, cls in ipairs(class_list) do
    if type(cls) == 'string' then
      if type(obj) == cls then
        return true
      end
    else
      class = class.__name
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
    if is_instance(val, 'ReQLOp') then
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

function should_wrap(arg)
  return is_instance(arg, 'DatumTerm', 'MakeArray', 'MakeObj')
end

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
  if (type(pos_opt) == 'table') and (not is_instance(pos_opt, 'ReQLOp')) then
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

    if not is_instance(connection, 'Connection') then
      if callback then
        return callback(ReQLDriverError('First argument to `run` must be a connection.'))
      end
      error('First argument to `run` must be a connection.')
    end

    return connection:_start(self, callback, options or {})
  end,
  add = function(...) return Add({}, ...) end,
  all = function(...) return All({}, ...) end,
  any = function(...) return Any({}, ...) end,
  append = function(...) return Append({}, ...) end,
  april = function(...) return April({}, ...) end,
  args = function(...) return Args({}, ...) end,
  array = function(...) return MakeArray({}, ...) end,
  asc = function(...) return Asc({}, ...) end,
  august = function(...) return August({}, ...) end,
  avg = function(...) return Avg({}, ...) end,
  between = function(self, left, right, opts) return Between(opts, self, left, right) end,
  binary = function(...) return Binary({}, ...) end,
  branch = function(...) return Branch({}, ...) end,
  change_at = function(...) return ChangeAt({}, ...) end,
  changes = function(...) return Changes({}, ...) end,
  circle = function(...) return Circle(get_opts(...)) end,
  coerce_to = function(...) return CoerceTo({}, ...) end,
  concat_map = function(...) return ConcatMap({}, ...) end,
  contains = function(...) return Contains({}, ...) end,
  count = function(...) return Count({}, ...) end,
  date = function(...) return Date({}, ...) end,
  day = function(...) return Day({}, ...) end,
  day_of_week = function(...) return DayOfWeek({}, ...) end,
  day_of_year = function(...) return DayOfYear({}, ...) end,
  db = function(...) return Db({}, ...) end,
  db_create = function(...) return DbCreate({}, ...) end,
  db_drop = function(...) return DbDrop({}, ...) end,
  db_list = function(...) return DbList({}, ...) end,
  december = function(...) return December({}, ...) end,
  default = function(...) return Default({}, ...) end,
  delete = function(...) return Delete(get_opts(...)) end,
  delete_at = function(...) return DeleteAt({}, ...) end,
  desc = function(...) return Desc({}, ...) end,
  difference = function(...) return Difference({}, ...) end,
  distance = function(self, g, opts) return Distance(opts, self, g) end,
  distinct = function(...) return Distinct(get_opts(...)) end,
  div = function(...) return Div({}, ...) end,
  do_ = function(...) return Do({}, ...) end,
  downcase = function(...) return Downcase({}, ...) end,
  during = function(t1, t2, t3, opts) return During(opts, t1, t2, t3) end,
  epoch_time = function(...) return EpochTime({}, ...) end,
  eq = function(...) return Eq({}, ...) end,
  eq_join = function(...) return EqJoin(get_opts(...)) end,
  error_ = function(...) return Error({}, ...) end,
  february = function(...) return February({}, ...) end,
  fill = function(...) return Fill({}, ...) end,
  filter = function(...) return Filter(get_opts(...)) end,
  for_each = function(...) return ForEach({}, ...) end,
  friday = function(...) return Friday({}, ...) end,
  func = function(...) return Func({}, ...) end,
  ge = function(...) return Ge({}, ...) end,
  geojson = function(...) return Geojson({}, ...) end,
  get = function(...) return Get({}, ...) end,
  get_all = function(...) return GetAll(get_opts(...)) end,
  get_field = function(...) return GetField({}, ...) end,
  get_intersecting = function(...) return GetIntersecting(get_opts(...)) end,
  get_nearest = function(...) return GetNearest(get_opts(...)) end,
  group = function(...) return Group(get_opts(...)) end,
  gt = function(...) return Gt({}, ...) end,
  has_fields = function(...) return HasFields({}, ...) end,
  hours = function(...) return Hours({}, ...) end,
  http = function(...) return Http(get_opts(...)) end,
  in_timezone = function(...) return InTimezone({}, ...) end,
  includes = function(...) return Includes({}, ...) end,
  index = function(...) return Bracket({}, ...) end,
  index_create = function(...) return IndexCreate(get_opts(...)) end,
  index_drop = function(...) return IndexDrop({}, ...) end,
  index_list = function(...) return IndexList({}, ...) end,
  index_rename = function(...) return IndexRename(get_opts(...)) end,
  index_status = function(...) return IndexStatus({}, ...) end,
  index_wait = function(...) return IndexWait({}, ...) end,
  indexes_of = function(...) return IndexesOf({}, ...) end,
  info = function(...) return Info({}, ...) end,
  inner_join = function(...) return InnerJoin({}, ...) end,
  insert = function(tbl, doc, opts) return Insert(opts, tbl, doc) end,
  insert_at = function(...) return InsertAt({}, ...) end,
  intersects = function(...) return Intersects({}, ...) end,
  is_empty = function(...) return IsEmpty({}, ...) end,
  iso8601 = function(...) return ISO8601(get_opts(...)) end,
  january = function(...) return January({}, ...) end,
  js = function(...) return JavaScript(get_opts(...)) end,
  json = function(...) return Json({}, ...) end,
  july = function(...) return July({}, ...) end,
  june = function(...) return June({}, ...) end,
  keys = function(...) return Keys({}, ...) end,
  le = function(...) return Le({}, ...) end,
  limit = function(...) return Limit({}, ...) end,
  line = function(...) return Line({}, ...) end,
  literal = function(...) return Literal({}, ...) end,
  lt = function(...) return Lt({}, ...) end,
  make_obj = function(...) return MakeObj({}, ...) end,
  map = function(...) return Map({}, ...) end,
  march = function(...) return March({}, ...) end,
  match = function(...) return Match({}, ...) end,
  max = function(...) return Max({}, ...) end,
  may = function(...) return May({}, ...) end,
  merge = function(...) return Merge({}, ...) end,
  min = function(...) return Min({}, ...) end,
  minutes = function(...) return Minutes({}, ...) end,
  mod = function(...) return Mod({}, ...) end,
  monday = function(...) return Monday({}, ...) end,
  month = function(...) return Month({}, ...) end,
  mul = function(...) return Mul({}, ...) end,
  ne = function(...) return Ne({}, ...) end,
  not_ = function(...) return Not({}, ...) end,
  november = function(...) return November({}, ...) end,
  now = function(...) return Now({}, ...) end,
  nth = function(...) return Nth({}, ...) end,
  object = function(...) return Object({}, ...) end,
  october = function(...) return October({}, ...) end,
  order_by = function(...) return OrderBy(get_opts(...)) end,
  outer_join = function(...) return OuterJoin({}, ...) end,
  pluck = function(...) return Pluck({}, ...) end,
  point = function(...) return Point({}, ...) end,
  polygon = function(...) return Polygon({}, ...) end,
  polygon_sub = function(...) return PolygonSub({}, ...) end,
  prepend = function(...) return Prepend({}, ...) end,
  random = function(...) return Random(get_opts(...)) end,
  range = function(...) return Range({}, ...) end,
  reduce = function(...) return Reduce({}, ...) end,
  replace = function(...) return Replace(get_opts(...)) end,
  sample = function(...) return Sample({}, ...) end,
  saturday = function(...) return Saturday({}, ...) end,
  seconds = function(...) return Seconds({}, ...) end,
  september = function(...) return September({}, ...) end,
  set_difference = function(...) return SetDifference({}, ...) end,
  set_insert = function(...) return SetInsert({}, ...) end,
  set_intersection = function(...) return SetIntersection({}, ...) end,
  set_union = function(...) return SetUnion({}, ...) end,
  skip = function(...) return Skip({}, ...) end,
  slice = function(...) return Slice(get_opts(...)) end,
  splice_at = function(...) return SpliceAt({}, ...) end,
  split = function(...) return Split({}, ...) end,
  sub = function(...) return Sub({}, ...) end,
  sum = function(...) return Sum({}, ...) end,
  sunday = function(...) return Sunday({}, ...) end,
  sync = function(...) return Sync({}, ...) end,
  table = function(...) return Table(get_opts(...)) end,
  table_create = function(...) return TableCreate(get_opts(...)) end,
  table_drop = function(...) return TableDrop({}, ...) end,
  table_list = function(...) return TableList({}, ...) end,
  thursday = function(...) return Thursday({}, ...) end,
  time = function(...) return Time({}, ...) end,
  time_of_day = function(...) return TimeOfDay({}, ...) end,
  timezone = function(...) return Timezone({}, ...) end,
  to_epoch_time = function(...) return ToEpochTime({}, ...) end,
  to_geojson = function(...) return ToGeojson({}, ...) end,
  to_iso8601 = function(...) return ToISO8601({}, ...) end,
  to_json_string = function(...) return ToJsonString({}, ...) end,
  tuesday = function(...) return Tuesday({}, ...) end,
  type_of = function(...) return TypeOf({}, ...) end,
  ungroup = function(...) return Ungroup({}, ...) end,
  union = function(...) return Union({}, ...) end,
  upcase = function(...) return Upcase({}, ...) end,
  update = function(...) return Update(get_opts(...)) end,
  uuid = function(...) return UUID({}, ...) end,
  var = function(...) return Var({}, ...) end,
  wednesday = function(...) return Wednesday({}, ...) end,
  with_fields = function(...) return WithFields({}, ...) end,
  without = function(...) return Without({}, ...) end,
  year = function(...) return Year({}, ...) end,
  zip = function(...) return Zip({}, ...) end
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
        table.insert(anon_args, Var({}, ReQLOp.next_var_id))
        ReQLOp.next_var_id = ReQLOp.next_var_id + 1
      end
      func = func(unpack(anon_args))
      if func == nil then
        error('Anonymous function returned `nil`. Did you forget a `return`?')
      end
      optargs.arity = nil
      args = {{unpack(arg_nums)}, func}
    elseif self.tt == 155 then
      local data = args[1]
      if is_instance(data, 'ReQLOp') then
      elseif type(data) == 'string' then
        self.base64_data = mime.b64(table.remove(args, 1))
      else
        error('Parameter to `r.binary` must be a string or ReQL query.')
      end
    elseif self.tt == 64 then
      local func = table.remove(args)
      if type(func) == 'function' then
        func = Func({arity = #args}, func)
      end
      table.insert(args, 1, func)
    elseif self.tt == 37 then
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
        if math.abs(val) == math.huge or '' .. val == 'nan' then
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

Add = ast('Add', {tt = 24, st = 'add'})
All = ast('All', {tt = 67, st = 'all'})
Any = ast('Any', {tt = 66, st = 'any'})
Append = ast('Append', {tt = 29, st = 'append'})
April = ast('April', {tt = 117, st = 'april'})
Args = ast('Args', {tt = 154, st = 'args'})
Asc = ast('Asc', {tt = 73, st = 'asc'})
August = ast('August', {tt = 121, st = 'august'})
Avg = ast('Avg', {tt = 146, st = 'avg'})
Between = ast('Between', {tt = 36, st = 'between'})
Binary = ast('Binary', {tt = 155, st = 'binary'})
Bracket = ast('Bracket', {tt = 170, st = 'index'})
Branch = ast('Branch', {tt = 65, st = 'branch'})
ChangeAt = ast('ChangeAt', {tt = 84, st = 'change_at'})
Changes = ast('Changes', {tt = 152, st = 'changes'})
Circle = ast('Circle', {tt = 165, st = 'circle'})
CoerceTo = ast('CoerceTo', {tt = 51, st = 'coerce_to'})
ConcatMap = ast('ConcatMap', {tt = 40, st = 'concat_map'})
Contains = ast('Contains', {tt = 93, st = 'contains'})
Count = ast('Count', {tt = 43, st = 'count'})
Date = ast('Date', {tt = 106, st = 'date'})
Day = ast('Day', {tt = 130, st = 'day'})
DayOfWeek = ast('DayOfWeek', {tt = 131, st = 'day_of_week'})
DayOfYear = ast('DayOfYear', {tt = 132, st = 'day_of_year'})
Db = ast('Db', {tt = 14, st = 'db'})
DbCreate = ast('DbCreate', {tt = 57, st = 'db_create'})
DbDrop = ast('DbDrop', {tt = 58, st = 'db_drop'})
DbList = ast('DbList', {tt = 59, st = 'db_list'})
December = ast('December', {tt = 125, st = 'december'})
Default = ast('Default', {tt = 92, st = 'default'})
Delete = ast('Delete', {tt = 54, st = 'delete'})
DeleteAt = ast('DeleteAt', {tt = 83, st = 'delete_at'})
Desc = ast('Desc', {tt = 74, st = 'desc'})
Difference = ast('Difference', {tt = 95, st = 'difference'})
Distance = ast('Distance', {tt = 162, st = 'distance'})
Distinct = ast('Distinct', {tt = 42, st = 'distinct'})
Div = ast('Div', {tt = 27, st = 'div'})
Do = ast('Do', {tt = 64, st = 'do_'})
Downcase = ast('Downcase', {tt = 142, st = 'downcase'})
During = ast('During', {tt = 105, st = 'during'})
EpochTime = ast('EpochTime', {tt = 101, st = 'epoch_time'})
Eq = ast('Eq', {tt = 17, st = 'eq'})
EqJoin = ast('EqJoin', {tt = 50, st = 'eq_join'})
Error = ast('Error', {tt = 12, st = 'error_'})
February = ast('February', {tt = 115, st = 'february'})
Fill = ast('Fill', {tt = 167, st = 'fill'})
Filter = ast('Filter', {tt = 39, st = 'filter'})
ForEach = ast('ForEach', {tt = 68, st = 'for_each'})
Friday = ast('Friday', {tt = 111, st = 'friday'})
Func = ast('Func', {tt = 69, st = 'func'})
Ge = ast('Ge', {tt = 22, st = 'ge'})
Geojson = ast('Geojson', {tt = 157, st = 'geojson'})
Get = ast('Get', {tt = 16, st = 'get'})
GetAll = ast('GetAll', {tt = 78, st = 'get_all'})
GetField = ast('GetField', {tt = 31, st = 'get_field'})
GetIntersecting = ast('GetIntersecting', {tt = 166, st = 'get_intersecting'})
GetNearest = ast('GetNearest', {tt = 168, st = 'get_nearest'})
Group = ast('Group', {tt = 144, st = 'group'})
Gt = ast('Gt', {tt = 21, st = 'gt'})
HasFields = ast('HasFields', {tt = 32, st = 'has_fields'})
Hours = ast('Hours', {tt = 133, st = 'hours'})
Http = ast('Http', {tt = 153, st = 'http'})
ISO8601 = ast('ISO8601', {tt = 99, st = 'iso8601'})
InTimezone = ast('InTimezone', {tt = 104, st = 'in_timezone'})
Includes = ast('Includes', {tt = 164, st = 'includes'})
IndexCreate = ast('IndexCreate', {tt = 75, st = 'index_create'})
IndexDrop = ast('IndexDrop', {tt = 76, st = 'index_drop'})
IndexList = ast('IndexList', {tt = 77, st = 'index_list'})
IndexRename = ast('IndexRename', {tt = 156, st = 'index_rename'})
IndexStatus = ast('IndexStatus', {tt = 139, st = 'index_status'})
IndexWait = ast('IndexWait', {tt = 140, st = 'index_wait'})
IndexesOf = ast('IndexesOf', {tt = 87, st = 'indexes_of'})
Info = ast('Info', {tt = 79, st = 'info'})
InnerJoin = ast('InnerJoin', {tt = 48, st = 'inner_join'})
Insert = ast('Insert', {tt = 56, st = 'insert'})
InsertAt = ast('InsertAt', {tt = 82, st = 'insert_at'})
Intersects = ast('Intersects', {tt = 163, st = 'intersects'})
IsEmpty = ast('IsEmpty', {tt = 86, st = 'is_empty'})
January = ast('January', {tt = 114, st = 'january'})
JavaScript = ast('JavaScript', {tt = 11, st = 'js'})
Json = ast('Json', {tt = 98, st = 'json'})
July = ast('July', {tt = 120, st = 'july'})
June = ast('June', {tt = 119, st = 'june'})
Keys = ast('Keys', {tt = 94, st = 'keys'})
Le = ast('Le', {tt = 20, st = 'le'})
Limit = ast('Limit', {tt = 71, st = 'limit'})
Line = ast('Line', {tt = 160, st = 'line'})
Literal = ast('Literal', {tt = 137, st = 'literal'})
Lt = ast('Lt', {tt = 19, st = 'lt'})
MakeArray = ast('MakeArray', {tt = 2, st = 'array'})
MakeObj = ast('MakeObj', {tt = 3, st = 'make_obj'})
Map = ast('Map', {tt = 38, st = 'map'})
March = ast('March', {tt = 116, st = 'march'})
Match = ast('Match', {tt = 97, st = 'match'})
Max = ast('Max', {tt = 148, st = 'max'})
May = ast('May', {tt = 118, st = 'may'})
Merge = ast('Merge', {tt = 35, st = 'merge'})
Min = ast('Min', {tt = 147, st = 'min'})
Minutes = ast('Minutes', {tt = 134, st = 'minutes'})
Mod = ast('Mod', {tt = 28, st = 'mod'})
Monday = ast('Monday', {tt = 107, st = 'monday'})
Month = ast('Month', {tt = 129, st = 'month'})
Mul = ast('Mul', {tt = 26, st = 'mul'})
Ne = ast('Ne', {tt = 18, st = 'ne'})
Not = ast('Not', {tt = 23, st = 'not_'})
November = ast('November', {tt = 124, st = 'november'})
Now = ast('Now', {tt = 103, st = 'now'})
Nth = ast('Nth', {tt = 45, st = 'nth'})
Object = ast('Object', {tt = 143, st = 'object'})
October = ast('October', {tt = 123, st = 'october'})
OrderBy = ast('OrderBy', {tt = 41, st = 'order_by'})
OuterJoin = ast('OuterJoin', {tt = 49, st = 'outer_join'})
Pluck = ast('Pluck', {tt = 33, st = 'pluck'})
Point = ast('Point', {tt = 159, st = 'point'})
Polygon = ast('Polygon', {tt = 161, st = 'polygon'})
PolygonSub = ast('PolygonSub', {tt = 171, st = 'polygon_sub'})
Prepend = ast('Prepend', {tt = 80, st = 'prepend'})
Random = ast('Random', {tt = 151, st = 'random'})
Range = ast('Range', {tt = 173, st = 'range'})
Reduce = ast('Reduce', {tt = 37, st = 'reduce'})
Replace = ast('Replace', {tt = 55, st = 'replace'})
Sample = ast('Sample', {tt = 81, st = 'sample'})
Saturday = ast('Saturday', {tt = 112, st = 'saturday'})
Seconds = ast('Seconds', {tt = 135, st = 'seconds'})
September = ast('September', {tt = 122, st = 'september'})
SetDifference = ast('SetDifference', {tt = 91, st = 'set_difference'})
SetInsert = ast('SetInsert', {tt = 88, st = 'set_insert'})
SetIntersection = ast('SetIntersection', {tt = 89, st = 'set_intersection'})
SetUnion = ast('SetUnion', {tt = 90, st = 'set_union'})
Skip = ast('Skip', {tt = 70, st = 'skip'})
Slice = ast('Slice', {tt = 30, st = 'slice'})
SpliceAt = ast('SpliceAt', {tt = 85, st = 'splice_at'})
Split = ast('Split', {tt = 149, st = 'split'})
Sub = ast('Sub', {tt = 25, st = 'sub'})
Sum = ast('Sum', {tt = 145, st = 'sum'})
Sunday = ast('Sunday', {tt = 113, st = 'sunday'})
Sync = ast('Sync', {tt = 138, st = 'sync'})
Table = ast('Table', {tt = 15, st = 'table'})
TableCreate = ast('TableCreate', {tt = 60, st = 'table_create'})
TableDrop = ast('TableDrop', {tt = 61, st = 'table_drop'})
TableList = ast('TableList', {tt = 62, st = 'table_list'})
Thursday = ast('Thursday', {tt = 110, st = 'thursday'})
Time = ast('Time', {tt = 136, st = 'time'})
TimeOfDay = ast('TimeOfDay', {tt = 126, st = 'time_of_day'})
Timezone = ast('Timezone', {tt = 127, st = 'timezone'})
ToEpochTime = ast('ToEpochTime', {tt = 102, st = 'to_epoch_time'})
ToGeojson = ast('ToGeojson', {tt = 158, st = 'to_geojson'})
ToISO8601 = ast('ToISO8601', {tt = 100, st = 'to_iso8601'})
ToJsonString = ast('ToJsonString', {tt = 172, st = 'to_json_string'})
Tuesday = ast('Tuesday', {tt = 108, st = 'tuesday'})
TypeOf = ast('TypeOf', {tt = 52, st = 'type_of'})
UUID = ast('UUID', {tt = 169, st = 'uuid'})
Ungroup = ast('Ungroup', {tt = 150, st = 'ungroup'})
Union = ast('Union', {tt = 44, st = 'union'})
Upcase = ast('Upcase', {tt = 141, st = 'upcase'})
Update = ast('Update', {tt = 53, st = 'update'})
Var = ast('Var', {tt = 10, st = 'var'})
Wednesday = ast('Wednesday', {tt = 109, st = 'wednesday'})
WithFields = ast('WithFields', {tt = 96, st = 'with_fields'})
Without = ast('Without', {tt = 34, st = 'without'})
Year = ast('Year', {tt = 128, st = 'year'})
Zip = ast('Zip', {tt = 72, st = 'zip'})

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
      if response.r[1] or t == 4 then
        table.insert(self._responses, response)
      end
      if t ~= 3 and t ~= 5 then
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
      local cb = function(err, row)
        return callback(err, row)
      end
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
      if t == 1 or t == 3 or t == 5 or t == 2 then
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
      elseif t == 17 then
        return cb(ReQLCompileError(response.r[1], self._root, response.b))
      elseif t == 16 then
        return cb(ReQLClientError(response.r[1], self._root, response.b))
      elseif t == 18 then
        return cb(ReQLRuntimeError(response.r[1], self._root, response.b))
      elseif t == 4 then
        return cb(nil, nil)
      end
      return cb(ReQLDriverError('Unknown response type ' .. t))
    end,
    close = function(self, callback)
      if not self._end_flag then
        self._conn:_end_query(self._token)
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
      if not self._type then self:_prompt_cont() end
      if self._type == 5 then
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
      else
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
          '\62\232\117\95' ..
          int_to_bytes(#self.auth_key, 4) ..
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
              t = 16,
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
      local data = json.encode(query)
      self.raw_socket:send(
        int_to_bytes(token, 8) ..
        int_to_bytes(#data, 4) ..
        data
      )
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
