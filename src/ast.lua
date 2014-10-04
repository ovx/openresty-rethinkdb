local json = require('json')
local mime = require('mime')

local errors = require('./errors')
local util = require('./util')

-- Import some names to this namespace for convienience
local is_instance = util.is_instance

-- r is both the main export object for the module
-- and a function that shortcuts `r.expr`.
local r = {}
setmetatable(r, {
  __call = function(cls, ...)
    return r.expr(...)
  end
})

local DatumTerm, ReQLOp, MakeArray, MakeObj, Var, PolygonSub
local JavaScript, Http, Json, Binary, Args, Error, Random, Db
local Table, Get, GetAll, Eq, Ne, Lt, Le, Gt, Ge, Not, Add, Sub, Mul, Div, Mod
local Append, Prepend, Difference, SetInsert, SetUnion, SetIntersection
local SetDifference, Slice, Skip, Limit, GetField, Bracket, Contains, InsertAt
local SpliceAt, DeleteAt, ChangeAt, HasFields, WithFields, Keys, Changes
local Object, Pluck, IndexesOf, Without, Merge, Between, Reduce, Map, Filter
local ConcatMap, OrderBy, Distinct, Count, Union, Nth, Match, Split, Upcase
local Downcase, IsEmpty, Group, Sum, Avg, Min, Max, InnerJoin, OuterJoin
local EqJoin, Zip, CoerceTo, Ungroup, TypeOf, Info, Sample, Update, Delete
local Replace, Insert, DbCreate, DbDrop, DbList, TableCreate, TableDrop
local TableList, IndexCreate, IndexDrop, IndexRename, IndexList, IndexStatus
local IndexWait, Sync, FunCall, Default, Branch, Any, All, ForEach, Func, Asc
local Desc, Literal, ISO8601, ToISO8601, EpochTime, ToEpochTime, Now
local InTimezone, During, Date, TimeOfDay, Timezone, Year, Month, Day
local DayOfWeek, DayOfYear, Hours, Minutes, Seconds, Time, GeoJson, ToGeoJson
local Point, Line, Polygon, Distance, Intersects, Includes, Circle
local GetIntersecting, GetNearest, Fill, UUID, Monday, Tuesday, Wednesday
local Thursday, Friday, Saturday, Sunday, January, February, March, April, May
local June, July, August, September, October, November, December, ToJsonString

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
  return is_instance(DatumTerm, arg) or is_instance(MakeArray, arg) or is_instance(MakeObj, arg)
end

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

function class(name, parent, base)
  if base == nil then
    base = parent
    parent = nil
  end

  if type(base) == 'function' then
    base = {__init = base}
  end
  for k, v in pairs(meta) do
    base[k] = v
  end
  return util.class(name, parent, base)
end

-- AST classes

ReQLOp = class(
  'ReQLOp',
  {
    __init = function(self, optargs, ...)
      optargs = optargs or {}
      self.args = {...}
      local first = self.args[1]
      if self.tt == 69 then
        local args = {}
        local arg_nums = {}
        for i=1, optargs.arity or 1 do
          table.insert(arg_nums, ReQLOp.next_var_id)
          table.insert(args, Var({}, ReQLOp.next_var_id))
          ReQLOp.next_var_id = ReQLOp.next_var_id + 1
        end
        first = first(unpack(args))
        if first == nil then
          error(errors.ReQLDriverError('Anonymous function returned `nil`. Did you forget a `return`?'))
        end
        optargs.arity = nil
        self.args = {MakeArray({}, arg_nums), r.expr(first)}
      elseif self.tt == 155 then
        if is_instance(ReQLOp, first) then
        elseif type(first) == 'string' then
          self.base64_data = mime.b64(first)
        else
          error('Parameter to `r.binary` must be a string or ReQL query.')
        end
      elseif self.tt == 2 then
        self.args = first
      elseif self.tt == 3 then
      else
        for i, a in ipairs(self.args) do
          self.args[i] = r.expr(a)
        end
      end
      self.optargs = optargs
    end,
    build = function(self)
      if self.tt == 155 and (not self.args[1]) then
        return {
          ['$reql_type$'] = 'BINARY',
          data = self.base64_data
        }
      end
      if self.tt == 2 then
        local args = {}
        for i, arg in ipairs(self.args) do
          if is_instance(ReQLOp, arg) then
            args[i] = arg:build()
          else
            args[i] = arg
          end
        end
        return {self.tt, args}
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
        if not args then return {} end
        for i, v in ipairs(args) do
          args[i] = 'var_' .. v
        end
        return args
      end
      if self.tt == 155 then
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
      if self.tt == 13 then
        return {
          'r.row'
        }
      end
      if self.tt == 15 then
        if is_instance(Db, self.args[1]) then
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
      if self.tt == 170 then
        return {
          args[1],
          '(',
          args[2],
          ')'
        }
      end
      if self.tt == 69 then
        if ivar_scan(self.args[2]) then
          return {
            args[2]
          }
        end
        local var_str = ''
        for i, arg in ipairs(args[1][2]) do -- ['0', ', ', '1']
          if i % 2 == 0 then
            var_str = var_str .. Var.compose(arg)
          else
            var_str = var_str .. arg
          end
        end
        return {
          'function(',
          var_str,
          ') return ',
          args[1],
          ' end'
        }
      end
      if self.tt == 64 then
        if #args > 2 then
          return {
            'r.do_(',
            intsp((function()
              local _accum_0 = {}
              for _index_0 = 2, #args do
                _accum_0[_index_0 - 1] = args[_index_0]
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
    run = function(self, connection, options, callback)
      -- Valid syntaxes are
      -- connection, callback
      -- connection, options, callback
      -- connection, nil, callback

      -- Handle run(connection, callback)
      if type(options) == 'function' then
        if not callback then
          callback = options
          options = {}
        else
          return options(errors.ReQLDriverError('Second argument to `run` cannot be a function if a third argument is provided.'))
        end
      end
      -- else we suppose that we have run(connection[, options][, callback])
      options = options or {}

      if type(connection._start) ~= 'function' then
        if callback then
          return callback(errors.ReQLDriverError('First argument to `run` must be an open connection.'))
        end
        return
      end

      return connection:_start(self, callback, options)
    end,
    next_var_id = 0,
    eq = function(...)
      return Eq({}, ...)
    end,
    ne = function(...)
      return Ne({}, ...)
    end,
    lt = function(...)
      return Lt({}, ...)
    end,
    le = function(...)
      return Le({}, ...)
    end,
    gt = function(...)
      return Gt({}, ...)
    end,
    ge = function(...)
      return Ge({}, ...)
    end,
    not_ = function(...)
      return Not({}, ...)
    end,
    add = function(...)
      return Add({}, ...)
    end,
    sub = function(...)
      return Sub({}, ...)
    end,
    mul = function(...)
      return Mul({}, ...)
    end,
    div = function(...)
      return Div({}, ...)
    end,
    mod = function(...)
      return Mod({}, ...)
    end,
    append = function(...)
      return Append({}, ...)
    end,
    prepend = function(...)
      return Prepend({}, ...)
    end,
    difference = function(...)
      return Difference({}, ...)
    end,
    set_insert = function(...)
      return SetInsert({}, ...)
    end,
    set_union = function(...)
      return SetUnion({}, ...)
    end,
    set_intersection = function(...)
      return SetIntersection({}, ...)
    end,
    set_difference = function(...)
      return SetDifference({}, ...)
    end,
    slice = function(self, left, right_or_opts, opts)
      if opts then
        return Slice(opts, self, left, right_or_opts)
      end
      if right_or_opts then
        if (type(right_or_opts) == 'table') and (not is_instance(ReQLOp, right_or_opts)) then
          return Slice(right_or_opts, self, left)
        end
        return Slice({}, self, left, right_or_opts)
      end
      return Slice({}, self, left)
    end,
    skip = function(...)
      return Skip({}, ...)
    end,
    limit = function(...)
      return Limit({}, ...)
    end,
    get_field = function(...)
      return GetField({}, ...)
    end,
    contains = function(...)
      return Contains({}, ...)
    end,
    insert_at = function(...)
      return InsertAt({}, ...)
    end,
    splice_at = function(...)
      return SpliceAt({}, ...)
    end,
    delete_at = function(...)
      return DeleteAt({}, ...)
    end,
    change_at = function(...)
      return ChangeAt({}, ...)
    end,
    indexes_of = function(...)
      return IndexesOf({}, ...)
    end,
    has_fields = function(...)
      return HasFields({}, ...)
    end,
    with_fields = function(...)
      return WithFields({}, ...)
    end,
    keys = function(...)
      return Keys({}, ...)
    end,
    changes = function(...)
      return Changes({}, ...)
    end,

    -- pluck and without on zero fields are allowed
    pluck = function(...)
      return Pluck({}, ...)
    end,
    without = function(...)
      return Without({}, ...)
    end,
    merge = function(...)
      return Merge({}, ...)
    end,
    between = function(self, left, right, opts)
      return Between(opts, self, left, right)
    end,
    reduce = function(...)
      return Reduce({arity = 2}, ...)
    end,
    map = function(...)
      return Map({}, ...)
    end,
    filter = function(self, predicate, opts)
      return Filter(opts, self, predicate)
    end,
    concat_map = function(...)
      return ConcatMap({}, ...)
    end,
    distinct = function(self, opts)
      return Distinct(opts, self)
    end,
    count = function(...)
      return Count({}, ...)
    end,
    union = function(...)
      return Union({}, ...)
    end,
    nth = function(...)
      return Nth({}, ...)
    end,
    to_json = function(...)
      return ToJsonString({}, ...)
    end,
    match = function(...)
      return Match({}, ...)
    end,
    split = function(...)
      return Split({}, ...)
    end,
    upcase = function(...)
      return Upcase({}, ...)
    end,
    downcase = function(...)
      return Downcase({}, ...)
    end,
    is_empty = function(...)
      return IsEmpty({}, ...)
    end,
    inner_join = function(...)
      return InnerJoin({}, ...)
    end,
    outer_join = function(...)
      return OuterJoin({}, ...)
    end,
    eq_join = function(self, left_attr, right, opts)
      return EqJoin(opts, self, r.expr(left_attr), right)
    end,
    zip = function(...)
      return Zip({}, ...)
    end,
    coerce_to = function(...)
      return CoerceTo({}, ...)
    end,
    ungroup = function(...)
      return Ungroup({}, ...)
    end,
    type_of = function(...)
      return TypeOf({}, ...)
    end,
    update = function(self, func, opts)
      return Update(opts, self, Func({}, func))
    end,
    delete = function(self, opts)
      return Delete(opts, self)
    end,
    replace = function(self, func, opts)
      return Replace(opts, self, Func({}, func))
    end,
    do_ = function(self, ...)
      local args = {...}
      local func = Func({arity = args.n - 1}, args[args.n])
      args[args.n] = nil
      return FunCall({}, func, self, unpack(args))
    end,
    default = function(...)
      return Default({}, ...)
    end,
    any = function(...)
      return Any({}, ...)
    end,
    all = function(...)
      return All({}, ...)
    end,
    for_each = function(...)
      return ForEach({}, ...)
    end,
    sum = function(...)
      return Sum({}, ...)
    end,
    avg = function(...)
      return Avg({}, ...)
    end,
    min = function(...)
      return Min({}, ...)
    end,
    max = function(...)
      return Max({}, ...)
    end,
    info = function(...)
      return Info({}, ...)
    end,
    sample = function(...)
      return Sample({}, ...)
    end,
    group = function(self, ...)
      -- Default if no opts dict provided
      local opts = {}
      local fields = {...}

      -- Look for opts dict
      if fields.n > 0 then
        local perhaps_opt_dict = fields[fields.n]
        if perhaps_opt_dict and (type(perhaps_opt_dict) == 'table') and not (is_instance(ReQLOp, perhaps_opt_dict)) then
          opts = perhaps_opt_dict
          fields[fields.n] = nil
          fields.n = fields.n - 1
        end
      end
      for i=1, fields.n do
        fields[i] = r.expr(fields[i])
      end
      return Group(opts, self, unpack(fields))
    end,
    order_by = function(self, ...)
      -- Default if no opts dict provided
      local opts = {}
      local attrs = {...}

      -- Look for opts dict
      local perhaps_opt_dict = attrs[attrs.n]
      if perhaps_opt_dict and (type(perhaps_opt_dict) == 'table') and not is_instance(ReQLOp, perhaps_opt_dict) then
        opts = perhaps_opt_dict
        attrs[attrs.n] = nil
        attrs.n = attrs.n - 1
      end
      for i, attr in ipairs(attrs) do
        if not (is_instance(Asc, attr) or is_instance(Desc, attr)) then
          attrs[i] = r.expr(attr)
        end
      end
      return OrderBy(opts, self, unpack(attrs))
    end,

    -- Geo operations
    to_geo_json = function(...)
      return ToGeoJson({}, ...)
    end,
    distance = function(self, g, opts)
      return Distance(opts, self, g)
    end,
    intersects = function(...)
      return Intersects({}, ...)
    end,
    includes = function(...)
      return Includes({}, ...)
    end,
    fill = function(...)
      return Fill({}, ...)
    end,
    polygon_sub = function(...)
      return PolygonSub({}, ...)
    end,

    -- Database operations

    table_create = function(self, tbl_name, opts)
      return TableCreate(opts, self, tbl_name)
    end,
    table_drop = function(...)
      return TableDrop({}, ...)
    end,
    table_list = function(...)
      return TableList({}, ...)
    end,
    table = function(self, tbl_name, opts)
      return Table(opts, self, tbl_name)
    end,

    -- Table operations

    get = function(...)
      return Get({}, ...)
    end,
    get_all = function(self, ...)
      -- Default if no opts dict provided
      local opts = {}
      local keys = {...}

      -- Look for opts dict
      if keys.n > 1 then
        local perhaps_opt_dict = keys[keys.n]
        if (type(perhaps_opt_dict) == 'table') and (not is_instance(ReQLOp, perhaps_opt_dict)) then
          opts = perhaps_opt_dict
          keys[keys.n] = nil
        end
      end
      return GetAll(opts, self, unpack(keys))
    end,
    insert = function(self, doc, opts)
      return Insert(opts, self, r.expr(doc))
    end,
    index_create = function(self, name, defun_or_opts, opts)
      if opts then
        return IndexCreate(opts, self, name, r.expr(defun_or_opts))
      end
      if defun_or_opts then
        if (type(defun_or_opts) == 'table') and (not is_instance(ReQLOp, defun_or_opts)) then
          return IndexCreate(defun_or_opts, self, name)
        end
        return IndexCreate({}, self, name, r.expr(defun_or_opts))
      end
      return IndexCreate({}, self, name)
    end,
    index_drop = function(...)
      return IndexDrop({}, ...)
    end,
    index_list = function(...)
      return IndexList({}, ...)
    end,
    index_status = function(...)
      return IndexStatus({}, ...)
    end,
    index_wait = function(...)
      return IndexWait({}, ...)
    end,
    index_rename = function(self, old_name, new_name, opts)
      return IndexRename(opts, self, old_name, new_name)
    end,
    sync = function(...)
      return Sync({}, ...)
    end,
    to_iso8601 = function(...)
      return ToISO8601({}, ...)
    end,
    to_epoch_time = function(...)
      return ToEpochTime({}, ...)
    end,
    in_timezone = function(...)
      return InTimezone({}, ...)
    end,
    during = function(self, t2, t3, opts)
      return During(opts, self, t2, t3)
    end,
    date = function(...)
      return Date({}, ...)
    end,
    time_of_day = function(...)
      return TimeOfDay({}, ...)
    end,
    timezone = function(...)
      return Timezone({}, ...)
    end,
    year = function(...)
      return Year({}, ...)
    end,
    month = function(...)
      return Month({}, ...)
    end,
    day = function(...)
      return Day({}, ...)
    end,
    day_of_week = function(...)
      return DayOfWeek({}, ...)
    end,
    day_of_year = function(...)
      return DayOfYear({}, ...)
    end,
    hours = function(...)
      return Hours({}, ...)
    end,
    minutes = function(...)
      return Minutes({}, ...)
    end,
    seconds = function(...)
      return Seconds({}, ...)
    end,
    uuid = function(...)
      return UUID({}, ...)
    end,
    get_intersecting = function(self, g, opts)
      return GetIntersecting(opts, self, g)
    end,
    get_nearest = function(self, g, opts)
      return GetNearest(opts, self, g)
    end
  }
)

DatumTerm = class(
  'DatumTerm', ReQLOp,
  {
    __init = function(self, val)
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
      if type(self.data) == 'number' then
        if math.abs(self.data) == 1/0 or self.data == ((1/0) * 0) then
          error('Illegal non-finite number `' .. self.data .. '`.')
        end
      end
      if self.data == nil then return json.util.null end
      return self.data
    end
  }
)

MakeArray = class('MakeArray', ReQLOp, {tt = 2, st = '{...}'})
MakeObj = class('MakeObj', ReQLOp, {tt = 3, st = 'make_obj'})
Var = class('Var', ReQLOp, {tt = 10, st = 'var'})
JavaScript = class('JavaScript', ReQLOp, {tt = 11, st = 'js'})
Http = class('Http', ReQLOp, {tt = 153, st = 'http'})
Json = class('Json', ReQLOp, {tt = 98, st = 'json'})
Binary = class('Binary', ReQLOp, {tt = 155, st = 'binary'})
Args = class('Args', ReQLOp, {tt = 154, st = 'args'})
Error = class('Error', ReQLOp, {tt = 12, st = 'error'})
Random = class('Random', ReQLOp, {tt = 151, st = 'random'})
Db = class('Db', ReQLOp, {tt = 14, st = 'db'})
Table = class('Table', ReQLOp, {tt = 15, st = 'table'})
Get = class('Get', ReQLOp, {tt = 16, st = 'get'})
GetAll = class('GetAll', ReQLOp, {tt = 78, st = 'get_all'})
Eq = class('Eq', ReQLOp, {tt = 17, st = 'eq'})
Ne = class('Ne', ReQLOp, {tt = 18, st = 'ne'})
Lt = class('Lt', ReQLOp, {tt = 19, st = 'lt'})
Le = class('Le', ReQLOp, {tt = 20, st = 'le'})
Gt = class('Gt', ReQLOp, {tt = 21, st = 'gt'})
Ge = class('Ge', ReQLOp, {tt = 22, st = 'ge'})
Not = class('Not', ReQLOp, {tt = 23, st = 'not_'})
Add = class('Add', ReQLOp, {tt = 24, st = 'add'})
Sub = class('Sub', ReQLOp, {tt = 25, st = 'sub'})
Mul = class('Mul', ReQLOp, {tt = 26, st = 'mul'})
Div = class('Div', ReQLOp, {tt = 27, st = 'div'})
Mod = class('Mod', ReQLOp, {tt = 28, st = 'mod'})
Append = class('Append', ReQLOp, {tt = 29, st = 'append'})
Prepend = class('Prepend', ReQLOp, {tt = 80, st = 'prepend'})
Difference = class('Difference', ReQLOp, {tt = 95, st = 'difference'})
SetInsert = class('SetInsert', ReQLOp, {tt = 88, st = 'set_insert'})
SetUnion = class('SetUnion', ReQLOp, {tt = 90, st = 'set_union'})
SetIntersection = class('SetIntersection', ReQLOp, {tt = 89, st = 'set_intersection'})
SetDifference = class('SetDifference', ReQLOp, {tt = 91, st = 'set_difference'})
Slice = class('Slice', ReQLOp, {tt = 30, st = 'slice'})
Skip = class('Skip', ReQLOp, {tt = 70, st = 'skip'})
Limit = class('Limit', ReQLOp, {tt = 71, st = 'limit'})
GetField = class('GetField', ReQLOp, {tt = 31, st = 'get_field'})
Bracket = class('Bracket', ReQLOp, {tt = 170, st = '(...)'})
Contains = class('Contains', ReQLOp, {tt = 93, st = 'contains'})
InsertAt = class('InsertAt', ReQLOp, {tt = 82, st = 'insert_at'})
SpliceAt = class('SpliceAt', ReQLOp, {tt = 85, st = 'splice_at'})
DeleteAt = class('DeleteAt', ReQLOp, {tt = 83, st = 'delete_at'})
ChangeAt = class('ChangeAt', ReQLOp, {tt = 84, st = 'change_at'})
Contains = class('Contains', ReQLOp, {tt = 93, st = 'contains'})
HasFields = class('HasFields', ReQLOp, {tt = 32, st = 'has_fields'})
WithFields = class('WithFields', ReQLOp, {tt = 96, st = 'with_fields'})
Keys = class('Keys', ReQLOp, {tt = 94, st = 'keys'})
Changes = class('Changes', ReQLOp, {tt = 152, st = 'changes'})
Object = class('Object', ReQLOp, {tt = 143, st = 'object'})
Pluck = class('Pluck', ReQLOp, {tt = 33, st = 'pluck'})
IndexesOf = class('IndexesOf', ReQLOp, {tt = 87, st = 'indexes_of'})
Without = class('Without', ReQLOp, {tt = 34, st = 'without'})
Merge = class('Merge', ReQLOp, {tt = 35, st = 'merge'})
Between = class('Between', ReQLOp, {tt = 36, st = 'between'})
Reduce = class('Reduce', ReQLOp, {tt = 37, st = 'reduce'})
Map = class('Map', ReQLOp, {tt = 38, st = 'map'})
Filter = class('Filter', ReQLOp, {tt = 39, st = 'filter'})
ConcatMap = class('ConcatMap', ReQLOp, {tt = 40, st = 'concat_map'})
OrderBy = class('OrderBy', ReQLOp, {tt = 41, st = 'order_by'})
Distinct = class('Distinct', ReQLOp, {tt = 42, st = 'distinct'})
Count = class('Count', ReQLOp, {tt = 43, st = 'count'})
Union = class('Union', ReQLOp, {tt = 44, st = 'union'})
Nth = class('Nth', ReQLOp, {tt = 45, st = 'nth'})
ToJsonString = class('ToJsonString', ReQLOp, {tt = 172, st = 'to_json_string'})
Match = class('Match', ReQLOp, {tt = 97, st = 'match'})
Split = class('Split', ReQLOp, {tt = 149, st = 'split'})
Upcase = class('Upcase', ReQLOp, {tt = 141, st = 'upcase'})
Downcase = class('Downcase', ReQLOp, {tt = 142, st = 'downcase'})
IsEmpty = class('IsEmpty', ReQLOp, {tt = 86, st = 'is_empty'})
Group = class('Group', ReQLOp, {tt = 144, st = 'group'})
Sum = class('Sum', ReQLOp, {tt = 145, st = 'sum'})
Avg = class('Avg', ReQLOp, {tt = 146, st = 'avg'})
Min = class('Min', ReQLOp, {tt = 147, st = 'min'})
Max = class('Max', ReQLOp, {tt = 148, st = 'max'})
InnerJoin = class('InnerJoin', ReQLOp, {tt = 48, st = 'inner_join'})
OuterJoin = class('OuterJoin', ReQLOp, {tt = 49, st = 'outer_join'})
EqJoin = class('EqJoin', ReQLOp, {tt = 50, st = 'eq_join'})
Zip = class('Zip', ReQLOp, {tt = 72, st = 'zip'})
CoerceTo = class('CoerceTo', ReQLOp, {tt = 51, st = 'coerce_to'})
Ungroup = class('Ungroup', ReQLOp, {tt = 150, st = 'ungroup'})
TypeOf = class('TypeOf', ReQLOp, {tt = 52, st = 'type_of'})
Info = class('Info', ReQLOp, {tt = 79, st = 'info'})
Sample = class('Sample', ReQLOp, {tt = 81, st = 'sample'})
Update = class('Update', ReQLOp, {tt = 53, st = 'update'})
Delete = class('Delete', ReQLOp, {tt = 54, st = 'delete'})
Replace = class('Replace', ReQLOp, {tt = 55, st = 'replace'})
Insert = class('Insert', ReQLOp, {tt = 56, st = 'insert'})
DbCreate = class('DbCreate', ReQLOp, {tt = 57, st = 'db_create'})
DbDrop = class('DbDrop', ReQLOp, {tt = 58, st = 'db_drop'})
DbList = class('DbList', ReQLOp, {tt = 59, st = 'db_list'})
TableCreate = class('TableCreate', ReQLOp, {tt = 60, st = 'table_create'})
TableDrop = class('TableDrop', ReQLOp, {tt = 61, st = 'table_drop'})
TableList = class('TableList', ReQLOp, {tt = 62, st = 'table_list'})
IndexCreate = class('IndexCreate', ReQLOp, {tt = 75, st = 'index_create'})
IndexDrop = class('IndexDrop', ReQLOp, {tt = 76, st = 'index_drop'})
IndexRename = class('IndexRename', ReQLOp, {tt = 156, st = 'index_rename'})
IndexList = class('IndexList', ReQLOp, {tt = 77, st = 'index_list'})
IndexStatus = class('IndexStatus', ReQLOp, {tt = 139, st = 'index_status'})
IndexWait = class('IndexWait', ReQLOp, {tt = 140, st = 'index_wait'})
Sync = class('Sync', ReQLOp, {tt = 138, st = 'sync'})
FunCall = class('FunCall', ReQLOp, {tt = 64, st = 'do_'})
Default = class('Default', ReQLOp, {tt = 92, st = 'default'})
Branch = class('Branch', ReQLOp, {tt = 65, st = 'branch'})
Any = class('Any', ReQLOp, {tt = 66, st = 'any'})
All = class('All', ReQLOp, {tt = 67, st = 'all'})
ForEach = class('ForEach', ReQLOp, {tt = 68, st = 'for_each'})
Func = class('Func', ReQLOp, {tt = 69, st = 'func'})
Asc = class('Asc', ReQLOp, {tt = 73, st = 'asc'})
Desc = class('Desc', ReQLOp, {tt = 74, st = 'desc'})
Literal = class('Literal', ReQLOp, {tt = 137, st = 'literal'})
ISO8601 = class('ISO8601', ReQLOp, {tt = 99, st = 'iso8601'})
ToISO8601 = class('ToISO8601', ReQLOp, {tt = 100, st = 'to_iso8601'})
EpochTime = class('EpochTime', ReQLOp, {tt = 101, st = 'epoch_time'})
ToEpochTime = class('ToEpochTime', ReQLOp, {tt = 102, st = 'to_epoch_time'})
Now = class('Now', ReQLOp, {tt = 103, st = 'now'})
InTimezone = class('InTimezone', ReQLOp, {tt = 104, st = 'in_timezone'})
During = class('During', ReQLOp, {tt = 105, st = 'during'})
Date = class('Date', ReQLOp, {tt = 106, st = 'date'})
TimeOfDay = class('TimeOfDay', ReQLOp, {tt = 126, st = 'time_of_day'})
Timezone = class('Timezone', ReQLOp, {tt = 127, st = 'timezone'})
Year = class('Year', ReQLOp, {tt = 128, st = 'year'})
Month = class('Month', ReQLOp, {tt = 129, st = 'month'})
Day = class('Day', ReQLOp, {tt = 130, st = 'day'})
DayOfWeek = class('DayOfWeek', ReQLOp, {tt = 131, st = 'day_of_week'})
DayOfYear = class('DayOfYear', ReQLOp, {tt = 132, st = 'day_of_year'})
Hours = class('Hours', ReQLOp, {tt = 133, st = 'hours'})
Minutes = class('Minutes', ReQLOp, {tt = 134, st = 'minutes'})
Seconds = class('Seconds', ReQLOp, {tt = 135, st = 'seconds'})
Time = class('Time', ReQLOp, {tt = 136, st = 'time'})
GeoJson = class('GeoJson', ReQLOp, {tt = 157, st = 'geo_json'})
ToGeoJson = class('ToGeoJson', ReQLOp, {tt = 158, st = 'to_geo_json'})
Point = class('Point', ReQLOp, {tt = 159, st = 'point'})
Line = class('Line', ReQLOp, {tt = 160, st = 'line'})
Polygon = class('Polygon', ReQLOp, {tt = 161, st = 'polygon'})
Distance = class('Distance', ReQLOp, {tt = 162, st = 'distance'})
Intersects = class('Intersects', ReQLOp, {tt = 163, st = 'intersects'})
Includes = class('Includes', ReQLOp, {tt = 164, st = 'includes'})
Circle = class('Circle', ReQLOp, {tt = 165, st = 'circle'})
GetIntersecting = class('GetIntersecting', ReQLOp, {tt = 166, st = 'get_intersecting'})
GetNearest = class('GetNearest', ReQLOp, {tt = 168, st = 'get_nearest'})
Fill = class('Fill', ReQLOp, {tt = 167, st = 'fill'})
PolygonSub = class('PolygonSub', ReQLOp, {tt = 171, st = 'polygon_sub'})
UUID = class('UUID', ReQLOp, {tt = 169, st = 'uuid'})

-- All top level exported functions

-- Wrap a native Lua value in an ReQL datum
function r.expr(val, nesting_depth)
  if nesting_depth == nil then
    nesting_depth = 20
  end
  if nesting_depth <= 0 then
    error(errors.ReQLDriverError('Nesting depth limit exceeded'))
  end
  if type(nesting_depth) ~= 'number' then
    error(errors.ReQLDriverError('Second argument to `r.expr` must be a number or nil.'))
  end
  if is_instance(ReQLOp, val) then
    return val
  end
  if type(val) == 'function' then
    return Func({}, val)
  end
  if type(val) == 'table' then
    local array = true
    for k, v in pairs(val) do
      if type(k) ~= 'number' then array = false end
      val[k] = r.expr(v, nesting_depth - 1)
    end
    if array then
      return MakeArray({}, val)
    end
    return MakeObj(val)
  end
  return DatumTerm(val)
end
function r.js(jssrc, opts)
  return JavaScript(opts, jssrc)
end
function r.http(url, opts)
  return Http(opts, url)
end
function r.json(...)
  return Json({}, ...)
end
function r.error(...)
  return Error({}, ...)
end
function r.random(...)
  -- Default if no opts dict provided
  local opts = {}
  local limits = {...}

  -- Look for opts dict
  local perhaps_opt_dict = limits[limits.n]
  if (type(perhaps_opt_dict) == 'table') and (not is_instance(ReQLOp, perhaps_opt_dict)) then
    opts = perhaps_opt_dict
    limits[limits.n] = nil
  end
  return Random(opts, unpack(limits))
end
function r.binary(data)
  return Binary(data)
end
function r.table(tbl_name, opts)
  return Table(opts, tbl_name)
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
function r.table_create(tbl_name, opts)
  return TableCreate(opts, tbl_name)
end
function r.table_drop(...)
  return TableDrop({}, ...)
end
function r.table_list(...)
  return TableList({}, ...)
end
function r.do_(...)
  args = {...}
  func = Func({arity = args.n - 1}, args[args.n])
  args[args.n] = nil
  return FunCall({}, func, unpack(args))
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
function r.or_(...)
  return Any({}, ...)
end
function r.any(...)
  return Any({}, ...)
end
function r.and_(...)
  return All({}, ...)
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
function r.iso8601(str, opts)
  return ISO8601(opts, str)
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

r.monday = class('Monday', ReQLOp, {tt = 107, st = 'monday'})()
r.tuesday = class('Tuesday', ReQLOp, {tt = 108, st = 'tuesday'})()
r.wednesday = class('Wednesday', ReQLOp, {tt = 109, st = 'wednesday'})()
r.thursday = class('Thursday', ReQLOp, {tt = 110, st = 'thursday'})()
r.friday = class('Friday', ReQLOp, {tt = 111, st = 'friday'})()
r.saturday = class('Saturday', ReQLOp, {tt = 112, st = 'saturday'})()
r.sunday = class('Sunday', ReQLOp, {tt = 113, st = 'sunday'})()

r.january = class('January', ReQLOp, {tt = 114, st = 'january'})()
r.february = class('February', ReQLOp, {tt = 115, st = 'february'})()
r.march = class('March', ReQLOp, {tt = 116, st = 'march'})()
r.april = class('April', ReQLOp, {tt = 117, st = 'april'})()
r.may = class('May', ReQLOp, {tt = 118, st = 'may'})()
r.june = class('June', ReQLOp, {tt = 119, st = 'june'})()
r.july = class('July', ReQLOp, {tt = 120, st = 'july'})()
r.august = class('August', ReQLOp, {tt = 121, st = 'august'})()
r.september = class('September', ReQLOp, {tt = 122, st = 'september'})()
r.october = class('October', ReQLOp, {tt = 123, st = 'october'})()
r.november = class('November', ReQLOp, {tt = 124, st = 'november'})()
r.december = class('December', ReQLOp, {tt = 125, st = 'december'})()

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
function r.distance(g1, g2, opts)
  return Distance(opts, g1, g2)
end
function r.circle(cen, rad, opts)
  return Circle(opts, cen, rad)
end
function r.uuid(...)
  return UUID({}, ...)
end

-- Export all names defined on r
return r
