local json = require('json')
local mime = require('mime')

local errors = require('./errors')
local net = require('./net')
local util = require('./util')

-- Import some names to this namespace for convienience
local is_instance = util.is_instance
local class = util.class

-- rethinkdb is both the main export object for the module
local rethinkdb = { }

local has_implicit, intsp, kved, intspallargs, should_wrap

local DatumTerm, RDBOp, RDBOp, MakeArray, MakeObject, Var, PolygonSub
local JavaScript, Http, Json, Binary, Args, UserError, Random, ImplicitVar, Db
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
local InTimezone, During, ReQLDate, TimeOfDay, Timezone, Year, Month, Day
local DayOfWeek, DayOfYear, Hours, Minutes, Seconds, Time, GeoJson, ToGeoJson
local Point, Line, Polygon, Distance, Intersects, Includes, Circle
local GetIntersecting, GetNearest, Fill, UUID, Monday, Tuesday, Wednesday
local Thursday, Friday, Saturday, Sunday, January, February, March, April, May
local June, July, August, September, October, November, December, ToJson

-- AST classes

RDBOp = class(
  'RDBOp',
  {
    __init = function(self, optargs, ...)
      self.args = {...}
      for i, a in ipairs(self.args) do
        self.args[i] = rethinkdb.expr(a)
      end
      self.optargs = optargs or {}
    end,
    build = function(self)
      local args = {}
      for i, arg in ipairs(self.args) do
        args[i] = arg:build()
      end
      res = {self.tt, args}
      if #self.optargs > 0 then
        local opts = { }
        for key, val in pairs(self.optargs) do
          opts[key] = val:build()
        end
        table.insert(res, opts)
      end
      return res
    end,
    compose = function(self, args, optargs)
      if self.st then
        return {
          'r.',
          self.st,
          '(',
          intspallargs(args, optargs),
          ')'
        }
      else
        if self.args then
          if should_wrap(self.args[1]) then
            args[1] = {
              'r(',
              args[1],
              ')'
            }
          end
        end
        return {
          args[1],
          ':',
          self.mt,
          '(',
          intspallargs((function()
            local _accum_0 = { }
            for _index_0 = 2, #args do
              _accum_0[_index_0 - 1] = args[_index_0]
            end
            return _accum_0
          end)(), optargs),
          ')'
        }
      end
    end,
    run = function(self, connection, options, callback)
      -- Valid syntaxes are
      -- connection, callback
      -- connection, options, callback
      -- connection, nil, callback

      -- Handle run(connection, callback)
      if type(options) == 'function' then
        if not (callback) then
          callback = options
          options = { }
        else
          options(errors.ReQLDriverError('Second argument to `run` cannot be a function if a third argument is provided.'))
          return
        end
      else
        -- else we suppose that we have run(connection[, options][, callback])
        if not (options) then
          options = { }
        end
      end

      if not net.is_connection(connection) then
        if callback then
          return callback(errors.ReQLDriverError('First argument to `run` must be an open connection.'))
        end
        return
      end

      return connection:_start(self, callback, options)
    end,
    eq = function(...)
      return Eq({ }, ...)
    end,
    ne = function(...)
      return Ne({ }, ...)
    end,
    lt = function(...)
      return Lt({ }, ...)
    end,
    le = function(...)
      return Le({ }, ...)
    end,
    gt = function(...)
      return Gt({ }, ...)
    end,
    ge = function(...)
      return Ge({ }, ...)
    end,
    not_ = function(...)
      return Not({ }, ...)
    end,
    add = function(...)
      return Add({ }, ...)
    end,
    sub = function(...)
      return Sub({ }, ...)
    end,
    mul = function(...)
      return Mul({ }, ...)
    end,
    div = function(...)
      return Div({ }, ...)
    end,
    mod = function(...)
      return Mod({ }, ...)
    end,
    append = function(...)
      return Append({ }, ...)
    end,
    prepend = function(...)
      return Prepend({ }, ...)
    end,
    difference = function(...)
      return Difference({ }, ...)
    end,
    set_insert = function(...)
      return SetInsert({ }, ...)
    end,
    set_union = function(...)
      return SetUnion({ }, ...)
    end,
    set_intersection = function(...)
      return SetIntersection({ }, ...)
    end,
    set_difference = function(...)
      return SetDifference({ }, ...)
    end,
    slice = function(self, left, right_or_opts, opts)
      if opts then
        return Slice(opts, self, left, right_or_opts)
      else
        if right_or_opts then
          if (type(right_or_opts) == 'table') and (not is_instance(RDBOp, right_or_opts)) then
            return Slice(right_or_opts, self, left)
          else
            return Slice({ }, self, left, right_or_opts)
          end
        else
          return Slice({ }, self, left)
        end
      end
    end,
    skip = function(...)
      return Skip({ }, ...)
    end,
    limit = function(...)
      return Limit({ }, ...)
    end,
    get_field = function(...)
      return GetField({ }, ...)
    end,
    contains = function(...)
      return Contains({ }, ...)
    end,
    insert_at = function(...)
      return InsertAt({ }, ...)
    end,
    splice_at = function(...)
      return SpliceAt({ }, ...)
    end,
    delete_at = function(...)
      return DeleteAt({ }, ...)
    end,
    change_at = function(...)
      return ChangeAt({ }, ...)
    end,
    indexes_of = function(...)
      return IndexesOf({ }, ...)
    end,
    has_fields = function(...)
      return HasFields({ }, ...)
    end,
    with_fields = function(...)
      return WithFields({ }, ...)
    end,
    keys = function(...)
      return Keys({ }, ...)
    end,
    changes = function(...)
      return Changes({ }, ...)
    end,

    -- pluck and without on zero fields are allowed
    pluck = function(...)
      return Pluck({ }, ...)
    end,
    without = function(...)
      return Without({ }, ...)
    end,
    merge = function(...)
      return Merge({ }, ...)
    end,
    between = function(self, left, right, opts)
      return Between(opts, self, left, right)
    end,
    reduce = function(...)
      return Reduce({arity = 2}, ...)
    end,
    map = function(...)
      return Map({ }, ...)
    end,
    filter = function(self, predicate, opts)
      return Filter(opts, self, rethinkdb.expr(predicate))
    end,
    concat_map = function(...)
      return ConcatMap({ }, ...)
    end,
    distinct = function(self, opts)
      return Distinct(opts, self)
    end,
    count = function(...)
      return Count({ }, ...)
    end,
    union = function(...)
      return Union({ }, ...)
    end,
    nth = function(...)
      return Nth({ }, ...)
    end,
    to_json = function(...):
      return ToJson({}, ...)
    end,
    bracket = function(...)
      return Bracket({ }, ...)
    end,
    match = function(...)
      return Match({ }, ...)
    end,
    split = function(...)
      return Split({ }, ...)
    end,
    upcase = function(...)
      return Upcase({ }, ...)
    end,
    downcase = function(...)
      return Downcase({ }, ...)
    end,
    is_empty = function(...)
      return IsEmpty({ }, ...)
    end,
    inner_join = function(...)
      return InnerJoin({ }, ...)
    end,
    outer_join = function(...)
      return OuterJoin({ }, ...)
    end,
    eq_join = function(self, left_attr, right, opts)
      return EqJoin(opts, self, rethinkdb.expr(left_attr), right)
    end,
    zip = function(...)
      return Zip({ }, ...)
    end,
    coerce_to = function(...)
      return CoerceTo({ }, ...)
    end,
    ungroup = function(...)
      return Ungroup({ }, ...)
    end,
    type_of = function(...)
      return TypeOf({ }, ...)
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
      return FunCall({ }, func, self, unpack(args))
    end,
    default = function(...)
      return Default({ }, ...)
    end,
    any = function(...)
      return Any({ }, ...)
    end,
    all = function(...)
      return All({ }, ...)
    end,
    for_each = function(...)
      return ForEach({ }, ...)
    end,
    sum = function(...)
      return Sum({ }, ...)
    end,
    avg = function(...)
      return Avg({ }, ...)
    end,
    min = function(...)
      return Min({ }, ...)
    end,
    max = function(...)
      return Max({ }, ...)
    end,
    info = function(...)
      return Info({}, ...)
    end,
    sample = function(...)
      return Sample({ }, ...)
    end,
    group = function(self, ...)
      -- Default if no opts dict provided
      local opts = { }
      local fields = {...}

      -- Look for opts dict
      if fields.n > 0 then
        local perhaps_opt_dict = fields[fields.n]
        if perhaps_opt_dict and (type(perhaps_opt_dict) == 'table') and not (is_instance(RDBOp, perhaps_opt_dict)) then
          opts = perhaps_opt_dict
          fields[fields.n] = nil
          fields.n = fields.n - 1
        end
      end
      for i=1, fields.n do
        fields[i] = rethinkdb.expr(fields[i])
      end
      return Group(opts, self, unpack(fields))
    end,
    order_by = function(self, ...)
      -- Default if no opts dict provided
      local opts = { }
      local attrs = {...}

      -- Look for opts dict
      local perhaps_opt_dict = attrs[attrs.n]
      if perhaps_opt_dict and (type(perhaps_opt_dict) == 'table') and not is_instance(RDBOp, perhaps_opt_dict) then
        opts = perhaps_opt_dict
        attrs[attrs.n] = nil
        attrs.n = attrs.n - 1
      end
      for i, attr in ipairs(attrs) do
        if not (is_instance(Asc, attr) or is_instance(Desc, attr)) then
          attrs[i] = rethinkdb.expr(attr)
        end
      end
      return OrderBy(opts, self, unpack(attrs))
    end,

    -- Geo operations
    to_geojson = function(...)
      return ToGeoJson({}, ...)
    end,
    distance = function(self, g, opts)
      return Distance(opts, self, g)
    end,
    intersects = Intersects,
    includes = function(...)
      return Includes({ }, ...)
    end,
    fill = function(...)
      return Fill({ }, ...)
    end,
    polygon_sub = function(...):
      return PolygonSub({}, ...)
    end,

    -- Database operations

    table_create = function(self, tbl_name, opts)
      return TableCreate(opts, self, tbl_name)
    end,
    table_drop = function(...)
      return TableDrop({ }, ...)
    end,
    table_list = function(...)
      return TableList({ }, ...)
    end,
    table = function(self, tbl_name, opts)
      return Table(opts, self, tbl_name)
    end,

    -- Table operations

    get = function(...)
      return Get({ }, ...)
    end,
    get_all = function(self, ...)
      -- Default if no opts dict provided
      local opts = { }
      local keys = {...}

      -- Look for opts dict
      if keys.n > 1 then
        local perhaps_opt_dict = keys[keys.n]
        if perhaps_opt_dict and ((type(perhaps_opt_dict) == 'table') and not (is_instance(RDBOp, perhaps_opt_dict))) then
          opts = perhaps_opt_dict
          keys[keys.n] = nil
        end
      end
      return GetAll(opts, self, unpack(keys))
    end,
    insert = function(self, doc, opts)
      return Insert(opts, self, rethinkdb.expr(doc))
    end,
    index_create = function(self, name, defun_or_opts, opts)
      if opts then
        return IndexCreate(opts, self, name, rethinkdb.expr(defun_or_opts))
      else
        if defun_or_opts then
          -- FIXME?
          if (type(defun_or_opts) == 'table') and not is_instance(RDBOp, defun_or_opts) then
            return IndexCreate(defun_or_opts, self, name)
          else
            return IndexCreate({ }, self, name, rethinkdb.expr(defun_or_opts))
          end
        else
          return IndexCreate({ }, self, name)
        end
      end
    end,
    index_drop = function(...)
      return IndexDrop({ }, ...)
    end,
    index_list = function(...)
      return IndexList({ }, ...)
    end,
    index_status = function(...)
      return IndexStatus({ }, ...)
    end,
    index_wait = function(...)
      return IndexWait({ }, ...)
    end,
    index_rename = function(self, old_name, new_name, opts)
      return IndexRename(opts, self, old_name, new_name)
    end,
    sync = function(...)
      return Sync({ }, ...)
    end,
    to_iso8601 = function(...)
      return ToISO8601({ }, ...)
    end,
    to_epoch_time = function(...)
      return ToEpochTime({}, ...)
    end,
    in_timezone = function(...)
      return InTimezone({ }, ...)
    end,
    during = function(self, t2, t3, opts)
      return During(opts, self, t2, t3)
    end,
    date = function(...)
      return ReQLDate({ }, ...)
    end,
    time_of_day = function(...)
      return TimeOfDay({ }, ...)
    end,
    timezone = function(...)
      return Timezone({ }, ...)
    end,
    year = function(...)
      return Year({ }, ...)
    end,
    month = function(...)
      return Month({ }, ...)
    end,
    day = function(...)
      return Day({ }, ...)
    end,
    day_of_week = function(...)
      return DayOfWeek({ }, ...)
    end,
    day_of_year = function(...)
      return DayOfYear({ }, ...)
    end,
    hours = function(...)
      return Hours({ }, ...)
    end,
    minutes = function(...)
      return Minutes({ }, ...)
    end,
    seconds = function(...)
      return Seconds({ }, ...)
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
  'DatumTerm', RDBOp,
  {
    __init = function(self, val)
      self.data = val
    end,
    args = { },
    optargs = { },
    compose = function(self)
      if type(self.data) == 'string' then
        return '"' .. self.data .. '"'
      else
        return '' .. self.data
      end
    end,
    build = function(self)
      if type(self.data) == 'number' then
        if math.abs(self.data) == 1/0 or self.data == ((1/0) * 0) then
          error('Illegal non-finite number `' .. self.data .. '`.')
        end
      end
      if self.data == nil then return json.null end
      return self.data
    end
  }
)

function intsp(seq)
  if seq[1] == nil then
    return { }
  end
  local res = {
    seq[1]
  }
  for _index_0 = 2, #seq do
    table.insert(res, ', ')
    table.insert(res, seq[_index_0])
  end
  return res
end
function kved(optargs)
  return {
    '{',
    intsp((function()
      local _accum_0 = { }
      local _len_0 = 1
      for k, v in pairs(optargs) do
        _accum_0[_len_0] = {
          k,
          ': ',
          v
        }
        _len_0 = _len_0 + 1
      end
      return _accum_0
    end)()),
    '}'
  }
end
function intspallargs(args, optargs)
  local argrepr = { }
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
  return is_instance(DatumTerm, arg) or is_instance(MakeArray, arg) or is_instance(MakeObject, arg)
end

MakeArray = class(
  'MakeArray', RDBOp,
  {
    __init = function(self, arr)
      self.args = arr
      self.optargs = {}
    end,
    tt = --[[Term.MAKE_ARRAY]],
    st = '{...}', -- This is only used by the `nil` argument checker
    build = function(self)
      local args = {}
      for i, arg in ipairs(self.args) do
        if type(arg) == 'table' then
          args[i] = arg:build()
        else
          args[i] = arg
        end
      end
      return {self.tt, args}
    end,
    compose = function(self, args)
      return {
        '{',
        intsp(args),
        '}'
      }
    end
  }
)

MakeObject = class(
  'MakeObject', RDBOp,
  {
    __init = function(self, obj)
      self.args = {}
      self.optargs = obj
    end,
    tt = --[[Term.MAKE_OBJ]],
    st = '{...}', -- This is only used by the `nil` argument checker
    compose = function(self, args, optargs)
      return kved(optargs)
    end,
    build = function(self)
      local res = { }
      for key, val in pairs(self.optargs) do
        res[key] = val:build()
      end
      return res
    end
  }
)

Var = class(
  'Var', RDBOp,
  {
    tt = --[[Term.VAR]],
    compose = function(self, args)
      if not args then return {} end
      for i, v in ipairs(args) do
        args[i] = 'var_' .. v
      end
      return args
    end
  }
)

JavaScript = class(
  'JavaScript', RDBOp,
  {
    tt = --[[Term.JAVASCRIPT]],
    st = 'js'
  }
)

Http = class(
  'Http', RDBOp,
  {
    tt = --[[Term.HTTP]],
    st = 'http'
  }
)

Json = class(
  'Json', RDBOp,
  {
    tt = --[[Term.JSON]],
    st = 'json'
  }
)

Binary = class(
  'Binary', RDBOp,
  {
    __init = function(self, data)
      self.args = {}
      self.optargs = {}
      if is_instance(RDBOp, data) then
        table.insert(self.args, data)
      else
        if type(data) == 'string' then
          self.base64_data = mime.b64(data)
        else
          error('Parameter to `r.binary` must be a string or ReQL query.')
        end
      end
    end,
    tt = --[[Term.BINARY]],
    st = 'binary',
    compose = function(self, args, optargs)
      if self.args[1] then
        return {
          'r.binary(',
          intspallargs(args, optargs),
          ')'
        }
      else
        return 'r.binary(<data>)'
      end
    end,
    build = function(self)
      if self.args[1] then
        local args = {}
        for i, arg in ipairs(self.args) do
          args[i] = arg:build()
        end
        return {self.tt, args}
      else
        return {
          ['$reql_type$'] = 'BINARY',
          data = self.base64_data
        }
      end
    end
  }
)

Args = class(
  'Args', RDBOp,
  {
    tt = --[[Term.ARGS]],
    st = 'args'
  }
)

UserError = class(
  'UserError', RDBOp,
  {
    tt = --[[Term.ERROR]],
    st = 'error'
  }
)

Random = class(
  'Random', RDBOp,
  {
    tt = --[[Term.RANDOM]],
    st = 'random'
  }
)

ImplicitVar = class(
  'ImplicitVar', RDBOp,
  {
    tt = --[[Term.IMPLICIT_VAR]],
    compose = function(self)
      return {
        'r.row'
      }
    end
  }
)

Db = class(
  'Db', RDBOp,
  {
    tt = --[[Term.DB]],
    st = 'db'
  }
)

Table = class(
  'Table', RDBOp,
  {
    tt = --[[Term.TABLE]],
    st = 'table',
    compose = function(self, args, optargs)
      if is_instance(Db, self.args[1]) then
        return {
          args[1],
          '.table(',
          intspallargs((function()
            local _accum_0 = { }
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
  }
)

Get = class(
  'Get', RDBOp,
  {
    tt = --[[Term.GET]],
    mt = 'get'
  }
)

GetAll = class(
  'GetAll', RDBOp,
  {
    tt = --[[Term.GET_ALL]],
    mt = 'get_all'
  }
)

Eq = class(
  'Eq', RDBOp,
  {
    tt = --[[Term.EQ]],
    mt = 'eq'
  }
)

Ne = class(
  'Ne', RDBOp,
  {
    tt = --[[Term.NE]],
    mt = 'ne'
  }
)

Lt = class(
  'Lt', RDBOp,
  {
    tt = --[[Term.LT]],
    mt = 'lt'
  }
)

Le = class(
  'Le', RDBOp,
  {
    tt = --[[Term.LE]],
    mt = 'le'
  }
)

Gt = class(
  'Gt', RDBOp,
  {
    tt = --[[Term.GT]],
    mt = 'gt'
  }
)

Ge = class(
  'Ge', RDBOp,
  {
    tt = --[[Term.GE]],
    mt = 'ge'
  }
)

Not = class(
  'Not', RDBOp,
  {
    tt = --[[Term.NOT]],
    mt = 'not_'
  }
)

Add = class(
  'Add', RDBOp,
  {
    tt = --[[Term.ADD]],
    mt = 'add'
  }
)

Sub = class(
  'Sub', RDBOp,
  {
    tt = --[[Term.SUB]],
    mt = 'sub'
  }
)

Mul = class(
  'Mul', RDBOp,
  {
    tt = --[[Term.MUL]],
    mt = 'mul'
  }
)

Div = class(
  'Div', RDBOp,
  {
    tt = --[[Term.DIV]],
    mt = 'div'
  }
)

Mod = class(
  'Mod', RDBOp,
  {
    tt = --[[Term.MOD]],
    mt = 'mod'
  }
)

Append = class(
  'Append', RDBOp,
  {
    tt = --[[Term.APPEND]],
    mt = 'append'
  }
)

Prepend = class(
  'Prepend', RDBOp,
  {
    tt = --[[Term.PREPEND]],
    mt = 'prepend'
  }
)

Difference = class(
  'Difference', RDBOp,
  {
    tt = --[[Term.DIFFERENCE]],
    mt = 'difference'
  }
)

SetInsert = class(
  'SetInsert', RDBOp,
  {
    tt = --[[Term.SET_INSERT]],
    mt = 'set_insert'
  }
)

SetUnion = class(
  'SetUnion', RDBOp,
  {
    tt = --[[Term.SET_UNION]],
    mt = 'set_union'
  }
)

SetIntersection = class(
  'SetIntersection', RDBOp,
  {
    tt = --[[Term.SET_INTERSECTION]],
    mt = 'set_intersection'
  }
)

SetDifference = class(
  'SetDifference', RDBOp,
  {
    tt = --[[Term.SET_DIFFERENCE]],
    mt = 'set_difference'
  }
)

Slice = class(
  'Slice', RDBOp,
  {
    tt = --[[Term.SLICE]],
    mt = 'slice'
  }
)

Skip = class(
  'Skip', RDBOp,
  {
    tt = --[[Term.SKIP]],
    mt = 'skip'
  }
)

Limit = class(
  'Limit', RDBOp,
  {
    tt = --[[Term.LIMIT]],
    mt = 'limit'
  }
)

GetField = class(
  'GetField', RDBOp,
  {
    tt = --[[Term.GET_FIELD]],
    mt = 'get_field'
  }
)

Bracket = class(
  'Bracket', RDBOp,
  {
    tt = --[[Term.BRACKET]],
    st = '[...]', -- This is only used by the `nil` argument checker
    compose = function(self, args)
      return {
        args[0],
        '[',
        args[1],
        ']'
      }
    end
  }
)

Contains = class(
  'Contains', RDBOp,
  {
    tt = --[[Term.CONTAINS]],
    mt = 'contains'
  }
)

InsertAt = class(
  'InsertAt', RDBOp,
  {
    tt = --[[Term.INSERT_AT]],
    mt = 'insert_at'
  }
)

SpliceAt = class(
  'SpliceAt', RDBOp,
  {
    tt = --[[Term.SPLICE_AT]],
    mt = 'splice_at'
  }
)

DeleteAt = class(
  'DeleteAt', RDBOp,
  {
    tt = --[[Term.DELETE_AT]],
    mt = 'delete_at'
  }
)

ChangeAt = class(
  'ChangeAt', RDBOp,
  {
    tt = --[[Term.CHANGE_AT]],
    mt = 'change_at'
  }
)

Contains = class(
  'Contains', RDBOp,
  {
    tt = --[[Term.CONTAINS]],
    mt = 'contains'
  }
)

HasFields = class(
  'HasFields', RDBOp,
  {
    tt = --[[Term.HAS_FIELDS]],
    mt = 'has_fields'
  }
)

WithFields = class(
  'WithFields', RDBOp,
  {
    tt = --[[Term.WITH_FIELDS]],
    mt = 'with_fields'
  }
)

Keys = class(
  'Keys', RDBOp,
  {
    tt = --[[Term.KEYS]],
    mt = 'keys'
  }
)

Changes = class(
  'Changes', RDBOp,
  {
    tt = --[[Term.CHANGES]],
    mt = 'changes'
  }
)

Object = class(
  'Object', RDBOp,
  {
    tt = --[[Term.OBJECT]],
    mt = 'object'
  }
)

Pluck = class(
  'Pluck', RDBOp,
  {
    tt = --[[Term.PLUCK]],
    mt = 'pluck'
  }
)

IndexesOf = class(
  'IndexesOf', RDBOp,
  {
    tt = --[[Term.INDEXES_OF]],
    mt = 'indexes_of'
  }
)

Without = class(
  'Without', RDBOp,
  {
    tt = --[[Term.WITHOUT]],
    mt = 'without'
  }
)

Merge = class(
  'Merge', RDBOp,
  {
    tt = --[[Term.MERGE]],
    mt = 'merge'
  }
)

Between = class(
  'Between', RDBOp,
  {
    tt = --[[Term.BETWEEN]],
    mt = 'between'
  }
)

Reduce = class(
  'Reduce', RDBOp,
  {
    tt = --[[Term.REDUCE]],
    mt = 'reduce'
  }
)

Map = class(
  'Map', RDBOp,
  {
    tt = --[[Term.MAP]],
    mt = 'map'
  }
)

Filter = class(
  'Filter', RDBOp,
  {
    tt = --[[Term.FILTER]],
    mt = 'filter'
  }
)

ConcatMap = class(
  'ConcatMap', RDBOp,
  {
    tt = --[[Term.CONCATMAP]],
    mt = 'concat_map'
  }
)

OrderBy = class(
  'OrderBy', RDBOp,
  {
    tt = --[[Term.ORDERBY]],
    mt = 'order_by'
  }
)

Distinct = class(
  'Distinct', RDBOp,
  {
    tt = --[[Term.DISTINCT]],
    mt = 'distinct'
  }
)

Count = class(
  'Count', RDBOp,
  {
    tt = --[[Term.COUNT]],
    mt = 'count'
  }
)

Union = class(
  'Union', RDBOp,
  {
    tt = --[[Term.UNION]],
    mt = 'union'
  }
)

Nth = class(
  'Nth', RDBOp,
  {
    tt = --[[Term.NTH]],
    mt = 'nth'
  }
)

ToJson class(
  'ToJson', RDBOp,
  {
    tt = --[[Term.TO_JSON_STRING]],
    st = 'to_json_string'
  }
)

Match = class(
  'Match', RDBOp,
  {
    tt = --[[Term.MATCH]],
    mt = 'match'
  }
)

Split = class(
  'Split', RDBOp,
  {
    tt = --[[Term.SPLIT]],
    mt = 'split'
  }
)

Upcase = class(
  'Upcase', RDBOp,
  {
    tt = --[[Term.UPCASE]],
    mt = 'upcase'
  }
)

Downcase = class(
  'Downcase', RDBOp,
  {
    tt = --[[Term.DOWNCASE]],
    mt = 'downcase'
  }
)

IsEmpty = class(
  'IsEmpty', RDBOp,
  {
    tt = --[[Term.IS_EMPTY]],
    mt = 'is_empty'
  }
)

Group = class(
  'Group', RDBOp,
  {
    tt = --[[Term.GROUP]],
    mt = 'group'
  }
)

Sum = class(
  'Sum', RDBOp,
  {
    tt = --[[Term.SUM]],
    mt = 'sum'
  }
)

Avg = class(
  'Avg', RDBOp,
  {
    tt = --[[Term.AVG]],
    mt = 'avg'
  }
)

Min = class(
  'Min', RDBOp,
  {
    tt = --[[Term.MIN]],
    mt = 'min'
  }
)

Max = class(
  'Max', RDBOp,
  {
    tt = --[[Term.MAX]],
    mt = 'max'
  }
)

InnerJoin = class(
  'InnerJoin', RDBOp,
  {
    tt = --[[Term.INNER_JOIN]],
    mt = 'inner_join'
  }
)

OuterJoin = class(
  'OuterJoin', RDBOp,
  {
    tt = --[[Term.OUTER_JOIN]],
    mt = 'outer_join'
  }
)

EqJoin = class(
  'EqJoin', RDBOp,
  {
    tt = --[[Term.EQ_JOIN]],
    mt = 'eq_join'
  }
)

Zip = class(
  'Zip', RDBOp,
  {
    tt = --[[Term.ZIP]],
    mt = 'zip'
  }
)

CoerceTo = class(
  'CoerceTo', RDBOp,
  {
    tt = --[[Term.COERCE_TO]],
    mt = 'coerce_to'
  }
)

Ungroup = class(
  'Ungroup', RDBOp,
  {
    tt = --[[Term.UNGROUP]],
    mt = 'ungroup'
  }
)

TypeOf = class(
  'TypeOf', RDBOp,
  {
    tt = --[[Term.TYPEOF]],
    mt = 'type_of'
  }
)

Info = class(
  'Info', RDBOp,
  {
    tt = --[[Term.INFO]],
    mt = 'info'
  }
)

Sample = class(
  'Sample', RDBOp,
  {
    tt = --[[Term.SAMPLE]],
    mt = 'sample'
  }
)

Update = class(
  'Update', RDBOp,
  {
    tt = --[[Term.UPDATE]],
    mt = 'update'
  }
)

Delete = class(
  'Delete', RDBOp,
  {
    tt = --[[Term.DELETE]],
    mt = 'delete'
  }
)

Replace = class(
  'Replace', RDBOp,
  {
    tt = --[[Term.REPLACE]],
    mt = 'replace'
  }
)

Insert = class(
  'Insert', RDBOp,
  {
    tt = --[[Term.INSERT]],
    mt = 'insert'
  }
)

DbCreate = class(
  'DbCreate', RDBOp,
  {
    tt = --[[Term.DB_CREATE]],
    st = 'db_create'
  }
)

DbDrop = class(
  'DbDrop', RDBOp,
  {
    tt = --[[Term.DB_DROP]],
    st = 'db_drop'
  }
)

DbList = class(
  'DbList', RDBOp,
  {
    tt = --[[Term.DB_LIST]],
    st = 'db_list'
  }
)

TableCreate = class(
  'TableCreate', RDBOp,
  {
    tt = --[[Term.TABLE_CREATE]],
    mt = 'table_create'
  }
)

TableDrop = class(
  'TableDrop', RDBOp,
  {
    tt = --[[Term.TABLE_DROP]],
    mt = 'table_drop'
  }
)

TableList = class(
  'TableList', RDBOp,
  {
    tt = --[[Term.TABLE_LIST]],
    mt = 'table_list'
  }
)

IndexCreate = class(
  'IndexCreate', RDBOp,
  {
    tt = --[[Term.INDEX_CREATE]],
    mt = 'index_create'
  }
)

IndexDrop = class(
  'IndexDrop', RDBOp,
  {
    tt = --[[Term.INDEX_DROP]],
    mt = 'index_drop'
  }
)

IndexRename = class(
  'IndexRename', RDBOp,
  {
    tt = --[[Term.INDEX_RENAME]],
    mt = 'index_rename'
  }
)

IndexList = class(
  'IndexList', RDBOp,
  {
    tt = --[[Term.INDEX_LIST]],
    mt = 'index_list'
  }
)

IndexStatus = class(
  'IndexStatus', RDBOp,
  {
    tt = --[[Term.INDEX_STATUS]],
    mt = 'index_status'
  }
)

IndexWait = class(
  'IndexWait', RDBOp,
  {
    tt = --[[Term.INDEX_WAIT]],
    mt = 'index_wait'
  }
)

Sync = class(
  'Sync', RDBOp,
  {
    tt = --[[Term.SYNC]],
    mt = 'sync'
  }
)

FunCall = class(
  'FunCall', RDBOp,
  {
    tt = --[[Term.FUNCALL]],
    st = 'do_', -- This is only used by the `nil` argument checker
    compose = function(self, args)
      if #args > 2 then
        return {
          'r.do_(',
          intsp((function()
            local _accum_0 = { }
            local _len_0 = 1
            for _index_0 = 2, #args do
              local a = args[_index_0]
              _accum_0[_len_0] = a
              _len_0 = _len_0 + 1
            end
            return _accum_0
          end)()),
          ', ',
          args[0],
          ')'
        }
      else
        if should_wrap(self.args[1]) then
          args[1] = {
            'r(',
            args[1],
            ')'
          }
        end
        return {
          args[1],
          '.do_(',
          args[0],
          ')'
        }
      end
    end
  }
)

Default = class(
  'Default', RDBOp,
  {
    tt = --[[Term.DEFAULT]],
    mt = 'default'
  }
)

Branch = class(
  'Branch', RDBOp,
  {
    tt = --[[Term.BRANCH]],
    st = 'branch'
  }
)

Any = class(
  'Any', RDBOp,
  {
    tt = --[[Term.ANY]],
    mt = 'or_'
  }
)

All = class(
  'All', RDBOp,
  {
    tt = --[[Term.ALL]],
    mt = 'and_'
  }
)

ForEach = class(
  'ForEach', RDBOp,
  {
    tt = --[[Term.FOREACH]],
    mt = 'for_each'
  }
)

function ivar_scan(node)
  if not is_instance(RDBOp, node) then
    return false
  end
  if is_instance(ImplicitVar, node) then
    return true
  end
  for _, v in ipairs(node.args) do
    if ivar_scan(v) then
      return true
    end
  end
  for _, v in pairs(node.optargs) do
    if ivar_scan(v) then
      return true
    end
  end
  return false
end

function has_implicit(args)
  -- args is an array of (strings and arrays)
  -- We recurse to look for `r.row` which is an implicit var
  if type(args) == 'table' then
    for _, arg in ipairs(args) do
      if has_implicit(arg) == true then
        return true
      end
    end
  else
    if args == 'r.row' then
      return true
    end
  end
  return false
end

Func = class(
  'Func', RDBOp,
  {
    __init = function(self, optargs, func)
      if ivar_scan(func) then
        func = function(x)
          return func
        end
      end
      local args = { }
      local arg_nums = { }
      if not optargs then optargs = {} end
      for i=1, optargs.arity or 1 do
        table.insert(arg_nums, Func.next_var_id)
        table.insert(args, Var({ }, Func.next_var_id))
        Func.next_var_id = Func.next_var_id + 1
      end
      local body = func(unpack(args))
      if not body then
        error(errors.ReQLDriverError('Anonymous function returned `nil`. Did you forget a `return`?'))
      end
      optargs.arity = nil
      self.args = {MakeArray(arg_nums), body}
      self.optargs = optargs
    end,
    next_var_id = 0,
    tt = --[[Term.FUNC]],
    compose = function(self, args)
      if has_implicit(args[1]) then
        return {
          args[1]
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
  }
)

Asc = class(
  'Asc', RDBOp,
  {
    tt = --[[Term.ASC]],
    st = 'asc'
  }
)

Desc = class(
  'Desc', RDBOp,
  {
    tt = --[[Term.DESC]],
    st = 'desc'
  }
)

Literal = class(
  'Literal', RDBOp,
  {
    tt = --[[Term.LITERAL]],
    st = 'literal'
  }
)

ISO8601 = class(
  'ISO8601', RDBOp,
  {
    tt = --[[Term.ISO8601]],
    st = 'iso8601'
  }
)

ToISO8601 = class(
  'ToISO8601', RDBOp,
  {
    tt = --[[Term.TO_ISO8601]],
    mt = 'to_iso8601'
  }
)

EpochTime = class(
  'EpochTime', RDBOp,
  {
    tt = --[[Term.EPOCH_TIME]],
    st = 'epoch_time'
  }
)

ToEpochTime = class(
  'ToEpochTime', RDBOp,
  {
    tt = --[[Term.TO_EPOCH_TIME]],
    mt = 'to_epoch_time'
  }
)

Now = class(
  'Now', RDBOp,
  {
    tt = --[[Term.NOW]],
    st = 'now'
  }
)

InTimezone = class(
  'InTimezone', RDBOp,
  {
    tt = --[[Term.IN_TIMEZONE]],
    mt = 'in_timezone'
  }
)

During = class(
  'During', RDBOp,
  {
    tt = --[[Term.DURING]],
    mt = 'during'
  }
)

ReQLDate = class(
  'ReQLDate', RDBOp,
  {
    tt = --[[Term.DATE]],
    mt = 'date'
  }
)

TimeOfDay = class(
  'TimeOfDay', RDBOp,
  {
    tt = --[[Term.TIME_OF_DAY]],
    mt = 'time_of_day'
  }
)

Timezone = class(
  'Timezone', RDBOp,
  {
    tt = --[[Term.TIMEZONE]],
    mt = 'timezone'
  }
)

Year = class(
  'Year', RDBOp,
  {
    tt = --[[Term.YEAR]],
    mt = 'year'
  }
)

Month = class(
  'Month', RDBOp,
  {
    tt = --[[Term.MONTH]],
    mt = 'month'
  }
)

Day = class(
  'Day', RDBOp,
  {
    tt = --[[Term.DAY]],
    mt = 'day'
  }
)

DayOfWeek = class(
  'DayOfWeek', RDBOp,
  {
    tt = --[[Term.DAY_OF_WEEK]],
    mt = 'day_of_week'
  }
)

DayOfYear = class(
  'DayOfYear', RDBOp,
  {
    tt = --[[Term.DAY_OF_YEAR]],
    mt = 'day_of_year'
  }
)

Hours = class(
  'Hours', RDBOp,
  {
    tt = --[[Term.HOURS]],
    mt = 'hours'
  }
)

Minutes = class(
  'Minutes', RDBOp,
  {
    tt = --[[Term.MINUTES]],
    mt = 'minutes'
  }
)

Seconds = class(
  'Seconds', RDBOp,
  {
    tt = --[[Term.SECONDS]],
    mt = 'seconds'
  }
)

Time = class(
  'Time', RDBOp,
  {
    tt = --[[Term.TIME]],
    st = 'time'
  }
)

GeoJson = class(
  'GeoJson', RDBOp,
  {
    tt = --[[Term.GEOJSON]],
    mt = 'geojson'
  }
)

ToGeoJson = class(
  'ToGeoJson', RDBOp,
  {
    tt = --[[Term.TO_GEOJSON]],
    mt = 'to_geojson'
  }
)

Point = class(
  'Point', RDBOp,
  {
    tt = --[[Term.POINT]],
    mt = 'point'
  }
)

Line = class(
  'Line', RDBOp,
  {
    tt = --[[Term.LINE]],
    mt = 'line'
  }
)

Polygon = class(
  'Polygon', RDBOp,
  {
    tt = --[[Term.POLYGON]],
    mt = 'polygon'
  }
)

Distance = class(
  'Distance', RDBOp,
  {
    tt = --[[Term.DISTANCE]],
    mt = 'distance'
  }
)

Intersects = class(
  'Intersects', RDBOp,
  {
    tt = --[[Term.INTERSECTS]],
    mt = 'intersects'
  }
)

Includes = class(
  'Includes', RDBOp,
  {
    tt = --[[Term.INCLUDES]],
    mt = 'includes'
  }
)

Circle = class(
  'Circle', RDBOp,
  {
    tt = --[[Term.CIRCLE]],
    mt = 'circle'
  }
)

GetIntersecting = class(
  'GetIntersecting', RDBOp,
  {
    tt = --[[Term.GET_INTERSECTING]],
    mt = 'get_intersecting'
  }
)

GetNearest = class(
  'GetNearest', RDBOp,
  {
    tt = --[[Term.GET_NEAREST]],
    mt = 'get_nearest'
  }
)

Fill = class(
  'Fill', RDBOp,
  {
    tt = --[[Term.FILL]],
    mt = 'fill'
  }
)

PolygonSub = class(
  'PolygonSub', RDBOp,
  {
    tt = --[[Term.POLYGON_SUB]],
    st = 'polygon_sub'
  }
)

UUID = class(
  'UUID', RDBOp,
  {
    tt = --[[Term.UUID]],
    st = 'uuid'
  }
)

-- All top level exported functions

-- Wrap a native Lua value in an ReQL datum
function rethinkdb.expr(val, nesting_depth)
  if nesting_depth == nil then
    nesting_depth = 20
  end
  if nesting_depth <= 0 then
    error(errors.ReQLDriverError('Nesting depth limit exceeded'))
  end
  if type(nesting_depth) ~= 'number' then
    error(errors.ReQLDriverError('Second argument to `r.expr` must be a number or nil.'))
  end
  if type(val) == 'function' or ivar_scan(val) then
    return Func({}, val)
  end
  if is_instance(RDBOp, val) then
    return val
  end
  if type(val) == 'table' then
    local array = true
    for k, v in pairs(val) do
      if type(k) ~= 'number' then array = false end
      val[k] = rethinkdb.expr(v, nesting_depth - 1)
    end
    if array then
      return MakeArray(val)
    end
    return MakeObject(val)
  end
  return DatumTerm(val)
end
function rethinkdb.js(jssrc, opts)
  return JavaScript(opts, jssrc)
end
function rethinkdb.http(url, opts)
  return Http(opts, url)
end
function rethinkdb.json(...)
  return Json({ }, ...)
end
function rethinkdb.error(...)
  return UserError({ }, ...)
end
function rethinkdb.random(...)
  -- Default if no opts dict provided
  local opts = { }
  local limits = {...}

  -- Look for opts dict
  local perhaps_opt_dict = limits[limits.n]
  if perhaps_opt_dict and ((type(perhaps_opt_dict) == 'table') and not (is_instance(RDBOp, perhaps_opt_dict))) then
    opts = perhaps_opt_dict
    limits[limits.n] = nil
  end
  return Random(opts, unpack(limits))
end
function rethinkdb.binary(data)
  return Binary(data)
end
rethinkdb.row = ImplicitVar({ })
function rethinkdb.table(tbl_name, opts)
  return Table(opts, tbl_name)
end
function rethinkdb.db(...)
  return Db({ }, ...)
end
function rethinkdb.db_create(...)
  return DbCreate({ }, ...)
end
function rethinkdb.db_drop(...)
  return DbDrop({ }, ...)
end
function rethinkdb.db_list(...)
  return DbList({ }, ...)
end
function rethinkdb.table_create(tbl_name, opts)
  return TableCreate(opts, tbl_name)
end
function rethinkdb.table_drop(...)
  return TableDrop({ }, ...)
end
function rethinkdb.table_list(...)
  return TableList({ }, ...)
end
function rethinkdb.do_(...)
  args = {...}
  func = Func({}, args[args.n])
  args[args.n] = nil
  return FunCall({ }, func, unpack(args))
end
function rethinkdb.branch(...)
  return Branch({ }, ...)
end
function rethinkdb.asc(...)
  return Asc({ }, ...)
end
function rethinkdb.desc(...)
  return Desc({ }, ...)
end
function rethinkdb.eq(...)
  return Eq({ }, ...)
end
function rethinkdb.ne(...)
  return Ne({ }, ...)
end
function rethinkdb.lt(...)
  return Lt({ }, ...)
end
function rethinkdb.le(...)
  return Le({ }, ...)
end
function rethinkdb.gt(...)
  return Gt({ }, ...)
end
function rethinkdb.ge(...)
  return Ge({ }, ...)
end
function rethinkdb.or_(...)
  return Any({ }, ...)
end
function rethinkdb.any(...)
  return Any({ }, ...)
end
function rethinkdb.and_(...)
  return All({ }, ...)
end
function rethinkdb.all(...)
  return All({ }, ...)
end
function rethinkdb.not_(...)
  return Not({ }, ...)
end
function rethinkdb.add(...)
  return Add({ }, ...)
end
function rethinkdb.sub(...)
  return Sub({ }, ...)
end
function rethinkdb.div(...)
  return Div({ }, ...)
end
function rethinkdb.mul(...)
  return Mul({ }, ...)
end
function rethinkdb.mod(...)
  return Mod({ }, ...)
end
function rethinkdb.type_of(...)
  return TypeOf({ }, ...)
end
function rethinkdb.info(...)
  return Info({ }, ...)
end
function rethinkdb.literal(...)
  return Literal({ }, ...)
end
function rethinkdb.iso8601(str, opts)
  return ISO8601(opts, str)
end
function rethinkdb.epoch_time(...)
  return EpochTime({ }, ...)
end
function rethinkdb.now(...)
  return Now({ }, ...)
end
function rethinkdb.time(...)
  return Time({ }, ...)
end

rethinkdb.monday = class('Monday', RDBOp, {tt = --[[Term.MONDAY]]})()
rethinkdb.tuesday = class('Tuesday', RDBOp, {tt = --[[Term.TUESDAY]]})()
rethinkdb.wednesday = class('Wednesday', RDBOp, {tt = --[[Term.WEDNESDAY]]})()
rethinkdb.thursday = class('Thursday', RDBOp, {tt = --[[Term.THURSDAY]]})()
rethinkdb.friday = class('Friday', RDBOp, {tt = --[[Term.FRIDAY]]})()
rethinkdb.saturday = class('Saturday', RDBOp, {tt = --[[Term.SATURDAY]]})()
rethinkdb.sunday = class('Sunday', RDBOp, {tt = --[[Term.SUNDAY]]})()

rethinkdb.january = class('January', RDBOp, {tt = --[[Term.JANUARY]]})()
rethinkdb.february = class('February', RDBOp, {tt = --[[Term.FEBRUARY]]})()
rethinkdb.march = class('March', RDBOp, {tt = --[[Term.MARCH]]})()
rethinkdb.april = class('April', RDBOp, {tt = --[[Term.APRIL]]})()
rethinkdb.may = class('May', RDBOp, {tt = --[[Term.MAY]]})()
rethinkdb.june = class('June', RDBOp, {tt = --[[Term.JUNE]]})()
rethinkdb.july = class('July', RDBOp, {tt = --[[Term.JULY]]})()
rethinkdb.august = class('August', RDBOp, {tt = --[[Term.AUGUST]]})()
rethinkdb.september = class('September', RDBOp, {tt = --[[Term.SEPTEMBER]]})()
rethinkdb.october = class('October', RDBOp, {tt = --[[Term.OCTOBER]]})()
rethinkdb.november = class('November', RDBOp, {tt = --[[Term.NOVEMBER]]})()
rethinkdb.december = class('December', RDBOp, {tt = --[[Term.DECEMBER]]})()

function rethinkdb.object(...)
  return Object({ }, ...)
end
function rethinkdb.args(...)
  return Args({ }, ...)
end
function rethinkdb.geojson(...)
  return GeoJson({ }, ...)
end
function rethinkdb.point(...)
  return Point({ }, ...)
end
function rethinkdb.line(...)
  return Line({ }, ...)
end
function rethinkdb.polygon(...)
  return Polygon({ }, ...)
end
function rethinkdb.intersects(...)
  return Intersects({ }, ...)
end
function rethinkdb.distance(g1, g2, opts)
  return Distance(opts, g1, g2)
end
function rethinkdb.circle(cen, rad, opts)
  return Circle(opts, cen, rad)
end
function rethinkdb.uuid(...)
  return UUID({ }, ...)
end

-- Export all names defined on rethinkdb
return rethinkdb
