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
      if self.tt == --[[Term.FUNC]] then
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
      elseif self.tt == --[[Term.BINARY]] then
        if is_instance(ReQLOp, first) then
        elseif type(first) == 'string' then
          self.base64_data = mime.b64(first)
        else
          error('Parameter to `r.binary` must be a string or ReQL query.')
        end
      elseif self.tt == --[[Term.MAKE_ARRAY]] then
        self.args = first
      elseif self.tt == --[[Term.MAKE_OBJ]] then
      else
        for i, a in ipairs(self.args) do
          self.args[i] = r.expr(a)
        end
      end
      self.optargs = optargs
    end,
    build = function(self)
      if self.tt == --[[Term.BINARY]] and (not self.args[1]) then
        return {
          ['$reql_type$'] = 'BINARY',
          data = self.base64_data
        }
      end
      if self.tt == --[[Term.MAKE_ARRAY]] then
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
      if self.tt == --[[Term.MAKE_OBJ]] then
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
      if self.tt == --[[Term.MAKE_ARRAY]] then
        return {
          '{',
          intsp(args),
          '}'
        }
      end
      if self.tt == --[[Term.MAKE_OBJ]] then
        return kved(optargs)
      end
      if self.tt == --[[Term.VAR]] then
        if not args then return {} end
        for i, v in ipairs(args) do
          args[i] = 'var_' .. v
        end
        return args
      end
      if self.tt == --[[Term.BINARY]] then
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
      if self.tt == --[[Term.IMPLICIT_VAR]] then
        return {
          'r.row'
        }
      end
      if self.tt == --[[Term.TABLE]] then
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
      if self.tt == --[[Term.BRACKET]] then
        return {
          args[1],
          '(',
          args[2],
          ')'
        }
      end
      if self.tt == --[[Term.FUNC]] then
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
      if self.tt == --[[Term.FUNCALL]] then
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
    to_geojson = function(...)
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

MakeArray = class(--[[Class.MAKE_ARRAY]])
MakeObj = class(--[[Class.MAKE_OBJ]])
Var = class(--[[Class.VAR]])
JavaScript = class(--[[Class.JAVASCRIPT]])
Http = class(--[[Class.HTTP]])
Json = class(--[[Class.JSON]])
Binary = class(--[[Class.BINARY]])
Args = class(--[[Class.ARGS]])
Error = class(--[[Class.ERROR]])
Random = class(--[[Class.RANDOM]])
Db = class(--[[Class.DB]])
Table = class(--[[Class.TABLE]])
Get = class(--[[Class.GET]])
GetAll = class(--[[Class.GET_ALL]])
Eq = class(--[[Class.EQ]])
Ne = class(--[[Class.NE]])
Lt = class(--[[Class.LT]])
Le = class(--[[Class.LE]])
Gt = class(--[[Class.GT]])
Ge = class(--[[Class.GE]])
Not = class(--[[Class.NOT]])
Add = class(--[[Class.ADD]])
Sub = class(--[[Class.SUB]])
Mul = class(--[[Class.MUL]])
Div = class(--[[Class.DIV]])
Mod = class(--[[Class.MOD]])
Append = class(--[[Class.APPEND]])
Prepend = class(--[[Class.PREPEND]])
Difference = class(--[[Class.DIFFERENCE]])
SetInsert = class(--[[Class.SET_INSERT]])
SetUnion = class(--[[Class.SET_UNION]])
SetIntersection = class(--[[Class.SET_INTERSECTION]])
SetDifference = class(--[[Class.SET_DIFFERENCE]])
Slice = class(--[[Class.SLICE]])
Skip = class(--[[Class.SKIP]])
Limit = class(--[[Class.LIMIT]])
GetField = class(--[[Class.GET_FIELD]])
Bracket = class(--[[Class.BRACKET]])
Contains = class(--[[Class.CONTAINS]])
InsertAt = class(--[[Class.INSERT_AT]])
SpliceAt = class(--[[Class.SPLICE_AT]])
DeleteAt = class(--[[Class.DELETE_AT]])
ChangeAt = class(--[[Class.CHANGE_AT]])
Contains = class(--[[Class.CONTAINS]])
HasFields = class(--[[Class.HAS_FIELDS]])
WithFields = class(--[[Class.WITH_FIELDS]])
Keys = class(--[[Class.KEYS]])
Changes = class(--[[Class.CHANGES]])
Object = class(--[[Class.OBJECT]])
Pluck = class(--[[Class.PLUCK]])
IndexesOf = class(--[[Class.INDEXES_OF]])
Without = class(--[[Class.WITHOUT]])
Merge = class(--[[Class.MERGE]])
Between = class(--[[Class.BETWEEN]])
Reduce = class(--[[Class.REDUCE]])
Map = class(--[[Class.MAP]])
Filter = class(--[[Class.FILTER]])
ConcatMap = class(--[[Class.CONCATMAP]])
OrderBy = class(--[[Class.ORDERBY]])
Distinct = class(--[[Class.DISTINCT]])
Count = class(--[[Class.COUNT]])
Union = class(--[[Class.UNION]])
Nth = class(--[[Class.NTH]])
ToJsonString = class(--[[Class.TO_JSON_STRING]])
Match = class(--[[Class.MATCH]])
Split = class(--[[Class.SPLIT]])
Upcase = class(--[[Class.UPCASE]])
Downcase = class(--[[Class.DOWNCASE]])
IsEmpty = class(--[[Class.IS_EMPTY]])
Group = class(--[[Class.GROUP]])
Sum = class(--[[Class.SUM]])
Avg = class(--[[Class.AVG]])
Min = class(--[[Class.MIN]])
Max = class(--[[Class.MAX]])
InnerJoin = class(--[[Class.INNER_JOIN]])
OuterJoin = class(--[[Class.OUTER_JOIN]])
EqJoin = class(--[[Class.EQ_JOIN]])
Zip = class(--[[Class.ZIP]])
CoerceTo = class(--[[Class.COERCE_TO]])
Ungroup = class(--[[Class.UNGROUP]])
TypeOf = class(--[[Class.TYPEOF]])
Info = class(--[[Class.INFO]])
Sample = class(--[[Class.SAMPLE]])
Update = class(--[[Class.UPDATE]])
Delete = class(--[[Class.DELETE]])
Replace = class(--[[Class.REPLACE]])
Insert = class(--[[Class.INSERT]])
DbCreate = class(--[[Class.DB_CREATE]])
DbDrop = class(--[[Class.DB_DROP]])
DbList = class(--[[Class.DB_LIST]])
TableCreate = class(--[[Class.TABLE_CREATE]])
TableDrop = class(--[[Class.TABLE_DROP]])
TableList = class(--[[Class.TABLE_LIST]])
IndexCreate = class(--[[Class.INDEX_CREATE]])
IndexDrop = class(--[[Class.INDEX_DROP]])
IndexRename = class(--[[Class.INDEX_RENAME]])
IndexList = class(--[[Class.INDEX_LIST]])
IndexStatus = class(--[[Class.INDEX_STATUS]])
IndexWait = class(--[[Class.INDEX_WAIT]])
Sync = class(--[[Class.SYNC]])
FunCall = class(--[[Class.FUNCALL]])
Default = class(--[[Class.DEFAULT]])
Branch = class(--[[Class.BRANCH]])
Any = class(--[[Class.ANY]])
All = class(--[[Class.ALL]])
ForEach = class(--[[Class.FOREACH]])
Func = class(--[[Class.FUNC]])
Asc = class(--[[Class.ASC]])
Desc = class(--[[Class.DESC]])
Literal = class(--[[Class.LITERAL]])
ISO8601 = class(--[[Class.ISO8601]])
ToISO8601 = class(--[[Class.TO_ISO8601]])
EpochTime = class(--[[Class.EPOCH_TIME]])
ToEpochTime = class(--[[Class.TO_EPOCH_TIME]])
Now = class(--[[Class.NOW]])
InTimezone = class(--[[Class.IN_TIMEZONE]])
During = class(--[[Class.DURING]])
Date = class(--[[Class.DATE]])
TimeOfDay = class(--[[Class.TIME_OF_DAY]])
Timezone = class(--[[Class.TIMEZONE]])
Year = class(--[[Class.YEAR]])
Month = class(--[[Class.MONTH]])
Day = class(--[[Class.DAY]])
DayOfWeek = class(--[[Class.DAY_OF_WEEK]])
DayOfYear = class(--[[Class.DAY_OF_YEAR]])
Hours = class(--[[Class.HOURS]])
Minutes = class(--[[Class.MINUTES]])
Seconds = class(--[[Class.SECONDS]])
Time = class(--[[Class.TIME]])
GeoJson = class(--[[Class.GEOJSON]])
ToGeoJson = class(--[[Class.TO_GEOJSON]])
Point = class(--[[Class.POINT]])
Line = class(--[[Class.LINE]])
Polygon = class(--[[Class.POLYGON]])
Distance = class(--[[Class.DISTANCE]])
Intersects = class(--[[Class.INTERSECTS]])
Includes = class(--[[Class.INCLUDES]])
Circle = class(--[[Class.CIRCLE]])
GetIntersecting = class(--[[Class.GET_INTERSECTING]])
GetNearest = class(--[[Class.GET_NEAREST]])
Fill = class(--[[Class.FILL]])
PolygonSub = class(--[[Class.POLYGON_SUB]])
UUID = class(--[[Class.UUID]])

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

r.monday = class(--[[Class.MONDAY]])()
r.tuesday = class(--[[Class.TUESDAY]])()
r.wednesday = class(--[[Class.WEDNESDAY]])()
r.thursday = class(--[[Class.THURSDAY]])()
r.friday = class(--[[Class.FRIDAY]])()
r.saturday = class(--[[Class.SATURDAY]])()
r.sunday = class(--[[Class.SUNDAY]])()

r.january = class(--[[Class.JANUARY]])()
r.february = class(--[[Class.FEBRUARY]])()
r.march = class(--[[Class.MARCH]])()
r.april = class(--[[Class.APRIL]])()
r.may = class(--[[Class.MAY]])()
r.june = class(--[[Class.JUNE]])()
r.july = class(--[[Class.JULY]])()
r.august = class(--[[Class.AUGUST]])()
r.september = class(--[[Class.SEPTEMBER]])()
r.october = class(--[[Class.OCTOBER]])()
r.november = class(--[[Class.NOVEMBER]])()
r.december = class(--[[Class.DECEMBER]])()

function r.object(...)
  return Object({}, ...)
end
function r.args(...)
  return Args({}, ...)
end
function r.geojson(...)
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
