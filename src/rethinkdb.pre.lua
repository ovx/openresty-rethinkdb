local json = require('json')
local mime = require('mime')
local socket = require('socket')

-- r is both the main export table for the module
-- and a function that wraps a native Lua value in an ReQL datum
local r = {}

local Connection, Cursor
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
local ReQLDriverError, ReQLServerError, ReQLRuntimeError, ReQLCompileError
local ReQLClientError, ReQLQueryPrinter, ReQLError

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
    if is_instance(val, ReQLOp) then
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
    parent.__inherited(parent, _class_0)
  end

  return _class_0
end

function is_instance(class, obj)
  if type(obj) ~= 'table' then return false end

  local obj_cls = obj.__class
  while obj_cls do
    if obj_cls.__name == class.__name then
      return true
    end
    obj_cls = obj_cls.__parent
  end

  return false
end

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
          error(ReQLDriverError('Anonymous function returned `nil`. Did you forget a `return`?'))
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
      if self.tt == --[[Term.FUN_CALL]] then
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
          return options(ReQLDriverError('Second argument to `run` cannot be a function if a third argument is provided.'))
        end
      end
      -- else we suppose that we have run(connection[, options][, callback])
      options = options or {}

      if type(connection._start) ~= 'function' then
        if callback then
          return callback(ReQLDriverError('First argument to `run` must be an open connection.'))
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
        if (type(perhaps_opt_dict) == 'table') and not (is_instance(ReQLOp, perhaps_opt_dict)) then
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
      if (type(perhaps_opt_dict) == 'table') and not is_instance(ReQLOp, perhaps_opt_dict) then
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

MakeArray = ast(--[[Class.MAKE_ARRAY]])
MakeObj = ast(--[[Class.MAKE_OBJ]])
Var = ast(--[[Class.VAR]])
JavaScript = ast(--[[Class.JAVASCRIPT]])
Http = ast(--[[Class.HTTP]])
Json = ast(--[[Class.JSON]])
Binary = ast(--[[Class.BINARY]])
Args = ast(--[[Class.ARGS]])
Error = ast(--[[Class.ERROR]])
Random = ast(--[[Class.RANDOM]])
Db = ast(--[[Class.DB]])
Table = ast(--[[Class.TABLE]])
Get = ast(--[[Class.GET]])
GetAll = ast(--[[Class.GET_ALL]])
Eq = ast(--[[Class.EQ]])
Ne = ast(--[[Class.NE]])
Lt = ast(--[[Class.LT]])
Le = ast(--[[Class.LE]])
Gt = ast(--[[Class.GT]])
Ge = ast(--[[Class.GE]])
Not = ast(--[[Class.NOT]])
Add = ast(--[[Class.ADD]])
Sub = ast(--[[Class.SUB]])
Mul = ast(--[[Class.MUL]])
Div = ast(--[[Class.DIV]])
Mod = ast(--[[Class.MOD]])
Append = ast(--[[Class.APPEND]])
Prepend = ast(--[[Class.PREPEND]])
Difference = ast(--[[Class.DIFFERENCE]])
SetInsert = ast(--[[Class.SET_INSERT]])
SetUnion = ast(--[[Class.SET_UNION]])
SetIntersection = ast(--[[Class.SET_INTERSECTION]])
SetDifference = ast(--[[Class.SET_DIFFERENCE]])
Slice = ast(--[[Class.SLICE]])
Skip = ast(--[[Class.SKIP]])
Limit = ast(--[[Class.LIMIT]])
GetField = ast(--[[Class.GET_FIELD]])
Bracket = ast(--[[Class.BRACKET]])
Contains = ast(--[[Class.CONTAINS]])
InsertAt = ast(--[[Class.INSERT_AT]])
SpliceAt = ast(--[[Class.SPLICE_AT]])
DeleteAt = ast(--[[Class.DELETE_AT]])
ChangeAt = ast(--[[Class.CHANGE_AT]])
Contains = ast(--[[Class.CONTAINS]])
HasFields = ast(--[[Class.HAS_FIELDS]])
WithFields = ast(--[[Class.WITH_FIELDS]])
Keys = ast(--[[Class.KEYS]])
Changes = ast(--[[Class.CHANGES]])
Object = ast(--[[Class.OBJECT]])
Pluck = ast(--[[Class.PLUCK]])
IndexesOf = ast(--[[Class.INDEXES_OF]])
Without = ast(--[[Class.WITHOUT]])
Merge = ast(--[[Class.MERGE]])
Between = ast(--[[Class.BETWEEN]])
Reduce = ast(--[[Class.REDUCE]])
Map = ast(--[[Class.MAP]])
Filter = ast(--[[Class.FILTER]])
ConcatMap = ast(--[[Class.CONCAT_MAP]])
OrderBy = ast(--[[Class.ORDER_BY]])
Distinct = ast(--[[Class.DISTINCT]])
Count = ast(--[[Class.COUNT]])
Union = ast(--[[Class.UNION]])
Nth = ast(--[[Class.NTH]])
ToJsonString = ast(--[[Class.TO_JSON_STRING]])
Match = ast(--[[Class.MATCH]])
Split = ast(--[[Class.SPLIT]])
Upcase = ast(--[[Class.UPCASE]])
Downcase = ast(--[[Class.DOWNCASE]])
IsEmpty = ast(--[[Class.IS_EMPTY]])
Group = ast(--[[Class.GROUP]])
Sum = ast(--[[Class.SUM]])
Avg = ast(--[[Class.AVG]])
Min = ast(--[[Class.MIN]])
Max = ast(--[[Class.MAX]])
InnerJoin = ast(--[[Class.INNER_JOIN]])
OuterJoin = ast(--[[Class.OUTER_JOIN]])
EqJoin = ast(--[[Class.EQ_JOIN]])
Zip = ast(--[[Class.ZIP]])
CoerceTo = ast(--[[Class.COERCE_TO]])
Ungroup = ast(--[[Class.UNGROUP]])
TypeOf = ast(--[[Class.TYPE_OF]])
Info = ast(--[[Class.INFO]])
Sample = ast(--[[Class.SAMPLE]])
Update = ast(--[[Class.UPDATE]])
Delete = ast(--[[Class.DELETE]])
Replace = ast(--[[Class.REPLACE]])
Insert = ast(--[[Class.INSERT]])
DbCreate = ast(--[[Class.DB_CREATE]])
DbDrop = ast(--[[Class.DB_DROP]])
DbList = ast(--[[Class.DB_LIST]])
TableCreate = ast(--[[Class.TABLE_CREATE]])
TableDrop = ast(--[[Class.TABLE_DROP]])
TableList = ast(--[[Class.TABLE_LIST]])
IndexCreate = ast(--[[Class.INDEX_CREATE]])
IndexDrop = ast(--[[Class.INDEX_DROP]])
IndexRename = ast(--[[Class.INDEX_RENAME]])
IndexList = ast(--[[Class.INDEX_LIST]])
IndexStatus = ast(--[[Class.INDEX_STATUS]])
IndexWait = ast(--[[Class.INDEX_WAIT]])
Sync = ast(--[[Class.SYNC]])
FunCall = ast(--[[Class.FUN_CALL]])
Default = ast(--[[Class.DEFAULT]])
Branch = ast(--[[Class.BRANCH]])
Any = ast(--[[Class.ANY]])
All = ast(--[[Class.ALL]])
ForEach = ast(--[[Class.FOR_EACH]])
Func = ast(--[[Class.FUNC]])
Asc = ast(--[[Class.ASC]])
Desc = ast(--[[Class.DESC]])
Literal = ast(--[[Class.LITERAL]])
ISO8601 = ast(--[[Class.ISO8601]])
ToISO8601 = ast(--[[Class.TO_ISO8601]])
EpochTime = ast(--[[Class.EPOCH_TIME]])
ToEpochTime = ast(--[[Class.TO_EPOCH_TIME]])
Now = ast(--[[Class.NOW]])
InTimezone = ast(--[[Class.IN_TIMEZONE]])
During = ast(--[[Class.DURING]])
Date = ast(--[[Class.DATE]])
TimeOfDay = ast(--[[Class.TIME_OF_DAY]])
Timezone = ast(--[[Class.TIMEZONE]])
Year = ast(--[[Class.YEAR]])
Month = ast(--[[Class.MONTH]])
Day = ast(--[[Class.DAY]])
DayOfWeek = ast(--[[Class.DAY_OF_WEEK]])
DayOfYear = ast(--[[Class.DAY_OF_YEAR]])
Hours = ast(--[[Class.HOURS]])
Minutes = ast(--[[Class.MINUTES]])
Seconds = ast(--[[Class.SECONDS]])
Time = ast(--[[Class.TIME]])
GeoJson = ast(--[[Class.GEO_JSON]])
ToGeoJson = ast(--[[Class.TO_GEO_JSON]])
Point = ast(--[[Class.POINT]])
Line = ast(--[[Class.LINE]])
Polygon = ast(--[[Class.POLYGON]])
Distance = ast(--[[Class.DISTANCE]])
Intersects = ast(--[[Class.INTERSECTS]])
Includes = ast(--[[Class.INCLUDES]])
Circle = ast(--[[Class.CIRCLE]])
GetIntersecting = ast(--[[Class.GET_INTERSECTING]])
GetNearest = ast(--[[Class.GET_NEAREST]])
Fill = ast(--[[Class.FILL]])
PolygonSub = ast(--[[Class.POLYGON_SUB]])
UUID = ast(--[[Class.UUID]])

-- All top level exported functions

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

r.monday = ast(--[[Class.MONDAY]])()
r.tuesday = ast(--[[Class.TUESDAY]])()
r.wednesday = ast(--[[Class.WEDNESDAY]])()
r.thursday = ast(--[[Class.THURSDAY]])()
r.friday = ast(--[[Class.FRIDAY]])()
r.saturday = ast(--[[Class.SATURDAY]])()
r.sunday = ast(--[[Class.SUNDAY]])()

r.january = ast(--[[Class.JANUARY]])()
r.february = ast(--[[Class.FEBRUARY]])()
r.march = ast(--[[Class.MARCH]])()
r.april = ast(--[[Class.APRIL]])()
r.may = ast(--[[Class.MAY]])()
r.june = ast(--[[Class.JUNE]])()
r.july = ast(--[[Class.JULY]])()
r.august = ast(--[[Class.AUGUST]])()
r.september = ast(--[[Class.SEPTEMBER]])()
r.october = ast(--[[Class.OCTOBER]])()
r.november = ast(--[[Class.NOVEMBER]])()
r.december = ast(--[[Class.DECEMBER]])()

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
  -- An R_OBJECT may be a regular object or a 'pseudo-type' so we need a
  -- second layer of type switching here on the obfuscated field '$reql_type$'
  local _exp_0 = obj['$reql_type$']
  if 'TIME' == _exp_0 then
    local _exp_1 = opts.time_format
    if 'native' == _exp_1 or not _exp_1 then
      if not (obj['epoch_time']) then
        error(err.ReQLDriverError('pseudo-type TIME ' .. tostring(obj) .. ' object missing expected field `epoch_time`.'))
      end

      -- We ignore the timezone field of the pseudo-type TIME object. JS dates do not support timezones.
      -- By converting to a native date object we are intentionally throwing out timezone information.

      -- field 'epoch_time' is in seconds but the Date constructor expects milliseconds
      return (Date(obj['epoch_time'] * 1000))
    elseif 'raw' == _exp_1 then
      -- Just return the raw (`{'$reql_type$'...}`) object
      return obj
    else
      error(err.ReQLDriverError('Unknown time_format run option ' .. tostring(opts.time_format) .. '.'))
    end
  elseif 'GROUPED_DATA' == _exp_0 then
    local _exp_1 = opts.group_format
    if 'native' == _exp_1 or not _exp_1 then
      -- Don't convert the data into a map, because the keys could be objects which doesn't work in JS
      -- Instead, we have the following format:
      -- [ { 'group': <group>, 'reduction': <value(s)> } }, ... ]
      res = {}
      j = 1
      for i, v in ipairs(obj['data']) do
        res[j] = {
          group = i,
          reduction = v
        }
        j = j + 1
      end
      obj = res
    elseif 'raw' == _exp_1 then
      return obj
    else
      error(err.ReQLDriverError('Unknown group_format run option ' .. tostring(opts.group_format) .. '.'))
    end
  elseif 'BINARY' == _exp_0 then
    local _exp_1 = opts.binary_format
    if 'native' == _exp_1 or not _exp_1 then
      if not obj.data then
        error(err.ReQLDriverError('pseudo-type BINARY object missing expected field `data`.'))
      end
      return (mime.unb64(obj.data))
    elseif 'raw' == _exp_1 then
      return obj
    else
      error(err.ReQLDriverError('Unknown binary_format run option ' .. tostring(opts.binary_format) .. '.'))
    end
  else
    -- Regular object or unknown pseudo type
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
      if response.r[1] or t == --[[Response.WAIT_COMPLETE]] then
        table.insert(self._responses, response)
      end
      if t ~= --[[Response.SUCCESS_PARTIAL]] and t ~= --[[Response.SUCCESS_FEED]] then
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
    next = function(self, cb)
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
      if t == --[[Response.SUCCESS_ATOM]] or t == --[[Response.SUCCESS_PARTIAL]] or t == --[[Response.SUCCESS_FEED]] or t == --[[Response.SUCCESS_SEQUENCE]] then
        local row = recursively_convert_pseudotype(response.r[self._response_index], self._opts)
        self._response_index = self._response_index + 1

        -- If we're done with this response, discard it
        if not response.r[self._response_index] then
          table.remove(self._responses, 1)
          self._response_index = 1
        end
        return cb(nil, row)
      elseif t == --[[Response.COMPILE_ERROR]] then
        return cb(ReQLCompileError(response.r[1], self._root, response.b))
      elseif t == --[[Response.CLIENT_ERROR]] then
        return cb(ReQLClientError(response.r[1], self._root, response.b))
      elseif t == --[[Response.RUNTIME_ERROR]] then
        return cb(ReQLRuntimeError(response.r[1], self._root, response.b))
      elseif t == --[[Response.WAIT_COMPLETE]] then
        return cb(nil, nil)
      end
      return cb(ReQLDriverError('Unknown response type ' .. t))
    end,
    close = function(self, cb)
      if not self._end_flag then
        self._conn:_end_query(self._token)
      end
      if cb then return cb() end
    end,
    each = function(self, cb, on_finished)
      if type(cb) ~= 'function' then
        error(ReQLDriverError('First argument to each must be a function.'))
      end
      if on_finished and type(on_finished) ~= 'function' then
        error(ReQLDriverError('Optional second argument to each must be a function.'))
      end
      function next_cb(err, data)
        if err then
          if err.message ~= 'ReQLDriverError No more rows in the cursor.' then
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
      if self._type == --[[Response.SUCCESS_FEED]] then
        return cb(ReQLDriverError('`to_array` is not available for feeds.'))
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
)

Connection = class(
  'Connection',
  {
    __init = function(self, host_or_callback, callback)
      local host = {}
      if type(host_or_callback) == 'function' then
        callback = host_or_callback
      else
        host = host_or_callback
      end
      if type(host) == 'string' then
        host = {
          host = host
        }
      end
      function cb(err, conn)
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
          int_to_bytes(--[[Version.V0_3]], 4) ..
          int_to_bytes(self.auth_key:len(), 4) ..
          self.auth_key ..
          int_to_bytes(--[[Protocol.JSON]], 4)
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
              t = --[[Response.CLIENT_ERROR]],
              r = {'connection returned: ' .. err},
              b = {}
            },
            reqest_token
          )
        end
        self.buffer = self.buffer .. buf
        if response_length > 0 then
          if string.len(self.buffer) >= response_length then
            local response_buffer = string.sub(self.buffer, 1, response_length)
            self.buffer = string.sub(self.buffer, response_length + 1)
            response_length = 0
            self:_process_response(json.decode(response_buffer), token)
            if token == reqest_token then return end
          end
        else
          if string.len(self.buffer) >= 12 then
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
        error(ReQLDriverError('Unexpected token ' .. token .. '.'))
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
          error(ReQLDriverError('First argument to two-argument `close` must be an object.'))
        end
        opts = opts_or_callback
        cb = callback
      else
        if type(opts_or_callback) == 'table' then
          opts = opts_or_callback
        else
          if type(opts_or_callback) == 'function' then
            cb = opts_or_callback
          end
        end
      end

      if cb and type(cb) ~= 'function' then
        error(ReQLDriverError('First argument to two-argument `close` must be an object.'))
      end

      local wrapped_cb = function(...)
        self.open = false
        self.raw_socket:shutdown()
        self.raw_socket:close()
        if cb then
          return cb(...)
        end
      end

      local noreply_wait = opts.noreply_wait and self.open

      if noreply_wait then
        return self:noreply_wait(wrapped_cb)
      end
      return wrapped_cb()
    end,
    noreply_wait = function(self, cb)
      if type(cb) ~= 'function' then
        cb = function() end
      end
      function callback(err, cur)
        if cur then
          local res = cur.next(function(err) return cb(err) end)
          cur:close()
          return res
        end
        return cb(err)
      end
      if not self.open then
        return callback(ReQLDriverError('Connection is closed.'))
      end

      -- Assign token
      local token = self.next_token
      self.next_token = self.next_token + 1

      -- Save cursor
      local cursor = Cursor(self, token, {})

      -- Save cursor
      self.outstanding_callbacks[token] = {cursor = cursor}

      -- Construct query
      self:_write_query(token, '[' .. --[[Query.NOREPLY_WAIT]] .. ']')

      return callback(nil, cursor)
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
      self.outstanding_callbacks = {}
    end,
    reconnect = function(self, opts_or_callback, callback)
      local opts, cb
      if callback then
        opts = opts_or_callback
        cb = callback
      else
        if type(opts_or_callback) == 'function' then
          opts = {}
          cb = opts_or_callback
        else
          if opts_or_callback then
            opts = opts_or_callback
          else
            opts = {}
          end
          cb = callback
        end
      end
      local close_cb = function(err)
        if err then
          return cb(err)
        end
        return Connection(self, cb)
      end
      return self:close(opts, close_cb)
    end,
    use = function(self, db)
      self.db = db
    end,
    _start = function(self, term, cb, opts)
      if not (self.open) then
        cb(ReQLDriverError('Connection is closed.'))
      end

      -- Assign token
      local token = self.next_token
      self.next_token = self.next_token + 1

      for k, v in pairs(opts) do
        if k == 'use_outdated' or k == 'noreply' or k == 'profile' then
          v = not not v
        end
        opts[k] = r(v):build()
      end

      -- Set global options
      if self.db then
        opts.db = r.db(self.db):build()
      end

      -- Construct query
      local query = {--[[Query.START]], term:build(), opts}

      local cursor = Cursor(self, token, opts, term)

      -- Save cursor
      self.outstanding_callbacks[token] = {cursor = cursor}
      self:_send_query(token, query)
      if type(cb) == 'function' and not opts.noreply then
        local res = cb(nil, cursor)
        cursor:close()
        return res
      end
    end,
    _continue_query = function(self, token)
      return self:_write_query(token, '[' .. --[[Query.CONTINUE]] .. ']')
    end,
    _end_query = function(self, token)
      return self:_write_query(token, '[' .. --[[Query.STOP]] .. ']')
    end,
    _send_query = function(self, token, query)
      return self:_write_query(token, json.encode(query))
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
