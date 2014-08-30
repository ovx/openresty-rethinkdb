util = require('./util')
err = require('./errors')
net = require('./net')
protoTermType = require('./proto-def').TermType

-- Import some names to this namespace for convienience
ar = util.ar
varar = util.varar
aropt = util.aropt

-- rethinkdb is both the main export object for the module
-- and a function that shortcuts `r.expr`.
rethinkdb = (...) -> rethinkdb.expr(unpack arg)

-- Utilities

funcWrap = (val) ->
    unless val
        -- Pass through the nil value so it's caught by
        -- the appropriate nil checker
        return val

    val = rethinkdb.expr(val)

    ivarScan = (node) ->
        unless node instanceof TermBase then return false
        if node instanceof ImplicitVar then return true
        if (node.args.map ivarScan).some((a)->a) then return true
        for k, v in node.optargs
            return true if ivarScan v
        return false

    if ivarScan(val)
        return Func {}, (x) -> val

    return val

hasImplicit = (args) ->
    -- args is an array of (strings and arrays)
    -- We recurse to look for `r.row` which is an implicit var
    if Array.isArray(args)
        for arg in args
            if hasImplicit(arg) == true
                return true
    else if args == 'r.row'
        return true
    return false

-- AST classes

class TermBase
    showRunWarning: true
    new: ->
        self = (ar (field) -> self.bracket(field))
        self.__proto__ = @.__proto__
        return self

    run: (connection, options, callback) ->
        -- Valid syntaxes are
        -- connection, callback
        -- connection, options, callback
        -- connection, nil, callback

        if net.isConnection(connection) == true
            -- Handle run(connection, callback)
            if type(options) == "function"
                unless callback
                    callback = options
                    options = {}
                else
                    options err.RqlDriverError("Second argument to `run` cannot be a function if a third argument is provided.")
                    return
            -- else we suppose that we have run(connection[, options][, callback])

        options = {} unless options

        -- Check if the arguments are valid types
        for key in options
            switch key
                when 'useOutdated', 'noreply', 'timeFormat', 'profile', 'durability', 'groupFormat', 'binaryFormat', 'batchConf', 'arrayLimit'
                    nil
                else
                    callback err.RqlDriverError "Found "+key+" which is not a valid option. valid options are {useOutdated: <bool>, noreply: <bool>, timeFormat: <string>, groupFormat: <string>, binaryFormat: <string>, profile: <bool>, durability: <string>, arrayLimit: <number>}."
        if net.isConnection(connection) is false
            callback err.RqlDriverError "First argument to `run` must be an open connection."

        if options.noreply == true or type(callback) == 'function'
            status, err = pcall(connection._start @, callback, options)
            unless status
                -- It was decided that, if we can, we prefer to invoke the callback
                -- with any errors rather than throw them as normal exceptions.
                -- Thus we catch errors here and invoke the callback instead of
                -- letting the error bubble up.
                if type(callback) == 'function'
                    callback(err)

    toString: -> err.printQuery(@)

class RDBVal extends TermBase
    eq: (...) -> Eq {}, @, unpack arg
    ne: (...) -> Ne {}, @, unpack arg
    lt: (...) -> Lt {}, @, unpack arg
    le: (...) -> Le {}, @, unpack arg
    gt: (...) -> Gt {}, @, unpack arg
    ge: (...) -> Ge {}, @, unpack arg

    not: (...) -> Not {}, @, unpack arg

    add: (...) -> Add {}, @, unpack arg
    sub: (...) -> Sub {}, @, unpack arg
    mul: (...) -> Mul {}, @, unpack arg
    div: (...) -> Div {}, @, unpack arg
    mod: (...) -> Mod {}, @, unpack arg

    append: (...) -> Append {}, @, unpack arg
    prepend: (...) -> Prepend {}, @, unpack arg
    difference: (...) -> Difference {}, @, unpack arg
    setInsert: (...) -> SetInsert {}, @, unpack arg
    setUnion: (...) -> SetUnion {}, @, unpack arg
    setIntersection: (...) -> SetIntersection {}, @, unpack arg
    setDifference: (...) -> SetDifference {}, @, unpack arg
    slice: varar(1, 3, (left, right_or_opts, opts) ->
        if opts
            Slice opts, @, left, right_or_opts
        else if right_or_opts
            -- FIXME
            if (Object::toString.call(right_or_opts) == '[object Object]') and not (right_or_opts.instanceof(TermBase)
                Slice right_or_opts, @, left
            else
                Slice {}, @, left, right_or_opts
        else
            Slice {}, @, left
        )
    skip: (...) -> Skip {}, @, unpack arg
    limit: (...) -> Limit {}, @, unpack arg
    getField: (...) -> GetField {}, @, unpack arg
    contains: (...) -> Contains {}, @, unpack arg
    insertAt: (...) -> InsertAt {}, @, unpack arg
    spliceAt: (...) -> SpliceAt {}, @, unpack arg
    deleteAt: (...) -> DeleteAt {}, @, unpack arg
    changeAt: (...) -> ChangeAt {}, @, unpack arg
    indexesOf: (...) -> IndexesOf {}, @, unpack arg
    hasFields: (...) -> HasFields {}, @, unpack arg
    withFields: (...) -> WithFields {}, @, unpack arg
    keys: (...) -> Keys {}, @, unpack arg
    changes: (...) -> Changes {}, @, unpack arg

    -- pluck and without on zero fields are allowed
    pluck: (...) -> Pluck {}, @, unpack arg
    without: (...) -> Without {}, @, unpack arg

    merge: (...) -> Merge {}, @, unpack arg
    between: aropt (left, right, opts) -> Between opts, @, left, right
    reduce: (...) -> Reduce {}, @, unpack arg
    map: (...) -> Map {}, @, unpack arg
    filter: aropt (predicate, opts) -> Filter opts, @, funcWrap(predicate)
    concatMap: (...) -> ConcatMap {}, @, unpack arg
    distinct: aropt (opts) -> Distinct opts, @
    count: (...) -> Count {}, @, unpack arg
    union: (...) -> Union {}, @, unpack arg
    nth: (...) -> Nth {}, @, unpack arg
    bracket: (...) -> Bracket {}, @, unpack arg
    match: (...) -> Match {}, @, unpack arg
    split: (...) -> Split {}, @, unpack arg
    upcase: (...) -> Upcase {}, @, unpack arg
    downcase: (...) -> Downcase {}, @, unpack arg
    isEmpty: (...) -> IsEmpty {}, @, unpack arg
    innerJoin: (...) -> InnerJoin {}, @, unpack arg
    outerJoin: (...) -> OuterJoin {}, @, unpack arg
    eqJoin: aropt (left_attr, right, opts) -> EqJoin opts, @, funcWrap(left_attr), right
    zip: (...) -> Zip {}, @, unpack arg
    coerceTo: (...) -> CoerceTo {}, @, unpack arg
    ungroup: (...) -> Ungroup {}, @, unpack arg
    typeOf: (...) -> TypeOf {}, @, unpack arg
    update: aropt (func, opts) -> Update opts, @, funcWrap(func)
    delete: aropt (opts) -> Delete opts, @
    replace: aropt (func, opts) -> Replace opts, @, funcWrap(func)
    do: (...) ->
        FunCall {}, funcWrap(arg[arg.n]), @, unpack args[,arg.n-1]

    default: (...) -> Default {}, @, unpack arg

    or: (...) -> Any {}, @, unpack arg
    any: (...) -> Any {}, @, unpack arg
    and: (...) -> All {}, @, unpack arg
    all: (...) -> All {}, @, unpack arg

    forEach: (...) -> ForEach {}, @, unpack arg

    sum: (...) -> Sum {}, @, unpack arg
    avg: (...) -> Avg {}, @, unpack arg
    min: (...) -> Min {}, @, unpack arg
    max: (...) -> Max {}, @, unpack arg

    info: (...) -> Info {}, @, unpack arg
    sample: (...) -> Sample {}, @, unpack arg

    group: (...) ->
        -- Default if no opts dict provided
        opts = {}
        fields = arg

        -- Look for opts dict
        if fieldsAndOpts.length > 0
            perhapsOptDict = fieldsAndOpts[fieldsAndOpts.length - 1]
            if perhapsOptDict and
                    (Object::toString.call(perhapsOptDict) == '[object Object]') and
                    not (TermBase.instanceof(perhapsOptDict))
                opts = perhapsOptDict
                fields = fieldsAndOpts[0...(fieldsAndOpts.length - 1)]
        fields = [funcWrap(field) for field in fields]

        Group opts, @, unpack fields

    orderBy: (...) ->
        -- Default if no opts dict provided
        opts = {}
        attrs = arg

        -- Look for opts dict
        perhapsOptDict = attrsAndOpts[attrsAndOpts.length - 1]
        if perhapsOptDict and
                (Object::toString.call(perhapsOptDict) == '[object Object]') and
                not (TermBase.instanceof(perhapsOptDict))
            opts = perhapsOptDict
            attrs = attrsAndOpts[0...(attrsAndOpts.length - 1)]

        attrs = (for attr in attrs
            if attr instanceof Asc or attr instanceof Desc
                attr
            else
                funcWrap(attr)
        )

        OrderBy opts, @, unpack attrs

    -- Geo operations
    toGeojson: (...) -> ToGeojson {}, @, unpack arg
    distance: aropt (g, opts) -> Distance opts, @, g
    intersects: (...) -> Intersects {}, @, unpack arg
    includes: (...) -> Includes {}, @, unpack arg
    fill: (...) -> Fill {}, @, unpack arg

    -- Database operations

    tableCreate: aropt (tblName, opts) -> TableCreate opts, @, tblName
    tableDrop: (...) -> TableDrop {}, @, unpack arg
    tableList: (...) -> TableList {}, @, unpack arg

    table: aropt (tblName, opts) -> Table opts, @, tblName

    -- Table operations

    get: (...) -> Get {}, @, unpack arg

    getAll: (...) ->
        -- Default if no opts dict provided
        opts = {}
        keys = arg

        -- Look for opts dict
        if keysAndOpts.length > 1
            perhapsOptDict = keysAndOpts[keysAndOpts.length - 1]
            if perhapsOptDict and
                    ((Object::toString.call(perhapsOptDict) == '[object Object]') and not (TermBase.instanceof(perhapsOptDict)))
                opts = perhapsOptDict
                keys = keysAndOpts[0...(keysAndOpts.length - 1)]

        GetAll opts, @, unpack keys

    insert: aropt (doc, opts) -> Insert opts, @, rethinkdb.expr(doc)
    indexCreate: varar(1, 3, (name, defun_or_opts, opts) ->
        if opts
            IndexCreate opts, @, name, funcWrap(defun_or_opts)
        else if defun_or_opts
            -- FIXME?
            if (Object::toString.call(defun_or_opts) is '[object Object]') and not (defun_or_opts instanceof Function) and not (defun_or_opts instanceof TermBase)
                IndexCreate defun_or_opts, @, name
            else
                IndexCreate {}, @, name, funcWrap(defun_or_opts)
        else
            IndexCreate {}, @, name
        )

    indexDrop: (...) -> IndexDrop {}, @, unpack arg
    indexList: (...) -> IndexList {}, @, unpack arg
    indexStatus: (...) -> IndexStatus {}, @, unpack arg
    indexWait: (...) -> IndexWait {}, @, unpack arg
    indexRename: aropt (old_name, new_name, opts) -> IndexRename opts, @, old_name, new_name

    sync: (...) -> Sync {}, @, unpack arg

    toISO8601: (...) -> ToISO8601 {}, @, unpack arg
    toEpochTime: (...) -> ToEpochTime {}, @, unpack arg
    inTimezone: (...) -> InTimezone {}, @, unpack arg
    during: aropt (t2, t3, opts) -> During opts, @, t2, t3
    date: (...) -> RQLDate {}, @, unpack arg
    timeOfDay: (...) -> TimeOfDay {}, @, unpack arg
    timezone: (...) -> Timezone {}, @, unpack arg

    year: (...) -> Year {}, @, unpack arg
    month: (...) -> Month {}, @, unpack arg
    day: (...) -> Day {}, @, unpack arg
    dayOfWeek: (...) -> DayOfWeek {}, @, unpack arg
    dayOfYear: (...) -> DayOfYear {}, @, unpack arg
    hours: (...) -> Hours {}, @, unpack arg
    minutes: (...) -> Minutes {}, @, unpack arg
    seconds: (...) -> Seconds {}, @, unpack arg

    uuid: (...) -> UUID {}, @, unpack arg

    getIntersecting: aropt (g, opts) -> GetIntersecting opts, @, g
    getNearest: aropt (g, opts) -> GetNearest opts, @, g

class DatumTerm extends RDBVal
    args: []
    optargs: {}

    new: (val) ->
        self = super()
        self.data = val
        return self

    compose: ->
        switch typeof @data
            when 'string'
                '"'+@data+'"'
            else
                ''+@data

    build: ->
        if typeof(@data) == 'number'
            unless isFinite(@data)
                error(TypeError("Illegal non-finite number `" + @data.toString() + "`."))
        @data

translateBackOptargs = (optargs) ->
    result = {}
    for key,val in optargs
        key = switch key
            when 'primary_key' then 'primaryKey'
            when 'return_vals' then 'returnVals'
            when 'return_changes' then 'returnChanges'
            when 'use_outdated' then 'useOutdated'
            when 'non_atomic' then 'nonAtomic'
            when 'left_bound' then 'leftBound'
            when 'right_bound' then 'rightBound'
            when 'default_timezone' then 'defaultTimezone'
            when 'result_format' then 'resultFormat'
            when 'page_limit' then 'pageLimit'
            when 'num_vertices' then 'numVertices'
            when 'geo_system' then 'geoSystem'
            when 'max_results' then 'maxResults'
            when 'max_dist' then 'maxDist'
            else key

        result[key] = val
    return result

translateOptargs = (optargs) ->
    result = {}
    for key,val in optargs
        -- We translate known two word opt-args to camel case for your convience
        key = switch key
            when 'primaryKey' then 'primary_key'
            when 'returnVals' then 'return_vals'
            when 'returnChanges' then 'return_changes'
            when 'useOutdated' then 'use_outdated'
            when 'nonAtomic' then 'non_atomic'
            when 'leftBound' then 'left_bound'
            when 'rightBound' then 'right_bound'
            when 'defaultTimezone' then 'default_timezone'
            when 'resultFormat' then 'result_format'
            when 'pageLimit' then 'page_limit'
            when 'numVertices' then 'num_vertices'
            when 'geoSystem' then 'geo_system'
            when 'maxResults' then 'max_results'
            when 'maxDist' then 'max_dist'
            else key

        continue unless key and val
        result[key] = rethinkdb.expr val
    return result

class RDBOp extends RDBVal
    new: (optargs, ...) ->
        self = super()
        self.args =
            for a,i in arg
                if a
                    rethinkdb.expr a
                else
                    error(err.RqlDriverError "Argument #{i} to #{@st || @mt} may not be `undefined`.")
        self.optargs = translateOptargs(optargs)
        return self

    build: ->
        res = [@tt, []]
        for arg in @args
            res[1].push(arg.build())

        opts = {}
        add_opts = false

        for key,val in @optargs
            add_opts = true
            opts[key] = val.build()

        if add_opts
            res.push(opts)
        res

    compose: (args, optargs) ->
        if @st
            return ['r.', @st, '(', intspallargs(args, optargs), ')']
        else
            if shouldWrap(@args[0])
                args[0] = ['r(', args[0], ')']
            return [args[0], '.', @mt, '(', intspallargs(args[1..], optargs), ')']

class RDBOpWrap extends RDBOp
    new: (optargs, unpack arg) ->
        self = super()
        self.args =
            for arg,i in args
                if arg
                    rethinkdb.expr funcWrap arg
                else
                    error(err.RqlDriverError "Argument #{i} to #{@st || @mt} may not be `undefined`.")
        self.optargs = translateOptargs(optargs)
        return self

intsp = (seq) ->
    unless seq[0] then return []
    res = [seq[0]]
    for e in seq[1..]
        res.push(', ', e)
    return res

kved = (optargs) ->
    ['{', intsp([k, ': ', v] for k,v in optargs), '}']

intspallargs = (args, optargs) ->
    argrepr = []
    if args.length > 0
        argrepr.push(intsp(args))
    if Object.keys(optargs).length > 0
        if argrepr.length > 0
            argrepr.push(', ')
        argrepr.push(kved(translateBackOptargs(optargs)))
    return argrepr

shouldWrap = (arg) ->
    arg instanceof DatumTerm or arg instanceof MakeArray or arg instanceof MakeObject

class MakeArray extends RDBOp
    tt: protoTermType.MAKE_ARRAY
    st: '[...]' -- This is only used by the `undefined` argument checker

    compose: (args) -> ['[', intsp(args), ']']

class MakeObject extends RDBOp
    tt: protoTermType.MAKE_OBJECT
    st: '{...}' -- This is only used by the `undefined` argument checker

    new: (obj, nestingDepth=20) ->
        self = super({})
        self.optargs = {}
        for key,val in obj
            unless val
                error(err.RqlDriverError "Object field '#{key}' may not be nil")
            self.optargs[key] = rethinkdb.expr val, nestingDepth-1
        return self

    compose: (args, optargs) -> kved(optargs)

    build: ->
        res = {}
        for key,val in @optargs
            res[key] = val.build()
        return res

class Var extends RDBOp
    tt: protoTermType.VAR
    compose: (args) -> ['var_'+args]

class JavaScript extends RDBOp
    tt: protoTermType.JAVASCRIPT
    st: 'js'

class Http extends RDBOp
    tt: protoTermType.HTTP
    st: 'http'

class Json extends RDBOp
    tt: protoTermType.JSON
    st: 'json'

class Binary extends RDBOp
    tt: protoTermType.BINARY
    st: 'binary'

    new: (data) ->
        if data instanceof TermBase
            self = super({}, data)
        else if data instanceof Buffer
            self = super()
            self.base64_data = data.toString("base64")
        else
            error(TypeError("Parameter to `r.binary` must be a Buffer object or RQL query.")

        return self

    compose: ->
        if @args.length == 0
            'r.binary(<data>)'
        else
            super

    build: ->
        if @args.length == 0
            { '$reql_type$': 'BINARY', 'data': @base64_data }
        else
            super

class Args extends RDBOp
    tt: protoTermType.ARGS
    st: 'args'

class UserError extends RDBOp
    tt: protoTermType.ERROR
    st: 'error'

class Random extends RDBOp
    tt: protoTermType.RANDOM
    st: 'random'

class ImplicitVar extends RDBOp
    tt: protoTermType.IMPLICIT_VAR
    compose: -> ['r.row']

class Db extends RDBOp
    tt: protoTermType.DB
    st: 'db'

class Table extends RDBOp
    tt: protoTermType.TABLE
    st: 'table'

    compose: (args, optargs) ->
        if @args[0] instanceof Db
            [args[0], '.table(', intspallargs(args[1..], optargs), ')']
        else
            ['r.table(', intspallargs(args, optargs), ')']

class Get extends RDBOp
    tt: protoTermType.GET
    mt: 'get'

class GetAll extends RDBOp
    tt: protoTermType.GET_ALL
    mt: 'getAll'

class Eq extends RDBOp
    tt: protoTermType.EQ
    mt: 'eq'

class Ne extends RDBOp
    tt: protoTermType.NE
    mt: 'ne'

class Lt extends RDBOp
    tt: protoTermType.LT
    mt: 'lt'

class Le extends RDBOp
    tt: protoTermType.LE
    mt: 'le'

class Gt extends RDBOp
    tt: protoTermType.GT
    mt: 'gt'

class Ge extends RDBOp
    tt: protoTermType.GE
    mt: 'ge'

class Not extends RDBOp
    tt: protoTermType.NOT
    mt: 'not'

class Add extends RDBOp
    tt: protoTermType.ADD
    mt: 'add'

class Sub extends RDBOp
    tt: protoTermType.SUB
    mt: 'sub'

class Mul extends RDBOp
    tt: protoTermType.MUL
    mt: 'mul'

class Div extends RDBOp
    tt: protoTermType.DIV
    mt: 'div'

class Mod extends RDBOp
    tt: protoTermType.MOD
    mt: 'mod'

class Append extends RDBOp
    tt: protoTermType.APPEND
    mt: 'append'

class Prepend extends RDBOp
    tt: protoTermType.PREPEND
    mt: 'prepend'

class Difference extends RDBOp
    tt: protoTermType.DIFFERENCE
    mt: 'difference'

class SetInsert extends RDBOp
    tt: protoTermType.SET_INSERT
    mt: 'setInsert'

class SetUnion extends RDBOp
    tt: protoTermType.SET_UNION
    mt: 'setUnion'

class SetIntersection extends RDBOp
    tt: protoTermType.SET_INTERSECTION
    mt: 'setIntersection'

class SetDifference extends RDBOp
    tt: protoTermType.SET_DIFFERENCE
    mt: 'setDifference'

class Slice extends RDBOp
    tt: protoTermType.SLICE
    mt: 'slice'

class Skip extends RDBOp
    tt: protoTermType.SKIP
    mt: 'skip'

class Limit extends RDBOp
    tt: protoTermType.LIMIT
    mt: 'limit'

class GetField extends RDBOp
    tt: protoTermType.GET_FIELD
    mt: 'getField'

class Bracket extends RDBOp
    tt: protoTermType.BRACKET
    st: '(...)' -- This is only used by the `undefined` argument checker

    compose: (args) -> [args[0], '(', args[1], ')']

class Contains extends RDBOp
    tt: protoTermType.CONTAINS
    mt: 'contains'

class InsertAt extends RDBOp
    tt: protoTermType.INSERT_AT
    mt: 'insertAt'

class SpliceAt extends RDBOp
    tt: protoTermType.SPLICE_AT
    mt: 'spliceAt'

class DeleteAt extends RDBOp
    tt: protoTermType.DELETE_AT
    mt: 'deleteAt'

class ChangeAt extends RDBOp
    tt: protoTermType.CHANGE_AT
    mt: 'changeAt'

class Contains extends RDBOp
    tt: protoTermType.CONTAINS
    mt: 'contains'

class HasFields extends RDBOp
    tt: protoTermType.HAS_FIELDS
    mt: 'hasFields'

class WithFields extends RDBOp
    tt: protoTermType.WITH_FIELDS
    mt: 'withFields'

class Keys extends RDBOp
    tt: protoTermType.KEYS
    mt: 'keys'

class Changes extends RDBOp
    tt: protoTermType.CHANGES
    mt: 'changes'


class Object_ extends RDBOp
    tt: protoTermType.OBJECT
    mt: 'object'

class Pluck extends RDBOp
    tt: protoTermType.PLUCK
    mt: 'pluck'

class IndexesOf extends RDBOpWrap
    tt: protoTermType.INDEXES_OF
    mt: 'indexesOf'

class Without extends RDBOp
    tt: protoTermType.WITHOUT
    mt: 'without'

class Merge extends RDBOpWrap
    tt: protoTermType.MERGE
    mt: 'merge'

class Between extends RDBOp
    tt: protoTermType.BETWEEN
    mt: 'between'

class Reduce extends RDBOpWrap
    tt: protoTermType.REDUCE
    mt: 'reduce'

class Map extends RDBOpWrap
    tt: protoTermType.MAP
    mt: 'map'

class Filter extends RDBOp
    tt: protoTermType.FILTER
    mt: 'filter'

class ConcatMap extends RDBOpWrap
    tt: protoTermType.CONCATMAP
    mt: 'concatMap'

class OrderBy extends RDBOp
    tt: protoTermType.ORDERBY
    mt: 'orderBy'

class Distinct extends RDBOp
    tt: protoTermType.DISTINCT
    mt: 'distinct'

class Count extends RDBOpWrap
    tt: protoTermType.COUNT
    mt: 'count'

class Union extends RDBOp
    tt: protoTermType.UNION
    mt: 'union'

class Nth extends RDBOp
    tt: protoTermType.NTH
    mt: 'nth'

class Match extends RDBOp
    tt: protoTermType.MATCH
    mt: 'match'

class Split extends RDBOpWrap
    tt: protoTermType.SPLIT
    mt: 'split'

class Upcase extends RDBOp
    tt: protoTermType.UPCASE
    mt: 'upcase'

class Downcase extends RDBOp
    tt: protoTermType.DOWNCASE
    mt: 'downcase'

class IsEmpty extends RDBOp
    tt: protoTermType.IS_EMPTY
    mt: 'isEmpty'

class Group extends RDBOp
    tt: protoTermType.GROUP
    mt: 'group'

class Sum extends RDBOpWrap
    tt: protoTermType.SUM
    mt: 'sum'

class Avg extends RDBOpWrap
    tt: protoTermType.AVG
    mt: 'avg'

class Min extends RDBOpWrap
    tt: protoTermType.MIN
    mt: 'min'

class Max extends RDBOpWrap
    tt: protoTermType.MAX
    mt: 'max'

class InnerJoin extends RDBOp
    tt: protoTermType.INNER_JOIN
    mt: 'innerJoin'

class OuterJoin extends RDBOp
    tt: protoTermType.OUTER_JOIN
    mt: 'outerJoin'

class EqJoin extends RDBOp
    tt: protoTermType.EQ_JOIN
    mt: 'eqJoin'

class Zip extends RDBOp
    tt: protoTermType.ZIP
    mt: 'zip'

class CoerceTo extends RDBOp
    tt: protoTermType.COERCE_TO
    mt: 'coerceTo'

class Ungroup extends RDBOp
    tt: protoTermType.UNGROUP
    mt: 'ungroup'

class TypeOf extends RDBOp
    tt: protoTermType.TYPEOF
    mt: 'typeOf'

class Info extends RDBOp
    tt: protoTermType.INFO
    mt: 'info'

class Sample extends RDBOp
    tt: protoTermType.SAMPLE
    mt: 'sample'

class Update extends RDBOp
    tt: protoTermType.UPDATE
    mt: 'update'

class Delete extends RDBOp
    tt: protoTermType.DELETE
    mt: 'delete'

class Replace extends RDBOp
    tt: protoTermType.REPLACE
    mt: 'replace'

class Insert extends RDBOp
    tt: protoTermType.INSERT
    mt: 'insert'

class DbCreate extends RDBOp
    tt: protoTermType.DB_CREATE
    st: 'dbCreate'

class DbDrop extends RDBOp
    tt: protoTermType.DB_DROP
    st: 'dbDrop'

class DbList extends RDBOp
    tt: protoTermType.DB_LIST
    st: 'dbList'

class TableCreate extends RDBOp
    tt: protoTermType.TABLE_CREATE
    mt: 'tableCreate'

class TableDrop extends RDBOp
    tt: protoTermType.TABLE_DROP
    mt: 'tableDrop'

class TableList extends RDBOp
    tt: protoTermType.TABLE_LIST
    mt: 'tableList'

class IndexCreate extends RDBOp
    tt: protoTermType.INDEX_CREATE
    mt: 'indexCreate'

class IndexDrop extends RDBOp
    tt: protoTermType.INDEX_DROP
    mt: 'indexDrop'

class IndexRename extends RDBOp
    tt: protoTermType.INDEX_RENAME
    mt: 'indexRename'

class IndexList extends RDBOp
    tt: protoTermType.INDEX_LIST
    mt: 'indexList'

class IndexStatus extends RDBOp
    tt: protoTermType.INDEX_STATUS
    mt: 'indexStatus'

class IndexWait extends RDBOp
    tt: protoTermType.INDEX_WAIT
    mt: 'indexWait'

class Sync extends RDBOp
    tt: protoTermType.SYNC
    mt: 'sync'

class FunCall extends RDBOp
    tt: protoTermType.FUNCALL
    st: 'do' -- This is only used by the `undefined` argument checker

    compose: (args) ->
        if args.length > 2
            ['r.do(', intsp(args[1..]), ', ', args[0], ')']
        else
            if shouldWrap(@args[1])
                args[1] = ['r(', args[1], ')']
            [args[1], '.do(', args[0], ')']

class Default extends RDBOp
    tt: protoTermType.DEFAULT
    mt: 'default'

class Branch extends RDBOp
    tt: protoTermType.BRANCH
    st: 'branch'

class Any extends RDBOp
    tt: protoTermType.ANY
    mt: 'or'

class All extends RDBOp
    tt: protoTermType.ALL
    mt: 'and'

class ForEach extends RDBOpWrap
    tt: protoTermType.FOREACH
    mt: 'forEach'

class Func extends RDBOp
    tt: protoTermType.FUNC
    @nextVarId: 0

    new: (optargs, func) ->
        args = []
        argNums = []
        i = 0
        while i < func.length
            argNums.push Func.nextVarId
            args.push Var {}, Func.nextVarId
            Func.nextVarId++
            i++

        body = func(unpack args)
        if body is undefined
            error(err.RqlDriverError "Anonymous function returned `undefined`. Did you forget a `return`?")

        argsArr = MakeArray({}, unpack argNums)
        return super(optargs, argsArr, body)

    compose: (args) ->
        if hasImplicit(args[1]) == true
            [args[1]]
        else
            varStr = ""
            for arg, i in args[0][1] -- ['0', ', ', '1']
                if i%2 == 0
                    varStr += Var::compose(arg)
                else
                    varStr += arg
            ['function(', varStr, ') { return ', args[1], '; }']

class Asc extends RDBOpWrap
    tt: protoTermType.ASC
    st: 'asc'

class Desc extends RDBOpWrap
    tt: protoTermType.DESC
    st: 'desc'

class Literal extends RDBOp
    tt: protoTermType.LITERAL
    st: 'literal'

class ISO8601 extends RDBOp
    tt: protoTermType.ISO8601
    st: 'ISO8601'

class ToISO8601 extends RDBOp
    tt: protoTermType.TO_ISO8601
    mt: 'toISO8601'

class EpochTime extends RDBOp
    tt: protoTermType.EPOCH_TIME
    st: 'epochTime'

class ToEpochTime extends RDBOp
    tt: protoTermType.TO_EPOCH_TIME
    mt: 'toEpochTime'

class Now extends RDBOp
    tt: protoTermType.NOW
    st: 'now'

class InTimezone extends RDBOp
    tt: protoTermType.IN_TIMEZONE
    mt: 'inTimezone'

class During extends RDBOp
    tt: protoTermType.DURING
    mt: 'during'

class RQLDate extends RDBOp
    tt: protoTermType.DATE
    mt: 'date'

class TimeOfDay extends RDBOp
    tt: protoTermType.TIME_OF_DAY
    mt: 'timeOfDay'

class Timezone extends RDBOp
    tt: protoTermType.TIMEZONE
    mt: 'timezone'

class Year extends RDBOp
    tt: protoTermType.YEAR
    mt: 'year'

class Month extends RDBOp
    tt: protoTermType.MONTH
    mt: 'month'

class Day extends RDBOp
    tt: protoTermType.DAY
    mt: 'day'

class DayOfWeek extends RDBOp
    tt: protoTermType.DAY_OF_WEEK
    mt: 'dayOfWeek'

class DayOfYear extends RDBOp
    tt: protoTermType.DAY_OF_YEAR
    mt: 'dayOfYear'

class Hours extends RDBOp
    tt: protoTermType.HOURS
    mt: 'hours'

class Minutes extends RDBOp
    tt: protoTermType.MINUTES
    mt: 'minutes'

class Seconds extends RDBOp
    tt: protoTermType.SECONDS
    mt: 'seconds'

class Time extends RDBOp
    tt: protoTermType.TIME
    st: 'time'

class Geojson extends RDBOp
    tt: protoTermType.GEOJSON
    mt: 'geojson'

class ToGeojson extends RDBOp
    tt: protoTermType.TO_GEOJSON
    mt: 'toGeojson'

class Point extends RDBOp
    tt: protoTermType.POINT
    mt: 'point'

class Line extends RDBOp
    tt: protoTermType.LINE
    mt: 'line'

class Polygon extends RDBOp
    tt: protoTermType.POLYGON
    mt: 'polygon'

class Distance extends RDBOp
    tt: protoTermType.DISTANCE
    mt: 'distance'

class Intersects extends RDBOp
    tt: protoTermType.INTERSECTS
    mt: 'intersects'

class Includes extends RDBOp
    tt: protoTermType.INCLUDES
    mt: 'includes'

class Circle extends RDBOp
    tt: protoTermType.CIRCLE
    mt: 'circle'

class GetIntersecting extends RDBOp
    tt: protoTermType.GET_INTERSECTING
    mt: 'getIntersecting'

class GetNearest extends RDBOp
    tt: protoTermType.GET_NEAREST
    mt: 'getNearest'

class Fill extends RDBOp
    tt: protoTermType.FILL
    mt: 'fill'

class UUID extends RDBOp
    tt: protoTermType.UUID
    st: 'uuid'


-- All top level exported functions

-- Wrap a native JS value in an ReQL datum
rethinkdb.expr = varar 1, 2, (val, nestingDepth=20) ->
    unless val
        error(err.RqlDriverError "Cannot wrap nil with r.expr().")

    if nestingDepth <= 0
        error(err.RqlDriverError "Nesting depth limit exceeded")

    if typeof nestingDepth isnt "number" or isNaN(nestingDepth)
        error(err.RqlDriverError "Second argument to `r.expr` must be a number or undefined.")

    else if val instanceof TermBase
        val
    else if val instanceof Function
        Func {}, val
    else if val instanceof Date
        ISO8601 {}, val.toISOString()
    else if val instanceof Buffer
        Binary val
    else if Array.isArray val
        val = [rethinkdb.expr(v, nestingDepth - 1) for v in val]
        MakeArray {}, unpack val
    else if typeof(val) == 'number'
        DatumTerm val
    else if Object::toString.call(val) == '[object Object]'
        MakeObject val, nestingDepth
    else
        DatumTerm val

rethinkdb.js = aropt (jssrc, opts) -> JavaScript opts, jssrc

rethinkdb.http = aropt (url, opts) -> Http opts, url

rethinkdb.json = (...) -> Json {}, unpack arg

rethinkdb.error = (...) -> UserError {}, unpack arg

rethinkdb.random = (...) ->
        -- Default if no opts dict provided
        opts = {}
        limits = arg

        -- Look for opts dict
        perhapsOptDict = limitsAndOpts[limitsAndOpts.length - 1]
        if perhapsOptDict and
                ((Object::toString.call(perhapsOptDict) is '[object Object]') and not (perhapsOptDict instanceof TermBase))
            opts = perhapsOptDict
            limits = limitsAndOpts[0...(limitsAndOpts.length - 1)]

        Random opts, unpack limits

rethinkdb.binary = ar (data) -> Binary data

rethinkdb.row = ImplicitVar {}

rethinkdb.table = aropt (tblName, opts) -> Table opts, tblName

rethinkdb.db = (...) -> Db {}, unpack arg

rethinkdb.dbCreate = (...) -> DbCreate {}, unpack arg
rethinkdb.dbDrop = (...) -> DbDrop {}, unpack arg
rethinkdb.dbList = (...) -> DbList {}, unpack arg

rethinkdb.tableCreate = aropt (tblName, opts) -> TableCreate opts, tblName
rethinkdb.tableDrop = (...) -> TableDrop {}, unpack arg
rethinkdb.tableList = (...) -> TableList {}, unpack arg

rethinkdb.do = varar 1, nil, (...) ->
    FunCall {}, funcWrap(arg[arg.n]), unpack *arg[,arg.n - 1]

rethinkdb.branch = (...) -> Branch {}, unpack arg

rethinkdb.asc = (...) -> Asc {}, unpack arg
rethinkdb.desc = (...) -> Desc {}, unpack arg

rethinkdb.eq = (...) -> Eq {}, unpack arg
rethinkdb.ne = (...) -> Ne {}, unpack arg
rethinkdb.lt = (...) -> Lt {}, unpack arg
rethinkdb.le = (...) -> Le {}, unpack arg
rethinkdb.gt = (...) -> Gt {}, unpack arg
rethinkdb.ge = (...) -> Ge {}, unpack arg
rethinkdb.or = (...) -> Any {}, unpack arg
rethinkdb.any = (...) -> Any {}, unpack arg
rethinkdb.and = (...) -> All {}, unpack arg
rethinkdb.all = (...) -> All {}, unpack arg

rethinkdb.not = (...) -> Not {}, unpack arg

rethinkdb.add = (...) -> Add {}, unpack arg
rethinkdb.sub = (...) -> Sub {}, unpack arg
rethinkdb.div = (...) -> Div {}, unpack arg
rethinkdb.mul = (...) -> Mul {}, unpack arg
rethinkdb.mod = (...) -> Mod {}, unpack arg

rethinkdb.typeOf = (...) -> TypeOf {}, unpack arg
rethinkdb.info = (...) -> Info {}, unpack arg

rethinkdb.literal = (...) -> Literal {}, unpack arg

rethinkdb.ISO8601 = aropt (str, opts) -> ISO8601 opts, str
rethinkdb.epochTime = (...) -> EpochTime {}, unpack arg
rethinkdb.now = (...) -> Now {}, unpack arg
rethinkdb.time = (...) -> Time {}, unpack arg

rethinkdb.monday = (class extends RDBOp then tt: protoTermType.MONDAY)()
rethinkdb.tuesday = (class extends RDBOp then tt: protoTermType.TUESDAY)()
rethinkdb.wednesday = (class extends RDBOp then tt: protoTermType.WEDNESDAY)()
rethinkdb.thursday = (class extends RDBOp then tt: protoTermType.THURSDAY)()
rethinkdb.friday = (class extends RDBOp then tt: protoTermType.FRIDAY)()
rethinkdb.saturday = (class extends RDBOp then tt: protoTermType.SATURDAY)()
rethinkdb.sunday = (class extends RDBOp then tt: protoTermType.SUNDAY)()

rethinkdb.january = (class extends RDBOp then tt: protoTermType.JANUARY)()
rethinkdb.february = (class extends RDBOp then tt: protoTermType.FEBRUARY)()
rethinkdb.march = (class extends RDBOp then tt: protoTermType.MARCH)()
rethinkdb.april = (class extends RDBOp then tt: protoTermType.APRIL)()
rethinkdb.may = (class extends RDBOp then tt: protoTermType.MAY)()
rethinkdb.june = (class extends RDBOp then tt: protoTermType.JUNE)()
rethinkdb.july = (class extends RDBOp then tt: protoTermType.JULY)()
rethinkdb.august = (class extends RDBOp then tt: protoTermType.AUGUST)()
rethinkdb.september = (class extends RDBOp then tt: protoTermType.SEPTEMBER)()
rethinkdb.october = (class extends RDBOp then tt: protoTermType.OCTOBER)()
rethinkdb.november = (class extends RDBOp then tt: protoTermType.NOVEMBER)()
rethinkdb.december = (class extends RDBOp then tt: protoTermType.DECEMBER)()

rethinkdb.object = (...) -> Object_ {}, unpack arg

rethinkdb.args = (...) -> Args {}, unpack arg

rethinkdb.geojson = (...) -> Geojson {}, unpack arg
rethinkdb.point = (...) -> Point {}, unpack arg
rethinkdb.line = (...) -> Line {}, unpack arg
rethinkdb.polygon = (...) -> Polygon {}, unpack arg
rethinkdb.intersects = (...) -> Intersects {}, unpack arg
rethinkdb.distance = aropt (g1, g2, opts) -> Distance opts, g1, g2
rethinkdb.circle = aropt (cen, rad, opts) -> Circle opts, cen, rad

rethinkdb.uuid = (...) -> UUID {}, unpack arg

-- Export all names defined on rethinkdb
rethinkdb
