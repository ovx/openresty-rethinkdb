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
        return new Func {}, (x) -> val

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
    constructor: ->
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
                    options new err.RqlDriverError("Second argument to `run` cannot be a function if a third argument is provided.")
                    return
            -- else we suppose that we have run(connection[, options][, callback])

        options = {} unless options

        -- Check if the arguments are valid types
        for key in options
            switch key
                when 'useOutdated', 'noreply', 'timeFormat', 'profile', 'durability', 'groupFormat', 'binaryFormat', 'batchConf', 'arrayLimit'
                    nil
                else
                    callback new err.RqlDriverError "Found "+key+" which is not a valid option. valid options are {useOutdated: <bool>, noreply: <bool>, timeFormat: <string>, groupFormat: <string>, binaryFormat: <string>, profile: <bool>, durability: <string>, arrayLimit: <number>}."
        if net.isConnection(connection) is false
            callback new err.RqlDriverError "First argument to `run` must be an open connection."

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
    eq: (...) -> new Eq {}, @, unpack arg
    ne: (...) -> new Ne {}, @, unpack arg
    lt: (...) -> new Lt {}, @, unpack arg
    le: (...) -> new Le {}, @, unpack arg
    gt: (...) -> new Gt {}, @, unpack arg
    ge: (...) -> new Ge {}, @, unpack arg

    not: (...) -> new Not {}, @, unpack arg

    add: (...) -> new Add {}, @, unpack arg
    sub: (...) -> new Sub {}, @, unpack arg
    mul: (...) -> new Mul {}, @, unpack arg
    div: (...) -> new Div {}, @, unpack arg
    mod: (...) -> new Mod {}, @, unpack arg

    append: (...) -> new Append {}, @, unpack arg
    prepend: (...) -> new Prepend {}, @, unpack arg
    difference: (...) -> new Difference {}, @, unpack arg
    setInsert: (...) -> new SetInsert {}, @, unpack arg
    setUnion: (...) -> new SetUnion {}, @, unpack arg
    setIntersection: (...) -> new SetIntersection {}, @, unpack arg
    setDifference: (...) -> new SetDifference {}, @, unpack arg
    slice: varar(1, 3, (left, right_or_opts, opts) ->
        if opts
            Slice opts, @, left, right_or_opts
        else if right_or_opts
            -- FIXME
            if (Object::toString.call(right_or_opts) == '[object Object]') and not (right_or_opts.instanceof(TermBase)
                Slice right_or_opts, @, left
            else
                new Slice {}, @, left, right_or_opts
        else
            new Slice {}, @, left
        )
    skip: (...) -> new Skip {}, @, unpack arg
    limit: (...) -> new Limit {}, @, unpack arg
    getField: (...) -> new GetField {}, @, unpack arg
    contains: (...) -> new Contains {}, @, unpack arg
    insertAt: (...) -> new InsertAt {}, @, unpack arg
    spliceAt: (...) -> new SpliceAt {}, @, unpack arg
    deleteAt: (...) -> new DeleteAt {}, @, unpack arg
    changeAt: (...) -> new ChangeAt {}, @, unpack arg
    indexesOf: (...) -> new IndexesOf {}, @, unpack arg
    hasFields: (...) -> new HasFields {}, @, unpack arg
    withFields: (...) -> new WithFields {}, @, unpack arg
    keys: (...) -> new Keys {}, @, unpack arg
    changes: (...) -> new Changes {}, @, unpack arg

    -- pluck and without on zero fields are allowed
    pluck: (...) -> new Pluck {}, @, unpack arg
    without: (...) -> new Without {}, @, unpack arg

    merge: (...) -> new Merge {}, @, unpack arg
    between: aropt (left, right, opts) -> new Between opts, @, left, right
    reduce: (...) -> new Reduce {}, @, unpack arg
    map: (...) -> new Map {}, @, unpack arg
    filter: aropt (predicate, opts) -> new Filter opts, @, funcWrap(predicate)
    concatMap: (...) -> new ConcatMap {}, @, unpack arg
    distinct: aropt (opts) -> new Distinct opts, @
    count: (...) -> new Count {}, @, unpack arg
    union: (...) -> new Union {}, @, unpack arg
    nth: (...) -> new Nth {}, @, unpack arg
    bracket: (...) -> new Bracket {}, @, unpack arg
    match: (...) -> new Match {}, @, unpack arg
    split: (...) -> new Split {}, @, unpack arg
    upcase: (...) -> new Upcase {}, @, unpack arg
    downcase: (...) -> new Downcase {}, @, unpack arg
    isEmpty: (...) -> new IsEmpty {}, @, unpack arg
    innerJoin: (...) -> new InnerJoin {}, @, unpack arg
    outerJoin: (...) -> new OuterJoin {}, @, unpack arg
    eqJoin: aropt (left_attr, right, opts) -> new EqJoin opts, @, funcWrap(left_attr), right
    zip: (...) -> new Zip {}, @, unpack arg
    coerceTo: (...) -> new CoerceTo {}, @, unpack arg
    ungroup: (...) -> new Ungroup {}, @, unpack arg
    typeOf: (...) -> new TypeOf {}, @, unpack arg
    update: aropt (func, opts) -> new Update opts, @, funcWrap(func)
    delete: aropt (opts) -> new Delete opts, @
    replace: aropt (func, opts) -> new Replace opts, @, funcWrap(func)
    do: (...) ->
        new FunCall {}, funcWrap(arg[arg.n]), @, unpack args[,arg.n-1]

    default: (...) -> new Default {}, @, unpack arg

    or: (...) -> new Any {}, @, unpack arg
    any: (...) -> new Any {}, @, unpack arg
    and: (...) -> new All {}, @, unpack arg
    all: (...) -> new All {}, @, unpack arg

    forEach: (...) -> new ForEach {}, @, unpack arg

    sum: (...) -> new Sum {}, @, unpack arg
    avg: (...) -> new Avg {}, @, unpack arg
    min: (...) -> new Min {}, @, unpack arg
    max: (...) -> new Max {}, @, unpack arg

    info: (...) -> new Info {}, @, unpack arg
    sample: (...) -> new Sample {}, @, unpack arg

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

        new Group opts, @, unpack fields

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

        new OrderBy opts, @, unpack attrs

    -- Geo operations
    toGeojson: (...) -> new ToGeojson {}, @, unpack arg
    distance: aropt (g, opts) -> new Distance opts, @, g
    intersects: (...) -> new Intersects {}, @, unpack arg
    includes: (...) -> new Includes {}, @, unpack arg
    fill: (...) -> new Fill {}, @, unpack arg

    -- Database operations

    tableCreate: aropt (tblName, opts) -> new TableCreate opts, @, tblName
    tableDrop: (...) -> new TableDrop {}, @, unpack arg
    tableList: (...) -> new TableList {}, @, unpack arg

    table: aropt (tblName, opts) -> new Table opts, @, tblName

    -- Table operations

    get: (...) -> new Get {}, @, unpack arg

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

        new GetAll opts, @, unpack keys

    insert: aropt (doc, opts) -> new Insert opts, @, rethinkdb.expr(doc)
    indexCreate: varar(1, 3, (name, defun_or_opts, opts) ->
        if opts?
            new IndexCreate opts, @, name, funcWrap(defun_or_opts)
        else if defun_or_opts?
            -- FIXME?
            if (Object::toString.call(defun_or_opts) is '[object Object]') and not (defun_or_opts instanceof Function) and not (defun_or_opts instanceof TermBase)
                new IndexCreate defun_or_opts, @, name
            else
                new IndexCreate {}, @, name, funcWrap(defun_or_opts)
        else
            new IndexCreate {}, @, name
        )

    indexDrop: (...) -> new IndexDrop {}, @, unpack arg
    indexList: (...) -> new IndexList {}, @, unpack arg
    indexStatus: (...) -> new IndexStatus {}, @, unpack arg
    indexWait: (...) -> new IndexWait {}, @, unpack arg
    indexRename: aropt (old_name, new_name, opts) -> new IndexRename opts, @, old_name, new_name

    sync: (...) -> new Sync {}, @, unpack arg

    toISO8601: (...) -> new ToISO8601 {}, @, unpack arg
    toEpochTime: (...) -> new ToEpochTime {}, @, unpack arg
    inTimezone: (...) -> new InTimezone {}, @, unpack arg
    during: aropt (t2, t3, opts) -> new During opts, @, t2, t3
    date: (...) -> new RQLDate {}, @, unpack arg
    timeOfDay: (...) -> new TimeOfDay {}, @, unpack arg
    timezone: (...) -> new Timezone {}, @, unpack arg

    year: (...) -> new Year {}, @, unpack arg
    month: (...) -> new Month {}, @, unpack arg
    day: (...) -> new Day {}, @, unpack arg
    dayOfWeek: (...) -> new DayOfWeek {}, @, unpack arg
    dayOfYear: (...) -> new DayOfYear {}, @, unpack arg
    hours: (...) -> new Hours {}, @, unpack arg
    minutes: (...) -> new Minutes {}, @, unpack arg
    seconds: (...) -> new Seconds {}, @, unpack arg

    uuid: (...) -> new UUID {}, @, unpack arg

    getIntersecting: aropt (g, opts) -> new GetIntersecting opts, @, g
    getNearest: aropt (g, opts) -> new GetNearest opts, @, g

class DatumTerm extends RDBVal
    args: []
    optargs: {}

    constructor: (val) ->
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
            if !isFinite(@data)
                error(new TypeError("Illegal non-finite number `" + @data.toString() + "`."))
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
    constructor: (optargs, ...) ->
        self = super()
        self.args =
            for a,i in arg
                if a
                    rethinkdb.expr a
                else
                    error(new err.RqlDriverError "Argument #{i} to #{@st || @mt} may not be `undefined`.")
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
    constructor: (optargs, unpack arg) ->
        self = super()
        self.args =
            for arg,i in args
                if arg
                    rethinkdb.expr funcWrap arg
                else
                    error(new err.RqlDriverError "Argument #{i} to #{@st || @mt} may not be `undefined`.")
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

    constructor: (obj, nestingDepth=20) ->
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

    constructor: (data) ->
        if data instanceof TermBase
            self = super({}, data)
        else if data instanceof Buffer
            self = super()
            self.base64_data = data.toString("base64")
        else
            error(new TypeError("Parameter to `r.binary` must be a Buffer object or RQL query.")

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

    constructor: (optargs, func) ->
        args = []
        argNums = []
        i = 0
        while i < func.length
            argNums.push Func.nextVarId
            args.push new Var {}, Func.nextVarId
            Func.nextVarId++
            i++

        body = func(unpack args)
        if body is undefined
            error(new err.RqlDriverError "Anonymous function returned `undefined`. Did you forget a `return`?")

        argsArr = new MakeArray({}, unpack argNums)
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
        error(new err.RqlDriverError "Nesting depth limit exceeded")

    if typeof nestingDepth isnt "number" or isNaN(nestingDepth)
        error(new err.RqlDriverError "Second argument to `r.expr` must be a number or undefined.")

    else if val instanceof TermBase
        val
    else if val instanceof Function
        new Func {}, val
    else if val instanceof Date
        new ISO8601 {}, val.toISOString()
    else if val instanceof Buffer
        new Binary val
    else if Array.isArray val
        val = (rethinkdb.expr(v, nestingDepth - 1) for v in val)
        MakeArray {}, unpack val
    else if typeof(val) == 'number'
        DatumTerm val
    else if Object::toString.call(val) == '[object Object]'
        MakeObject val, nestingDepth
    else
        new DatumTerm val

rethinkdb.js = aropt (jssrc, opts) -> new JavaScript opts, jssrc

rethinkdb.http = aropt (url, opts) -> new Http opts, url

rethinkdb.json = (...) -> new Json {}, unpack arg

rethinkdb.error = (...) -> new UserError {}, unpack arg

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

        new Random opts, unpack limits

rethinkdb.binary = ar (data) -> new Binary data

rethinkdb.row = new ImplicitVar {}

rethinkdb.table = aropt (tblName, opts) -> new Table opts, tblName

rethinkdb.db = (...) -> new Db {}, unpack arg

rethinkdb.dbCreate = (...) -> new DbCreate {}, unpack arg
rethinkdb.dbDrop = (...) -> new DbDrop {}, unpack arg
rethinkdb.dbList = (...) -> new DbList {}, unpack arg

rethinkdb.tableCreate = aropt (tblName, opts) -> new TableCreate opts, tblName
rethinkdb.tableDrop = (...) -> new TableDrop {}, unpack arg
rethinkdb.tableList = (...) -> new TableList {}, unpack arg

rethinkdb.do = varar 1, nil, (...) ->
    new FunCall {}, funcWrap(arg[arg.n]), unpack *arg[,arg.n - 1]

rethinkdb.branch = (...) -> new Branch {}, unpack arg

rethinkdb.asc = (...) -> new Asc {}, unpack arg
rethinkdb.desc = (...) -> new Desc {}, unpack arg

rethinkdb.eq = (...) -> new Eq {}, unpack arg
rethinkdb.ne = (...) -> new Ne {}, unpack arg
rethinkdb.lt = (...) -> new Lt {}, unpack arg
rethinkdb.le = (...) -> new Le {}, unpack arg
rethinkdb.gt = (...) -> new Gt {}, unpack arg
rethinkdb.ge = (...) -> new Ge {}, unpack arg
rethinkdb.or = (...) -> new Any {}, unpack arg
rethinkdb.any = (...) -> new Any {}, unpack arg
rethinkdb.and = (...) -> new All {}, unpack arg
rethinkdb.all = (...) -> new All {}, unpack arg

rethinkdb.not = (...) -> new Not {}, unpack arg

rethinkdb.add = (...) -> new Add {}, unpack arg
rethinkdb.sub = (...) -> new Sub {}, unpack arg
rethinkdb.div = (...) -> new Div {}, unpack arg
rethinkdb.mul = (...) -> new Mul {}, unpack arg
rethinkdb.mod = (...) -> new Mod {}, unpack arg

rethinkdb.typeOf = (...) -> new TypeOf {}, unpack arg
rethinkdb.info = (...) -> new Info {}, unpack arg

rethinkdb.literal = (...) -> new Literal {}, unpack arg

rethinkdb.ISO8601 = aropt (str, opts) -> new ISO8601 opts, str
rethinkdb.epochTime = (...) -> new EpochTime {}, unpack arg
rethinkdb.now = (...) -> new Now {}, unpack arg
rethinkdb.time = (...) -> new Time {}, unpack arg

rethinkdb.monday = new (class extends RDBOp then tt: protoTermType.MONDAY)()
rethinkdb.tuesday = new (class extends RDBOp then tt: protoTermType.TUESDAY)()
rethinkdb.wednesday = new (class extends RDBOp then tt: protoTermType.WEDNESDAY)()
rethinkdb.thursday = new (class extends RDBOp then tt: protoTermType.THURSDAY)()
rethinkdb.friday = new (class extends RDBOp then tt: protoTermType.FRIDAY)()
rethinkdb.saturday = new (class extends RDBOp then tt: protoTermType.SATURDAY)()
rethinkdb.sunday = new (class extends RDBOp then tt: protoTermType.SUNDAY)()

rethinkdb.january = new (class extends RDBOp then tt: protoTermType.JANUARY)()
rethinkdb.february = new (class extends RDBOp then tt: protoTermType.FEBRUARY)()
rethinkdb.march = new (class extends RDBOp then tt: protoTermType.MARCH)()
rethinkdb.april = new (class extends RDBOp then tt: protoTermType.APRIL)()
rethinkdb.may = new (class extends RDBOp then tt: protoTermType.MAY)()
rethinkdb.june = new (class extends RDBOp then tt: protoTermType.JUNE)()
rethinkdb.july = new (class extends RDBOp then tt: protoTermType.JULY)()
rethinkdb.august = new (class extends RDBOp then tt: protoTermType.AUGUST)()
rethinkdb.september = new (class extends RDBOp then tt: protoTermType.SEPTEMBER)()
rethinkdb.october = new (class extends RDBOp then tt: protoTermType.OCTOBER)()
rethinkdb.november = new (class extends RDBOp then tt: protoTermType.NOVEMBER)()
rethinkdb.december = new (class extends RDBOp then tt: protoTermType.DECEMBER)()

rethinkdb.object = (...) -> new Object_ {}, unpack arg

rethinkdb.args = (...) -> new Args {}, unpack arg

rethinkdb.geojson = (...) -> new Geojson {}, unpack arg
rethinkdb.point = (...) -> new Point {}, unpack arg
rethinkdb.line = (...) -> new Line {}, unpack arg
rethinkdb.polygon = (...) -> new Polygon {}, unpack arg
rethinkdb.intersects = (...) -> new Intersects {}, unpack arg
rethinkdb.distance = aropt (g1, g2, opts) -> new Distance opts, g1, g2
rethinkdb.circle = aropt (cen, rad, opts) -> new Circle opts, cen, rad

rethinkdb.uuid = (...) -> new UUID {}, unpack arg

-- Export all names defined on rethinkdb
module.exports = rethinkdb
