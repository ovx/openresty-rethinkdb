local r = require('rethinkdb')

describe('control', function()
  local reql_db, reql_table, c

  reql_table = 'func'

  setup(function()
    reql_db = 'control'

    local err

    c, err = r.connect()
    if err then error(err.message) end

    r.db_create(reql_db):run(c)
    c:use(reql_db)
    r.table_create(reql_table):run(c)
  end)

  after_each(function()
    r.table(reql_table):delete():run(c)
  end)

  function test(name, query, res)
    it(name, function()
      assert.same(res, query:run(
        c, function(err, cur)
          if err then error(err.message) end
          return cur:to_array(function(err, arr)
            if err then error(err.message) end
            return arr
          end)
        end
      ))
    end)
  end

  function test_error(name, query, res)
    it(name, function()
      assert.has_error(
        function()
          query:run(
            c, function(err, cur)
              if err then error(err.message) end
              cur:to_array(function(err, arr)
                if err then error(err.msg) end
                error(arr)
              end)
            end
          )
        end, res
      )
    end)
  end

--[[
  test_error('branch db', r.db(reql_db):branch(1, 2), 'Expected type DATUM but found DATABASE:')
--]]
  test_error('branch error', r.branch(r.error_('a'), 1, 2), 'a')
  test('branch false', r.branch(false, 1, 2), {2})
  test('branch nil', r():branch(1, 2), {2})
  test('branch num', r.branch(1, 'c', false), {'c'})
--[[
  test_error('branch table', r.table(reql_table):branch(1, 2), 'Expected type DATUM but found TABLE:')
--]]
  test('branch true', r.branch(true, 1, 2), {1})
  test('do', r.do_(function() return 1 end), {1})
  test('do add', r.do_(1, 2, function(x, y) return x:add(y) end), {3})
  test('do append', r({0, 1, 2}):do_(function(v) return v:append(3) end), {{0, 1, 2, 3}})

  if string.match(_VERSION, '5.[23]') then
    test_error('do extra arg', r.do_(1, function(x, y) return x + y end), 'Expected 2 arguments but found 1.')
    test_error('do missing arg', r.do_(1, 2, function(x) return x end), 'Expected 1 argument but found 2.')
  end

  test('do mul', r(1):do_(function(v) return v:mul(2) end), {2})
  test_error('do no args', r.do_(), 'Expected 1 or more arguments but found 0.')
  test('do no func', r.do_(1), {1})

  it('do no return', function()
    assert.has_error(
      function()
        r.do_(1, function(x) end)
      end, 'Anonymous function returned `nil`. Did you forget a `return`?'
    )
  end)

  it('do return nil', function()
    assert.has_error(
      function()
        r.do_(1, function(x) return nil end)
      end, 'Anonymous function returned `nil`. Did you forget a `return`?'
    )
  end)

  test_error('do str add num', r('abc'):do_(function(v) return v:add(3) end), 'Expected type STRING but found NUMBER.')
  test_error('do str add str add num', r('abc'):do_(function(v) return v:add('def') end):add(3), 'Expected type STRING but found NUMBER.')
  test_error('do str append', r('abc'):do_(function(v) return v:append(3) end), 'Expected type ARRAY but found STRING.')
  test_error('error', r.error_('Hello World'), 'Hello World')
  test('js', r.js('1 + 1'), {2})
  test('js add add', r.js('1 + 1; 2 + 2'), {4})
  test('do js function add', r.do_(1, 2, r.js('(function(a, b) { return a + b; })')), {3})
  test('do js function', r.do_(1, r.js('(function(x) { return x + 1; })')), {2})
  test('do js function add str', r.do_('foo', r.js('(function(x) { return x + "bar"; })')), {'foobar'})
  test('do js no timeout', r.js('1 + 2', {timeout = 1.2}), {3})
  test_error('js function result', r.js('(function() { return 1; })'), 'Query result must be of type DATUM, GROUPED_DATA, or STREAM (got FUNCTION).')
  test_error('js function no wrap', r.js('function() { return 1; }'), 'SyntaxError: Unexpected token (')
  test('do js function missing arg', r.do_(1, 2, r.js('(function(a) { return a; })')), {1})
  test('do js function extra arg', r.do_(1, 2, r.js('(function(a, b, c) { return a; })')), {1})
  test_error('do js function return undefined', r.do_(1, 2, r.js('(function(a, b, c) { return c; })')), 'Cannot convert javascript `undefined` to ql::datum_t.')
  test('filter js', r.filter({1, 2, 3}, r.js('(function(a) { return a >= 2; })')), {{2, 3}})
  test('map js', r.map({1, 2, 3}, r.js('(function(a) { return a + 1; })')), {{2, 3, 4}})
--[[
  test_error('map js constant', r.map({1, 2, 3}, r.js('1')), 'Expected type FUNCTION but found DATUM.')
--]]
  test_error('filter js undefined', r.filter({1, 2, 3}, r.js('(function(a) {})')), 'Cannot convert javascript `undefined` to ql::datum_t.')
--[[
  test_error('map constant', r.map({1, 2, 3}, 1), 'Expected type FUNCTION but found DATUM.')
--]]
  test('filter constant str', r.filter({1, 2, 3}, 'foo'), {{1, 2, 3}})
  test('filter constant obj', r.filter({1, 2, 3}, {}), {{1, 2, 3}})
  test('filter nil', r.filter({1, 2, 3}, r()), {{}})
  test('filter false', r.filter({1, 2, 3}, false), {{}})
  test('for each insert', r.for_each({1, 2, 3}, function(row) return r.table(reql_table):insert({id = row}) end), {{deleted = 0, replaced = 0, unchanged = 0, errors = 0, skipped = 0, inserted = 3}})

  it('count for each insert', function()
    r.for_each({1, 2, 3}, function(row) return r.table(reql_table):insert({id = row}) end):run(
      c, function(err, cur)
        if err then error(err.message) end
        cur:to_array(function(err, arr)
          if err then error(err.message) end
        end)
      end
    )
    assert.same(r.table(reql_table):count():run(
      c, function(err, cur)
        if err then error(err.message) end
        return cur:to_array(function(err, arr)
          if err then error(err.message) end
          return arr
        end)
      end
    ), {3})
  end)

  it('for each update', function()
    r.for_each({1, 2, 3}, function(row) return r.table(reql_table):insert({id = row}) end):run(
      c, function(err, cur)
        if err then error(err.message) end
        cur:to_array(function(err, arr)
          if err then error(err.message) end
        end)
      end
    )
    assert.same(r.for_each({1, 2, 3}, function(row) return r.table(reql_table):update({foo = row}) end):run(
      c, function(err, cur)
        if err then error(err.message) end
        return cur:to_array(function(err, arr)
          if err then error(err.message) end
          return arr
        end)
      end
    ), {{deleted = 0, replaced = 9, unchanged = 0, errors = 0, skipped = 0, inserted = 0}})
  end)

  it('for each insert with duplicates', function()
    r.for_each({1, 2, 3}, function(row) return r.table(reql_table):insert({id = row}) end):run(
      c, function(err, cur)
        if err then error(err.message) end
        cur:to_array(function(err, arr)
          if err then error(err.message) end
        end)
      end
    )
    r.for_each({1, 2, 3}, function(row) return r.table(reql_table):update({foo = row}) end):run(
      c, function(err, cur)
        if err then error(err.message) end
        return cur:to_array(function(err, arr)
          if err then error(err.message) end
          return arr
        end)
      end
    )
    assert.same(r.for_each({1, 2, 3}, function(row) return {r.table(reql_table):insert({id = row}), r.table(reql_table):insert({id = row * 10})} end):run(
      c, function(err, cur)
        if err then error(err.message) end
        return cur:to_array(function(err, arr)
          if err then error(err.message) end
          return arr
        end)
      end
    ), {{first_error = 'Duplicate primary key `id`:\n{\n\t"foo":\t3,\n\t"id":\t1\n}\n{\n\t"id":\t1\n}', deleted = 0, replaced = 0, unchanged = 0, errors = 3, skipped = 0, inserted = 3}})
  end)

  it('for each update many', function()
    r.for_each({1, 2, 3}, function(row) return r.table(reql_table):insert({id = row}) end):run(
      c, function(err, cur)
        if err then error(err.message) end
        cur:to_array(function(err, arr)
          if err then error(err.message) end
        end)
      end
    )
    r.for_each({1, 2, 3}, function(row) return r.table(reql_table):update({foo = row}) end):run(
      c, function(err, cur)
        if err then error(err.message) end
        return cur:to_array(function(err, arr)
          if err then error(err.message) end
          return arr
        end)
      end
    )
    r.for_each({1, 2, 3}, function(row) return {r.table(reql_table):insert({id = row}), r.table(reql_table):insert({id = row * 10})} end):run(
      c, function(err, cur)
        if err then error(err.message) end
        return cur:to_array(function(err, arr)
          if err then error(err.message) end
          return arr
        end)
      end
    )
    assert.same(r.for_each({1, 2, 3}, function(row) return {r.table(reql_table):update({foo = row}), r.table(reql_table):update({bar = row})} end):run(
      c, function(err, cur)
        if err then error(err.message) end
        return cur:to_array(function(err, arr)
          if err then error(err.message) end
          return arr
        end)
      end
    ), {{deleted = 0, replaced = 36, unchanged = 0, errors = 0, skipped = 0, inserted = 0}})
  end)
end)


--[[
    # forEach negative cases
    - cd: r.expr([1, 2, 3]).for_each( tbl2.insert({ 'id':r.row }) )
      rb: []
      ot: ({'deleted':0.0,'replaced':0.0,'unchanged':0.0,'errors':0.0,'skipped':0.0,'inserted':3})

    - cd: r.expr([1, 2, 3]).for_each(1)
      ot: 'err("RqlRuntimeError", "FOREACH expects one or more basic write queries.  Expected type ARRAY but found NUMBER.", [0])'

    - py: r.expr([1, 2, 3]).for_each(lambda x:x)
      js: r([1, 2, 3]).forEach(function (x) { return x; })
      rb: r([1, 2, 3]).for_each{ |x| x }
      ot: 'err("RqlRuntimeError", "FOREACH expects one or more basic write queries.  Expected type ARRAY but found NUMBER.", [1, 1])'

    - cd: r.expr([1, 2, 3]).for_each(r.row)
      rb: []
      ot: 'err("RqlRuntimeError", "FOREACH expects one or more basic write queries.  Expected type ARRAY but found NUMBER.", [1, 1])'

    - js: r([1, 2, 3]).forEach(function (row) { return tbl; })
      py: r.expr([1, 2, 3]).for_each(lambda row:tbl)
      rb: r([1, 2, 3]).for_each{ |row| tbl }
      ot: 'err("RqlRuntimeError", "FOREACH expects one or more basic write queries.", [1, 1])'

    # Make sure write queries can't be nested into stream ops
    - cd: r.expr(1).do(tbl.insert({'foo':r.row}))
      rb: r(1).do{ |row| tbl.insert({ :foo => row }) }
      ot: ({'deleted':0.0,'replaced':0.0,'generated_keys':arrlen(1,uuid()),'unchanged':0.0,'errors':0.0,'skipped':0.0,'inserted':1})

    - py: r.expr([1, 2])[0].do(tbl.insert({'foo':r.row}))
      js: r.expr([1, 2]).nth(0).do(tbl.insert({'foo':r.row}))
      rb: r([1, 2])[0].do{ |row| tbl.insert({ :foo => row }) }
      ot: ({'deleted':0.0,'replaced':0.0,'generated_keys':arrlen(1,uuid()),'unchanged':0.0,'errors':0.0,'skipped':0.0,'inserted':1})

    - cd: r.expr([1, 2]).map(tbl.insert({'foo':r.row}))
      rb: r([1, 2]).map{ |row| tbl.insert({ :foo => row }) }
      ot: err('RqlCompileError', 'Cannot nest writes or meta ops in stream operations.  Use FOREACH instead.', [0])

    - cd: r.expr([1, 2]).map(r.db('test').table_create('nested_table'))
      ot: err('RqlCompileError', 'Cannot nest writes or meta ops in stream operations.  Use FOREACH instead.', [0])

    - cd: r.expr(1).do(r.db('test').table_create('nested_table'))
      ot: ({'created':1})
--]]
