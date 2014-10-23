describe('control', function()
  local r, reql_db, reql_table, c

  setup(function()
    r = require('rethinkdb')

    reql_db = 'control'
    reql_table = 'func'

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

  it('branch db', function()
    assert.has_error(
      function()
        r.db(reql_db):branch(1, 2):run(
          c, function(err, cur)
            if err then error(err.message) end
            cur:to_array(function(err, arr)
              if err then error(err.msg) end
              error('no error thrown')
            end)
          end
        )
      end, 'Expected type DATUM but found DATABASE:'
    )
  end)

  it('branch error', function()
    assert.has_error(
      function()
        r.branch(r.error_("a"), 1, 2):run(
          c, function(err, cur)
            if err then error(err.message) end
            cur:to_array(function(err, arr)
              if err then error(err.message) end
              error('no error thrown')
            end)
          end
        )
      end, 'a'
    )
  end)

  it('branch false', function()
    assert.same(
      r.branch(false, 1, 2):run(
        c, function(err, cur)
          if err then error(err.message) end
          return cur:to_array(function(err, arr)
            if err then error(err.message) end
            return arr
          end)
        end
      ), {2}
    )
  end)

  it('branch nil', function()
    assert.same(
      r():branch(1, 2):run(
        c, function(err, cur)
          if err then error(err.message) end
          return cur:to_array(function(err, arr)
            if err then error(err.message) end
            return arr
          end)
        end
      ), {2}
    )
  end)

  it('branch num', function()
    assert.same(
      r.branch(1, 'c', false):run(
        c, function(err, cur)
          if err then error(err.message) end
          return cur:to_array(function(err, arr)
            if err then error(err.message) end
            return arr
          end)
        end
      ), {'c'}
    )
  end)

  it('branch table', function()
    assert.has_error(
      function()
        r.table(reql_table):branch(1, 2):run(
          c, function(err, cur)
            if err then error(err.message) end
            cur:to_array(function(err, arr)
              if err then error(err.msg) end
              error('no error thrown')
            end)
          end
        )
      end, 'Expected type DATUM but found TABLE:'
    )
  end)

  it('branch true', function()
    assert.same(
      r.branch(true, 1, 2):run(
        c, function(err, cur)
          if err then error(err.message) end
          return cur:to_array(function(err, arr)
            if err then error(err.message) end
            return arr
          end)
        end
      ), {1}
    )
  end)

  it('do', function()
    assert.same(
      r.do_(function() return 1 end):run(
        c, function(err, cur)
          if err then error(err.message) end
          return cur:to_array(function(err, arr)
            if err then error(err.message) end
            return arr
          end)
        end
      ), {1}
    )
  end)

  it('do add', function()
    assert.same(
      r.do_(1, 2, function(x, y) return x:add(y) end):run(
        c, function(err, cur)
          if err then error(err.message) end
          return cur:to_array(function(err, arr)
            if err then error(err.message) end
            return arr
          end)
        end
      ), {3}
    )
  end)

  it('do append', function()
    assert.same(
      r({0, 1, 2}):do_(function(v) return v:append(3) end):run(
        c, function(err, cur)
          if err then error(err.message) end
          return cur:to_array(function(err, arr)
            if err then error(err.message) end
            return arr
          end)
        end
      ), {{0, 1, 2, 3}}
    )
  end)

  if string.match(_VERSION, '5.[23]') then
    it('do extra arg', function()
      assert.has_error(
        function()
          r.do_(1, function(x, y) return x + y end):run(
            c, function(err, cur)
              if err then error(err.message) end
              cur:to_array(function(err, arr)
                if err then error(err.msg) end
                error('no error thrown')
              end)
            end
          )
        end, 'Expected 2 arguments but found 1.'
      )
    end)

    it('do missing arg', function()
      assert.has_error(
        function()
          r.do_(1, 2, function(x) return x end):run(
            c, function(err, cur)
              if err then error(err.message) end
              cur:to_array(function(err, arr)
                if err then error(err.msg) end
                error('no error thrown')
              end)
            end
          )
        end, 'Expected 1 argument but found 2.'
      )
    end)
  end

  it('do mul', function()
    assert.same(
      r(1):do_(function(v) return v:mul(2) end):run(
        c, function(err, cur)
          if err then error(err.message) end
          return cur:to_array(function(err, arr)
            if err then error(err.message) end
            return arr
          end)
        end
      ), {2}
    )
  end)

  it('do no args', function()
    assert.has_error(
      function()
        r.do_():run(
          c, function(err, cur)
            if err then error(err.message) end
            cur:to_array(function(err, arr)
              if err then error(err.msg) end
              error('no error thrown')
            end)
          end
        )
      end, 'Expected 1 or more arguments but found 0.'
    )
  end)

  it('do no func', function()
    assert.same(
      r.do_(1):run(
        c, function(err, cur)
          if err then error(err.message) end
          cur:to_array(function(err, arr)
            if err then error(err.msg) end
            error('no error thrown')
          end)
        end
      ), {1}
    )
  end)

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

  it('do str add num', function()
    assert.has_error(
      function()
        r('abc'):do_(function(v) return v:add(3) end):run(
          c, function(err, cur)
            if err then error(err.message) end
            cur:to_array(function(err, arr)
              if err then error(err.msg) end
              error('no error thrown')
            end)
          end
        )
      end, 'Expected type STRING but found NUMBER.'
    )
  end)

  it('do str add str add num', function()
    assert.has_error(
      function()
        r('abc'):do_(function(v) return v:add('def') end):add(3):run(
          c, function(err, cur)
            if err then error(err.message) end
            cur:to_array(function(err, arr)
              if err then error(err.msg) end
              error('no error thrown')
            end)
          end
        )
      end, 'Expected type STRING but found NUMBER.'
    )
  end)

  it('do str append', function()
    assert.has_error(
      function()
        r('abc'):do_(function(v) return v:append(3) end):run(
          c, function(err, cur)
            if err then error(err.message) end
            cur:to_array(function(err, arr)
              if err then error(err.msg) end
              error('no error thrown')
            end)
          end
        )
      end, 'Expected type ARRAY but found STRING.'
    )
  end)

  it('error', function()
    assert.has_error(
      function()
        r.error_('Hello World'):run(
          c, function(err, cur)
            if err then error(err.message) end
            cur:to_array(function(err, arr)
              if err then error(err.msg) end
              error('no error thrown')
            end)
          end
        )
      end, 'Hello World'
    )
  end)
end)


--[[
    # r.js()
    - cd: r.js('1 + 1')
      ot: 2

    - cd: r.js('1 + 1; 2 + 2')
      ot: 4

    - cd: r.do(1, 2, r.js('(function(a, b) { return a + b; })'))
      ot: 3

    - cd: r.expr(1).do(r.js('(function(x) { return x + 1; })'))
      ot: 2

    - cd: r.expr('foo').do(r.js('(function(x) { return x + "bar"; })'))
      ot: "'foobar'"

    # js timeout optarg shouldn't be triggered
    - py: r.js('1 + 2', timeout=1.2)
      js: r.js('1 + 2', {timeout:1.2})
      ot: 3

    # js error cases
    - cd: r.js('(function() { return 1; })')
      ot: err("RqlRuntimeError", "Query result must be of type DATUM, GROUPED_DATA, or STREAM (got FUNCTION).", [0])

    - cd: r.js('function() { return 1; }')
      ot: 'err("RqlRuntimeError", "SyntaxError: Unexpected token (", [0])'

    # Play with the number of arguments in the JS function
    - cd: r.do(1, 2, r.js('(function(a) { return a; })'))
      ot: 1

    - cd: r.do(1, 2, r.js('(function(a, b, c) { return a; })'))
      ot: 1

    - cd: r.do(1, 2, r.js('(function(a, b, c) { return c; })'))
      ot: err("RqlRuntimeError", "Cannot convert javascript `undefined` to ql::datum_t.", [0])

    - cd: r.expr([1, 2, 3]).filter(r.js('(function(a) { return a >= 2; })'))
      ot: ([2, 3])

    - cd: r.expr([1, 2, 3]).map(r.js('(function(a) { return a + 1; })'))
      ot: ([2, 3, 4])

    - cd: r.expr([1, 2, 3]).map(r.js('1'))
      ot: err("RqlRuntimeError", "Expected type FUNCTION but found DATUM.", [0])

    - cd: r.expr([1, 2, 3]).filter(r.js('(function(a) {})'))
      ot: err("RqlRuntimeError", "Cannot convert javascript `undefined` to ql::datum_t.", [0])

    # What happens if we pass static values to things that expect functions
    - cd: r.expr([1, 2, 3]).map(1)
      ot: err("RqlRuntimeError", "Expected type FUNCTION but found DATUM.", [0])

    - cd: r.expr([1, 2, 3]).filter('foo')
      ot: ([1, 2, 3])
    - cd: r.expr([1, 2, 4]).filter([])
      ot: ([1, 2, 4])
    - cd: r.expr([1, 2, 3]).filter(null)
      ot: ([])

    - py: r.expr([1, 2, 4]).filter(False)
      js: r.expr([1, 2, 4]).filter(false)
      rb: r([1, 2, 4]).filter(false)
      ot: ([])

    # forEach
    - cd: tbl.count()
      ot: 0

    # Insert three elements
    - js: r([1, 2, 3]).forEach(function (row) { return tbl.insert({ id:row }) })
      py: r.expr([1, 2, 3]).for_each(lambda row:tbl.insert({ 'id':row }))
      rb: r([1, 2, 3]).for_each{ |row| tbl.insert({ :id => row }) }
      ot: ({'deleted':0.0,'replaced':0.0,'unchanged':0.0,'errors':0.0,'skipped':0.0,'inserted':3})

    - cd: tbl.count()
      ot: 3

    # Update each row to add additional attribute
    - js: r([1, 2, 3]).forEach(function (row) { return tbl.update({ foo:row }) })
      py: r.expr([1,2,3]).for_each(lambda row:tbl.update({'foo':row}))
      rb: r.expr([1,2,3]).for_each{ |row| tbl.update({ :foo => row }) }
      ot: ({'deleted':0.0,'replaced':9,'unchanged':0.0,'errors':0.0,'skipped':0.0,'inserted':0.0})

    # Insert three more elements (and error on three)
    - js: r([1, 2, 3]).forEach(function (row) { return [tbl.insert({ id:row }), tbl.insert({ id:row.mul(10) })] })
      py: r.expr([1,2,3]).for_each(lambda row:[tbl.insert({ 'id':row }), tbl.insert({ 'id':row*10 })])
      rb: r.expr([1,2,3]).for_each{ |row| [tbl.insert({ :id => row}), tbl.insert({ :id => row*10})] }
      ot: ({'first_error':"Duplicate primary key `id`:\n{\n\t\"foo\":\t3,\n\t\"id\":\t1\n}\n{\n\t\"id\":\t1\n}",'deleted':0.0,'replaced':0.0,'unchanged':0.0,'errors':3,'skipped':0.0,'inserted':3})

    - cd: tbl.count()
      ot: 6

    - cd: r.expr([1, 2, 3]).for_each( tbl2.insert({}) )
      ot: ({'deleted':0.0,'replaced':0.0,'generated_keys':arrlen(3,uuid()),'unchanged':0.0,'errors':0.0,'skipped':0.0,'inserted':3})

    # We have six elements, update them 6*2*3=36 times
    - js: r([1, 2, 3]).forEach(function (row) { return [tbl.update({ foo:row }), tbl.update({ bar:row })] })
      py: r.expr([1,2,3]).for_each(lambda row:[tbl.update({'foo':row}), tbl.update({'bar':row})])
      rb: r.expr([1,2,3]).for_each{ |row| [tbl.update({:foo => row}), tbl.update({:bar => row})]}
      ot: ({'deleted':0.0,'replaced':36,'unchanged':0.0,'errors':0.0,'skipped':0.0,'inserted':0.0})

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

      # This is only relevant in JS -- what happens when we return undefined
    - js: "r([1, 2, 3]).forEach(function (row) {})"
      cd: []
      ot: err("RqlDriverError", 'Anonymous function returned `undefined`. Did you forget a `return`?', [1])

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

    # Cleanup
    - cd: r.db('test').table_drop('test2')
      ot: ({'dropped':1})

    - cd: r.db('test').table_drop('nested_table')
      ot: ({'dropped':1})
--]]
