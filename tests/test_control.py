import util


class TestControl(util.LuaTestCase):
    def setUp(self):
        self.create_table('control.func')

    def test_branch_db(self):
        self.expect(
            'test_control_branch_db',
            'ReQLRuntimeError Expected type DATUM but found DATABASE:\n'
            'db("test") in:\n'
            'r.branch(r.db("test"), 1, 2)'
        )

    def test_branch_error(self):
        self.expect(
            'test_control_branch_error',
            'ReQLRuntimeError a in:\nr.branch(r.error_("a"), 1, 2)'
        )

    def test_branch_false(self):
        self.expect('test_control_branch_false', [2])

    def test_branch_nil(self):
        self.expect('test_control_branch_nil', [2])

    def test_branch_num(self):
        self.expect('test_control_branch_num', ['c'])

    def test_branch_table(self):
        self.expect(
            'test_control_branch_table',
            'ReQLRuntimeError Expected type DATUM but found TABLE:\n'
            'table("func") in:\n'
            'r.branch(r.table("func"), 1, 2)'
        )

    def test_branch_true(self):
        self.expect('test_control_branch_true', [1])

    def test_do(self):
        self.expect('test_control_do', [1])

    def test_do_add(self):
        self.expect('test_control_do_add', [3])

    def test_do_append(self):
        self.expect('test_control_do_append', [[0, 1, 2, 3]])

    @util.unittest.skipIf(util.lua_version() < 5.2, 'requires Lua 5.2')
    def test_do_extra_arg(self):
        self.expect(
            'test_control_do_extra_arg',
            'ReQLRuntimeError Expected 2 arguments but found 1. in:\n'
            'r.do_(1, function() return r.add(var_0, var_1) end)'
        )

    @util.unittest.skipIf(util.lua_version() < 5.2, 'requires Lua 5.2')
    def test_do_missing_arg(self):
        self.expect(
            'test_control_do_missing_arg',
            'ReQLRuntimeError Expected 1 argument but found 2. in:\n'
            'r.do_(1, 2, function() return var_0 end)'
        )

    def test_do_mul(self):
        self.expect('test_control_do_mul', [2])

    def test_do_no_args(self):
        self.expect(
            'test_control_do_no_args',
            'ReQLCompileError Expected 1 or more arguments but found 0. in:\n'
            'r.do_()'
        )

    def test_do_no_func(self):
        self.expect('test_control_do_no_func', [1])

    def test_do_no_return(self):
        self.expect(
            'test_control_do_no_return',
            '../src/rethinkdb.lua:591: Anonymous function returned `nil`. Did you forget a `return`?'
        )

    def test_do_return_nil(self):
        self.expect(
            'test_control_do_return_nil',
            '../src/rethinkdb.lua:591: Anonymous function returned `nil`. Did you forget a `return`?'
        )

    def test_do_str_add_num(self):
        self.expect(
            'test_control_do_str_add_num',
            'ReQLRuntimeError Expected type STRING but found NUMBER. in:\n'
            'r.do_("abc", function() return r.add(var_0, 3) end)'
        )

    def test_do_str_add_str_add_num(self):
        self.expect(
            'test_control_do_str_add_str_add_num',
            'ReQLRuntimeError Expected type STRING but found NUMBER. in:\n'
            'r.add(r.do_("abc", function() return r.add(var_0, "def") end), 3)'
        )

    def test_do_str_append(self):
        self.expect(
            'test_control_do_str_append',
            'ReQLRuntimeError Expected type ARRAY but found STRING. in:\n'
            'r.do_("abc", function() return r.append(var_0, 3) end)'
        )

    def test_error(self):
        self.expect(
            'test_control_error',
            'ReQLRuntimeError Hello World in:\n'
            'r.error_("Hello World")'
        )


if __name__ == '__main__':
    unittest.main()
'''
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
'''
