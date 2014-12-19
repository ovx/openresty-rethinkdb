local r = require('rethinkdb')
local enable, dkjson = pcall(require, 'dkjson')

if enable then
  describe('control dkjson', function()
    local reql_db, reql_table, c

    reql_table = 'func'

    setup(function()
      reql_db = 'dkjson'

      local err

      r.json_parser = dkjson

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

    test('branch false', r.branch(false, 1, 2), {2})
    test('branch num', r.branch(1, 'c', false), {'c'})
    test('branch true', r.branch(true, 1, 2), {1})
    test('do', r.do_(function() return 1 end), {1})
    test('do add', r.do_(1, 2, function(x, y) return x:add(y) end), {3})
    test('do append', r({0, 1, 2}):do_(function(v) return v:append(3) end), {{0, 1, 2, 3}})
    test('do mul', r(1):do_(function(v) return v:mul(2) end), {2})
    test('do no func', r.do_(1), {1})
    test('js', r.js('1 + 1'), {2})
    test('js add add', r.js('1 + 1; 2 + 2'), {4})
    test('do js function add', r.do_(1, 2, r.js('(function(a, b) { return a + b; })')), {3})
    test('do js function', r.do_(1, r.js('(function(x) { return x + 1; })')), {2})
    test('do js function add str', r.do_('foo', r.js('(function(x) { return x + "bar"; })')), {'foobar'})
    test('do js no timeout', r.js('1 + 2', {timeout = 1.2}), {3})
    test('do js function missing arg', r.do_(1, 2, r.js('(function(a) { return a; })')), {1})
    test('do js function extra arg', r.do_(1, 2, r.js('(function(a, b, c) { return a; })')), {1})
    test('filter js', r.filter({1, 2, 3}, r.js('(function(a) { return a >= 2; })')), {{2, 3}})
    test('map js', r.map({1, 2, 3}, r.js('(function(a) { return a + 1; })')), {{2, 3, 4}})
    test('filter constant str', r.filter({1, 2, 3}, 'foo'), {{1, 2, 3}})
    test('filter constant obj', r.filter({1, 2, 3}, {}), {{1, 2, 3}})
    test('filter false', r.filter({1, 2, 3}, false), {{}})
    test('for each insert', r.for_each({1, 2, 3}, function(row) return r.table(reql_table):insert({id = row}) end), {{deleted = 0, replaced = 0, unchanged = 0, errors = 0, skipped = 0, inserted = 3}})
  end)
end
