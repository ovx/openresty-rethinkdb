local r = require('rethinkdb')

describe('array limits', function()
  local reql_db, reql_table, c, huge_l

  local ten_l = r({1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
  local ten_f = function() return ten_l end
  huge_l = ten_l:concat_map(ten_f):concat_map(ten_f):concat_map(
    ten_f):concat_map(ten_f)
  reql_table = 'limits'

  setup(function()
    reql_db = 'array'

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

  function test(name, query, limit, res)
    it(name, function()
      assert.same(res, query:run(
        c, {array_limit = limit}, function(err, cur)
          if err then error(err.message) end
          return cur:to_array(function(err, arr)
            if err then error(err.message) end
            return arr
          end)
        end
      ))
    end)
  end

  function test_error(name, query, limit, res)
    it(name, function()
      assert.has_error(
        function()
          query:run(
            c, {array_limit = limit}, function(err, cur)
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

  test_error('create', r({1, 2, 3, 4, 5, 6, 7, 8}), 4, 'Array over size limit `4`.')
  test('equal', r({1, 2, 3, 4}):union({5, 6, 7, 8}), 8, {{1, 2, 3, 4, 5, 6, 7, 8}})
  test('huge', huge_l:append(1):count(), 100001, {100001})

  it('huge read', function()
    r.table(reql_table):insert({id = 0, array = huge_l:append(1)}):run(
      c, {array_limit = 100001}, function(err, cur)
        if err then error(err.message) end
        cur:to_array(function(err, arr)
          if err then error(err.message) end
        end)
      end
    )
    assert.same(
      r.table(reql_table):get(0):run(
        c, {array_limit = 100001}, function(err, cur)
          if err then error(err.message) end
          return cur:to_array(function(err, arr)
            if err then error(err.message) end
            return arr
          end)
        end
      ), {}
    )
  end)

  test('huge table', r.table(reql_table):insert({id = 0, array = huge_l:append(1)}), 100001, {{
        deleted = 0, unchanged = 0, replaced = 0, skipped = 0,
        errors = 1, inserted = 0,
        first_error =
        'Array too large for disk writes (limit 100,000 elements)'
      }})
  test_error('less than', r({1, 2, 3, 4}):union({5, 6, 7, 8}), 4, 'Array over size limit `4`.')

  it('less than read', function()
    r.table(reql_table):insert(
      {id = 1, array = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}}
    ):run(
      c, function(err, cur)
        if err then error(err.message) end
        cur:to_array(function(err, arr)
          if err then error(err.message) end
        end)
      end
    )
    assert.same(
      r.table(reql_table):get(1):run(
        c, {array_limit = 4}, function(err, cur)
          if err then error(err.message) end
          return cur:to_array(function(err, arr)
            if err then error(err.message) end
            return arr
          end)
        end
      ), {{array = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}, id = 1}}
    )
  end)

  test_error('negative', r({1, 2, 3, 4, 5, 6, 7, 8}), -1, 'Illegal array size limit `-1`.')
  test_error('zero', r({1, 2, 3, 4, 5, 6, 7, 8}), 0, 'Illegal array size limit `0`.')
end)
