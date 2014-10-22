describe('array limits', function()
  local r, reql_db, reql_table, c, huge_l

  setup(function()
    r = require('rethinkdb')

    reql_db = 'array'
    reql_table = 'limits'

    local err

    c, err = r.connect()
    if err then error(err.message) end

    r.db_create(reql_db):run(c)
    c:use(reql_db)
    r.table_create(reql_table):run(c)

    local ten_l = r({1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
    huge_l = ten_l:concat_map(ten_l):concat_map(ten_l):concat_map(
      ten_l):concat_map(ten_l)
  end)

  after_each(function()
    r.table(reql_table):delete():run(c)
  end)

  it('create', function()
    assert.has_error(function()
      r({1, 2, 3, 4, 5, 6, 7, 8}):run(
        c, {array_limit = 4}, function(err, cur)
          if err then error(err.message) end
          cur:to_array(function(err, arr)
            if err then error(err.msg) end
            error('no error thrown')
          end)
        end
      )
    end, 'Array over size limit `4`.')
  end)

  it('equal', function()
    assert.are.equal(
      r({1, 2, 3, 4}):union({5, 6, 7, 8}):run(
        c, {array_limit = 8}, function(err, cur)
          if err then error(err.message) end
          return cur:to_array(function(err, arr)
            if err then error(err.message) end
            return arr
          end)
        end
      ), {{1, 2, 3, 4, 5, 6, 7, 8}}
    )
  end)

  it('huge', function()
    assert.are.equal(
      huge_l:append(1):count():run(
        c, {array_limit = 100001}, function(err, cur)
          if err then error(err.message) end
          return cur:to_array(function(err, arr)
            if err then error(err.message) end
            return arr
          end)
        end
      ), {100001}
    )
  end)

  it('huge read', function()
    r.table(reql_table):insert({id = 0, array = huge_l:append(1)}):run(
      c, {array_limit = 100001}, function(err, cur)
        if err then error(err.message) end
        cur:to_array(function(err, arr)
          if err then error(err.message) end
        end)
      end
    )
    assert.are.equal(
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

  it('huge table', function()
    assert.are.equal(
      r.table(reql_table):insert({id = 0, array = huge_l:append(1)}):run(
        c, {array_limit = 100001}, function(err, cur)
          if err then error(err.message) end
          return cur:to_array(function(err, arr)
            if err then error(err.message) end
            return arr
          end)
        end
      ), {{
        deleted = 0, unchanged = 0, replaced = 0, skipped = 0,
        errors = 1, inserted = 1,
        first_error =
        'Array too large for disk writes (limit 100,000 elements)'
      }}
    )
  end)

  it('less than', function()
    assert.has_error(function()
      r({1, 2, 3, 4}):union({5, 6, 7, 8}):run(
        c, {array_limit = 4}, function(err, cur)
          if err then error(err.message) end
          cur:to_array(function(err, arr)
            if err then error(err.msg) end
            error('no error thrown')
          end)
        end
      )
    end, 'Array over size limit `4`.')
  end)

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
    assert.are.equal(
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

  it('negative', function()
    assert.has_error(function()
      r({1, 2, 3, 4, 5, 6, 7, 8}):run(
        c, {array_limit = -1}, function(err, cur)
          if err then error(err.message) end
          cur:to_array(function(err, arr)
            if err then error(err.msg) end
            error('no error thrown')
          end)
        end
      )
    end, 'Illegal array size limit `-1`.')
  end)

  it('zero', function()
    assert.has_error(function()
      r({1, 2, 3, 4, 5, 6, 7, 8}):run(
        c, {array_limit = 0}, function(err, cur)
          if err then error(err.message) end
          cur:to_array(function(err, arr)
            if err then error(err.msg) end
            error('no error thrown')
          end)
        end
      )
    end, 'Illegal array size limit `0`.')
  end)
end)
