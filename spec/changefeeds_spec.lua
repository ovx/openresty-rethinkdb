local r = require('rethinkdb')

describe('change feeds', function()
  local reql_db, reql_table, c

  setup(function()
    reql_db = 'changefeeds'
    reql_table = 'watched'

    local err

    c, err = r.connect()
    if err then error(err.message) end

    r.db_create(reql_db):run(c)
    c:use(reql_db)
    r.table_create(reql_table):run(c)
  end)

  before_each(function()
    r.table(reql_table):insert({
      {id = 1}, {id = 2}, {id = 3},
      {id = 4}, {id = 5}, {id = 6}
    }):run(c)
  end)

  after_each(function()
    r.table(reql_table):delete():run(c)
  end)

  it('all', function()
    local res = r.table(reql_table):changes():limit(4):run(
      c, function(err, cur)
        if err then error(err.message) end
        r.table(reql_table):insert(
          {{id = 7}, {id = 8}, {id = 9}, {id = 10}}
        ):run(c, function(err)
          if err then error(err.message) end
        end)
        local res = {}
        cur:each(function(row)
          table.insert(res, row.new_val.id)
        end, function(err)
          if err then error(err.message) end
        end)
        return res
      end
    )
    table.sort(res)
    assert.same(res, {7, 8, 9, 10})
  end)

  it('even', function()
    local res = r.table(reql_table):changes():filter(
      function(row)
        return (row('new_val')('id') % 2):eq(0)
      end
    ):limit(2):run(
      c, function(err, cur)
        if err then error(err.message) end
        r.table(reql_table):insert(
          {{id = 7}, {id = 8}, {id = 9}, {id = 10}}
        ):run(c, function(err)
          if err then error(err.message) end
        end)
        local res = {}
        cur:each(function(row)
          table.insert(res, row.new_val.id)
        end, function(err)
          if err then error(err.message) end
        end)
        return res
      end
    )
    table.sort(res)
    assert.same(res, {8, 10})
  end)
end)
