describe('cursor', function()
  local r, reql_db, reql_table, c, num_rows

  setup(function()
    r = require('rethinkdb')

    reql_db = 'cursor'
    reql_table = 'tests'

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

  before_each(function()
    num_rows = random(1111, 2222)

    local doc = {}
    for i=0, i<500, 1 do
      table.insert(doc, i)
    end
    local document = {}
    for i=0, i<num_rows, 1 do
      table.insert(document, doc)
    end

    r.db(reql_db):table(reql_table):insert(document):run(c)
  end)

  it('type', function()
    assert.are.equal(
      r.table(reql_table):run(
        c, function(err, cur)
          if err then error(err.message) end
          return cur.__class.__name
        end
      ), 'Cursor'
    )
  end)

  it('count', function()
    assert.are.equal(
      r.table(reql_table):run(
        c, function(err, cur)
          if err then error(err.message) end
          return cur:to_array(function(err, arr)
            if err then error(err.message) end
            return #arr
          end)
        end
      ), num_rows
    )
  end)

  it('close', function()
    assert.has_no.errors(function()
      r.table(reql_table):run(
        c, function(err, cur)
          if err then error(err.message) end
          cur:close(function(err) if err then error(err.message) end end)
        end
      )
    end)
  end)
end)
