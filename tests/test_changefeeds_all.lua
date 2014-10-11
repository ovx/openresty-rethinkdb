local r = require('rethinkdb')
local json = require('json')

r.connect({timeout = 1, db = 'changefeeds'}, function(err, c)
  if err then error(err.message) end
  r.table('watched'):changes():limit(4):run(
    c, function(err, cur)
      if err then error(err.message) end
      r.table('watched'):insert(
        {{id = 7}, {id = 8}, {id = 9}, {id = 10}}
      ):run(c, function(err)
        if err then error(err.message) end
      end)
      res = {}
      cur:each(function(row)
        table.insert(res, row.new_val.id)
      end, function(err)
        if err then error(err.message) end
      end)
      print(json.encode(res))
    end
  )
end)
