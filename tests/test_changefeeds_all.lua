local r = require('rethinkdb')
local json = require('json')

r.connect({timeout = 1}, function(err, c)
  if err then error(err.message) end
  r.db('changefeeds'):table('watched'):changes():limit(4):run(
    c, function(err, cur)
      if err then error(err.message) end
      r.db('changefeeds'):table('watched'):insert(
        {{id = 7}, {id = 8}, {id = 9}, {id = 10}}
      ):run(c)
      res = {}
      cur:each(function(err, row)
        if err then error(err.message) end
        table.insert(res, row.new_val.id)
      end)
      print(json.encode(res))
    end
  )
end)
