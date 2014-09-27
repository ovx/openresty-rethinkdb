r = require('rethinkdb')

r.connect({timeout = 1}, function(err, c)
  r.db('changefeeds'):table('watched'):changes():limit(4):run(
    c, function(err, cur)
      if err then error(err.message) end
      r.db('changefeeds'):table('watched'):insert(
        {{id = 7}, {id = 8}, {id = 9}, {id = 10}}
      ):run(c)
      cur:each(function(err, row)
        if err then error(err.message) end
        print(row.new_val.id)
      end)
    end
  )
end)
