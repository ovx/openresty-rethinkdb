r = require('rethinkdb')

r.connect(function(err, c)
  r.db('changefeeds'):table('watched'):changes():limit(4):run(
    c, function(err, cur)
      r.db('changefeeds'):table('watched'):insert(
        {{id = 7}, {id = 8}, {id = 9}, {id = 10}}
      ):run(c)
      cur.each(function(row)
        print(row.new_val.id)
      end)
    end
  )
end)
