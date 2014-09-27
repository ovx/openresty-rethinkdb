r = require('rethinkdb')

r.connect(function(err, c)
  r.db('changefeeds'):table('watched'):changes():filter(
    r.row:get_field('new_val'):get_field('id'):mod(2):eq(0)
  ):limit(2):run(
    c, function(err, cur)
      r.db('changefeeds'):table('watched'):insert(
        {{id = 7}, {id = 8}, {id = 9}, {id = 10}}
      ):run(c)
      cur:each(function(err, row)
        print(row.new_val.id)
      end)
    end
  )
end)
