r = require('rethinkdb')

r.connect({timeout = 1}, function(err, c)
  ten_l = r.expr({1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
  huge_l = ten_l:concat_map(ten_l):concat_map(ten_l):concat_map(ten_l):concat_map(ten_l)
  r.db():table():insert({id = 0, array = huge_l.append(1)}):run(
    c, {array_limit = 100001}, function(err, cur)
      if err then error(err.message) end
      cur:to_array(function(err, arr)
        if err then print(err.message) end
      end)
    end
  )
  r.db('array'):table('limits'):get(0):run(
    c, {array_limit = 100001}, function(err, cur)
      if err then error(err.message) end
      cur:to_array(function(err, arr)
        if err then print(err.message) end
      end)
    end
  )
end)
