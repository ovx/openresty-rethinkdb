r = require('rethinkdb')

r.connect({timeout = 1}, function(err, c)
  r.expr({1, 2, 3, 4, 5, 6, 7, 8}):run(
    c, {array_limit = 4}, function(err, cur)
      if err then error(err.message) end
      cur:to_array(function(err, arr)
        if err then print(err.message) end
      end)
    end
  )
end)
