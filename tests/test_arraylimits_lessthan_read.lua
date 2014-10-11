local r = require('rethinkdb')
local json = require('json')

r.connect({timeout = 1}, function(err, c)
  if err then error(err.message) end
  ten_l = r({1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
  r.db('array'):table('limits'):insert({id = 1, array = ten_l}):run(
    c, function(err, cur)
      if err then error(err.message) end
      cur:to_array(function(err, arr)
        if err then error(err.message) end
      end)
    end
  )
  r.db('array'):table('limits'):get(1):run(
    c, {array_limit = 4}, function(err, cur)
      if err then error(err.message) end
      cur:to_array(function(err, arr)
        if err then error(err.message) end
        print(json.encode(arr))
      end)
    end
  )
end)
