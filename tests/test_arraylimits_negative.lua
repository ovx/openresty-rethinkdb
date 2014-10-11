r = require('rethinkdb')
local json = require('json')

r.connect({timeout = 1}, function(err, c)
  if err then error(err.message) end
  r({1, 2, 3, 4, 5, 6, 7, 8}):run(
    c, {array_limit = -1}, function(err, cur)
      if err then error(err.message) end
      cur:to_array(function(err, arr)
        if err then print(json.encode(err.message)) end
      end)
    end
  )
end)
