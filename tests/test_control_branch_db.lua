local r = require('rethinkdb')
local json = require('json')

r.connect({timeout = 1}, function(err, c)
  if err then error(err.message) end
  r.db('test'):branch(1, 2):run(
    c, function(err, cur)
      if err then error(err.message) end
      cur:to_array(function(err, arr)
        if err then return print(json.encode(err.message)) end
        error(json.encode(arr))
      end)
    end
  )
end)
