local r = require('rethinkdb')
local json = require('json')

r.connect({timeout = 1}, function(err, c)
  if err then error(err.message) end
  r.expr({1, 2, 3, 4}):union({5, 6, 7, 8}):run(
    c, {array_limit = 8}, function(err, cur)
      if err then error(err.message) end
      cur:next(function(err, row)
        if err then error(err.message) end
        print(json.encode(row))
      end)
    end
  )
end)
