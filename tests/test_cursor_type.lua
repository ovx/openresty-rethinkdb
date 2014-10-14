local r = require('rethinkdb')
local json = require('json')

r.connect({timeout = 1, db = 'cursor'}, function(err, c)
  if err then error(err.message) end
  r.table('tests'):run(
    c, function(err, cur)
      if err then error(err.message) end
      print(json.encode(cur.__class.__name))
    end
  )
end)
