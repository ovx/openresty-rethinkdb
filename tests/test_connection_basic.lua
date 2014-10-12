local r = require('rethinkdb')

r.connect(function(err, c)
  if err then error(err.message) end
  if c then print('"SUCCESS"') end
end)
