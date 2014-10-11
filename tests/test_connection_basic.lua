r = require('rethinkdb')

r.connect(function(err, c)
  if err then error(err) end
  if c then print('"SUCCESS"') end
end)
