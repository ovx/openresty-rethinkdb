return (function(rethinkdb)
  -- Add connect from net module
  rethinkdb.connect = require('./net').connect

  -- Export ReQL Errors
  rethinkdb.error = require('./errors')

  return rethinkdb
end)(require('./ast'))
