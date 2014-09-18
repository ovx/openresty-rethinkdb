return (function(rethinkdb)
  -- Add connect from net module
  rethinkdb.connect = require('./net').connect

  -- Export ReQL Errors
  rethinkdb.Error = require('./errors')

  return rethinkdb
end)(require('./ast'))
