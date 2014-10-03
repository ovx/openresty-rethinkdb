return (function(rethinkdb)
  -- Add connect from net module
  rethinkdb.connect = require('./net').connect

  -- Export ReQL Errors
  rethinkdb.error = require('./errors')

  -- Export class introspection
  rethinkdb.is_instance = require('./util').is_instance

  return rethinkdb
end)(require('./ast'))
