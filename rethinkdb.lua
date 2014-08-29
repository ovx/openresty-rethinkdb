local net = require('./net')
local rethinkdb = require('./ast')
local error = require('./errors')

-- Add connect from net module
rethinkdb.connect = net.connect

-- Export Rql Errors
rethinkdb.Error = error

return rethinkdb
