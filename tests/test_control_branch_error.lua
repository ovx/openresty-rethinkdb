local r = require('rethinkdb')
local json = require('json')

r.connect({timeout = 1}, function(err, c)
  if err then error(err.message) end
  r.branch(r.error_("a"), 1, 2):run(
    c, function(err, cur)
      if err then error(err.message) end
      cur:to_array(function(err, arr)
        if err then return print(json.encode(err.message)) end
        error(arr)
      end)
    end
  )
end)
