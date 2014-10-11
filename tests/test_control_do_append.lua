local r = require('rethinkdb')
local json = require('json')

r.connect({timeout = 1}, function(err, c)
  if err then error(err.message) end
  r({0, 1, 2}):do_(function(v) return v:append(3) end):run(
    c, function(err, cur)
      if err then error(err.message) end
      cur:to_array(function(err, arr)
        if err then error(err.message) end
        print(json.encode(arr))
      end)
    end
  )
end)
