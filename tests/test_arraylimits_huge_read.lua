local r = require('rethinkdb')
local json = require('json')

r.connect({timeout = 1}, function(err, c)
  if err then error(err.message) end
  ten_l = r({1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
  ten_f = function(l) return ten_l end
  huge_l = ten_l:concat_map(ten_f):concat_map(ten_f):concat_map(ten_f):concat_map(ten_f)
  r.db('array'):table('limits'):insert({id = 0, array = huge_l:append(1)}):run(
    c, {array_limit = 100001}, function(err, cur)
      if err then error(err.message) end
      cur:to_array(function(err, arr)
        if err then error(err.message) end
      end)
    end
  )
  r.db('array'):table('limits'):get(0):run(
    c, {array_limit = 100001}, function(err, cur)
      if err then error(err.message) end
      cur:next(function(err, row)
        if err then error(err.message) end
        print(json.encode(row))
      end)
    end
  )
end)
