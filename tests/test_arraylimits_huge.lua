r = require('rethinkdb')

r.connect({timeout = 1}, function(err, c)
  ten_l = r.expr({1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
  ten_f = function(l) return ten_l end
  huge_l = ten_l:concat_map(ten_f):concat_map(ten_f):concat_map(ten_f):concat_map(ten_f)
  huge_l:append(1):count():run(
    c, {array_limit = 100001}, function(err, cur)
      if err then error(err.message) end
      print(cur:to_array(function(err, arr)
        if err then error(err.message) end
        return arr[1]
      end))
    end
  )
end)
