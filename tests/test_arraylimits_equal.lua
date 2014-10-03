r = require('rethinkdb')

r.connect({timeout = 1}, function(err, c)
  if err then error(err.message) end
  r.expr({1, 2, 3, 4}):union({5, 6, 7, 8}):run(
    c, {array_limit = 8}, function(err, cur)
      if err then error(err.message) end
      cur = cur:to_array(function(err, arr)
        if err then error(err.message) end
        return arr[1]
      end)
      table.sort(cur)
      local s = "{"
      local sep = ""
      for _, e in ipairs(cur) do
        s = s .. sep .. e
        sep = ", "
      end
      print(s .. "}")
    end
  )
end)
