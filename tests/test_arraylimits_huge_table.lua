r = require('rethinkdb')

r.connect({timeout = 1}, function(err, c)
  ten_l = r.expr({1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
  ten_f = function(l) return ten_l end
  huge_l = ten_l:concat_map(ten_f):concat_map(ten_f):concat_map(ten_f):concat_map(ten_f)
  r.db('array'):table('limits'):insert({id = 0, array = huge_l:append(1)}):run(
    c, {array_limit = 100001}, function(err, cur)
      if err then error(err.message) end
      cur:next(function(err, row)
        if err then error(err.message) end
        function pairsByKeys(t)
          local a = {}
          for n in pairs(t) do table.insert(a, n) end
          table.sort(a)
          local i = 0  -- iterator variable
          return function()  -- iterator function
            i = i + 1
            if a[i] ~= nil then
              return a[i], t[a[i]]
            end
          end
        end
        local s = "{"
        local sep = ""
        for k, e in pairsByKeys(row) do
          s = s .. sep .. k .. ': ' .. e
          sep = ", "
        end
        print(s .. "}")
      end)
    end
  )
end)
