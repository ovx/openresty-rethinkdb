use Test::Nginx::Socket::Lua;

plan tests => repeat_each() * (3 * blocks());

our $HttpConfig = <<'_EOC_';
    lua_package_path 'src/?.lua;;';
    error_log logs/error.log debug;
_EOC_

no_long_string();

run_tests();

__DATA__

=== Query
--- http_config eval: $::HttpConfig
--- config
    location /t {
      content_by_lua "
        local r = require 'rethinkdb'
  
        local reql_db = 'dbtest'
        local reql_table = 'test'
        local document_name = 'test document'
        local document = {
          name = document_name
        }
  
        r.connect(function(err, c)
          if err then 
            error(err.message)
          end
          
          assert(c, 'Connection failed')
      
          local function setup(callback)
            -- init db
            r.db_create(reql_db):run(c)
            c:use(reql_db)
            r.table_create(reql_table):run(c, function()
              -- remove data
              r.table(reql_table):delete():run(c, callback)
            end)
          end
        
          setup(function()
            -- insert doc
            r.table(reql_table):insert(document):run(c, function(err)
              if err then 
                error(err.message)
              end
              
              r.table(reql_table):run(c, function(err, cur)
                if err then 
                  error(err.message)
                end

                cur:to_array(function(err, arr)
                  if err then 
                    error(err.message)
                  end

                  assert(#arr == 1, 'Wrong array length')
                  assert(arr[1].name == document_name, 'Wrong document name')
                      
                  ngx.print('pass')
                end)
              end)
            end)
          end)
        end)
      ";
    }
--- request
GET /t
--- response_body: pass
--- no_error_log
[error]