use Test::Nginx::Socket::Lua;

plan tests => repeat_each() * (3 * blocks());

our $HttpConfig = <<'_EOC_';
    lua_package_path 'src/?.lua;;';
    error_log logs/error.log debug;
_EOC_

no_long_string();

run_tests();

__DATA__

=== Connect
--- http_config eval: $::HttpConfig
--- config
    location /t {
      content_by_lua "
        local r = require 'rethinkdb'
  
        r.connect(function(err, c)
          if err then
            error(err.message)
          end
          
          assert(c)
          ngx.print('pass')
        end)
      ";
    }
--- request
GET /t
--- response_body: pass
--- no_error_log
[error]