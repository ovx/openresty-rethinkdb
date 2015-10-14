# openresty-rethinkdb

Rethinkdb driver for OpenResty, using cjson and cosocket. Based on [Lua-ReQL](https://github.com/grandquista/Lua-ReQL)


## Dependencies
- OpenResty >= 1.9.3.1
- ~~luajson~~ -> cjson
- ~~luasocket~~ -> ngx.socket

## Testing
- https://github.com/openresty/test-nginx
- `prove t`
- or `busted`
