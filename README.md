# Lua-ReQL

[![Build Status](https://travis-ci.org/grandquista/Lua-ReQL.svg?branch=master)](https://travis-ci.org/grandquista/Lua-ReQL)

Rethinkdb driver in Lua

## Installing
- luarocks install lua-reql

## Testing
- Linux:
  1. sudo apt-get install liblua5.1-dev luarocks python3 pip3 rethinkdb
  - sudo luarocks install luasocket luajson
  - pip3 install rethinkdb
  - cd *Lua-ReQL directory*
  - python3 build.py test
- Mac:
  1. brew install lua luarocks python3 rethinkdb
  - luarocks install luasocket luajson
  - pip3 install rethinkdb
  - cd *Lua-ReQL directory*
  - python3 build.py test

## Installing from source
- Linux:
  1. sudo apt-get install liblua5.1-dev luarocks
  - cd *Lua-ReQL directory*
  - sudo luarocks make
- Mac:
  1. brew install lua luarocks
  - cd *Lua-ReQL directory*
  - luarocks make
