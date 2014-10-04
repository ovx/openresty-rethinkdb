# Lua-ReQL

[![Build Status](https://travis-ci.org/grandquista/Lua-ReQL.svg?branch=master)](https://travis-ci.org/grandquista/Lua-ReQL)

Rethinkdb driver in Lua

## Installing
- luarocks install lua-reql

## Testing
- Linux:
  1. sudo apt-get install luarocks pip3 rethinkdb
  - sudo luarocks install luasocket luajson
  - sudo pip3 install rethinkdb
  - cd *Lua-ReQL directory*
  - python3 build.py
- Mac:
  1. brew install luarocks python3 rethinkdb
  - luarocks install luasocket luajson
  - sudo pip3 install rethinkdb
  - cd *Lua-ReQL directory*
  - python3 build.py

## Installing from source
- Linux:
  1. sudo apt-get install luarocks
  - cd *Lua-ReQL directory*
  - sudo luarocks make
- Mac:
  1. brew install luarocks
  - cd *Lua-ReQL directory*
  - luarocks make
