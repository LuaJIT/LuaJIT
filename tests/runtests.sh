#!/bin/bash
cd "${0%/*}"
export LUA_PATH="$PWD/?.lua;$PWD/../src/?.lua;$LUA_PATH"
../src/luajit ./test.lua