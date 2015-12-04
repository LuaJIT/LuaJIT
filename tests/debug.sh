#!/bin/bash
cd "${0%/*}"
cd ..
make CCDEBUG="-g" CCOPT=" -fomit-frame-pointer"
cd tests
export LUA_PATH="$PWD/?.lua;$PWD/../src/?.lua;$LUA_PATH"
gdb "${@:1}"