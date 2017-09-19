#!/bin/bash
TEST_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
export LUA_PATH="${TEST_DIR}/?.lua;${TEST_DIR}/../src/?.lua;$LUA_PATH"

args=${TEST_DIR}/runtests.lua

if [ -d "${TEST_DIR}/builds" ]; then 
  echo $'\nTesting Normal 64 bit:'
  (cd "${TEST_DIR}" && ${TEST_DIR}/builds/luajit ${args})

  if [ -f "${TEST_DIR}/builds/luajit_gc64" ]; then
    echo $'\nTesting GC64 build:'
    (cd "${TEST_DIR}" && ${TEST_DIR}/builds/luajit_gc64 ${args})
  fi
  
  if [ -f "${TEST_DIR}/builds/luajit_dualnum" ]; then
    echo $'\nTesting with Dual Number:'
    (cd "${TEST_DIR}" && ${TEST_DIR}/builds/luajit_dualnum ${args})
  fi

  if [ -f "${TEST_DIR}/builds/luajit32" ]; then
    echo $'\nTesting 32 bit build:'
    (cd "${TEST_DIR}" && ${TEST_DIR}/builds/luajit32 ${args})
  fi
else
  echo $'Testing luajit built in src\n'
  (cd "${TEST_DIR}" && ${TEST_DIR}/../src/luajit ${args})
fi
