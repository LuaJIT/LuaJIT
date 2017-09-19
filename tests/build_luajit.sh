#!/bin/bash
#Build the various intresting flavors of LuaJIT

TEST_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ljsrc=${TEST_DIR}/../src
ljbins=${ljbins:=${TEST_DIR}/builds}

mkdir -p ${ljbins}

#Unmodified build with 32 bit sized gc object pointers. Object allocataion limited to the lower 4gb virtual address space
make -C ${ljsrc} clean
make -C ${ljsrc} -j
cp ${ljsrc}/luajit ${ljbins}/luajit

#Build with JIT removed
make -C ${ljsrc} clean
make -C ${ljsrc} -j XCFLAGS=-DLUAJIT_DISABLE_JIT
cp ${ljsrc}/luajit ${ljbins}/luajit_nojit

#GC64 64 bit sized gc object pointer
make -C ${ljsrc} clean
make -C ${ljsrc} -j XCFLAGS=-DLUAJIT_ENABLE_GC64
cp ${ljsrc}/luajit ${ljbins}/luajit_gc64

# Build with dual number mode enabled
make -C ${ljsrc} clean
make -C ${ljsrc} -j XCFLAGS=-DLUAJIT_NUMMODE=2
cp ${ljsrc}/luajit ${ljbins}/luajit_dualnum

#32 bit build
#make -C ${ljsrc} clean
#make -C ${ljsrc} -j CC="gcc -m32"
#cp ${ljsrc}/luajit ${ljbins}/luajit32
