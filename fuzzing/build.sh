#!/bin/bash -eu

# Tests during compilation contain a memory leak,
# so we switch of detection
export ASAN_OPTIONS=detect_leaks=0

make BUILDMODE=static \
	DEFAULT_CC="${CC}" \
	CFLAGS="${CFLAGS}" \
	HOST_CC="${CC}" \
	HOST_CFLAGS="${CFLAGS}" \
	FUZZ_CFLAGS="${CFLAGS}" \
	LDFLAGS="" \
	HOST_LDFLAGS="" \
	-j$(nproc)

# Build .a file with all .o files
find . -name "*.o" -exec ar rcs fuzz_lib.a {} \;

# Build the fuzzer
$CC $CFLAGS -c fuzzer.c -o fuzzer.o -O2 -I. -I./src \
	-DLUAJIT_TARGET=LUAJIT_ARCH_x64 \
	-DLJ_ARCH_HASFPU=1 -DLJ_ABI_SOFTFP=0

$CC $CFLAGS $LIB_FUZZING_ENGINE fuzzer.o -o $OUT/fuzzer \
	-O2 -I. -DLUAJIT_TARGET=LUAJIT_ARCH_x64 \
	-DLJ_ARCH_HASFPU=1 -DLJ_ABI_SOFTFP=0 \
	./src/libluajit.a fuzz_lib.a

