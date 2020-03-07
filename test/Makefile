CC = gcc
CFLAGS = -fPIC -Wall -Wextra -O2 -g  -I ../src
LDFLAGS = -shared
RM = rm -rf

ifeq ($(OS),Windows_NT)
	HOST = Win32
	CC = x86_64-w64-mingw32-gcc
	CTEST = clib/ctest.dll
	CPPTEST = clib/cpptest.dll
else
	CTEST = clib/ctest
	CPPTEST = clib/cpptest
	UNAME_S := $(shell uname -s)
	ifeq ($(UNAME_S),Linux)
		HOST = Linux
		CC = gcc
	endif
	ifeq ($(UNAME_S),Darwin)
		CC = gcc
	endif
endif


.PHONY: all
all: ctest cpptest

ctest:
	$(CC) $(CFLAGS) ${LDFLAGS}  -o $(CTEST) src/ctest.c;


cpptest:
	$(CC)  $(CFLAGS)  ${LDFLAGS} -o $(CPPTEST) src/cpptest.cpp;


.PHONY: clean
clean:
	-${RM} clib/cpptest clib/ctest src/cpptest.o src/ctest.o

