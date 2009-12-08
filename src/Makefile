##############################################################################
# LuaJIT Makefile. Requires GNU Make.
#
# Suitable for POSIX platforms (Linux, *BSD, OSX etc.).
# Also works with MinGW and Cygwin on Windows.
# Please check msvcbuild.bat for building with MSVC on Windows.
#
# Copyright (C) 2005-2009 Mike Pall. See Copyright Notice in luajit.h
##############################################################################

##############################################################################
# Compiler options: change them as needed. This mainly affects the speed of
# the JIT compiler itself, not the speed of the JIT compiled code.
# Turn any of the optional settings on by removing the '#' in front of them.
#
# Note: LuaJIT can only be compiled for x86, and not for x64 (yet)!
#       In the meantime, the x86 binary runs fine under a x64 OS.
#
# It's recommended to compile at least for i686. By default the assembler part
# of the interpreter makes use of CMOV/FCOMI*/FUCOMI* instructions, anyway.
CC= gcc -m32 -march=i686
# Use this for GCC 4.2 or higher if you don't intend to distribute the
# binaries to a different machine:
#CC= gcc -m32 -march=native
#
# Since the assembler part does NOT maintain a frame pointer, it's pointless
# to slow down the C part by not omitting it. Debugging and tracebacks are
# not affected -- the assembler part has frame unwind information and GCC
# emits it with -g (see CCDEBUG below).
CCOPT= -O2 -fomit-frame-pointer
# Use this if you want to generate a smaller binary (but it's slower):
#CCOPT= -Os -fomit-frame-pointer
# Note: it's no longer recommended to use -O3 with GCC 4.x.
# The I-Cache bloat usually outweighs the benefits from aggressive inlining.
#
CCDEBUG=
# Uncomment the next line to generate debug information:
#CCDEBUG= -g
#
CCWARN= -Wall
# Uncomment the next line to enable more warnings:
#CCWARN+= -Wextra -Wdeclaration-after-statement -Wredundant-decls -Wshadow -Wpointer-arith
#
##############################################################################

##############################################################################
# Compile time definitions: change them as needed, but make sure you force
# a full recompile with "make clean", followed by "make".
# Note that most of these are NOT suitable for benchmarking or release mode!
XCFLAGS=
#
# Disable the use of CMOV and FCOMI*/FUCOMI* instructions in the interpreter.
# This is only necessary if you intend to run the code on REALLY ANCIENT CPUs
# (before Pentium Pro, or on the VIA C3). This generally slows down the
# interpreter. Don't bother if your OS wouldn't run on them, anyway.
#XCFLAGS+= -DLUAJIT_CPU_NOCMOV
#
# Disable the JIT compiler, i.e. turn LuaJIT into a pure interpreter:
#XCFLAGS+= -DLUAJIT_DISABLE_JIT
#
# Use the system provided memory allocator (realloc) instead of the
# bundled memory allocator. This is slower, but sometimes helpful for
# debugging. It's mandatory for Valgrind's memcheck tool, too.
#XCFLAGS+= -DLUAJIT_USE_SYSMALLOC
#
# This define is required to run LuaJIT under Valgrind. The Valgrind
# header files must be installed. You should enable debug information, too.
#XCFLAGS+= -DLUAJIT_USE_VALGRIND
#
# This is the client for the GDB JIT API. GDB 7.0 or higher is required
# to make use of it. See lj_gdbjit.c for details. Enabling this causes
# a non-negligible overhead, even when not running under GDB.
#XCFLAGS+= -DLUAJIT_USE_GDBJIT
#
# Turn on assertions for the Lua/C API to debug problems with lua_* calls.
# This is rather slow -- use only while developing C libraries/embeddings.
#XCFLAGS+= -DLUA_USE_APICHECK
#
# Turn on assertions for the whole LuaJIT VM. This significantly slows down
# everything. Use only if you suspect a problem with LuaJIT itself.
#XCFLAGS+= -DLUA_USE_ASSERT
#
##############################################################################
# You probably don't need to change anything below this line.
##############################################################################

CCOPTIONS= $(CCDEBUG) $(CCOPT) $(CCWARN) $(CFLAGS) $(XCFLAGS)
LDOPTIONS= $(CCDEBUG) $(LDFLAGS)

HOST_CC= $(CC)
HOST_RM= rm -f
HOST_XCFLAGS=
HOST_XLDFLAGS=
HOST_XLIBS=

TARGET_CC= $(CC)
TARGET_STRIP= strip
TARGET_XCFLAGS= -D_FILE_OFFSET_BITS=64
TARGET_XLDFLAGS=
TARGET_XSHLDFLAGS= -shared
TARGET_XLIBS=
TARGET_ARCH= $(patsubst %,-DLUAJIT_TARGET=LUAJIT_ARCH_%,$(TARGET))
TARGET_DISABLE= -U_FORTIFY_SOURCE
ifneq (,$(findstring stack-protector,$(shell $(CC) -dumpspecs)))
  TARGET_DISABLE+= -fno-stack-protector
endif

ifneq (,$(findstring Windows,$(OS)))
  TARGET_SYS= Windows
else
  TARGET_SYS:= $(shell uname -s)
  ifneq (,$(findstring CYGWIN,$(TARGET_SYS)))
    TARGET_SYS= Windows
  endif
endif

ifeq (Linux,$(TARGET_SYS))
  TARGET_XLIBS= -ldl
  TARGET_XLDFLAGS= -Wl,-E
else
ifeq (Windows,$(TARGET_SYS))
  HOST_RM= del
  TARGET_STRIP= strip --strip-unneeded
else
ifeq (Darwin,$(TARGET_SYS))
  TARGET_XSHLDFLAGS= -dynamiclib -single_module -undefined dynamic_lookup
  TARGET_STRIP= strip -x
  export MACOSX_DEPLOYMENT_TARGET=10.3
else
  TARGET_XLDFLAGS= -Wl,-E
endif
endif
endif

# NOTE: The LuaJIT distribution comes with a pre-generated buildvm_*.h.
# You DO NOT NEED an installed copy of (plain) Lua 5.1 to run DynASM unless
# you want to MODIFY the corresponding *.dasc file. You can also use LuaJIT
# itself (bootstrapped from the pre-generated file) to run DynASM of course.
DASM_LUA= lua

Q= @
E= @echo
#Q=
#E= @:

##############################################################################

TARGET_CFLAGS= $(CCOPTIONS) $(TARGET_DISABLE) $(TARGET_XCFLAGS)
TARGET_LDFLAGS= $(LDOPTIONS) $(TARGET_XLDFLAGS)
TARGET_SHLDFLAGS= $(LDOPTIONS) $(TARGET_XSHLDFLAGS)
TARGET_LIBS= -lm $(TARGET_XLIBS)
ifneq (,$(CCDEBUG))
  TARGET_STRIP= @:
endif

HOST_CFLAGS= $(CCOPTIONS) $(HOST_XCFLAGS) $(TARGET_ARCH)
HOST_LDFLAGS= $(LDOPTIONS) $(HOST_XLDFLAGS)
HOST_LIBS= $(HOST_XLIBS)

DASM_DIR= ../dynasm
DASM= $(DASM_LUA) $(DASM_DIR)/dynasm.lua
DASM_FLAGS=
DASM_DISTFLAGS= -LN

BUILDVM_O= buildvm.o buildvm_asm.o buildvm_peobj.o buildvm_lib.o buildvm_fold.o
BUILDVM_T= buildvm

HOST_O= $(BUILDVM_O)
HOST_T= $(BUILDVM_T)

LJVM_S= lj_vm.s
LJVM_O= lj_vm.o
LJVM_BOUT= $(LJVM_S)
LJVM_MODE= asm

LJLIB_O= lib_base.o lib_math.o lib_bit.o lib_string.o lib_table.o \
	 lib_io.o lib_os.o lib_package.o lib_debug.o lib_jit.o
LJLIB_C= $(LJLIB_O:.o=.c)

LJCORE_O= lj_gc.o lj_err.o lj_ctype.o lj_bc.o lj_obj.o \
	  lj_str.o lj_tab.o lj_func.o lj_udata.o lj_meta.o \
	  lj_state.o lj_dispatch.o lj_vmevent.o lj_api.o \
	  lj_lex.o lj_parse.o \
	  lj_ir.o lj_opt_mem.o lj_opt_fold.o lj_opt_narrow.o \
	  lj_opt_dce.o lj_opt_loop.o \
	  lj_mcode.o lj_snap.o lj_record.o lj_asm.o lj_trace.o lj_gdbjit.o \
	  lj_lib.o lj_alloc.o lib_aux.o \
	  $(LJLIB_O) lib_init.o

LJVMCORE_O= $(LJVM_O) $(LJCORE_O)

# NYI: Need complete support for building as a shared library on POSIX.
#      This is currently *only* suitable for MinGW and Cygwin, see below.
LUAJIT_O= luajit.o
LUAJIT_SO= luajit.so
LUAJIT_T= luajit

LIB_VMDEF= ../lib/vmdef.lua

TARGET_DEP= $(LIB_VMDEF)
TARGET_O= $(LJVMCORE_O) $(LUAJIT_O)
TARGET_T= $(LUAJIT_T)

ALL_GEN= $(LJVM_S) lj_ffdef.h lj_libdef.h lj_recdef.h $(LIB_VMDEF) lj_folddef.h
ALL_DYNGEN= buildvm_x86.h
WIN_RM= *.obj *.lib *.exp *.dll *.exe *.manifest
ALL_RM= $(LUAJIT_T) $(LUAJIT_SO) $(HOST_T) $(ALL_GEN) *.o $(WIN_RM)

ifeq (Windows,$(TARGET_SYS))
  LJVM_BOUT= $(LJVM_O)
  LJVM_MODE= peobj
  LIB_VMDEF= ..\lib\vmdef.lua
  # Imported symbols are bound to a specific DLL name under Windows.
  LUAJIT_SO= lua51.dll
  LUAJIT_T= luajit.exe
  BUILDVM_T= buildvm.exe
  #
  # You can comment out the following two lines to build a static executable.
  # But then you won't be able to dynamically load any C modules, because
  # they bind to lua51.dll.
  #
  TARGET_XCFLAGS+= -DLUA_BUILD_AS_DLL
  TARGET_O= $(LUAJIT_SO) $(LUAJIT_O)
endif

##############################################################################

default: $(TARGET_T)

all:	$(TARGET_T)

amalg:
	@grep "^[+|]" ljamalg.c
	$(MAKE) all "LJCORE_O=ljamalg.o"

MAKE_TARGETS= amalg

##############################################################################

buildvm_x86.h: buildvm_x86.dasc
	$(E) "DYNASM    $@"
	$(Q)$(DASM) $(DASM_FLAGS) -o $@ buildvm_x86.dasc

$(BUILDVM_T): $(BUILDVM_O)
	$(E) "HOSTLINK  $@"
	$(Q)$(HOST_CC) $(HOST_LDFLAGS) -o $@ $(BUILDVM_O) $(HOST_LIBS)

$(LJVM_BOUT): $(BUILDVM_T)
	$(E) "BUILDVM   $@"
	$(Q)./$(BUILDVM_T) -m $(LJVM_MODE) -o $@

lj_ffdef.h: $(BUILDVM_T) $(LJLIB_C)
	$(E) "BUILDVM   $@"
	$(Q)./$(BUILDVM_T) -m ffdef -o $@ $(LJLIB_C)

lj_libdef.h: $(BUILDVM_T) $(LJLIB_C)
	$(E) "BUILDVM   $@"
	$(Q)./$(BUILDVM_T) -m libdef -o $@ $(LJLIB_C)

lj_recdef.h: $(BUILDVM_T) $(LJLIB_C)
	$(E) "BUILDVM   $@"
	$(Q)./$(BUILDVM_T) -m recdef -o $@ $(LJLIB_C)

$(LIB_VMDEF): $(BUILDVM_T) $(LJLIB_C)
	$(E) "BUILDVM   $@"
	$(Q)./$(BUILDVM_T) -m vmdef -o $@ $(LJLIB_C)

lj_folddef.h: $(BUILDVM_T) lj_opt_fold.c
	$(E) "BUILDVM   $@"
	$(Q)./$(BUILDVM_T) -m folddef -o $@ lj_opt_fold.c

$(LUAJIT_SO): $(LJVMCORE_O)
	$(E) "LINK      $@"
	$(Q)$(TARGET_CC) $(TARGET_SHLDFLAGS) -o $@ $(LJVMCORE_O) $(TARGET_LIBS)
	$(Q)$(TARGET_STRIP) $@

$(LUAJIT_T): $(TARGET_O) $(TARGET_DEP)
	$(E) "LINK      $@"
	$(Q)$(TARGET_CC) $(TARGET_LDFLAGS) -o $@ $(TARGET_O) $(TARGET_LIBS)
	$(Q)$(TARGET_STRIP) $@
	$(E) "OK        Successfully built LuaJIT"

##############################################################################

%.o: %.c
	$(E) "CC        $@"
	$(Q)$(TARGET_CC) $(TARGET_CFLAGS) -c -o $@ $<

%.o: %.s
	$(E) "ASM       $@"
	$(Q)$(TARGET_CC) $(TARGET_CFLAGS) -c -o $@ $<

$(HOST_O): %.o: %.c
	$(E) "HOSTCC    $@"
	$(Q)$(HOST_CC) $(HOST_CFLAGS) -c -o $@ $<

include Makefile.dep

##############################################################################

clean:
	$(HOST_RM) $(ALL_RM)

cleaner:	clean
	$(HOST_RM) $(ALL_DYNGEN)

distclean:	clean
	$(E) "DYNASM    $@"
	$(Q)$(DASM) $(DASM_DISTFLAGS) -o buildvm_x86.h buildvm_x86.dasc

depend:
	@test -f lj_ffdef.h || touch lj_ffdef.h
	@test -f lj_libdef.h || touch lj_libdef.h
	@test -f lj_recdef.h || touch lj_recdef.h
	@test -f lj_folddef.h || touch lj_folddef.h
	@test -f buildvm_x86.h || touch buildvm_x86.h
	@$(HOST_CC) $(HOST_CFLAGS) -MM *.c | sed "s|$(DASM_DIR)|\$$(DASM_DIR)|g" >Makefile.dep
	@test -s lj_ffdef.h || $(HOST_RM) lj_ffdef.h
	@test -s lj_libdef.h || $(HOST_RM) lj_libdef.h
	@test -s lj_recdef.h || $(HOST_RM) lj_recdef.h
	@test -s lj_folddef.h || $(HOST_RM) lj_folddef.h
	@test -s buildvm_x86.h || $(HOST_RM) buildvm_x86.h

.PHONY: default all $(MAKE_TARGETS) clean cleaner distclean depend

##############################################################################
