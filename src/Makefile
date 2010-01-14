##############################################################################
# LuaJIT Makefile. Requires GNU Make.
#
# Suitable for POSIX platforms (Linux, *BSD, OSX etc.).
# Also works with MinGW and Cygwin on Windows.
# Please check msvcbuild.bat for building with MSVC on Windows.
#
# Copyright (C) 2005-2010 Mike Pall. See Copyright Notice in luajit.h
##############################################################################

MAJVER=  2
MINVER=  0
RELVER=  0
ABIVER=  5.1
NODOTABIVER= 51

##############################################################################
# Compiler options: change them as needed. This mainly affects the speed of
# the JIT compiler itself, not the speed of the JIT compiled code.
# Turn any of the optional settings on by removing the '#' in front of them.
# You need to 'make clean' and 'make' again, if you change any options.
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
# Use SSE2 instructions instead of x87 instructions in the x86 interpreter
# (always enabled for x64). A pure interpreter built with this flag won't
# run on older CPUs (before P4 or K8). There isn't much of a speed
# difference, so this is not enabled by default.
# The JIT compiler is not affected by this flag. It always uses runtime
# CPU feature detection before emitting code for SSE2 up to SSE4.1.
#XCFLAGS+= -DLUAJIT_CPU_SSE2
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

##############################################################################
# Build mode: override the mode as needed. Default is mixed mode on POSIX.
# On Windows this is the same as dynamic mode.
#
# Mixed mode creates a static + dynamic library and a statically linked luajit.
BUILDMODE= mixed
#
# Static mode creates a static library and a statically linked luajit.
#BUILDMODE= static
#
# Dynamic mode creates a dynamic library and a dynamically linked luajit.
# Note: this executable will only run when the library is installed!
#BUILDMODE= dynamic
##############################################################################
# You probably don't need to change anything below this line.
##############################################################################

##############################################################################
# Flags and options for host and target.
##############################################################################

# You may also override the following variables at the make command line:
#   CC       HOST_CC       STATIC_CC       DYNAMIC_CC
#   CFLAGS   HOST_CFLAGS   TARGET_CFLAGS
#   LDFLAGS  HOST_LDFLAGS  TARGET_LDFLAGS  TARGET_SHLDFLAGS
#   LIBS     HOST_LIBS     TARGET_LIBS
#   CROSS    HOST_SYS      TARGET_SYS
#
# Cross-compilation example: make CROSS=i586-mingw32msvc- TARGET_SYS=Windows

CCOPTIONS= $(CCDEBUG) $(CCOPT) $(CCWARN) $(XCFLAGS) $(CFLAGS)
LDOPTIONS= $(CCDEBUG) $(LDFLAGS)

TARGET_ARCH= $(patsubst %,-DLUAJIT_TARGET=LUAJIT_ARCH_%,$(TARGET))

HOST_CC= $(CC)
HOST_RM= rm -f
# NOTE: The LuaJIT distribution comes with pre-generated buildvm_*.h files.
# You DO NOT NEED an installed copy of (plain) Lua 5.1 to run DynASM unless
# you want to MODIFY the corresponding *.dasc file. You can also use LuaJIT
# itself (bootstrapped from a pre-generated file) to run DynASM of course.
HOST_LUA= lua

HOST_XCFLAGS=
HOST_XLDFLAGS=
HOST_XLIBS=
HOST_ACFLAGS= $(CCOPTIONS) $(HOST_XCFLAGS) $(TARGET_ARCH) $(HOST_CFLAGS)
HOST_ALDFLAGS= $(LDOPTIONS) $(HOST_XLDFLAGS) $(HOST_LDFLAGS)
HOST_ALIBS= $(HOST_XLIBS) $(LIBS) $(HOST_LIBS)

STATIC_CC = $(CROSS)$(CC)
DYNAMIC_CC = $(CROSS)$(CC) -fPIC
TARGET_CC= $(STATIC_CC)
TARGET_STCC= $(STATIC_CC)
TARGET_DYNCC= $(DYNAMIC_CC)
TARGET_LD= $(CROSS)$(CC)
TARGET_AR= $(CROSS)ar rcus
TARGET_STRIP= $(CROSS)strip

TARGET_SONAME= libluajit-$(ABIVER).so.$(MAJVER)
TARGET_DYLIBNAME= libluajit-$(NODOTABIVER).$(MAJVER).$(MINVER).$(RELVER).dylib
TARGET_DLLNAME= lua$(NODOTABIVER).dll
TARGET_XSHLDFLAGS= -shared -fPIC -Wl,-soname,$(TARGET_SONAME)
TARGET_DYNXLDOPTS=

TARGET_XCFLAGS= -D_FILE_OFFSET_BITS=64 -U_FORTIFY_SOURCE
TARGET_XLDFLAGS=
TARGET_XLIBS= -lm
TARGET_ACFLAGS= $(CCOPTIONS) $(TARGET_XCFLAGS) $(TARGET_ARCH) $(TARGET_CFLAGS)
TARGET_ALDFLAGS= $(LDOPTIONS) $(TARGET_XLDFLAGS) $(TARGET_LDFLAGS)
TARGET_ASHLDFLAGS= $(LDOPTIONS) $(TARGET_XSHLDFLAGS) $(TARGET_SHLDFLAGS)
TARGET_ALIBS= $(TARGET_XLIBS) $(LIBS) $(TARGET_LIBS)

ifneq (,$(findstring stack-protector,$(shell $(TARGET_CC) -dumpspecs)))
  TARGET_XCFLAGS+= -fno-stack-protector
endif

ifneq (,$(PREFIX))
ifneq (/usr/local,$(PREFIX))
  TARGET_XCFLAGS+= -DLUA_XROOT=\"$(PREFIX)/\"
  ifneq (/usr,$(PREFIX))
    TARGET_DYNXLDOPTS= -Wl,-rpath,$(PREFIX)/lib
  endif
endif
endif

##############################################################################
# System detection.
##############################################################################

ifneq (,$(findstring Windows,$(OS)))
  HOST_SYS= Windows
else
  HOST_SYS:= $(shell uname -s)
  ifneq (,$(findstring CYGWIN,$(TARGET_SYS)))
    HOST_SYS= Windows
  endif
endif
ifeq (Windows,$(HOST_SYS))
  HOST_RM= del
endif

TARGET_SYS= $(HOST_SYS)
ifeq (Windows,$(TARGET_SYS))
  TARGET_STRIP+= --strip-unneeded
  TARGET_XSHLDFLAGS= -shared
  TARGET_DYNXLDOPTS=
else
ifeq (Darwin,$(TARGET_SYS))
  export MACOSX_DEPLOYMENT_TARGET=10.4
  TARGET_STRIP+= -x
  TARGET_AR+= 2>/dev/null
  TARGET_XSHLDFLAGS= -dynamiclib -single_module -undefined dynamic_lookup -fPIC
  ifneq (,$(TARGET_DYNXLDOPTS))
    TARGET_DYNXLDOPTS=
    TARGET_XSHLDFLAGS+= -install_name $(PREFIX)/lib/$(TARGET_DYLIBNAME)
  endif
else
  TARGET_XLDFLAGS+= -Wl,-E
  ifeq (Linux,$(TARGET_SYS))
    TARGET_XLIBS+= -ldl
  endif
  ifeq (GNU/kFreeBSD,$(TARGET_SYS))
    TARGET_XLIBS+= -ldl
  endif
endif
endif

ifneq (,$(CCDEBUG))
  TARGET_STRIP= @:
endif

##############################################################################
# Files and pathnames.
##############################################################################

DASM_DIR= ../dynasm
DASM= $(HOST_LUA) $(DASM_DIR)/dynasm.lua
DASM_FLAGS=
DASM_DISTFLAGS= -LN
DASM_FLAGS_X86=
DASM_FLAGS_X64= -D X64
DASM_FLAGS_X64WIN= -D X64 -D X64WIN

BUILDVM_O= buildvm.o buildvm_asm.o buildvm_peobj.o buildvm_lib.o buildvm_fold.o
BUILDVM_T= buildvm
BUILDVM_X= ./$(BUILDVM_T)

HOST_O= $(BUILDVM_O)
HOST_T= $(BUILDVM_T)

LJVM_S= lj_vm.s
LJVM_O= lj_vm.o
LJVM_BOUT= $(LJVM_S)
LJVM_MODE= elfasm

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
LJVMCORE_DYNO= $(LJVMCORE_O:.o=_dyn.o)

LIB_VMDEF= ../lib/vmdef.lua
LIB_VMDEFP= $(LIB_VMDEF)

LUAJIT_O= luajit.o
LUAJIT_A= libluajit.a
LUAJIT_SO= libluajit.so
LUAJIT_T= luajit

ALL_T= $(LUAJIT_T) $(LUAJIT_A) $(LUAJIT_SO) $(BUILDVM_T)
ALL_GEN= $(LJVM_S) lj_ffdef.h lj_libdef.h lj_recdef.h $(LIB_VMDEFP) lj_folddef.h
ALL_DYNGEN= buildvm_*.h
WIN_RM= *.obj *.lib *.exp *.dll *.exe *.manifest *.pdb *.ilk
ALL_RM= $(ALL_T) $(ALL_GEN) *.o $(WIN_RM)

##############################################################################
# Build mode handling.
##############################################################################

# Mixed mode defaults.
TARGET_O= $(LUAJIT_A)
TARGET_T= $(LUAJIT_T) $(LUAJIT_SO)
TARGET_DEP= $(LIB_VMDEF) $(LUAJIT_SO)

ifeq (Windows,$(HOST_SYS))
  BUILDVM_T= buildvm.exe
  LIB_VMDEFP= $(subst /,\\,$(LIB_VMDEF))
endif
ifeq (Windows,$(TARGET_SYS))
  TARGET_DYNCC= $(STATIC_CC)
  LJVM_BOUT= $(LJVM_O)
  LJVM_MODE= peobj
  LUAJIT_SO= $(TARGET_DLLNAME)
  LUAJIT_T= luajit.exe
  ifneq ($(HOST_SYS),$(TARGET_SYS))
    HOST_XCFLAGS+= -malign-double
  endif
  # Mixed mode is not supported on Windows. And static mode doesn't work well.
  # C modules cannot be loaded, because they bind to lua51.dll.
  ifneq (static,$(BUILDMODE))
    BUILDMODE= dynamic
    TARGET_XCFLAGS+= -DLUA_BUILD_AS_DLL
  endif
endif
ifeq (Darwin,$(TARGET_SYS))
  LJVM_MODE= machasm
endif

ifeq (static,$(BUILDMODE))
  TARGET_DYNCC= @:
  TARGET_T= $(LUAJIT_T)
  TARGET_DEP= $(LIB_VMDEF)
else
ifeq (dynamic,$(BUILDMODE))
  ifneq (Windows,$(TARGET_SYS))
    TARGET_CC= $(DYNAMIC_CC)
  endif
  TARGET_DYNCC= @:
  LJVMCORE_DYNO= $(LJVMCORE_O)
  TARGET_O= $(LUAJIT_SO)
  TARGET_XLDFLAGS+= $(TARGET_DYNXLDOPTS)
else
ifeq (Darwin,$(TARGET_SYS))
  TARGET_DYNCC= @:
  LJVMCORE_DYNO= $(LJVMCORE_O)
endif
endif
endif

Q= @
E= @echo
#Q=
#E= @:

##############################################################################
# Make targets.
##############################################################################

default all:	$(TARGET_T)

amalg:
	@grep "^[+|]" ljamalg.c
	$(MAKE) all "LJCORE_O=ljamalg.o"

clean:
	$(HOST_RM) $(ALL_RM)

cleaner:
	$(HOST_RM) $(ALL_RM) $(ALL_DYNGEN)

distclean:	clean
	$(E) "DYNASM    $@"
	$(Q)$(DASM) $(DASM_DISTFLAGS) $(DASM_FLAGS_X86) -o buildvm_x86.h buildvm_x86.dasc
	$(Q)$(DASM) $(DASM_DISTFLAGS) $(DASM_FLAGS_X64) -o buildvm_x64.h buildvm_x86.dasc
	$(Q)$(DASM) $(DASM_DISTFLAGS) $(DASM_FLAGS_X64WIN) -o buildvm_x64win.h buildvm_x86.dasc

depend:
	@test -f lj_ffdef.h || touch lj_ffdef.h
	@test -f lj_libdef.h || touch lj_libdef.h
	@test -f lj_recdef.h || touch lj_recdef.h
	@test -f lj_folddef.h || touch lj_folddef.h
	@test -f buildvm_x86.h || touch buildvm_x86.h
	@test -f buildvm_x64.h || touch buildvm_x64.h
	@test -f buildvm_x64win.h || touch buildvm_x64win.h
	@$(HOST_CC) $(HOST_ACFLAGS) -MM *.c | sed "s|$(DASM_DIR)|\$$(DASM_DIR)|g" >Makefile.dep
	@test -s lj_ffdef.h || $(HOST_RM) lj_ffdef.h
	@test -s lj_libdef.h || $(HOST_RM) lj_libdef.h
	@test -s lj_recdef.h || $(HOST_RM) lj_recdef.h
	@test -s lj_folddef.h || $(HOST_RM) lj_folddef.h
	@test -s buildvm_x86.h || $(HOST_RM) buildvm_x86.h
	@test -s buildvm_x64.h || $(HOST_RM) buildvm_x64.h
	@test -s buildvm_x64win.h || $(HOST_RM) buildvm_x64win.h

.PHONY: default all amalg clean cleaner distclean depend

##############################################################################
# Rules for generated files.
##############################################################################

buildvm_x86.h: buildvm_x86.dasc
	$(E) "DYNASM    $@"
	$(Q)$(DASM) $(DASM_FLAGS) $(DASM_FLAGS_X86) -o $@ buildvm_x86.dasc

buildvm_x64.h: buildvm_x86.dasc
	$(E) "DYNASM    $@"
	$(Q)$(DASM) $(DASM_FLAGS) $(DASM_FLAGS_X64) -o $@ buildvm_x86.dasc

buildvm_x64win.h: buildvm_x86.dasc
	$(E) "DYNASM    $@"
	$(Q)$(DASM) $(DASM_FLAGS) $(DASM_FLAGS_X64WIN) -o $@ buildvm_x86.dasc

buildvm.o: buildvm_x86.h buildvm_x64.h buildvm_x64win.h

$(BUILDVM_T): $(BUILDVM_O)
	$(E) "HOSTLINK  $@"
	$(Q)$(HOST_CC) $(HOST_ALDFLAGS) -o $@ $(BUILDVM_O) $(HOST_ALIBS)

$(LJVM_BOUT): $(BUILDVM_T)
	$(E) "BUILDVM   $@"
	$(Q)$(BUILDVM_X) -m $(LJVM_MODE) -o $@

lj_ffdef.h: $(BUILDVM_T) $(LJLIB_C)
	$(E) "BUILDVM   $@"
	$(Q)$(BUILDVM_X) -m ffdef -o $@ $(LJLIB_C)

lj_libdef.h: $(BUILDVM_T) $(LJLIB_C)
	$(E) "BUILDVM   $@"
	$(Q)$(BUILDVM_X) -m libdef -o $@ $(LJLIB_C)

lj_recdef.h: $(BUILDVM_T) $(LJLIB_C)
	$(E) "BUILDVM   $@"
	$(Q)$(BUILDVM_X) -m recdef -o $@ $(LJLIB_C)

$(LIB_VMDEF): $(BUILDVM_T) $(LJLIB_C)
	$(E) "BUILDVM   $@"
	$(Q)$(BUILDVM_X) -m vmdef -o $(LIB_VMDEFP) $(LJLIB_C)

lj_folddef.h: $(BUILDVM_T) lj_opt_fold.c
	$(E) "BUILDVM   $@"
	$(Q)$(BUILDVM_X) -m folddef -o $@ lj_opt_fold.c

##############################################################################
# Object file rules.
##############################################################################

%.o: %.c
	$(E) "CC        $@"
	$(Q)$(TARGET_DYNCC) $(TARGET_ACFLAGS) -c -o $(@:.o=_dyn.o) $<
	$(Q)$(TARGET_CC) $(TARGET_ACFLAGS) -c -o $@ $<

%.o: %.s
	$(E) "ASM       $@"
	$(Q)$(TARGET_DYNCC) $(TARGET_ACFLAGS) -c -o $(@:.o=_dyn.o) $<
	$(Q)$(TARGET_CC) $(TARGET_ACFLAGS) -c -o $@ $<

$(LUAJIT_O):
	$(E) "CC        $@"
	$(Q)$(TARGET_STCC) $(TARGET_ACFLAGS) -c -o $@ $<

$(HOST_O): %.o: %.c
	$(E) "HOSTCC    $@"
	$(Q)$(HOST_CC) $(HOST_ACFLAGS) -c -o $@ $<

include Makefile.dep

##############################################################################
# Target file rules.
##############################################################################

$(LUAJIT_A): $(LJVMCORE_O)
	$(E) "AR        $@"
	$(Q)$(TARGET_AR) $@ $(LJVMCORE_O)

# The dependency on _O, but linking with _DYNO is intentional.
$(LUAJIT_SO): $(LJVMCORE_O)
	$(E) "DYNLINK   $@"
	$(Q)$(TARGET_LD) $(TARGET_ASHLDFLAGS) -o $@ $(LJVMCORE_DYNO) $(TARGET_ALIBS)
	$(Q)$(TARGET_STRIP) $@

$(LUAJIT_T): $(TARGET_O) $(LUAJIT_O) $(TARGET_DEP)
	$(E) "LINK      $@"
	$(Q)$(TARGET_LD) $(TARGET_ALDFLAGS) -o $@ $(LUAJIT_O) $(TARGET_O) $(TARGET_ALIBS)
	$(Q)$(TARGET_STRIP) $@
	$(E) "OK        Successfully built LuaJIT"

##############################################################################
