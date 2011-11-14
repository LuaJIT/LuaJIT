##############################################################################
# LuaJIT Makefile. Requires GNU Make.
#
# Please read doc/install.html before changing any variables!
#
# Suitable for POSIX platforms (Linux, *BSD, OSX etc.).
# Also works with MinGW and Cygwin on Windows.
# Please check msvcbuild.bat for building with MSVC on Windows.
#
# Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
##############################################################################

MAJVER=  2
MINVER=  0
RELVER=  0
ABIVER=  5.1
NODOTABIVER= 51

##############################################################################
#############################  COMPILER OPTIONS  #############################
##############################################################################
# These options mainly affect the speed of the JIT compiler itself, not the
# speed of the JIT-compiled code. Turn any of the optional settings on by
# removing the '#' in front of them. Make sure you force a full recompile
# with "make clean", followed by "make" if you change any options.
#
# LuaJIT builds as a native 32 or 64 bit binary by default.
CC= gcc
#
# Use this if you want to force a 32 bit build on a 64 bit multilib OS.
#CC= gcc -m32
#
# Since the assembler part does NOT maintain a frame pointer, it's pointless
# to slow down the C part by not omitting it. Debugging, tracebacks and
# unwinding are not affected -- the assembler part has frame unwind
# information and GCC emits it where needed (x64) or with -g (see CCDEBUG).
CCOPT= -O2 -fomit-frame-pointer
# Use this if you want to generate a smaller binary (but it's slower):
#CCOPT= -Os -fomit-frame-pointer
# Note: it's no longer recommended to use -O3 with GCC 4.x.
# The I-Cache bloat usually outweighs the benefits from aggressive inlining.
#
# Target-specific compiler options:
#
# x86 only: it's recommended to compile at least for i686. By default the
# assembler part of the interpreter makes use of CMOV/FCOMI*/FUCOMI*
# instructions, anyway.
#
# x86/x64 only: For GCC 4.2 or higher and if you don't intend to distribute
# the binaries to a different machine you could also use: -march=native
#
CCOPT_X86= -march=i686
CCOPT_X64=
CCOPT_ARM=
CCOPT_PPC=
CCOPT_PPCSPE=
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
################################  BUILD MODE  ################################
##############################################################################
# The default build mode is mixed mode on POSIX. On Windows this is the same
# as dynamic mode.
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
#
##############################################################################

##############################################################################
#################################  FEATURES  #################################
##############################################################################
# Enable/disable these features as needed, but make sure you force a full
# recompile with "make clean", followed by "make".
XCFLAGS=
#
# Permanently disable the FFI extension to reduce the size of the LuaJIT
# executable. But please consider that the FFI library is compiled-in,
# but NOT loaded by default. It only allocates any memory, if you actually
# make use of it.
#XCFLAGS+= -DLUAJIT_DISABLE_FFI
#
# Enable some upwards-compatible features from Lua 5.2 that are unlikely
# to break existing code (e.g. __pairs). Note that this does not provide
# full compatibility with Lua 5.2 at this time.
#XCFLAGS+= -DLUAJIT_ENABLE_LUA52COMPAT
#
# Disable the JIT compiler, i.e. turn LuaJIT into a pure interpreter.
#XCFLAGS+= -DLUAJIT_DISABLE_JIT
#
# x86 only: use SSE2 instead of x87 instructions in the interpreter
# (always enabled for x64). A pure interpreter built with this flag won't
# run on older CPUs (before P4 or K8). There isn't much of a speed
# difference, so this is not enabled by default.
# The JIT compiler is not affected by this flag. It always uses runtime
# CPU feature detection before emitting code for SSE2 up to SSE4.1.
#XCFLAGS+= -DLUAJIT_CPU_SSE2
#
# x86 only: Disable the use of CMOV and FCOMI*/FUCOMI* instructions in the
# interpreter. Do this only if you intend to use REALLY ANCIENT CPUs
# (before Pentium Pro, or on the VIA C3). This generally slows down the
# interpreter. Don't bother if your OS wouldn't run on them, anyway.
#XCFLAGS+= -DLUAJIT_CPU_NOCMOV
#
# Some architectures (e.g. PPC) can use either single-number (1) or
# dual-number (2) mode. Uncomment one of these lines to override the
# default mode. Please see LJ_ARCH_NUMMODE in lj_arch.h for details.
#XCFLAGS+= -DLUAJIT_NUMMODE=1
#XCFLAGS+= -DLUAJIT_NUMMODE=2
#
##############################################################################

##############################################################################
############################  DEBUGGING SUPPORT  #############################
##############################################################################
# Enable these options as needed, but make sure you force a full recompile
# with "make clean", followed by "make".
# Note that most of these are NOT suitable for benchmarking or release mode!
#
# Use the system provided memory allocator (realloc) instead of the
# bundled memory allocator. This is slower, but sometimes helpful for
# debugging. It's helpful for Valgrind's memcheck tool, too. This option
# cannot be enabled on x64, since the built-in allocator is mandatory.
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
# You probably don't need to change anything below this line!
##############################################################################

##############################################################################
# Flags and options for host and target.
##############################################################################

# You can override the following variables at the make command line:
#   CC       HOST_CC       STATIC_CC       DYNAMIC_CC
#   CFLAGS   HOST_CFLAGS   TARGET_CFLAGS
#   LDFLAGS  HOST_LDFLAGS  TARGET_LDFLAGS  TARGET_SHLDFLAGS
#   LIBS     HOST_LIBS     TARGET_LIBS
#   CROSS    HOST_SYS      TARGET_SYS      TARGET_FLAGS
#
# Cross-compilation examples:
#   make HOST_CC="gcc -m32" CROSS=i586-mingw32msvc- TARGET_SYS=Windows
#   make HOST_CC="gcc -m32" CROSS=powerpc-e500v2-linux-gnuspe- TARGET=ppcspe

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
TARGET_DYLIBNAME= libluajit-$(NODOTABIVER).$(MAJVER).dylib
TARGET_DYLIBPATH= $(or $(PREFIX),/usr/local)/lib/$(TARGET_DYLIBNAME)
TARGET_DLLNAME= lua$(NODOTABIVER).dll
TARGET_XSHLDFLAGS= -shared -fPIC -Wl,-soname,$(TARGET_SONAME)
TARGET_DYNXLDOPTS=

TARGET_XCFLAGS= -D_FILE_OFFSET_BITS=64 -D_LARGEFILE_SOURCE -U_FORTIFY_SOURCE
TARGET_XLDFLAGS=
TARGET_XLIBS= -lm
TARGET_ACFLAGS= $(CCOPTIONS) $(TARGET_XCFLAGS) $(TARGET_ARCH) $(TARGET_FLAGS) $(TARGET_CFLAGS)
TARGET_ALDFLAGS= $(LDOPTIONS) $(TARGET_XLDFLAGS) $(TARGET_FLAGS) $(TARGET_LDFLAGS)
TARGET_ASHLDFLAGS= $(LDOPTIONS) $(TARGET_XSHLDFLAGS) $(TARGET_FLAGS) $(TARGET_SHLDFLAGS)
TARGET_ALIBS= $(TARGET_XLIBS) $(LIBS) $(TARGET_LIBS)

ifneq (,$(findstring stack-protector,$(shell $(TARGET_CC) -dumpspecs)))
  TARGET_XCFLAGS+= -fno-stack-protector
endif

TARGET_TESTARCH=$(shell $(TARGET_CC) $(TARGET_ACFLAGS) -E lj_arch.h -dM)
ifneq (,$(findstring LJ_TARGET_X64 ,$(TARGET_TESTARCH)))
  TARGET_CCARCH= x64
  TARGET_XCFLAGS+= $(CCOPT_X64)
else
ifneq (,$(findstring LJ_TARGET_X86 ,$(TARGET_TESTARCH)))
  TARGET_CCARCH= x86
  TARGET_XCFLAGS+= $(CCOPT_X86)
else
ifneq (,$(findstring LJ_TARGET_ARM ,$(TARGET_TESTARCH)))
  TARGET_CCARCH= arm
  TARGET_XCFLAGS+= $(CCOPT_ARM)
else
ifneq (,$(findstring LJ_TARGET_PPC ,$(TARGET_TESTARCH)))
  TARGET_CCARCH= ppc
  TARGET_XCFLAGS+= $(CCOPT_PPC)
else
ifneq (,$(findstring LJ_TARGET_PPCSPE ,$(TARGET_TESTARCH)))
  TARGET_CCARCH= ppcspe
  TARGET_XCFLAGS+= $(CCOPT_PPCSPE)
else
  $(error Unsupported target architecture)
endif
endif
endif
endif
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
  TARGET_DYNXLDOPTS=
  TARGET_XSHLDFLAGS+= -install_name $(TARGET_DYLIBPATH) -compatibility_version $(MAJVER).$(MINVER) -current_version $(MAJVER).$(MINVER).$(RELVER)
  ifeq (x64,$(TARGET_CCARCH))
    TARGET_XLDFLAGS+= -pagezero_size 10000 -image_base 100000000
    TARGET_XSHLDFLAGS+= -image_base 7fff04c4a000
  endif
else
ifeq (iOS,$(TARGET_SYS))
  TARGET_STRIP+= -x
  TARGET_AR+= 2>/dev/null
  TARGET_XSHLDFLAGS= -dynamiclib -single_module -undefined dynamic_lookup -fPIC
  TARGET_DYNXLDOPTS=
  TARGET_XSHLDFLAGS+= -install_name $(TARGET_DYLIBPATH) -compatibility_version $(MAJVER).$(MINVER) -current_version $(MAJVER).$(MINVER).$(RELVER)
else
  ifneq (SunOS,$(TARGET_SYS))
    TARGET_XLDFLAGS+= -Wl,-E
  endif
  ifeq (Linux,$(TARGET_SYS))
    TARGET_XLIBS+= -ldl
  endif
  ifeq (GNU/kFreeBSD,$(TARGET_SYS))
    TARGET_XLIBS+= -ldl
  endif
endif
endif
endif

ifneq ($(HOST_SYS),$(TARGET_SYS))
  ifeq (Windows,$(TARGET_SYS))
    HOST_XCFLAGS+= -malign-double -DLUAJIT_OS=LUAJIT_OS_WINDOWS
  else
  ifeq (Linux,$(TARGET_SYS))
    HOST_XCFLAGS+= -DLUAJIT_OS=LUAJIT_OS_LINUX
  else
  ifeq (Darwin,$(TARGET_SYS))
    HOST_XCFLAGS+= -DLUAJIT_OS=LUAJIT_OS_OSX
  else
  ifeq (iOS,$(TARGET_SYS))
    HOST_XCFLAGS+= -DLUAJIT_OS=LUAJIT_OS_OSX
  else
    HOST_XCFLAGS+= -DLUAJIT_OS=LUAJIT_OS_OTHER
  endif
  endif
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
DASM_FLAGS_ARM=
DASM_FLAGS_PPC=
DASM_FLAGS_PPCSPE=

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
	 lib_io.o lib_os.o lib_package.o lib_debug.o lib_jit.o lib_ffi.o
LJLIB_C= $(LJLIB_O:.o=.c)

LJCORE_O= lj_gc.o lj_err.o lj_char.o lj_bc.o lj_obj.o \
	  lj_str.o lj_tab.o lj_func.o lj_udata.o lj_meta.o lj_debug.o \
	  lj_state.o lj_dispatch.o lj_vmevent.o lj_vmmath.o lj_api.o \
	  lj_lex.o lj_parse.o lj_bcread.o lj_bcwrite.o \
	  lj_ir.o lj_opt_mem.o lj_opt_fold.o lj_opt_narrow.o \
	  lj_opt_dce.o lj_opt_loop.o lj_opt_split.o \
	  lj_mcode.o lj_snap.o lj_record.o lj_crecord.o lj_ffrecord.o \
	  lj_asm.o lj_trace.o lj_gdbjit.o \
	  lj_ctype.o lj_cdata.o lj_cconv.o lj_ccall.o lj_ccallback.o \
	  lj_carith.o lj_clib.o lj_cparse.o \
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
ALL_HDRGEN= lj_bcdef.h lj_ffdef.h lj_libdef.h lj_recdef.h lj_folddef.h
ALL_GEN= $(LJVM_S) $(ALL_HDRGEN) $(LIB_VMDEFP)
ALL_DYNGEN= buildvm_x86.h buildvm_x64.h buildvm_x64win.h buildvm_arm.h \
	    buildvm_ppc.h buildvm_ppcspe.h
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
  LJVM_MODE= coffasm
  LUAJIT_SO= $(TARGET_DLLNAME)
  LUAJIT_T= luajit.exe
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
ifeq (iOS,$(TARGET_SYS))
  LJVM_MODE= machasm
endif
ifeq (SunOS,$(TARGET_SYS))
  BUILDMODE= static
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
ifeq (iOS,$(TARGET_SYS))
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
	$(Q)$(DASM) $(DASM_DISTFLAGS) $(DASM_FLAGS_ARM) -o buildvm_arm.h buildvm_arm.dasc
	$(Q)$(DASM) $(DASM_DISTFLAGS) $(DASM_FLAGS_PPC) -o buildvm_ppc.h buildvm_ppc.dasc
	$(Q)$(DASM) $(DASM_DISTFLAGS) $(DASM_FLAGS_PPCSPE) -o buildvm_ppcspe.h buildvm_ppcspe.dasc

depend:
	@for file in $(ALL_HDRGEN) $(ALL_DYNGEN); do \
	  test -f $$file || touch $$file; \
	  done
	@$(HOST_CC) $(HOST_ACFLAGS) -MM *.c | \
	  sed -e "s| [^ ]*/dasm_\S*\.h||g" \
	      -e "s| buildvm_\S*\.h||g" \
	      -e "s| lj_target_\S*\.h| lj_target_*.h|g" \
	      -e "s| lj_emit_\S*\.h| lj_emit_*.h|g" \
	      -e "s| lj_asm_\S*\.h| lj_asm_*.h|g" >Makefile.dep
	@for file in $(ALL_HDRGEN) $(ALL_DYNGEN); do \
	  test -s $$file || $(HOST_RM) $$file; \
	  done

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

buildvm_arm.h: buildvm_arm.dasc
	$(E) "DYNASM    $@"
	$(Q)$(DASM) $(DASM_FLAGS) $(DASM_FLAGS_ARM) -o $@ buildvm_arm.dasc

buildvm_ppc.h: buildvm_ppc.dasc
	$(E) "DYNASM    $@"
	$(Q)$(DASM) $(DASM_FLAGS) $(DASM_FLAGS_PPC) -o $@ buildvm_ppc.dasc

buildvm_ppcspe.h: buildvm_ppcspe.dasc
	$(E) "DYNASM    $@"
	$(Q)$(DASM) $(DASM_FLAGS) $(DASM_FLAGS_PPCSPE) -o $@ buildvm_ppcspe.dasc

buildvm.o: $(ALL_DYNGEN) $(DASM_DIR)/dasm_*.h

$(BUILDVM_T): $(BUILDVM_O)
	$(E) "HOSTLINK  $@"
	$(Q)$(HOST_CC) $(HOST_ALDFLAGS) -o $@ $(BUILDVM_O) $(HOST_ALIBS)

$(LJVM_BOUT): $(BUILDVM_T)
	$(E) "BUILDVM   $@"
	$(Q)$(BUILDVM_X) -m $(LJVM_MODE) -o $@

lj_bcdef.h: $(BUILDVM_T) $(LJLIB_C)
	$(E) "BUILDVM   $@"
	$(Q)$(BUILDVM_X) -m bcdef -o $@ $(LJLIB_C)

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
