# Installing moonjit

Moonjit should build out of the box on Linux, BSD, Android, Windows 8/10 and
OSX.  The following architectures are currently supported:

* x64
* x86
* arm64
* armhf
* ppc (big-endian)
* ppc64le
* s390x
* mips32
* mips64

## Configuring moonjit builds

The standard configuration should work fine for most installations. Usually
there is no need to tweak the settings. The following files hold all
user-configurable settings:

* `src/luaconf.h` sets some configuration variables.
* `Makefile` has settings for installing moonjit (POSIX only).
* `src/Makefile` has settings for compiling moonjit under POSIX, MinGW or
   Cygwin.
* `src/msvcbuild.bat` has settings for compiling moonjit with MSVC.

Please read the instructions given in these files, before changing any settings.

Moonjit on x64 currently uses 32 bit GC objects by default. `LJ_GC64` mode may
be explicitly enabled: add `XCFLAGS=-DLUAJIT_ENABLE_GC64` to the make command
or run `msvcbuild gc64` for MSVC. Please check the note about the
[bytecode format differences](doc/extensions.md#string_dump), too.

## POSIX Systems (Linux, OSX, *BSD etc.)

Depending on your distribution, you may need to install a package for GCC, the
development headers and/or a complete SDK.  Typically a `dnf install gcc make`
or `apt-get install gcc make` ought to be suficient.

Download and unpack the current source package of moonjit:

```
tar zxf moonjit-2.2.0.tar.gz
cd moonjit-2.2.0
```

or clone the repo from github:

```
git clone https://github.com/moonjit/moonjit
cd moonjit
```

### Building moonjit

The supplied Makefiles try to auto-detect the settings needed for your
operating system and your compiler. They need to be run with GNU Make, which is
probably the default on your system, anyway. Simply run:

```
make
```

This always builds a native binary, depending on the host OS you're running
this command on. Check the section on cross-compilation for more options.

By default, modules are only searched under the prefix `/usr/local`. You can
add an extra prefix to the search paths by appending the `PREFIX` option, e.g.:

```
make PREFIX=/home/myself/lj2
```

**Note for OSX**: You must set the `MACOSX_DEPLOYMENT_TARGET` environment
variable to a value supported by your toolchain.

### Installing moonjit

The top-level Makefile installs moonjit by default under `/usr/local`, i.e. the
executable ends up in `/usr/local/bin` and so on. You need root privileges to
write to this path. So, assuming sudo is installed on your system, run the
following command and enter your sudo password:

```
sudo make install
```

Otherwise specify the directory prefix as an absolute path, e.g.:

```
make install PREFIX=/home/myself/lj2
```

Obviously the prefixes given during build and installation need to be the same.

## Windows Systems

Either install one of the open source SDKs (e.g. [MinGW](http://mingw.org), [Cygwin](https://cygwin.com) or [MSYS2](https://www.msys2.org/)), which come with a modified GCC plus the required development headers or use Microsoft's Visual C++ (MSVC) by installing Microsoft Visual Studio.

Next, download the source package and unpack it using an archive manager (e.g.
the Windows Explorer) to a directory of your choice.

### Building with MSVC

Open a x86 or x64 Visual Studio command prompt (e.g. "x64 Native Tools Command Prompt for VS 2019"), cd to the directory where you've unpacked the sources and run these commands:

```
cd src
msvcbuild
```

Please note that Visual Studio 2015 or newer is recommended.

The script `msvcbuild` accepts different arguments (only one at a time) to customize the build process:

* `gc64`: enable `LJ_GC64` mode.
* `debug`: generate debug information.
* `amalg`: enable amalgamated build.
* `static`: create a static library and link `luajit.exe` statically.

Then follow the installation instructions below.

### Building with MinGW, Cygwin or MSYS2

Moonjit can also be compiled using MinGW, Cygwin or MSYS2 and the [POSIX Makefile](#posix-systems-linux-osx-bsd-etc). Please consult the corresponding documentation of your toolchain for further information.

Then follow the installation instructions below.

### Installing moonjit

Copy `luajit.exe` and (if not built statically) `lua51.dll` (built in the `src` directory) to a newly
created directory (any location is ok). Add `lua` and `lua\jit` directories
below it and copy all Lua files from the `src\jit` directory of the
distribution to the latter directory.

There are no hardcoded absolute path names — all modules are loaded relative to
the directory where `luajit.exe` is installed (see `src/luaconf.h`).

## Cross-compiling moonjit

First, let's clear up some terminology:

* `Host`: This is your development system, usually based on a x64 or x86 CPU.
* `Target`: This is the target system you want moonjit to run on, e.g.
  Android/ARM.
* `Toolchain`: This comprises a C compiler, linker, assembler and a matching C
  library.
* `Host (or system) toolchain`: This is the toolchain used to build native
  binaries for your host system.
* `Cross-compile toolchain`: This is the toolchain used to build binaries for
  the target system. They can only be run on the target system.

The GNU Makefile-based build system allows cross-compiling on any host for any
supported target:

* Yes, you need a toolchain for both your host and your target!
* Both host and target architectures must have the same pointer size.
* E.g. if you want to cross-compile to a 32 bit target on a 64 bit host, you
  need to install the multilib development package (e.g. `libc6-dev-i386` on
  Debian/Ubuntu) and build a 32 bit host part (`HOST_CC="gcc -m32"`).
* 64 bit targets always require compilation on a 64 bit host.

You need to specify `TARGET_SYS` whenever the host OS and the target OS differ,
or you'll get assembler or linker errors:

* E.g. if you're compiling on a Windows or OSX host for embedded Linux or
  Android, you need to add TARGET_SYS=Linux to the examples below.
* For a minimal target OS, you may need to disable the built-in allocator in
  src/Makefile and use TARGET_SYS=Other.
* Don't forget to specify the same TARGET_SYS for the install step, too.

Here are some examples where host and target have the same CPU:

```
# Cross-compile to a 32 bit binary on a multilib x64 OS
make CC="gcc -m32"

# Cross-compile on Debian/Ubuntu for Windows (mingw32 package)
make HOST_CC="gcc -m32" CROSS=i586-mingw32msvc- TARGET_SYS=Windows
```

The `CROSS` prefix allows specifying a standard GNU cross-compile toolchain
(Binutils, GCC and a matching libc). The prefix may vary depending on the
--target the toolchain was built for (note the `CROSS` prefix has a trailing
"-"). The examples below use the canonical toolchain triplets for Linux.

Since there's often no easy way to detect CPU features at runtime, it's
important to compile with the proper CPU or architecture settings:

* The best way to get consistent results is to specify the correct settings
  when building the toolchain yourself.
* For a pre-built, generic toolchain add `-mcpu=...` or `-march=...` and other
  necessary flags to `TARGET_CFLAGS`.
* For ARM it's important to have the correct `-mfloat-abi=...` setting, too.
  Otherwise moonjit may not run at the full performance of your target CPU.
* For MIPS it's important to select a supported ABI (`o32` on MIPS32, `n64` on
  MIPS64) and consistently compile your project either with hard-float or
  soft-float compiler settings.

Here are some examples for targets with a different CPU than the host:

```
# ARM soft-float
make HOST_CC="gcc -m32" CROSS=arm-linux-gnueabi- \
     TARGET_CFLAGS="-mfloat-abi=soft"

# ARM soft-float ABI with VFP (example for Cortex-A9)
make HOST_CC="gcc -m32" CROSS=arm-linux-gnueabi- \
     TARGET_CFLAGS="-mcpu=cortex-a9 -mfloat-abi=softfp"

# ARM hard-float ABI with VFP (armhf, most modern toolchains)
make HOST_CC="gcc -m32" CROSS=arm-linux-gnueabihf-

# ARM64
make CROSS=aarch64-linux-

# PPC
make HOST_CC="gcc -m32" CROSS=powerpc-linux-gnu-

# MIPS32 big-endian
make HOST_CC="gcc -m32" CROSS=mips-linux-
# MIPS32 little-endian
make HOST_CC="gcc -m32" CROSS=mipsel-linux-

# MIPS64 big-endian
make CROSS=mips-linux- TARGET_CFLAGS="-mips64r2 -mabi=64"
# MIPS64 little-endian
make CROSS=mipsel-linux- TARGET_CFLAGS="-mips64r2 -mabi=64"
```

You can cross-compile for **Android** using the [Android
NDK](http://developer.android.com/ndk/).  Please adapt the environment
variables to match the install locations and the desired target platform. E.g.
Android 4.1 corresponds to ABI level 16.

```
# Android/ARM64, aarch64, Android 5.0+ (L)
NDKDIR=/opt/android/ndk
NDKBIN=$NDKDIR/toolchains/llvm/prebuilt/linux-x86_64/bin
NDKCROSS=$NDKBIN/aarch64-linux-android-
NDKCC=$NDKBIN/aarch64-linux-android21-clang
make CROSS=$NDKCROSS \
     STATIC_CC=$NDKCC DYNAMIC_CC="$NDKCC -fPIC" \
     TARGET_LD=$NDKCC

# Android/ARM, armeabi-v7a (ARMv7 VFP), Android 4.1+ (JB)
NDKDIR=/opt/android/ndk
NDKBIN=$NDKDIR/toolchains/llvm/prebuilt/linux-x86_64/bin
NDKCROSS=$NDKBIN/arm-linux-androideabi-
NDKCC=$NDKBIN/armv7a-linux-androideabi16-clang
make HOST_CC="gcc -m32" CROSS=$NDKCROSS \
     STATIC_CC=$NDKCC DYNAMIC_CC="$NDKCC -fPIC" \
     TARGET_LD=$NDKCC
```

### Cross-compiling for consoles

Building moonjitfor consoles requires both a supported host compiler (x86 or
x64) and a cross-compiler (to PPC or ARM) from the official console SDK.

Due to restrictions on consoles, the JIT compiler is disabled and only the fast
interpreter is built. This is still faster than plain Lua, but much slower than
the JIT compiler. The FFI is disabled, too, since it's not very useful in such
an environment.

The following commands build a static library `libluajit.a`, which can be
linked against your game, just like the Lua library.

To cross-compile for PS3 from a Linux host (requires 32 bit GCC, i.e. multilib
Linux/x64) or a Windows host (requires 32 bit MinGW), run this command:

```
make HOST_CC="gcc -m32" CROSS=ppu-lv2-
```

To cross-compile for PS4 from a Windows host, open a "Visual Studio .NET
Command Prompt" (64 bit host compiler), cd to the directory where you've
unpacked the sources and run the following commands:

```
cd src
ps4build
```

To cross-compile for PS Vita from a Windows host, open a "Visual Studio .NET
Command Prompt" (32 bit host compiler), cd to the directory where you've
unpacked the sources and run the following commands:

```
cd src
psvitabuild
```

To cross-compile for Xbox 360 from a Windows host, open a "Visual Studio .NET
Command Prompt" (32 bit host compiler), cd to the directory where you've
unpacked the sources and run the following commands:

```
cd src
xedkbuild
```

To cross-compile for Xbox One from a Windows host, open a "Visual Studio .NET
Command Prompt" (64 bit host compiler), cd to the directory where you've
unpacked the sources and run the following commands:

```
cd src
xb1build
```

## Embedding moonjit

moonjit is API-compatible with Lua 5.1. If you've already embedded Lua into
your application, you probably don't need to do anything to switch to moonjit,
except link with a different library:

* It's strongly suggested to build moonjit separately using the supplied build
  system. Please do not attempt to integrate the individual source files into
  your build tree. You'll most likely get the internal build dependencies wrong
  or mess up the compiler flags. Treat moonjit like any other external library
  and link your application with either the dynamic or static library, depending
  on your needs.
* If you want to load C modules compiled for plain Lua with `require()`, you need
  to make sure the public symbols (e.g. `lua_pushnumber`) are exported, too:
* On POSIX systems you can either link to the shared library or link the static
  library into your application. In the latter case you'll need to export all
  public symbols from your main executable (e.g. `-Wl,-E` on Linux) and add the
  external dependencies (e.g. `-lm -ldl` on Linux).
* Since Windows symbols are bound to a specific DLL name, you need to link to
  the `lua51.dll` created by the moonjit build (do not rename the DLL). You may
  link moonjit statically on Windows only if you don't intend to load Lua/C
  modules at runtime.
* If you're building a 64 bit application on OSX which links directly or
  indirectly against moonjit which is not built for `LJ_GC64` mode, you need to
  link your main executable with these flags: `-pagezero_size 10000 -image_base 100000000`

Additional hints for initializing moonjit using the C API functions:

* Here's a [simple example](http://lua-users.org/wiki/SimpleLuaApiExample) for
  embedding Lua or moonjit into your application.
* Make sure you use `luaL_newstate`. Avoid using `lua_newstate`, since this
  uses the (slower) default memory allocator from your system (no support for
  this on x64).
* Make sure you use `luaL_openlibs` and not the old Lua 5.0 style of calling
  `luaopen_base` etc. directly.
* To change or extend the list of standard libraries to load, copy
  `src/lib_init.c` to your project and modify it accordingly. Make sure the jit
  library is loaded or the JIT compiler will not be activated.
* The `bit.*` module for bitwise operations is already built-in. There's no
  need to statically link [Lua BitOp](http://bitop.luajit.org) to your
  application.

## Hints for Distribution Maintainers

The moonjit build system has extra provisions for the needs of most POSIX-based
distributions. If you're a package maintainer for a distribution, please make
use of these features and avoid patching, subverting, autotoolizing or messing
up the build system in unspeakable ways.

There should be absolutely no need to patch `luaconf.h` or any of the
Makefiles.  And please do not hand-pick files for your packages — simply use
whatever make install creates. There's a reason for all of the files and
directories it creates.

The build system uses GNU make and auto-detects most settings based on the host
you're building it on. This should work fine for native builds, even when
sandboxed. You may need to pass some of the following flags to both the make
and the make install command lines for a regular distribution build:

* `PREFIX` overrides the installation path and should usually be set to `/usr`.
  Setting this also changes the module paths and the paths needed to locate the
  shared library.
* `DESTDIR` is an absolute path which allows you to install to a shadow tree
  instead of the root tree of the build system.
* `MULTILIB` sets the architecture-specific library path component for multilib
  systems. The default is `lib`.
* Have a look at the top-level `Makefile` and `src/Makefile` for additional
  variables to tweak. The following variables may be overridden, but it's not
  recommended, except for special needs like cross-builds: `BUILDMODE, CC,
  HOST_CC, STATIC_CC, DYNAMIC_CC, CFLAGS, HOST_CFLAGS, TARGET_CFLAGS, LDFLAGS,
  HOST_LDFLAGS, TARGET_LDFLAGS, TARGET_SHLDFLAGS, TARGET_FLAGS, LIBS, HOST_LIBS,
  TARGET_LIBS, CROSS, HOST_SYS, TARGET_SYS`

The build system has a special target for an amalgamated build, i.e. `make
amalg`. This compiles the moonjit core as one huge C file and allows GCC to
generate faster and shorter code. Alas, this requires lots of memory during the
build. This may be a problem for some users, that's why it's not enabled by
default. But it shouldn't be a problem for most build farms. It's recommended
that binary distributions use this target for their moonjit builds.

The tl;dr version of the above:

```
make amalg PREFIX=/usr && \
make install PREFIX=/usr DESTDIR=/tmp/buildroot
```
