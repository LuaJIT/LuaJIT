@rem Script to build LuaJIT with NintendoSDK + NX Addon.
@rem Donated to the public domain.
@rem
@rem Open a "Visual Studio .NET Command Prompt" (64 bit host compiler)
@rem or "VS20xx x64 Native Tools Command Prompt".
@rem
@rem Then cd to this directory and run this script.
@rem
@rem Recommended invocation:
@rem
@rem nxbuild        release build, amalgamated
@rem nxbuild debug    debug build, amalgamated
@rem
@rem Additional command-line options (not generally recommended):
@rem
@rem nx32 (before debug)    32-bit target library
@rem noamalg (after debug)  non-amalgamated build

@if not defined INCLUDE goto :FAIL
@if not defined NINTENDO_SDK_ROOT goto :FAIL

@setlocal
@rem ---- Host compiler ----
@set LJCOMPILE=cl /nologo /c /MD /O2 /W3 /wo4146 /wo4244 /D_CRT_SECURE_NO_DEPRECATE
@set LJLINK=link /nologo
@set LJMT=mt /nologo
@set DASMDIR=..\dynasm
@set DASM=%DASMDIR%\dynasm.lua
@set ALL_LIB=lib_base.c lib_math.c lib_bit.c lib_string.c lib_table.c lib_io.c lib_os.c lib_package.c lib_debug.c lib_jit.c lib_ffi.c lib_buffer.c
@set DASC=vm_arm.dasc

%LJCOMPILE% host\minilua.c
@if errorlevel 1 goto :BAD
%LJLINK% /out:minilua.exe minilua.obj
@if errorlevel 1 goto :BAD
if exist minilua.exe.manifest^
  %LJMT% -manifest minilua.exe.manifest -outputresource:minilua.exe

@rem Check for 64 bit host compiler.
@minilua
@echo lol
@if not errorlevel 4 goto :FAIL
@echo lold
@set DASMFLAGS= -D HFABI -D FPU -D NO_UNWIND -D LUAJIT_TARGET=LUAJIT_ARCH_ARM -D LUAJIT_OS=LUAJIT_OS_OTHER -D LUAJIT_DISABLE_JIT -D LUAJIT_DISABLE_FFI
minilua %DASM% -LN %DASMFLAGS% -o host\buildvm_arch.h %DASC%
@if errorlevel 1 goto :BAD
%LJCOMPILE% /I "." /I %DASMDIR% -DLUAJIT_TARGET=LUAJIT_ARCH_ARM -D LJ_TARGET_NX -DLUAJIT_OS=LUAJIT_OS_OTHER -DLUAJIT_DISABLE_JIT -DLUAJIT_DISABLE_FFI -DLUAJIT_NO_UNWIND host\buildvm*.c
@if errorlevel 1 goto :BAD
%LJLINK% /out:buildvm.exe buildvm*.obj
@if errorlevel 1 goto :BAD
if exist buildvm.exe.manifest^
  %LJMT% -manifest buildvm.exe.manifest -outputresource:buildvm.exe

buildvm -m elfasm -o lj_vm.s
@if errorlevel 1 goto :BAD
buildvm -m bcdef -o lj_bcdef.h %ALL_LIB%
@if errorlevel 1 goto :BAD
buildvm -m ffdef -o lj_ffdef.h %ALL_LIB%
@if errorlevel 1 goto :BAD
buildvm -m libdef -o lj_libdef.h %ALL_LIB%
@if errorlevel 1 goto :BAD
buildvm -m recdef -o lj_recdef.h %ALL_LIB%
@if errorlevel 1 goto :BAD
buildvm -m vmdef -o jit\vmdef.lua %ALL_LIB%
@if errorlevel 1 goto :BAD
buildvm -m folddef -o lj_folddef.h lj_opt_fold.c
@if errorlevel 1 goto :BAD

@rem ---- Cross compiler ----
::@if "%1" neq "nx32" goto :NX32BUILD
::@set LJCOMPILE="%NINTENDO_SDK_ROOT%\Compilers\NX\nx\aarch64\bin\clang" -Wall -I%NINTENDO_SDK_ROOT%\Include -DLUAJIT_TARGET=LUAJIT_ARCH_ARM -DLUAJIT_OS=LUAJIT_OS_OTHER -DLUAJIT_DISABLE_JIT -DLUAJIT_DISABLE_FFI -DLUAJIT_USE_SYSMALLOC -DLUAJIT_NO_UNWIND -c
::@set LJLIB="%NINTENDO_SDK_ROOT%\Compilers\NX\nx\aarch64\bin\aarch64-nintendo-nx-elf-ar" rc
::@set TARGETLIB_SUFFIX="nx64"

::%NINTENDO_SDK_ROOT%\Compilers\NX\nx\aarch64\bin\aarch64-nintendo-nx-elf-as -o lj_vm.o lj_vm.s
::goto :DEBUGCHECK

:::NX32BUILD
@set LJCOMPILE="%NINTENDO_SDK_ROOT%\Compilers\NX\nx\armv7l\bin\clang" -Wall -I%NINTENDO_SDK_ROOT%\Include -DLUAJIT_TARGET=LUAJIT_ARCH_ARM -DLUAJIT_OS=LUAJIT_OS_OTHER -DLUAJIT_DISABLE_JIT -DLUAJIT_DISABLE_FFI -DLUAJIT_USE_SYSMALLOC -DLUAJIT_NO_UNWIND -c
@set LJLIB="%NINTENDO_SDK_ROOT%\Compilers\NX\nx\armv7l\bin\armv7l-nintendo-nx-eabihf-ar" rc
@set TARGETLIB_SUFFIX="nx32"

%NINTENDO_SDK_ROOT%\Compilers\NX\nx\armv7l\bin\armv7l-nintendo-nx-eabihf-as -o lj_vm.o lj_vm.s
:DEBUGCHECK

@if "%1" neq "debug" goto :NODEBUG
@shift
@set LJCOMPILE=%LJCOMPILE% -DNN_SDK_BUILD_DEBUG -g -O0
@set TARGETLIB=libluajitD_%TARGETLIB_SUFFIX%.a
goto :BUILD
:NODEBUG
@set LJCOMPILE=%LJCOMPILE% -DNN_SDK_BUILD_RELEASE -O3
@set TARGETLIB=libluajit_%TARGETLIB_SUFFIX%.a
:BUILD
del %TARGETLIB%
@if "%1" neq "noamalg" goto :AMALG
for %%f in (lj_*.c lib_*.c) do (
  %LJCOMPILE% %%f
  @if errorlevel 1 goto :BAD
)

%LJLIB% %TARGETLIB% lj_*.o lib_*.o
@if errorlevel 1 goto :BAD
@goto :NOAMALG
:AMALG
%LJCOMPILE% ljamalg.c
@if errorlevel 1 goto :BAD
%LJLIB% %TARGETLIB% ljamalg.o lj_vm.o
@if errorlevel 1 goto :BAD
:NOAMALG

@del *.o *.obj *.manifest minilua.exe buildvm.exe
@echo.
@echo === Successfully built LuaJIT for Nintendo Switch ===

@goto :END
:BAD
@echo.
@echo *******************************************************
@echo *** Build FAILED -- Please check the error messages ***
@echo *******************************************************
@goto :END
:FAIL
@echo To run this script you must open a "Visual Studio .NET Command Prompt"
@echo (64 bit host compiler). NintendoSDK + NX Addon must be installed, too.
:END