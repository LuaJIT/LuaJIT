@rem Script to build LuaJIT for UWP.
@rem
@rem Open a command prompt.
@rem Then cd to this directory and run this script.

@setlocal
@echo ---- Host compiler ----
@set LJCOMPILE=cl /nologo /c /W3 /D_CRT_SECURE_NO_DEPRECATE /D_CRT_STDIO_INLINE=__declspec(dllexport)__inline /D_UWP
@set LJHOSTCOMPILE=/MD /O2
@set LJLINK=link /nologo
@set LJMT=mt /nologo
@set LJLIB=lib /nologo /nodefaultlib
@set DASMDIR=..\dynasm
@set DASM=%DASMDIR%\dynasm.lua
@set LJDLLNAME=lua51.dll
@set LJLIBNAME=lua51.lib
@set ALL_LIB=lib_base.c lib_math.c lib_bit.c lib_string.c lib_table.c lib_io.c lib_os.c lib_package.c lib_debug.c lib_jit.c lib_ffi.c
@set CWD=%cd%

@for /f "tokens=*" %%i in ('"%ProgramFiles(x86)%\Microsoft Visual Studio\Installer\vswhere.exe" -latest -property installationPath') do set VSPATH=%%i

:ARCHSWITCH
@if "%1" == "x86" goto :X86
@if "%1" == "x64" goto :X64
@if "%1" == "arm" goto :ARM
@if "%1" == "arm64" goto:ARM64
@goto :BAD

:X86
@set DASMFLAGS=-D WIN -D JIT -D FFI
@set DASC=vm_x86.dasc
@REM Target architecture
@set LJARCH=x86
@REM Target tool architecture passed to vcvarsall.bat
@set VCTARGETARCH=x86
@REM Host tool architecture passed to vcvarsall.bat
@set VCHOSTARCH=x86
@set LJCOMPILE=%LJCOMPILE% /DLUAJIT_TARGET=LUAJIT_ARCH_X86 /arch:SSE2
@goto :HOSTBUILD

:X64
@set DASMFLAGS=-D WIN -D JIT -D FFI -D P64
@set DASC=vm_x64.dasc
@set LJARCH=x64
@set VCTARGETARCH=x64
@set VCHOSTARCH=x64
@set LJCOMPILE=%LJCOMPILE% /DLUAJIT_ENABLE_GC64 /DLUAJIT_TARGET=LUAJIT_ARCH_X64
@goto :HOSTBUILD

:ARM
@set DASMFLAGS=-D WIN -D JIT -D FFI
@set DASC=vm_arm.dasc
@set LJARCH=arm
@set VCTARGETARCH=x86_arm
@set VCHOSTARCH=x86
@set LJCOMPILE=%LJCOMPILE% /DLUAJIT_TARGET=LUAJIT_ARCH_ARM
@goto :HOSTBUILD

:ARM64
@set DASMFLAGS=-D WIN -D JIT -D FFI -D P64
@set DASC=vm_arm64.dasc
@set LJARCH=arm64
@set VCTARGETARCH=x64_arm64
@set VCHOSTARCH=x64
@set LJCOMPILE=%LJCOMPILE% /DLUAJIT_ENABLE_GC64 /DLUAJIT_TARGET=LUAJIT_ARCH_ARM64
@goto :HOSTBUILD

:HOSTBUILD
@shift
call "%VSPATH%\VC\Auxiliary\Build\vcvarsall.bat" %VCHOSTARCH%
@REM Seems vcvarsall.bat disables echo
@echo on
@cd /D "%CWD%"

@if %VCHOSTARCH%=="x86" (
  @set LJHOSTCOMPILE=%LJHOSTCOMPILE% /arch:SSE2
)

%LJCOMPILE% %LJHOSTCOMPILE% host\minilua.c
@if errorlevel 1 goto :BAD
%LJLINK% /out:minilua.exe minilua.obj
@if errorlevel 1 goto :BAD
if exist minilua.exe.manifest^
  %LJMT% -manifest minilua.exe.manifest -outputresource:minilua.exe

minilua %DASM% -LN %DASMFLAGS% -o host\buildvm_arch.h %DASC%
@if errorlevel 1 goto :BAD

%LJCOMPILE% %LJHOSTCOMPILE% /I "." /I %DASMDIR% host\buildvm*.c
@if errorlevel 1 goto :BAD
%LJLINK% /out:buildvm.exe buildvm*.obj
@if errorlevel 1 goto :BAD
if exist buildvm.exe.manifest^
  %LJMT% -manifest buildvm.exe.manifest -outputresource:buildvm.exe

buildvm -m peobj -o lj_vm.obj
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

@echo ---- Cross compiler ----

call "%VSPATH%\VC\Auxiliary\Build\vcvarsall.bat" %VCTARGETARCH% uwp
@echo on
@cd /D "%CWD%"

@if "%1"=="debug" (
  @shift
  @set VSCONFIG=Debug
  @set "LJCOMPILE=%LJCOMPILE% /Zi /MDd /Od"
  @set LJLINK=%LJLINK% /debug /opt:ref /opt:icf /incremental:no
) else (
  @set VSCONFIG=Release
  @set "LJCOMPILE=%LJCOMPILE% /MD /O2"
)

@if "%1"=="amalg" goto :AMALGDLL
@if "%1"=="static" goto :STATIC

%LJCOMPILE% /DLUA_BUILD_AS_DLL lj_*.c lib_*.c
@if errorlevel 1 goto :BAD
%LJLINK% /DLL /out:%LJDLLNAME% lj_*.obj lib_*.obj mincore.lib
@if errorlevel 1 goto :BAD
@goto :MTDLL

:STATIC
%LJCOMPILE% lj_*.c lib_*.c
@if errorlevel 1 goto :BAD
%LJLIB% /OUT:%LJLIBNAME% lj_*.obj lib_*.obj
@if errorlevel 1 goto :BAD

@REM Only build the UWP application with the static library
msbuild /p:Configuration=%VSCONFIG% uwp
@if errorlevel 1 goto :BAD
@goto :MTDLL

:AMALGDLL
%LJCOMPILE% /DLUA_BUILD_AS_DLL ljamalg.c
@if errorlevel 1 goto :BAD
%LJLINK% /DLL /out:%LJDLLNAME% ljamalg.obj lj_vm.obj mincore.lib
@if errorlevel 1 goto :BAD

:MTDLL
if exist %LJDLLNAME%.manifest^
  %LJMT% -manifest %LJDLLNAME%.manifest -outputresource:%LJDLLNAME%;2

@del *.obj *.manifest minilua.exe buildvm.exe
@del host\buildvm_arch.h
@del lj_bcdef.h lj_ffdef.h lj_libdef.h lj_recdef.h lj_folddef.h
@echo.
@echo === Successfully built LuaJIT for UWP/%LJARCH% ===

@goto :END
:BAD
@echo.
@echo *******************************************************
@echo *** Build FAILED -- Please check the error messages ***
@echo *******************************************************
:END
