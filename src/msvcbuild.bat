@rem Script to build LuaJIT with MSVC.
@rem Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
@rem
@rem Either open a "Visual Studio .NET Command Prompt"
@rem (Note that the Express Edition does not contain an x64 compiler)
@rem -or-
@rem Open a "Windows SDK Command Shell" and set the compiler environment:
@rem     setenv /release /x86
@rem   -or-
@rem     setenv /release /x64
@rem
@rem Then cd to this directory and run this script.

@if not defined INCLUDE goto :FAIL

@setlocal
@set LJCOMPILE=cl /nologo /c /MD /O2 /W3 /D_CRT_SECURE_NO_DEPRECATE
@set LJLINK=link /nologo
@set LJMT=mt /nologo
@set LJLIB=lib /nologo
@set DASMDIR=..\dynasm
@set DASM=lua %DASMDIR%\dynasm.lua
@set ALL_LIB=lib_base.c lib_math.c lib_bit.c lib_string.c lib_table.c lib_io.c lib_os.c lib_package.c lib_debug.c lib_jit.c lib_ffi.c

if not exist buildvm_x86.h^
  %DASM% -LN -o buildvm_x86.h buildvm_x86.dasc
if not exist buildvm_x64win.h^
  %DASM% -LN -D X64 -D X64WIN -o buildvm_x64win.h buildvm_x86.dasc

%LJCOMPILE% /I "." /I %DASMDIR% buildvm*.c
%LJLINK% /out:buildvm.exe buildvm*.obj
if exist buildvm.exe.manifest^
  %LJMT% -manifest buildvm.exe.manifest -outputresource:buildvm.exe

buildvm -m peobj -o lj_vm.obj
buildvm -m bcdef -o lj_bcdef.h %ALL_LIB%
buildvm -m ffdef -o lj_ffdef.h %ALL_LIB%
buildvm -m libdef -o lj_libdef.h %ALL_LIB%
buildvm -m recdef -o lj_recdef.h %ALL_LIB%
buildvm -m vmdef -o ..\lib\vmdef.lua %ALL_LIB%
buildvm -m folddef -o lj_folddef.h lj_opt_fold.c

@if "%1"=="amalg" goto :AMALGDLL
@if "%1"=="static" goto :STATIC
%LJCOMPILE% /DLUA_BUILD_AS_DLL lj_*.c lib_*.c
%LJLINK% /DLL /out:lua51.dll lj_*.obj lib_*.obj
@goto :MTDLL
:STATIC
%LJCOMPILE% /DLUA_BUILD_AS_DLL lj_*.c lib_*.c
%LJLIB% /OUT:lua51.lib lj_*.obj lib_*.obj
@goto :MTDLL
:AMALGDLL
%LJCOMPILE% /DLUA_BUILD_AS_DLL ljamalg.c
%LJLINK% /DLL /out:lua51.dll ljamalg.obj lj_vm.obj
:MTDLL
if exist lua51.dll.manifest^
  %LJMT% -manifest lua51.dll.manifest -outputresource:lua51.dll;2

%LJCOMPILE% luajit.c
%LJLINK% /out:luajit.exe luajit.obj lua51.lib
if exist luajit.exe.manifest^
  %LJMT% -manifest luajit.exe.manifest -outputresource:luajit.exe

del *.obj *.manifest buildvm.exe

@goto :END
:FAIL
@echo You must open a "Visual Studio .NET Command Prompt" to run this script
:END
