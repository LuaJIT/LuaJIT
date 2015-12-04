ECHO off

cd "%~dp0"

SET src=..\src
CALL :normalise "%src%"

set LUA_PATH=%~dp0?.lua;%src%/?.lua

..\src\luajit.exe runtests.lua
pause

:normalise
SET "src=%~f1"
GOTO :EOF