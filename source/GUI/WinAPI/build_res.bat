@echo off
rem call brcc32 -id:\progs\mingw\include %1
call brcc32 -ii:\. %1
if errorlevel 1 pause
