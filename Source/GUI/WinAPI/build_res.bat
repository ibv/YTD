@echo off
rem call brcc32 -id:\progs\mingw\include %1
call brcc32 -iI:\. %1
if errorlevel 1 pause
ren "%~n1.res" "%~n1.res_"
ren "%~n1.res_" "%~n1.res"
