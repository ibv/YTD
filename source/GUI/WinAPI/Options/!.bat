@echo off
call :build guiOptionsWINAPI_Downloads.rc
call :build guiOptionsWINAPI_Downloaders.rc
call :build guiOptionsWINAPI_Main.rc
call :build guiOptionsWINAPI_Network.rc
goto konec

:build
brcc32 -id:\progs\mingw\include %1
rem brcc32 -ii:\. %1
if errorlevel 1 pause
goto konec

:konec
