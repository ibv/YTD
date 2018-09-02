@echo off
call :build guiOptionsWINAPI_CommonDownloader.rc
call :build guiOptionsWINAPI_Barrandov.rc
call :build guiOptionsWINAPI_CT.rc
call :build guiOptionsWINAPI_Nova.rc
call :build guiOptionsWINAPI_YouTube.rc
goto konec

:build
rem brcc32 -id:\progs\mingw\include %1
brcc32 -ii:\. %1
if errorlevel 1 pause
goto konec

:konec
