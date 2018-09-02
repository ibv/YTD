@echo off
call :build guiAboutWINAPI.rc
call :build guiMainWINAPI.rc
call :build guiOptionsWINAPI.rc
call :build guiConverterWINAPI.rc
call :build guiSetupWINAPI.rc
goto konec

:build
brcc32 -id:\progs\mingw\include %1
rem brcc32 -ii:\. %1
if errorlevel 1 pause
goto konec

:konec
