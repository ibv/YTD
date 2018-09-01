@echo off
call :build guiAboutWINAPI.rc
call :build guiMainWINAPI.rc
call :build guiOptionsWINAPI.rc
call :build guiOptionsWINAPI_Downloads.rc
call :build guiOptionsWINAPI_Main.rc
call :build guiOptionsWINAPI_Network.rc
call :build guiConverterWINAPI.rc
goto konec

:build
brcc32 -id:\progs\mingw\include %1
if errorlevel 1 pause
goto konec

:konec
