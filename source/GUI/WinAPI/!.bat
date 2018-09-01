@echo off
call :build guiAboutWINAPI.rc
call :build guiMainWINAPI.rc
call :build guiOptionsWINAPI.rc
call :build guiConverterWINAPI.rc
goto konec

:build
brcc32 -ic:\c\mingw\include %1
if errorlevel 1 pause
goto konec

:konec
