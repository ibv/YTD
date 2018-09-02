@echo off
call :build guiAboutWINAPI.rc
call :build guiMainWINAPI.rc
call :build guiOptionsWINAPI.rc
call :build guiConverterWINAPI.rc
call :build guiSetupWINAPI.rc
goto konec

:build
call .\build_res.bat %1
goto konec

:konec
