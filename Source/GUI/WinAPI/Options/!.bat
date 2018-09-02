@echo off
call :build guiOptionsWINAPI_Downloads.rc
call :build guiOptionsWINAPI_Downloaders.rc
call :build guiOptionsWINAPI_Main.rc
call :build guiOptionsWINAPI_Network.rc
goto konec

:build
call ..\build_res.bat %1

:konec
