@echo off
call :build guiOptionsWINAPI_CommonDownloader.rc
call :build guiOptionsWINAPI_Barrandov.rc
call :build guiOptionsWINAPI_CT.rc
call :build guiOptionsWINAPI_EuroSeptik.rc
call :build guiOptionsWINAPI_Joj.rc
call :build guiOptionsWINAPI_Nova.rc
call :build guiOptionsWINAPI_YouTube.rc
goto konec

:build
call ..\build_res.bat %1

:konec
