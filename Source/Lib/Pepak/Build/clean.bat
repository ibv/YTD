@echo off
setlocal
call "info" info
if errorlevel 1 exit 1
rem del /q "%exedir%%project%%ext%" >nul 2>&1
del /q "%exedir%%project%.map" >nul 2>&1
del /q "%exedir%%project%.map.sorted" >nul 2>&1
del /q "%exedir%%project%.rsm" >nul 2>&1
del /s /q "%srcdir%Units\*.*" >nul 2>&1
echo All compiled units come here. They can be rebuilt any time using the build.bat >Units\!_info.txt
echo script, so you can delete any files in this directory whenever you like. >>Units\!_info.txt
del /s /q "%srcdir%*.~*" >nul 2>&1
del /s /q "%srcdir%*.dcu" >nul 2>&1
del /s /q "%srcdir%*.dsk" >nul 2>&1
del /s /q "%srcdir%*.drc" >nul 2>&1
del /q "%srcdir%%project%.rc.bak" >nul 2>&1
del /q "%srcdir%%project%.res.bak" >nul 2>&1
del /q "%srcdir%%project%.dproj.2007" >nul 2>&1
del /q "%srcdir%%project%.dproj.local" >nul 2>&1
del /q "%srcdir%%project%.identcache" >nul 2>&1
del /q "%srcdir%%project%.skincfg" >nul 2>&1
del /q "%srcdir%%project%.x64.dproj.local" >nul 2>&1
del /q "%srcdir%%project%.x64.identcache" >nul 2>&1
call :history "%srcdir%"
goto :eof

:history
pushd "%~1"
if exist "__history" rd /s /q "__history" >nul 2>&1
for /d %%i in (*.*) do call :history "%%i"
popd
goto :eof
