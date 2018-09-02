@echo off
setlocal
if "%~1"=="" goto syntax
if /i "%2"=="zip" goto zip
if /i "%2"=="7z" goto 7z
if /i "%2"=="exe" goto exe
rem goto zip
rem goto 7z
goto exe

:zip
set pack=zip
set packext=zip
set build=delphi upx
goto build

:7z
set pack=7z
set packext=7z
set build=delphi noupx
goto build

:exe
set pack=exe
set packext=exe
set build=delphi noupx
set packdir=%~d0%~p0lib\7-zip\
goto build

:build
set version=%1

updver.exe -v %version% "%srcdir%YTD.res"
if exist ..\ytd-%version%.%packext% del ..\ytd-%version%.%packext%
if exist ..\ytd-%version%.zip del ..\ytd-%version%.zip
if exist ..\ytd-%version%-lite.%packext% del ..\ytd-%version%-lite.%packext%
if exist ..\ytd-%version%-lite.zip del ..\ytd-%version%-lite.zip
if exist ..\ytd-%version%-source.zip del ..\ytd-%version%-source.zip
rd /s /q ..\bin\locale
md ..\bin\locale
xcopy locale\*.mo ..\bin\locale /s /i
del /q ..\bin\ytd.xml >nul 2>&1
call clean.bat
call build.bat release noxxx %build% %2 %3 %4 %5 %6 %7 %8 %9
call sign "YTD v%version% Lite" http://www.pepak.net/ytd ..\bin\ytd.exe
call clean.bat
pushd ..\bin
call :pack-%pack% ..\ytd-%version%-lite.%packext% 
if "%packext%"=="exe" call sign "YTD v%version% Lite - Installer" http://www.pepak.net/ytd ..\ytd-%version%-lite.%packext% 
if not "%packext%"=="zip" call :pack-zip ..\ytd-%version%-lite.zip
popd
call clean.bat
call build.bat release %build% %2 %3 %4 %5 %6 %7 %8 %9
call sign "YTD v%version%" http://www.pepak.net/ytd ..\bin\ytd.exe
call clean.bat
pushd ..\bin
call :pack-%pack% ..\ytd-%version%.%packext%
if "%packext%"=="exe" call sign "YTD v%version% - Installer" http://www.pepak.net/ytd ..\ytd-%version%.%packext%
if not "%packext%"=="zip" call :pack-zip ..\ytd-%version%.zip
popd
pushd ..
call :pack-zip ytd-%version%-source.zip bin\* source\*
popd
goto konec

:syntax
echo build-rel ^<version^> [build-arguments]
goto konec

:pack-zip
kzip /r %*
goto konec

:pack-7z
7z a -t7z -mx=9 -r %*
goto konec

:pack-exe
setlocal
set tempytd=%temp%\ytd.7z
if exist "%~1" del "%~1"
if exist "%tempytd%" del "%tempytd%"
"%packdir%\7zr.exe" a "%tempytd%" -t7z -m0=BCJ2 -m1=LZMA:d25:fb255 -m2=LZMA:d19 -m3=LZMA:d19 -mb0:1 -mb0s1:2 -mb0s2:3 -mx
if not errorlevel 1 copy /b "%packdir%7zS2.sfx" + "%tempytd%" "%~1"
if exist "%tempytd%" del "%tempytd%"
goto konec  


:konec
