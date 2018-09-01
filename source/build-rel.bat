@echo off
setlocal
if "%~1"=="" goto syntax
rem goto 7z
goto zip

:zip
rem set pack=7z a -tzip -mx -r
set pack=kzip /r
set packext=zip
set build=delphi upx
goto build

:7z
set pack=7z a -t7z -mx=9 -r
set packext=7z
set build=delphi noupx
goto build

:build
set version=%1

updver.exe -v %version% "%srcdir%YTD.res"
if exist ..\ytd-%version%.%packext% del ..\ytd-%version%.%packext%
if exist ..\ytd-%version%-source.%packext% del ..\ytd-%version%-source.%packext%
if exist ..\ytd-%version%-lite.%packext% del ..\ytd-%version%-lite.%packext%
rd /s /q ..\bin\locale
md ..\bin\locale
xcopy locale\*.mo ..\bin\locale /s /i
del /q ..\bin\ytd.xml >nul 2>&1
call clean.bat
call build.bat release noxxx %build% %2 %3 %4 %5 %6 %7 %8 %9
call sign "YouTube Downloader Lite" http://www.pepak.net/download/youtube-downloader/ ..\bin\ytd.exe
call clean.bat
pushd ..\bin
%pack% ..\ytd-%version%-lite.%packext%
popd
call clean.bat
call build.bat release %build% %2 %3 %4 %5 %6 %7 %8 %9
call sign "YouTube Downloader" http://www.pepak.net/download/youtube-downloader/ ..\bin\ytd.exe
call clean.bat
pushd ..\bin
%pack% ..\ytd-%version%.%packext%
popd
pushd ..
%pack% ytd-%version%-source.%packext% exe\* source\*
popd
goto konec

:syntax
echo build-rel ^<version^> [build-arguments]
goto konec

:konec
