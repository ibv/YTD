@echo off
setlocal
if "%~1"=="" goto syntax
rem goto 7z
goto zip

:zip
set pack=7z a -tzip -mx -r
set packext=zip
set build=delphi5 upx
goto build

:7z
set pack=7z a -t7z -mx=9 -r
set packext=7z
set build=delphi5 noupx
goto build

:build
set version=%1
shift

if exist ..\ytd-%version%.%packext% del ..\ytd-%version%.%packext%
if exist ..\ytd-%version%-source.%packext% del ..\ytd-%version%-source.%packext%
if exist ..\ytdlite-%version%.%packext% del ..\ytdlite-%version%.%packext%
rd /s /q ..\exe\locale
md ..\exe\locale
xcopy locale\*.mo ..\exe\locale /s /i
call clean.bat
call build.bat noxxx %build% %*
call clean.bat
pushd ..\exe
%pack% ..\ytdlite-%version%.%packext%
popd
call clean.bat
call build.bat %build% %*
call clean.bat
pushd ..\exe
%pack% ..\ytd-%version%.%packext%
popd
pushd ..
%pack% ytd-%version%-source.%packext% exe\* source\*
popd
goto konec

:syntax
echo build-all ^<version^> [arguments]
goto konec

:konec
