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
shift

if exist ..\ytd-%version%.%packext% del ..\ytd-%version%.%packext%
if exist ..\ytd-%version%-source.%packext% del ..\ytd-%version%-source.%packext%
if exist ..\ytd-%version%-lite.%packext% del ..\ytd-%version%-lite.%packext%
rd /s /q ..\exe\locale
md ..\exe\locale
xcopy locale\*.mo ..\exe\locale /s /i
call clean.bat
call build.bat noxxx %build% %*
call clean.bat
pushd ..\exe
%pack% ..\ytd-%version%-lite.%packext%
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
