@echo off
setlocal
if "%~1"=="" goto syntax

set version=%1
shift

if exist ytd-%version%.zip del ytd-%version%.zip
if exist ytd-%version%-source.zip del ytd-%version%-source.zip
if exist ytdlite-%version%.zip del ytdlite-%version%.zip
rd /s /q exe\locale
md exe\locale
xcopy source\locale\*.mo exe\locale /s /i
call clean.bat
call build.bat noxxx %*
del exe\amfview.exe
call clean.bat
pushd exe
7z a -tzip -r ..\ytdlite-%version%.zip
popd
call clean.bat
call build.bat %*
del exe\amfview.exe
call clean.bat
pushd exe
7z a -tzip -r ..\ytd-%version%.zip
popd
7z a -tzip -r -x!units\* -x!todo -x!*.zip ytd-%version%-source.zip *
goto konec

:syntax
echo build-all ^<version^> [arguments]
goto konec

:konec
