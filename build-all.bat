@echo off
if exist ytd-%1.zip del ytd-%1.zip
if exist ytd-%1-source.zip del ytd-%1-source.zip
if exist ytdlite-%1.zip del ytdlite-%1.zip
call clean.bat
call build.bat noxxx
call clean.bat
pushd exe
7z a -tzip -r ..\ytdlite-%1.zip
popd
call clean.bat
call build.bat
call clean.bat
pushd exe
7z a -tzip -r ..\ytd-%1.zip
popd
7z a -tzip -r -x!units\* -x!todo ytd-%1-source.zip *
