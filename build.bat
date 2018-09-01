@echo off
setlocal

set delphi=call dcc32 -B -E..\Exe -N..\Units -U..\Units

:params
if "%~1"=="" goto noparams
if /i "%~1"=="-?" goto help
if /i "%~1"=="-h" goto help
if /i "%~1"=="nocli" set delphi=%delphi% -DNO_CLI
if /i "%~1"=="nogui" set delphi=%delphi% -DNO_GUI
if /i "%~1"=="noxxx" set delphi=%delphi% -DNO_XXX
shift
goto :params

:noparams
del /q Units\*.*
pushd Source

%delphi% lib\Synapse\source\lib\*.pas
if errorlevel 1 goto konec
%delphi% lib\dpcre67\pcre.pas
if errorlevel 1 goto konec
%delphi% lib\janXmlParser2\janXmlParser2.pas
if errorlevel 1 goto konec
%delphi% lib\Pepak\*.pas
if errorlevel 1 goto konec
%delphi% lib\RtmpDump\rtmpdump_dll.pas
if errorlevel 1 goto konec
%delphi% lib\msdl\src\msdl_dll.pas
if errorlevel 1 goto konec
%delphi% YTD.dpr
if errorlevel 1 goto konec
popd
upx --best Exe\ytd.exe
goto konec

:help
echo Possible arguments:
echo    nocli ... Build only the GUI version.
echo    nogui ... Build only the CLI version.
echo    noxxx ... Don't build support for porn providers.
goto konec

:konec
