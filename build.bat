@echo off
setlocal

set compiler=delphi5

:params
if "%~1"=="" goto paramend
if /i "%~1"=="-?" goto help
if /i "%~1"=="-h" goto help
if /i "%~1"=="debug" set defs=%defs% -dDEBUG
if /i "%~1"=="nocli" set defs=%defs% -dNO_CLI
if /i "%~1"=="nogui" set defs=%defs% -dNO_GUI
if /i "%~1"=="noxxx" set defs=%defs% -dNO_XXX
if /i "%~1"=="fpc" set compiler=fpc
shift
goto :params

:paramend
del /q Units\*.*
pushd Source

call :%compiler% lib\Synapse\source\lib\httpsend.pas
call :%compiler% lib\dpcre67\pcre.pas
call :%compiler% lib\janXmlParser2\janXmlParser2.pas
call :%compiler% lib\Pepak\*.pas
call :%compiler% lib\RtmpDump\rtmpdump_dll.pas
call :%compiler% lib\msdl\src\msdl_dll.pas
call :%compiler% YTD.dpr
popd
upx --lzma Exe\ytd.exe
goto konec

:delphi5
call dcc32 -B -E..\Exe -N..\Units -U..\Units %defs% %*
if errorlevel 1 pause
goto konec

:fpc
if "%~1"=="" goto konec
for %%i in (%1) do (
  call fpc -B -Mdelphi -FE..\Exe -Fu..\Units -FU..\Units %defs% "%%i"
  if errorlevel 1 pause
)
shift
goto fpc

:help
echo Possible arguments:
echo    fpc ..... Build using FreePascal.
echo    nocli ... Build only the GUI version.
echo    nogui ... Build only the CLI version.
echo    noxxx ... Don't build support for porn providers.
goto konec

:konec
