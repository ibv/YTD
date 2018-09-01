@echo off
setlocal

set compiler=delphi5
set params=
set debug=0
set cli=1
set gui=1
set kol=0
set xxx=1

:params
if "%~1"=="" goto paramend
if /i "%~1"=="-?" goto help
if /i "%~1"=="-h" goto help
if /i "%~1"=="debug" set debug=1
if /i "%~1"=="nodebug" set debug=0
if /i "%~1"=="cli" set cli=1
if /i "%~1"=="nocli" set cli=0
if /i "%~1"=="gui" set gui=1
if /i "%~1"=="nogui" set gui=0
if /i "%~1"=="kol" set kol=1
if /i "%~1"=="nokol" set kol=0
if /i "%~1"=="xxx" set xxx=1
if /i "%~1"=="noxxx" set xxx=0
if /i "%~1"=="fpc" set compiler=fpc
if /i "%~1"=="delphi" set compiler=delphi
shift
goto :params

:paramend

set defs=
if not "%cli%"=="1" set defs=%defs% -dNO_CLI
if not "%gui%"=="1" set defs=%defs% -dNO_GUI
if not "%xxx%"=="1" set defs=%defs% -dNO_XXX
if "%kol%"=="1" set defs=%defs% -dKOL
if "%debug%"=="1" set defs=%defs% -dDEBUG

set root=%~d0%~p0
del /q Units\*.*
pushd Source

if "%kol%"=="1" (
  set defs=%defs% -dKOL
  call :%compiler% lib\KOL\kol.pas
  rem call :%compiler% lib\KOL\sysclasses\Classes.pas
)

call :%compiler% lib\Pepak\*.pas
call :%compiler% lib\Pepak\uAMF.pas
call :%compiler% lib\Synapse\source\lib\httpsend.pas
call :%compiler% lib\dpcre67\pcre.pas
call :%compiler% lib\janXmlParser2\janXmlParser2.pas
call :%compiler% lib\RtmpDump\rtmpdump_dll.pas
call :%compiler% lib\msdl\src\msdl_dll.pas
call :%compiler% lib\DxGetText\gnugettext.pas
rem call :%compiler% Tools\AmfView.dpr
call :%compiler% YTD.dpr
popd
upx --lzma Exe\ytd.exe
goto konec

:delphi5
if "%~1"=="" goto konec
for %%i in (%1) do (
  call dcc32 -B -E%root%\Exe -N%root%\Units -U%root%\Units %defs% %params% %*
  if errorlevel 1 pause
)
goto konec

:fpc
if "%~1"=="" goto konec
for %%i in (%1) do (
  call fpc -B -Mdelphi -FE%root%\Exe -Fu%root%\Units -FU%root%\Units %defs% %params% "%%i"
  if errorlevel 1 pause
)
shift
goto fpc

:help
echo Possible arguments:
echo    fpc/delphi ...... Build using FreePascal/Delphi.
echo    cli/nocli ....... Include/exclude CLI support.
echo    gui/nogui ....... Include/exclude GUI support.
echo    debug/nodebug ... Include/exclude debug code.
echo    xxx/noxxx ....... Include/exclude XXX providers
goto konec

:konec
