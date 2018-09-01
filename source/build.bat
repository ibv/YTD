@echo off
setlocal

set compiler=delphi
set params=
set debug=0
set cli=1
set gui=1
set kol=0
set xxx=1
set upx=0
set extra=

set exedir=..\Exe\
set srcdir=

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
if /i "%~1"=="upx" set upx=1
if /i "%~1"=="noupx" set upx=0
if /i "%~1"=="fpc" set compiler=fpc&set extra=fpc\
if /i "%~1"=="delphi" set compiler=delphi&set extra=
if /i "%~1"=="delphi5" set compiler=delphi&set extra=delphi5\
if /i "%~1"=="delphi2009" set compiler=delphi&set extra=delphi2009\
shift
goto :params

:paramend

set defs=
if not "%cli%"=="1" set defs=%defs% -dNO_CLI
if not "%gui%"=="1" set defs=%defs% -dNO_GUI
if not "%xxx%"=="1" set defs=%defs% -dNO_XXX
if "%kol%"=="1" set defs=%defs% -dKOL
if "%debug%"=="1" set defs=%defs% -dDEBUG

del /q "%srcdir%Units\*.*"

if "%kol%"=="1" (
  set defs=%defs% -dKOL
  call :%compiler% "%srcdir%lib\KOL\kol.pas"
  rem call :%compiler% ""%srcdir%lib\KOL\sysclasses\Classes.pas"
)

call :%compiler% "%srcdir%lib\Pepak\*.pas"
call :%compiler% "%srcdir%lib\Pepak\%extra%*.pas"
call :%compiler% "%srcdir%lib\Synapse\source\lib\httpsend.pas"
call :%compiler% "%srcdir%lib\PerlRegEx\PerlRegEx.pas"
call :%compiler% "%srcdir%lib\NativeXml\NativeXml.pas"
call :%compiler% "%srcdir%lib\RtmpDump\rtmpdump_dll.pas"
call :%compiler% "%srcdir%lib\msdl\src\msdl_dll.pas"
call :%compiler% "%srcdir%lib\DxGetText\%extra%gnugettext.pas"
rem call :%compiler% "%srcdir%Tools\AmfView.dpr"
call :%compiler% "%srcdir%ytd.dpr"

if "%upx%"=="1" (
  set upx=
  upx --best --lzma "%exedir%ytd.exe"
  set upx=1
)
goto konec

:delphi
if "%~1"=="" goto konec
for %%i in (%1) do (
  echo.
  echo Compiling: %%i
  call dcc32 -E%exedir% -N%srcdir%Units -U%srcdir%\Units %defs% %params% %*
  if errorlevel 1 pause
)
goto konec

:fpc
if "%~1"=="" goto konec
for %%i in (%1) do (
  echo.
  call fpc -B -Mdelphi -FE%exedir% -Fu%srcdir%Units -FU%srcdir%Units %defs% %params% "%%i"
  if errorlevel 1 pause
)
shift
goto fpc

:help
echo Possible arguments:
echo    fpc/delphi/delphi5/delphi2009 ... Build using FreePascal/Delphi/Delphi 5/Delphi 2009.
echo    cli/nocli ....................... Include/exclude CLI support.
echo    gui/nogui ....................... Include/exclude GUI support.
echo    debug/nodebug ................... Include/exclude debug code.
echo    xxx/noxxx ....................... Include/exclude XXX providers
goto konec

:konec
