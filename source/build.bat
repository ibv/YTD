@echo off
setlocal

rem --- Default settings ------------------------------------------------------
set compiler=delphi
set params=-GD
set debug=0
set cli=1
set gui=1
set lvcl=0
set xxx=1
set upx=0

set exedir=..\Exe\
set srcdir=

rem --- Read command-line parameters ------------------------------------------
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
if /i "%~1"=="lvcl" set lvcl=1
if /i "%~1"=="nolvcl" set lvcl=0
if /i "%~1"=="xxx" set xxx=1
if /i "%~1"=="noxxx" set xxx=0
if /i "%~1"=="upx" set upx=1
if /i "%~1"=="noupx" set upx=0
if /i "%~1"=="fpc" set compiler=fpc
if /i "%~1"=="delphi" set compiler=delphi
shift
goto :params

:paramend

rem --- Detect compiler version -----------------------------------------------
set compver=

if "%compiler%"=="fpc" (
  set compver=fpc
) else (
  if "%compiler%"=="delphi" (
    dcc32 | find /i "Version 13.0"
    if not errorlevel 1 set compver=d5
    dcc32 | find /i "Version 20.0"
    if not errorlevel 1 set compver=d2009
  )
)

rem --- Prepare command line --------------------------------------------------
set defs=-dPEPAK -dPEPAK_YTD
if not "%cli%"=="1" set defs=%defs% -dNO_CLI
if not "%gui%"=="1" set defs=%defs% -dNO_GUI
if not "%xxx%"=="1" set defs=%defs% -dNO_XXX
if "%debug%"=="1" set defs=%defs% -dDEBUG

rem --- Delete compiled units -------------------------------------------------
del /q "%srcdir%Units\*.*"

rem --- Build YouTube Downloader ----------------------------------------------
if "%lvcl%"=="1" (
  set defs=%defs% -dLVCL
  call :%compiler% "%srcdir%lib\LVCL\*.pas"
)

if "%compver%"=="d5" call :%compiler% "%srcdir%lib\Pepak\delphi5\*.pas"
call :%compiler% "%srcdir%lib\PerlRegEx\*.pas"
call :%compiler% "%srcdir%lib\Pepak\*.pas"
call :%compiler% "%srcdir%lib\Synapse\source\lib\httpsend.pas"
call :%compiler% "%srcdir%lib\NativeXml\NativeXml.pas"
call :%compiler% "%srcdir%lib\RtmpDump\rtmpdump_dll.pas"
call :%compiler% "%srcdir%lib\msdl\src\msdl_dll.pas"
if "%compver%"=="fpc" (
  call :%compiler% "%srcdir%lib\DxGetText\fpc\gnugettext.pas"
) else if "%compver%"=="d5" (
  call :%compiler% "%srcdir%lib\DxGetText\delphi5\gnugettext.pas"
) else if "%compver%"=="d2009" (
  call :%compiler% "%srcdir%lib\DxGetText\delphi2009\gnugettext.pas"
) else (
  call :%compiler% "%srcdir%lib\DxGetText\gnugettext.pas"
)
rem call :%compiler% "%srcdir%Tools\AmfView.dpr"
call :%compiler% "%srcdir%ytd.dpr"

rem --- Finalize the exe file -------------------------------------------------
ren "%exedir%ytd.exe" "ytd.exe"

if "%upx%"=="1" (
  set upx=
  upx --best --lzma "%exedir%ytd.exe"
  set upx=1
)
goto konec

rem --- Compile with Delphi ---------------------------------------------------
:delphi
if "%~1"=="" goto konec
for %%i in (%~1) do (
  echo.
  echo Compiling: %%i
  call dcc32 -E%exedir% -N%srcdir%Units -U%srcdir%Units %defs% %params% "%%i"
  if errorlevel 1 pause
)
goto konec

rem --- Compile with FreePascal -----------------------------------------------
:fpc
if "%~1"=="" goto konec
for %%i in (%~1) do (
  echo.
  call fpc -B -Mdelphi -FE%exedir% -Fu%srcdir%Units -FU%srcdir%Units %defs% %params% "%%i"
  if errorlevel 1 pause
)
shift
goto fpc

rem --- Syntax ----------------------------------------------------------------
:help
echo Possible arguments:
echo    fpc/delphi/delphi5/delphi2009 ... Build using FreePascal/Delphi/Delphi 5/Delphi 2009.
echo    cli/nocli ....................... Include/exclude CLI support.
echo    gui/nogui ....................... Include/exclude GUI support.
echo    debug/nodebug ................... Include/exclude debug code.
echo    xxx/noxxx ....................... Include/exclude XXX providers
goto konec

:konec
