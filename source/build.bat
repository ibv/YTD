@echo off
setlocal enabledelayedexpansion

rem --- Default settings ------------------------------------------------------
set compiler=delphi
set params=
set debug=0
set release=0
set cli=1
set gui=1
set xxx=1
set fastmm=0
set upx=0
set map=0

set exedir=..\Bin\
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
if /i "%~1"=="xxx" set xxx=1
if /i "%~1"=="noxxx" set xxx=0
if /i "%~1"=="fastmm" set fastmm=1
if /i "%~1"=="nofastmm" set nofastmm=0
if /i "%~1"=="upx" set upx=1
if /i "%~1"=="noupx" set upx=0
if /i "%~1"=="map" set map=1
if /i "%~1"=="nomap" set map=0
if /i "%~1"=="release" set release=1
if /i "%~1"=="norelease" set release=0
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
if "%release%"=="1" (
  set params=%params% -$D- -$L- -$Y- -$C-
  set defs=%defs% -dRELEASE
  set debug=0
)
if "%debug%"=="1" (
  set defs=%defs% -dDEBUG -dFULLDEBUGMODE -dCLEARLOGFILEONSTARTUP
    rem -dFULLDEBUGMODE -dCLEARLOGFILEONSTARTUP are for FastMM
  set release=0
)
if not "%cli%"=="1" set defs=%defs% -dNO_CLI
if not "%gui%"=="1" set defs=%defs% -dNO_GUI
if not "%xxx%"=="1" set defs=%defs% -dNO_XXX
if "%fastmm%"=="1" set defs=%defs% -dFASTMM
if "%map%"=="1" if "%compiler%"=="delphi" set params=%params% -GD

rem --- Delete compiled units -------------------------------------------------
del /q "%srcdir%Units\*.*"

rem --- Build YouTube Downloader ----------------------------------------------
ren "%srcdir%ytd.cfg" ytd.cfg._
ren "%srcdir%ytd.dof" ytd.dof._

if not "%compiler%"=="fpc" call :%compiler% "%srcdir%lib\FastMM\FastMM4.pas"
if "%compver%"=="fpc" (
  call :%compiler% "%srcdir%lib\DxGetText\fpc\gnugettext.pas"
) else if "%compver%"=="d5" (
  call :%compiler% "%srcdir%lib\DxGetText\delphi5\gnugettextD5.pas"
) else if "%compver%"=="d2009" (
  call :%compiler% "%srcdir%lib\DxGetText\delphi2009\gnugettext.pas"
) else (
  call :%compiler% "%srcdir%lib\DxGetText\gnugettext.pas"
)
call :%compiler% "%srcdir%lib\PerlRegEx\*.pas"
call :%compiler% "%srcdir%lib\lkJSON\uLkJSON.pas"
call :%compiler% "%srcdir%lib\Pepak\*.pas"
call :%compiler% "%srcdir%lib\Pepak\ApiForm\*.pas"
if "%compver%"=="d5" call :%compiler% "%srcdir%lib\Pepak\delphi5\*.pas"
copy "%srcdir%lib\Pepak\ApiForm\*.res" "%srcdir%Units" >nul
call :%compiler% "%srcdir%lib\Synapse\source\lib\SSL_OpenSSL.pas"
call :%compiler% "%srcdir%lib\Synapse\source\lib\httpsend.pas"
call :%compiler% "%srcdir%lib\NativeXml\NativeXml.pas"
call :%compiler% "%srcdir%lib\RtmpDump\rtmpdump_dll.pas"
call :%compiler% "%srcdir%lib\msdl\src\msdl_dll.pas"
rem call :%compiler% "%srcdir%Tools\AMFview\AmfView.dpr"
updver.exe -b "%srcdir%YTD.res"
call :%compiler% "%srcdir%ytd.dpr"

ren "%srcdir%ytd.cfg._" ytd.cfg
ren "%srcdir%ytd.dof._" ytd.dof

rem --- Finalize the exe file -------------------------------------------------
ren "%exedir%ytd.exe" "ytd.exe"

if "%debug%"=="1" (
  if not exist "%exedir%FastMM_FullDebugMode.dll" copy "%srcdir%lib\fastmm\FastMM_FullDebugMode.dll" "%exedir%FastMM_FullDebugMode.dll"
) else (
  if exist "%exedir%FastMM_FullDebugMode.dll" del "%exedir%FastMM_FullDebugMode.dll"
)

if "%compiler%"=="fpc" (
  if not exist "%exedir%pcrelib.dll" copy "%srcdir%lib\perlregex\pcrelib.dll" "%exedir%pcrelib.dll"
) else (
  if exist "%exedir%pcrelib.dll" del "%exedir%pcrelib.dll"
)

if "%upx%"=="1" (
  set upx=
  upx --best --lzma --brute --compress-icons=1 "%exedir%ytd.exe"
  set upx=1
)
goto konec

rem --- Compile with Delphi ---------------------------------------------------
:delphi
if "%~1"=="" goto konec
for %%i in (%~1) do (
  echo.
  echo Compiling: %%i
  echo dcc32 -B -E%exedir% -N%srcdir%Units -U%srcdir%Units %defs% %params% -Q "%%i"
  call dcc32 -B -E%exedir% -N%srcdir%Units -U%srcdir%Units %defs% %params% -Q "%%i"
  if errorlevel 1 pause
)
goto konec

rem --- Compile with FreePascal -----------------------------------------------
:fpc
if "%~1"=="" goto konec
for %%i in (%~1) do (
  echo.
  echo fpc -B -Mdelphi -FE%exedir% -Fu%srcdir%Units -FU%srcdir%Units %defs% %params% "%%i"
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
