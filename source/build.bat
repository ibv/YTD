@echo off
setlocal enabledelayedexpansion

rem --- Default settings ------------------------------------------------------
set project=ytd
set compiler=delphi
set target=x86
set params=
set debug=0
set release=0
set cli=1
set gui=1
set setup=1
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
if /i "%~1"=="setup" set setup=1
if /i "%~1"=="nosetup" set setup=0
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
if /i "%~1"=="x86" set target=x86
if /i "%~1"=="x32" set target=x86
if /i "%~1"=="x64" set target=x64
shift
goto :params

:paramend

rem --- Decide executable name ------------------------------------------------
set compexe=dcc32
if "%compiler%"=="delphi" (
  if "%target%"=="x86" (
    set compexe=dcc32
  ) else (
    set compexe=dcc64
  )
) else (
  set compexe=fpc
)

rem --- Detect compiler version -----------------------------------------------
set compver=

if "%compiler%"=="fpc" (
  set compver=fpc
) else (
  if "%compiler%"=="delphi" (
    %compexe% | find /i "Version 13.0"
    if not errorlevel 1 set compver=d5
    %compexe% | find /i "Version 20.0"
    if not errorlevel 1 set compver=d2009
    %compexe% | find /i "Version 21.0"
    if not errorlevel 1 set compver=d2009
    %compexe% | find /i "Version 22.0"
    if not errorlevel 1 set compver=d2009
    %compexe% | find /i "Version 23.0"
    if not errorlevel 1 set compver=dxe2
  )
)

rem --- Prepare command line --------------------------------------------------
set defs=-dPEPAK -dPEPAK_%project%
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
if not "%setup%"=="1" set defs=%defs% -dNO_SETUP
if not "%xxx%"=="1" set defs=%defs% -dNO_XXX
if "%fastmm%"=="1" set defs=%defs% -dFASTMM
if "%map%"=="1" if "%compiler%"=="delphi" set params=%params% -GD
if "%compver%"=="dxe2" set params=%params% -NSSystem;WinApi;Vcl

rem --- Delete compiled units -------------------------------------------------
del /q "%srcdir%Units\*.*"

rem --- Build the program -----------------------------------------------------
ren "%srcdir%%project%.cfg" "%project%.cfg._"
ren "%srcdir%%project%.dof" "%project%.dof._"

rem Pepak - compatibility
if exist "%srcdir%lib\Pepak\uCompatibility.pas" (
  call :%compiler% "%srcdir%lib\Pepak\uCompatibility.pas"
)

rem FastMM4
if exist "%srcdir%lib\FastMM\FastMM4" (
  if "%fastmm%"=="1" if not "%compiler%"=="fpc" (
    call :%compiler% "%srcdir%lib\FastMM\FastMM4.pas"
  )
)

rem DxGetText
if exist "%srcdir%lib\DxGetText\gnugettext.pas" (
  if "%compver%"=="fpc" (
    call :%compiler% "%srcdir%lib\DxGetText\fpc\gnugettext.pas"
  ) else if "%compver%"=="d5" (
    call :%compiler% "%srcdir%lib\DxGetText\delphi5\gnugettextD5.pas"
  ) else if "%compver%"=="d2009" (
    call :%compiler% "%srcdir%lib\DxGetText\delphi2009\gnugettext.pas"
  ) else if "%compver%"=="dxe2" (
    call :%compiler% "%srcdir%lib\DxGetText\delphi2009\gnugettext.pas"
  ) else (
    call :%compiler% "%srcdir%lib\DxGetText\gnugettext.pas"
  )
)

rem DCPCrypt
if exist "%srcdir%lib\DCPCrypt\DCPCrypt2.pas" (
  call :%compiler% "%srcdir%lib\DCPCrypt\DCPCrypt2.pas"
  call :%compiler% "%srcdir%lib\DCPCrypt\DCPblockciphers.pas"
  call :%compiler% "%srcdir%lib\DCPCrypt\ciphers\*.pas"
  call :%compiler% "%srcdir%lib\DCPCrypt\hashes\*.pas"
)

rem PerlRegEx
if exist "%srcdir%lib\PerlRegEx\PerlRegEx.pas" (
  if not "%target%"=="x64" (
    call :%compiler% "%srcdir%lib\PerlRegEx\PerlRegEx.pas"
  )
)

rem LkJSON
if exist "%srcdir%lib\lkJSON\uLkJSON.pas" (
  call :%compiler% "%srcdir%lib\lkJSON\uLkJSON.pas"
)

rem NativeXml
if exist "%srcdir%lib\NativeXml\NativeXml.pas" (
  call :%compiler% "%srcdir%lib\NativeXml\NativeXml.pas"
)

rem SciZipFile
if exist "%srcdir%lib\SciZipFile\SciZipFile.pas" (
  call :%compiler% "%srcdir%lib\SciZipFile\SciZipFile.pas"
)

rem SqliteWrapper
if exist "%srcdir%lib\SqliteWrapper\SQLiteTable3.pas" (
  call :%compiler% "%srcdir%lib\SqliteWrapper\SQLiteTable3.pas"
)

rem Synapse
if exist "%srcdir%lib\Synapse\source\lib\httpsend.pas" (
  call :%compiler% "%srcdir%lib\Synapse\source\lib\SSL_OpenSSL.pas"
  call :%compiler% "%srcdir%lib\Synapse\source\lib\httpsend.pas"
)

rem PepakLib
if exist "%srcdir%lib\Pepak\uCompatibility.pas" (
  if exist "%srcdir%lib\Pepak\uJSON.pas" if not exist "%srcdir%lib\lkJSON\uLkJSON.pas" ren "%srcdir%lib\Pepak\uJSON.pas" "uJSON.pas._"
  if exist "%srcdir%lib\Pepak\uPCRE.pas" if not exist "%srcdir%lib\PerlRegEx\PerlRegEx.pas" ren "%srcdir%lib\Pepak\uPCRE.pas" "uPCRE.pas._"
  if exist "%srcdir%lib\Pepak\uXML.pas" if not exist "%srcdir%lib\NativeXml\NativeXml.pas" ren "%srcdir%lib\Pepak\uXML.pas" "uXML.pas._"
  call :%compiler% "%srcdir%lib\Pepak\*.pas"
  call :%compiler% "%srcdir%lib\Pepak\ApiForm\*.pas"
  if "%compver%"=="d5" call :%compiler% "%srcdir%lib\Pepak\delphi5\*.pas"
  copy "%srcdir%lib\Pepak\ApiForm\*.res" "%srcdir%Units" >nul
  if exist "%srcdir%lib\Pepak\uJSON.pas._" ren "%srcdir%lib\Pepak\uJSON.pas._" "uJSON.pas"
  if exist "%srcdir%lib\Pepak\uPCRE.pas._" ren "%srcdir%lib\Pepak\uPCRE.pas._" "uPCRE.pas"
  if exist "%srcdir%lib\Pepak\uXML.pas._"  ren "%srcdir%lib\Pepak\uXML.pas._" "uXML.pas"
)

call :%compiler% "%srcdir%lib\RtmpDump\rtmpdump_dll.pas"
call :%compiler% "%srcdir%lib\msdl\src\msdl_dll.pas"
rem call :%compiler% "%srcdir%Tools\AMFview\AmfView.dpr"

updver.exe -b "%srcdir%%project%.res"
call :%compiler% "%srcdir%%project%.dpr"

ren "%srcdir%%project%.cfg._" "%project%.cfg"
ren "%srcdir%%project%.dof._" "%project%.dof"

rem --- Finalize the exe file -------------------------------------------------
ren "%exedir%%project%.exe" "%project%.exe"

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
  upx --best --lzma --brute --compress-icons=1 "%exedir%%project%.exe"
  set upx=1
)
goto konec

rem --- Compile with Delphi ---------------------------------------------------
:delphi
if "%~1"=="" goto konec
for %%i in (%~1) do (
  echo.
  echo Compiling: %%i
  echo %compexe% -B -E%exedir% -N%srcdir%Units -U%srcdir%Units %defs% %params% -Q "%%i"
  call %compexe% -B -E%exedir% -N%srcdir%Units -U%srcdir%Units %defs% %params% -Q "%%i"
  if errorlevel 1 goto halt
)
goto konec

rem --- Compile with FreePascal -----------------------------------------------
:fpc
if "%~1"=="" goto konec
for %%i in (%~1) do (
  echo.
  echo %compexe% -B -Mdelphi -FE%exedir% -Fu%srcdir%Units -FU%srcdir%Units %defs% %params% "%%i"
  call %compexe% -B -Mdelphi -FE%exedir% -Fu%srcdir%Units -FU%srcdir%Units %defs% %params% "%%i"
  if errorlevel 1 goto halt
)
shift
goto fpc

rem --- Stop compile process prematurely --------------------------------------
:halt
ren "%srcdir%%project%.cfg._" "%project%.cfg"
ren "%srcdir%%project%.dof._" "%project%.dof"
exit

rem --- Syntax ----------------------------------------------------------------
:help
echo Possible arguments:
echo    delphi/fpc ...... Build using Delphi/FreePascal.
echo    x86/x64 ......... Build for Win32/Win64.
echo    cli/nocli ....... Include/exclude CLI support.
echo    gui/nogui ....... Include/exclude GUI support.
echo    debug/nodebug ... Include/exclude debug code.
echo    xxx/noxxx ....... Include/exclude XXX providers
goto konec

:konec
