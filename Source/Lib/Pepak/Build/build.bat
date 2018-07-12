@echo off
setlocal enabledelayedexpansion

rem --- Default settings ------------------------------------------------------
set project=undefined
set project_title=undefined
set project_url=
set releasedir=.
set releasefiles=*
set exedir=.\
set srcdir=.\
set ext=.exe
set compiler=delphi
set target=x86
set params=
set extralib=
set debug=0
set release=0
set fastmm=0
set upx=0
set map=0
call "info" info
if errorlevel 1 exit 1

rem --- Read command-line parameters ------------------------------------------
call "info" params %*
:params
if "%~1"=="" goto paramend
if /i "%~1"=="-?" goto help
if /i "%~1"=="-h" goto help
if /i "%~1"=="debug" set debug=1
if /i "%~1"=="nodebug" set debug=0
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
call "info" paramloop %1
shift
goto :params

:paramend

rem --- Decide compiler executable --------------------------------------------
set compexe=dcc32
if "%compiler%"=="delphi" (
  if "%target%"=="x86" (
    set compexe=dcc32
  ) else (
    set compexe=dcc64
  )
)
if "%compiler%"=="fpc" (
  if "%target%"=="x86" (
    set compexe=fpc
  ) else (
    set compexe=ppcrossx64
  )
)

rem --- Detect compiler version -----------------------------------------------
set is_fpc=0
set is_delphi5=0
set is_delphi6=0
set is_delphi7=0
set is_delphi8=0
set is_delphi2005=0
set is_delphi2006=0
set is_delphi2007=0
set is_delphi2009=0
set is_delphi2010=0
set is_delphixe=0
set is_delphixe2=0
set is_delphixe3=0
set is_delphixe4=0
set is_delphixe5=0
set is_delphixe6=0
set is_delphixe7=0
set is_delphixe8=0
set is_delphixe10=0
set is_delphi5_up=0
set is_delphi6_up=0
set is_delphi7_up=0
set is_delphi8_up=0
set is_delphi2005_up=0
set is_delphi2006_up=0
set is_delphi2007_up=0
set is_delphi2009_up=0
set is_delphi2010_up=0
set is_delphixe_up=0
set is_delphixe2_up=0
set is_delphixe3_up=0
set is_delphixe4_up=0
set is_delphixe5_up=0
set is_delphixe6_up=0
set is_delphixe7_up=0
set is_delphixe8_up=0
set is_delphixe10_up=0
set is_delphixe10_1_up=0
set has_unicode=0
set has_namespaces=0

if "%compiler%"=="fpc" (
  set is_fpc=1
)
if "%compiler%"=="delphi" (
  %compexe% | find /i "Version 13.0"
  if not errorlevel 1 (
    set is_delphi5=1
    set is_delphi5_up=1
  )
  %compexe% | find /i "Version 14.0"
  if not errorlevel 1 (
    set is_delphi6=1
    set is_delphi5_up=1
    set is_delphi6_up=1
  )
  %compexe% | find /i "Version 15.0"
  if not errorlevel 1 (
    set is_delphi7=1
    set is_delphi5_up=1
    set is_delphi6_up=1
    set is_delphi7_up=1
  )
  %compexe% | find /i "Version 16.0"
  if not errorlevel 1 (
    set is_delphi8=1
    set is_delphi5_up=1
    set is_delphi6_up=1
    set is_delphi7_up=1
    set is_delphi8_up=1
  )
  %compexe% | find /i "Version 17.0"
  if not errorlevel 1 (
    set is_delphi2005=1
    set is_delphi5_up=1
    set is_delphi6_up=1
    set is_delphi7_up=1
    set is_delphi8_up=1
    set is_delphi2005_up=1
  )
  %compexe% | find /i "Version 18.0"
  if not errorlevel 1 (
    set is_delphi2006=1
    set is_delphi5_up=1
    set is_delphi6_up=1
    set is_delphi7_up=1
    set is_delphi8_up=1
    set is_delphi2005_up=1
    set is_delphi2006_up=1
  )
  %compexe% | find /i "Version 18.5"
  if not errorlevel 1 (
    set is_delphi2006=1
    set is_delphi5_up=1
    set is_delphi6_up=1
    set is_delphi7_up=1
    set is_delphi8_up=1
    set is_delphi2005_up=1
    set is_delphi2006_up=1
  )
  %compexe% | find /i "Version 19.0"
  if not errorlevel 1 (
    set is_delphi2007=1
    set is_delphi5_up=1
    set is_delphi6_up=1
    set is_delphi7_up=1
    set is_delphi8_up=1
    set is_delphi2005_up=1
    set is_delphi2006_up=1
    set is_delphi2007_up=1
  )
  %compexe% | find /i "Version 20.0"
  if not errorlevel 1 (
    set is_delphi2009=1
    set is_delphi5_up=1
    set is_delphi6_up=1
    set is_delphi7_up=1
    set is_delphi8_up=1
    set is_delphi2005_up=1
    set is_delphi2006_up=1
    set is_delphi2007_up=1
    set is_delphi2009_up=1
    set has_unicode=1
  )
  %compexe% | find /i "Version 21.0"
  if not errorlevel 1 (
    set is_delphi2010=1
    set is_delphi5_up=1
    set is_delphi6_up=1
    set is_delphi7_up=1
    set is_delphi8_up=1
    set is_delphi2005_up=1
    set is_delphi2006_up=1
    set is_delphi2007_up=1
    set is_delphi2009_up=1
    set is_delphi2010_up=1
    set has_unicode=1
  )
  %compexe% | find /i "Version 22.0"
  if not errorlevel 1 (
    set is_delphixe=1
    set is_delphi5_up=1
    set is_delphi6_up=1
    set is_delphi7_up=1
    set is_delphi8_up=1
    set is_delphi2005_up=1
    set is_delphi2006_up=1
    set is_delphi2007_up=1
    set is_delphi2009_up=1
    set is_delphi2010_up=1
    set is_delphixe_up=1
    set has_unicode=1
  )
  %compexe% | find /i "Version 23.0"
  if not errorlevel 1 (
    set is_delphixe2=1
    set is_delphi5_up=1
    set is_delphi6_up=1
    set is_delphi7_up=1
    set is_delphi8_up=1
    set is_delphi2005_up=1
    set is_delphi2006_up=1
    set is_delphi2007_up=1
    set is_delphi2009_up=1
    set is_delphi2010_up=1
    set is_delphixe_up=1
    set is_delphixe2_up=1
    set has_unicode=1
    set has_namespaces=1
  )
  %compexe% | find /i "Version 24.0"
  if not errorlevel 1 (
    set is_delphixe3=1
    set is_delphi5_up=1
    set is_delphi6_up=1
    set is_delphi7_up=1
    set is_delphi8_up=1
    set is_delphi2005_up=1
    set is_delphi2006_up=1
    set is_delphi2007_up=1
    set is_delphi2009_up=1
    set is_delphi2010_up=1
    set is_delphixe_up=1
    set is_delphixe2_up=1
    set is_delphixe3_up=1
    set has_unicode=1
    set has_namespaces=1
  )
  %compexe% | find /i "Version 25.0"
  if not errorlevel 1 (
    set is_delphixe4=1
    set is_delphi5_up=1
    set is_delphi6_up=1
    set is_delphi7_up=1
    set is_delphi8_up=1
    set is_delphi2005_up=1
    set is_delphi2006_up=1
    set is_delphi2007_up=1
    set is_delphi2009_up=1
    set is_delphi2010_up=1
    set is_delphixe_up=1
    set is_delphixe2_up=1
    set is_delphixe3_up=1
    set is_delphixe4_up=1
    set has_unicode=1
    set has_namespaces=1
  )
  %compexe% | find /i "Version 26.0"
  if not errorlevel 1 (
    set is_delphixe5=1
    set is_delphi5_up=1
    set is_delphi6_up=1
    set is_delphi7_up=1
    set is_delphi8_up=1
    set is_delphi2005_up=1
    set is_delphi2006_up=1
    set is_delphi2007_up=1
    set is_delphi2009_up=1
    set is_delphi2010_up=1
    set is_delphixe_up=1
    set is_delphixe2_up=1
    set is_delphixe3_up=1
    set is_delphixe4_up=1
    set is_delphixe5_up=1
    set has_unicode=1
    set has_namespaces=1
  )
  %compexe% | find /i "Version 27.0"
  if not errorlevel 1 (
    set is_delphixe6=1
    set is_delphi5_up=1
    set is_delphi6_up=1
    set is_delphi7_up=1
    set is_delphi8_up=1
    set is_delphi2005_up=1
    set is_delphi2006_up=1
    set is_delphi2007_up=1
    set is_delphi2009_up=1
    set is_delphi2010_up=1
    set is_delphixe_up=1
    set is_delphixe2_up=1
    set is_delphixe3_up=1
    set is_delphixe4_up=1
    set is_delphixe5_up=1
    set is_delphixe6_up=1
    set has_unicode=1
    set has_namespaces=1
  )
  %compexe% | find /i "Version 28.0"
  if not errorlevel 1 (
    set is_delphixe7=1
    set is_delphi5_up=1
    set is_delphi6_up=1
    set is_delphi7_up=1
    set is_delphi8_up=1
    set is_delphi2005_up=1
    set is_delphi2006_up=1
    set is_delphi2007_up=1
    set is_delphi2009_up=1
    set is_delphi2010_up=1
    set is_delphixe_up=1
    set is_delphixe2_up=1
    set is_delphixe3_up=1
    set is_delphixe4_up=1
    set is_delphixe5_up=1
    set is_delphixe6_up=1
    set is_delphixe7_up=1
    set has_unicode=1
    set has_namespaces=1
  )
  %compexe% | find /i "Version 29.0"
  if not errorlevel 1 (
    set is_delphixe8=1
    set is_delphi5_up=1
    set is_delphi6_up=1
    set is_delphi7_up=1
    set is_delphi8_up=1
    set is_delphi2005_up=1
    set is_delphi2006_up=1
    set is_delphi2007_up=1
    set is_delphi2009_up=1
    set is_delphi2010_up=1
    set is_delphixe_up=1
    set is_delphixe2_up=1
    set is_delphixe3_up=1
    set is_delphixe4_up=1
    set is_delphixe5_up=1
    set is_delphixe6_up=1
    set is_delphixe7_up=1
    set is_delphixe8_up=1
    set has_unicode=1
    set has_namespaces=1
  )
  %compexe% | find /i "Version 30.0"
  if not errorlevel 1 (
    set is_delphixe10=1
    set is_delphi5_up=1
    set is_delphi6_up=1
    set is_delphi7_up=1
    set is_delphi8_up=1
    set is_delphi2005_up=1
    set is_delphi2006_up=1
    set is_delphi2007_up=1
    set is_delphi2009_up=1
    set is_delphi2010_up=1
    set is_delphixe_up=1
    set is_delphixe2_up=1
    set is_delphixe3_up=1
    set is_delphixe4_up=1
    set is_delphixe5_up=1
    set is_delphixe6_up=1
    set is_delphixe7_up=1
    set is_delphixe8_up=1
    set is_delphixe10_up=1
    set has_unicode=1
    set has_namespaces=1
  )
  %compexe% | find /i "Version 31.0"
  if not errorlevel 1 (
    set is_delphixe10=1
    set is_delphi5_up=1
    set is_delphi6_up=1
    set is_delphi7_up=1
    set is_delphi8_up=1
    set is_delphi2005_up=1
    set is_delphi2006_up=1
    set is_delphi2007_up=1
    set is_delphi2009_up=1
    set is_delphi2010_up=1
    set is_delphixe_up=1
    set is_delphixe2_up=1
    set is_delphixe3_up=1
    set is_delphixe4_up=1
    set is_delphixe5_up=1
    set is_delphixe6_up=1
    set is_delphixe7_up=1
    set is_delphixe8_up=1
    set is_delphixe10_up=1
    set is_delphixe10_1_up=1
    set has_unicode=1
    set has_namespaces=1
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
if "%fastmm%"=="1" set defs=%defs% -dFASTMM
if "%map%"=="1" if "%compiler%"=="delphi" set params=%params% -GD
if "%has_namespaces%"=="1" set params=%params% -NSSystem;System.Win;WinApi;Vcl;Xml
call "info" defines

rem --- Delete compiled units -------------------------------------------------
del /q "%srcdir%Units\*.*"

rem --- Build the library units -----------------------------------------------
set lib=.

if not "%extralib%"=="" (
  set lib=%lib%;%extralib%
)

rem Pepak
if exist "%srcdir%lib\Pepak\." (
  set lib=%lib%;%srcdir%lib\Pepak;%srcdir%lib\Pepak\ApiForm
)
if exist "%srcdir%lib\Pepak\." (
  if "%is_delphi5%"=="1" (
    set lib=%lib%;%srcdir%lib\Pepak\delphi5
  )
)

rem FastMM4
if exist "%exedir%FastMM_FullDebugMode.dll" del "%exedir%FastMM_FullDebugMode.dll"
if exist "%srcdir%lib\FastMM\." (
  if "%fastmm%"=="1" if not "%is_fpc%"=="1" (
    set lib=%lib%;%srcdir%lib\FastMM
    if "%debug%"=="1" (
      if not exist "%exedir%FastMM_FullDebugMode.dll" copy "%srcdir%lib\fastmm\FastMM_FullDebugMode.dll" "%exedir%FastMM_FullDebugMode.dll"
    )
  )
)

rem DxGetText
if exist "%srcdir%lib\DxGetText\." (
  if "%is_fpc%"=="1" (
    set lib=%lib%;%srcdir%lib\DxGetText\fpc
  ) else if "%is_delphi5%"=="1" (
    set lib=%lib%;%srcdir%lib\DxGetText\delphi5
  ) else if "%has_unicode%"=="1" (
    set lib=%lib%;%srcdir%lib\DxGetText\delphi2009
  ) else (
    set lib=%lib%;%srcdir%lib\DxGetText
  )
)

rem DCPCrypt
if exist "%srcdir%lib\DCPCrypt\." (
  set lib=%lib%;%srcdir%lib\DCPCrypt;%srcdir%lib\DCPCrypt\ciphers;%srcdir%lib\DCPCrypt\hashes
)

rem FAR
if exist "%srcdir%lib\FAR\." (
  set lib=%lib%;%srcdir%lib\FAR
)

rem PerlRegEx
if exist "%exedir%pcrelib.dll" del "%exedir%pcrelib.dll"
if exist "%srcdir%lib\PerlRegEx\." (
  if not "%target%"=="x64" (
    set lib=%lib%;%srcdir%lib\PerlRegEx
    if "%is_fpc%"=="1" (
      if not exist "%exedir%pcrelib.dll" copy "%srcdir%lib\perlregex\pcrelib.dll" "%exedir%pcrelib.dll"
    )
  )
)

rem LkJSON
if exist "%srcdir%lib\lkJSON\." (
  set lib=%lib%;%srcdir%lib\lkJSON
)

rem NativeXml
if exist "%srcdir%lib\NativeXml\." (
  set lib=%lib%;%srcdir%lib\NativeXml
)

rem SciZipFile
if exist "%srcdir%lib\SciZipFile\." (
  set lib=%lib%;%srcdir%lib\SciZipFile
)

rem SqliteWrapper
if exist "%srcdir%lib\SqliteWrapper\." (
  set lib=%lib%;%srcdir%lib\SqliteWrapper
)

rem SuperObject
if exist "%srcdir%lib\SuperObject\." (
  set lib=%lib%;%srcdir%lib\SuperObject
)

rem Synapse
if exist "%srcdir%lib\Synapse\source\lib\." (
  set lib=%lib%;%srcdir%lib\Synapse\source\lib
)

rem SynEdit
if exist "%srcdir%lib\SynEdit\Source\." (
  set lib=%lib%;%srcdir%lib\SynEdit\Source
)

rem --- Build the program -----------------------------------------------------
call "info" prebuild

if exist "%project%.cfg" (
  ren "%srcdir%%project%.cfg" "%project%.cfg._"
)
if exist "%project%.dof" (
  ren "%srcdir%%project%.dof" "%project%.dof._"
)

if exist "%srcdir%%project%.rc" updver.exe -b "%srcdir%%project%.rc"
if exist "%srcdir%%project%.res" updver.exe -b "%srcdir%%project%.res"
if exist "%srcdir%%project%.rc" call :resource "%srcdir%%project%.rc"
call :%compiler% "%srcdir%%project%.dpr"

if exist "%project%.cfg._" (
  ren "%srcdir%%project%.cfg._" "%project%.cfg"
)
if exist "%project%.dof._" (
  ren "%srcdir%%project%.dof._" "%project%.dof"
)

rem --- Finalize the exe file -------------------------------------------------
ren "%exedir%%project%%ext%" "%project%%ext%"

if "%upx%"=="1" (
  set upx=
  upx --best --lzma --brute --compress-icons=1 "%exedir%%project%%ext%"
  set upx=1
)

call "info" postbuild
goto :eof

rem --- Compile with Delphi ---------------------------------------------------
:delphi
if "%~1"=="" goto :eof
for %%i in (%~1) do (
  echo.
  echo Compiling: %%i
  if "%is_delphixe4_up%"=="1" (
    call "%compexe%" -B -E%exedir% -NU"%srcdir%Units" -U"%srcdir%Units;%lib%" -I"%lib%" %defs% %params% -Q "%%i"
  ) else (
    call "%compexe%" -B -E%exedir% -N"%srcdir%Units" -U"%srcdir%Units;%lib%" -I"%lib%" %defs% %params% -Q "%%i"
  )
  if errorlevel 1 goto halt
)
goto :eof

rem --- Compile with FreePascal -----------------------------------------------
:fpc
if "%~1"=="" goto :eof
for %%i in (%~1) do (
  echo.
  call "%compexe%" -B -Mdelphi -FE"%exedir%" -Fu"%srcdir%Units;%lib%" -FU"%srcdir%Units" %defs% %params% "%%i"
  if errorlevel 1 goto halt
)
goto :eof

rem --- Compile resources -----------------------------------------------------
:resource
if "%compiler%"=="delphi" (
  rem brcc32 %1
  rem Note: BRCC32 is buggy. Use GoRC (FreePascal) or RC (Delphi 2009+) if possible
  if "%is_delphi2009_up%"=="1" (
    call "rc" %1
  ) else (
    call "gorc" /r %1 2>nul
    if errorlevel 1 call "brcc32" %1
  )
) else (
  call "gorc" /r %1
)
goto :eof

rem --- Stop compile process prematurely --------------------------------------
:halt
if exist "%srcdir%%project%.cfg._" ren "%srcdir%%project%.cfg._" "%project%.cfg"
if exist "%srcdir%%project%.dof._" ren "%srcdir%%project%.dof._" "%project%.dof"
exit

rem --- Syntax ----------------------------------------------------------------
:help
echo Possible arguments:
echo    delphi/fpc ...... Build using Delphi/FreePascal.
echo    x86/x64 ......... Build for Win32/Win64.
echo    debug/nodebug ... Include/exclude debug code.
call "info" help
goto :eof
