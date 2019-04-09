echo off
setlocal
if "%~1"=="" goto syntax


call "info" info
if errorlevel 1 exit 1
set version=%~1
set build=release noupx %build%
set skip_source=1
set skip_build=0
rem set compress=exe
rem set variant_name=
rem set variant_suffix=

if exist "%releasedir%%project%-%version%%variant_suffix%.%compress%" del "%releasedir%%project%-%version%%variant_suffix%.%compress%"
if not "%skip_source%"=="1" (
  if exist "%releasedir%%project%-%version%%variant_suffix%-source.zip" del "%releasedir%%project%-%version%%variant_suffix%-source.zip"
)
call "info" prerelease

if not "%skip_build%"=="1" (
 if exist "%srcdir%%project%.rc" updver.exe -v %version% "%srcdir%%project%.rc"
 if exist "%srcdir%%project%.res" updver.exe -v %version% "%srcdir%%project%.res"
  if exist "%srcdir%%project%.version" echo '%version%'>"%srcdir%%project%.version"
  call "build" %build% %2 %3 %4 %5 %6 %7 %8 %9
  if errorlevel 1 goto :error Failed to build the release
  rem call "sign" "%project_title% v%version%%variant_name%" "%project_url%" "%exedir%%project%%ext%"
  call "clean"
  if not "%variant_suffix%"=="" (
    if "%compress%"=="none" (
      if exist "%exedir%%project%%variant_suffix%%ext%" del "%exedir%%project%%variant_suffix%%ext%"
      ren "%exedir%%project%%ext%" "%project%%variant_suffix%%ext%"
    )
  )
)
call "info" postrelease


set workdir=%temp%\release-%project%-v%version%%variant_suffix%
if not "%compress%"=="none" (
  if exist "%workdir%" rd /s /q "%workdir%"
  mkdir "%workdir%\Bin"
  xcopy "%exedir%*.*" "%workdir%\Bin" /s /i >nul
  call :pack "%compress%" "%releasedir%%project%-%version%%variant_suffix%.%compress%" "%workdir%\Bin"
  if errorlevel 1 goto :error Failed to compress the binary version
  if not "%skip_source%"=="1" (
    mkdir "%workdir%\Source"
    xcopy "%srcdir%*.*" "%workdir%\Source" /s /i >nul
    call :pack "zip" "%releasedir%%project%-%version%-source.zip" "%workdir%"
    if errorlevel 1 goto :error Failed to compress the source version
  )
  if exist "%workdir%" rd /s /q "%workdir%"
)
goto :eof


:syntax
echo build-rel ^<version^> [build-arguments]
exit 1
goto :eof


:error
echo %*
exit 1
goto :eof


:pack
rem 1 = compression
rem 2 = target filename
rem 3 = source directory
if "%~1"=="exe" (
  call :pack-exe "%~1" "%~2" "%~3" "%srcdir%"
) else if "%~1"=="7z" (
  call :pack-7z "%~1" "%~2" "%~3"
) else if "%~1"=="none" (
  rem Nothing to do
) else (
  call :pack-zip "%~1" "%~2" "%~3"
)
goto :eof


:pack-exe
rem 1 = compression
rem 2 = target filename
rem 3 = source directory
rem 4 = %srcdir% (needed for expansion)
rem set sevenzip=%~f4%lib\7-zip\7zr.exe
set sevenzip=%~f4%lib\7-zip\7z.exe
pushd "%~3"
if exist "$$packed$$.7z" del "$$packed$$.7z"
"%sevenzip%" a "$$packed$$.7z" -t7z -m0=BCJ2 -m1=LZMA:d25:fb255 -m2=LZMA:d19 -m3=LZMA:d19 -mb0:1 -mb0s1:2 -mb0s2:3 -mx
if errorlevel 1 goto :eof
popd
rem copy /b "%srcdir%lib\7-zip\7zS2.sfx" + "%~3\$$packed$$.7z" "%~2"
copy /b "%srcdir%lib\7-zip\7z.sfx" + "%~3\$$packed$$.7z" "%~2"
if errorlevel 1 goto :eof
if exist "%~3\$$packed$$.7z" del "%~3\$$packed$$.7z"
rem call "sign" "%project_title% v%version%%variant_name% - Installer" "%project_url%" "%~2"
exit /b 0
goto :eof

:pack-7z
pushd "%~3"
if exist "$$packed$$.7z" del "$$packed$$.7z"
7z a -t7z -ms=on -mx=9 -md=64m "$$packed$$.7z"
if errorlevel 1 goto :eof
popd
move "%~3\$$packed$$.7z" "%~2"
if errorlevel 1 goto :eof
goto :eof

:pack-zip
pushd "%~3"
if exist "$$packed$$.zip" del "$$packed$$.zip"
kzip /r "$$packed$$.zip"
if errorlevel 1 goto :eof
popd
move "%~3\$$packed$$.zip" "%~2"
if errorlevel 1 goto :eof
goto :eof
