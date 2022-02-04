@echo off
setlocal
if "%~1"=="" goto :syntax

set version=
set variant_name=
set variant_suffix=
set compress=
set skip_build=
set skip_source=

if not "%~1"=="" (
  set version=%~1
  shift /1
)
if not "%~1"=="" (
  if not "%~1"=="-" set variant_name=%~1
  shift /1
)
if not "%~1"=="" (
  if not "%~1"=="-" set variant_suffix=%~1
  shift /1
)
if not "%~1"=="" (
  set compress=%~1
  shift /1
)
if not "%~1"=="" (
  set skip_build=%~1
  shift /1
)
if not "%~1"=="" (
  set skip_source=%~1
  shift /1
)

call "%~d0%~p0release-internal" "%version%" %1 %2 %3 %4 %5 %6 %7 %8 %9
if errorlevel 1 exit 1
goto :eof

:syntax
echo %~n0 ^<version^> [variant_name] [variant_suffix] [compression] [skip_build] [skip_source] [user params ...]
echo Examples:
echo   %~n0 1.23 "" "" exe 0 0
echo   %~n0 1.23 " Lite" "-lite" exe 0 1
goto :eof
