@echo off
if "%~1"=="" (
  start /w "YTD" "%~d0%~p0ytd.exe" --setup
) else (
  start /w "YTD" "%~d0%~p0ytd.exe" %*
)
