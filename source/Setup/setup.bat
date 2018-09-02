@echo off
if "%~1"=="" (
  start /w "YouTube Downloader" "%~d0%~p0ytd.exe" --setup
) else (
  start /w "YouTube Downloader" "%~d0%~p0ytd.exe" %*
)
