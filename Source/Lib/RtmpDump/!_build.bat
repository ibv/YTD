@echo off
if exist rtmpdump_dll.dll del rtmpdump_dll.dll
make SYS=mingw CRYPTO=POLARSSL OPT=-Os
if errorlevel 1 goto :eof
rem upx --lzma --best --brute --ultra-brute rtmpdump_dll.dll
if errorlevel 1 goto :eof
rem make clean
if errorlevel 1 goto :eof
rem del *.exe
