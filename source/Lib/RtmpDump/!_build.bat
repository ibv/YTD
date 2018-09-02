@echo off
if exist rtmpdump_dll.dll del rtmpdump_dll.dll
make SYS=mingw CRYPTO=POLARSSL OPT=-Os -f Makefile.YTD
upx --lzma --best --brute --ultra-brute rtmpdump_dll.dll
make clean
del *.exe
