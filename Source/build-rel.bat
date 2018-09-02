@echo off
setlocal
call "%~d0%~p0Lib\Pepak\Build\release" %1 " Lite" "-lite" exe 0 1 noxxx %2 %3 %4 %5 %6 %7 %8 %9
call "%~d0%~p0Lib\Pepak\Build\release" %1 " Lite" "-lite" zip 1 1 noxxx %2 %3 %4 %5 %6 %7 %8 %9
call "%~d0%~p0Lib\Pepak\Build\release" %1 "-" "-" exe 0 1 %2 %3 %4 %5 %6 %7 %8 %9
call "%~d0%~p0Lib\Pepak\Build\release" %1 "-" "-" zip 1 0 %2 %3 %4 %5 %6 %7 %8 %9
goto :eof
