@echo off
setlocal
call "%~d0%~p0release" %1 " FAR2/x86" ".far2.x86" none 0 1 far2 x32 %2 %3 %4 %5 %6 %7 %8 %9
call "%~d0%~p0release" %1 " FAR2/x64" ".far2.x64" none 0 1 far2 x64 %2 %3 %4 %5 %6 %7 %8 %9
call "%~d0%~p0release" %1 " FAR3/x86" ".far3.x86" none 0 1 far3 x32 %2 %3 %4 %5 %6 %7 %8 %9
call "%~d0%~p0release" %1 " FAR3/x64" ".far3.x64" none 0 1 far3 x64 %2 %3 %4 %5 %6 %7 %8 %9
call "%~d0%~p0release" %1 "-" "-" zip 1 0 %2 %3 %4 %5 %6 %7 %8 %9
goto :eof
