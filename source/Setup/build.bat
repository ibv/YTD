@echo off
setlocal
set masm=d:\progs\masm32

if exist setup.exe del setup.exe
if exist setup.obj del setup.obj
"%masm%\bin\ml.exe" /c /coff /nologo /I"%masm%\include" setup.asm
if errorlevel 1 goto konec
"%masm%\bin\link.exe" /SUBSYSTEM:WINDOWS /MERGE:.rdata=.text /LIBPATH:"%masm%\lib" setup.obj
if exist setup.obj del setup.obj
