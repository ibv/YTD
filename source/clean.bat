@echo off
del /q ..\Exe\ytd.xml >nul 2>&1
del /s /q Units\*.* >nul 2>&1
echo All compiled units come here. They can be rebuilt any time using the build.bat >Units\!_info.txt
echo script, so you can delete any files in this directory whenever you like. >>Units\!_info.txt
del /s /q *.~* >nul 2>&1
del /s /q *.dcu >nul 2>&1
del /s /q *.dsk >nul 2>&1
del /s /q *.drc >nul 2>&1
del /s /q *.exe >nul 2>&1
del /s /q *.map >nul 2>&1
