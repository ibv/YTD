@echo off
del /s /q Units\*.*
echo All compiled units come here. They can be rebuilt any time using the build.bat >Units\!_info.txt
echo script, so you can delete any files in this directory whenever you like. >>Units\!_info.txt
del /s /q *.~*
del /s /q *.dcu
del /s /q *.dsk
del /s /q *.drc
del /s /q *.exe
del /s /q *.map
