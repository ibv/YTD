

pushd ..\Install\
@echo #define AppVer '%~1' ; > version.txt
popd

c:\lazarus\lazbuild.exe --cpu=x86_64 YTD.lpi
rem "c:\Program Files (x86)\Inno Setup 6\ISCC.exe" ..\Install\ytd-64.iss  

pushd ..\Bin
..\Source\Lib\7-zip\7za a -t7z -ms=on -mx=9 -md=64m ytd-"%~1"_x64.7z @..\Install\archive7z_64.txt
popd


c:\lazarus\lazbuild.exe --cpu=i386 YTD.lpi
rem "c:\Program Files (x86)\Inno Setup 5\ISCC.exe" ..\Install\ytd-32-xp.iss

pushd ..\Bin
..\Source\Lib\7-zip\7za a -t7z -ms=on -mx=9 -md=64m ytd-"%~1".7z @..\Install\archive7z_32.txt
popd
