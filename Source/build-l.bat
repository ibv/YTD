

@echo '%~1'  > ytd.version

pushd ..\Install\
@echo #define AppVer '%~1' ; > version.txt
popd


rem ##### 64 bits #####
c:\lazarus\lazbuild.exe --cpu=x86_64 YTD.lpi
pushd ..\Bin
if exist ytd-64.exe del /f ytd-64.exe
ren ytd.exe ytd-64.exe
popd

rem pushd ..\Bin
rem ..\Source\Lib\7-zip\7za a -t7z -ms=on -mx=9 -md=64m ..\Install\ytd-"%~1"_x64.7z @..\Install\archive7z_64.txt
rem popd


rem ##### 32 bits #####
c:\lazarus\lazbuild.exe --cpu=i386 YTD.lpi
rem "c:\Program Files (x86)\Inno Setup 6\ISCC.exe" ..\Install\ytd-32.iss

rem pushd ..\Bin
rem ..\Source\Lib\7-zip\7za a -t7z -ms=on -mx=9 -md=64m ..\Install\ytd-"%~1".7z @..\Install\archive7z_32.txt
rem popd

rem #### Instalator 64 bits #########
"c:\Program Files (x86)\Inno Setup 6\ISCC.exe" ..\Install\ytd-64.iss  

rem #### Instalator All bits #########
rem "c:\Program Files (x86)\Inno Setup 6\ISCC.exe" ..\Install\ytd.iss  



rem ##### 7z All bits #####
pushd ..\Bin

if not exist 32bit (
  mkdir 32bit
) 
if not exist 64bit (
  mkdir 64bit
) 
rem 32bit
if exist 32bit (
   copy /y ytd.exe 32bit
   copy /y libcrypto-1_1.dll 32bit 
   copy /y libssl-1_1.dll 32bit
   copy /y pcre32.dll 32bit
)
rem 64bit
if exist 64bit (
   copy /y ytd-64.exe 64bit
   copy /y libcrypto-1_1-x64.dll 64bit 
   copy /y libssl-1_1-x64.dll 64bit
   copy /y pcre64.dll 64bit

)

..\Source\Lib\7-zip\7za a -t7z -ms=on -mx=9 -md=64m ..\Install\ytd-"%~1"-x32_64.7z @..\Install\archive7z.txt
popd


