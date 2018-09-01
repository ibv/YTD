@echo off
setlocal
set delphi=call dcc32 -B -E..\Exe -N..\Units -U..\Units
del /q Units\*.*
pushd Source
%delphi% lib\lkJSON\uLkJSON.pas
if errorlevel 1 goto konec
%delphi% lib\Synapse\source\lib\*.pas
if errorlevel 1 goto konec
%delphi% lib\dpcre67\pcre.pas
if errorlevel 1 goto konec
%delphi% lib\janXmlParser2\janXmlParser2.pas
if errorlevel 1 goto konec
%delphi% lib\Pepak\*.pas
if errorlevel 1 goto konec
%delphi% YTD.dpr
if errorlevel 1 goto konec
popd
upx Exe\ytd.exe

:konec
