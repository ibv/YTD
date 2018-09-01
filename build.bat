@echo off
setlocal
set delphi=call dcc32 -B -E..\Exe -N..\Units -U..\Units
del /q Units\*.*
pushd Source
%delphi% lib\lkJSON\uLkJSON.pas
%delphi% lib\Synapse\source\lib\*.pas
%delphi% lib\dpcre67\pcre.pas
%delphi% lib\janXmlParser2\janXmlParser2.pas
%delphi% lib\Pepak\*.pas
%delphi% YTD.dpr
popd
