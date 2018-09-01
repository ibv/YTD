unit msdl_dll;

interface

type
  TMsdlDownloadProgressCallback = procedure(Tag: integer; DownloadedSize, TotalSize: integer; var DoAbort: integer); cdecl;

function Msdl_Init: boolean;
procedure Msdl_Done;

function Msdl_Download(
  Tag: integer; 
  Callback: TMsdlDownloadProgressCallback; 
  LogFileName, AsfFileName, MSUrl: PChar
  ): integer;

implementation

uses
  Windows;

type
  TMsdlMainFn = function(Tag: integer; Callback: TMsdlDownloadProgressCallback; LogFileName, AsfFileName, MSUrl: PChar): integer; cdecl;

var LibHandle: THandle;
    MsdlMain: TMsdlMainFn;

function Msdl_Init: boolean;
begin
  if LibHandle = 0 then
    begin
    LibHandle := LoadLibrary('msdl_dll.dll');
    if LibHandle <> 0 then
      begin
      MsdlMain := GetProcAddress(LibHandle, 'MsdlMain');
      end;
    end;
  Result := (LibHandle <> 0) and (@MsdlMain <> nil);
end;

procedure Msdl_Done;
begin
  if LibHandle <> 0 then
    FreeLibrary(LibHandle);
  LibHandle := 0;
  MsdlMain := nil;
end;

function Msdl_Download(Tag: integer; Callback: TMsdlDownloadProgressCallback; LogFileName, AsfFileName, MSUrl: PChar): integer;
begin
  Msdl_Init;
  Result := MsdlMain(Tag, Callback, LogFileName, AsfFileName, MSUrl);
end;

initialization
  LibHandle := 0;
  MsdlMain := nil;

finalization
  Msdl_Done;

end.
