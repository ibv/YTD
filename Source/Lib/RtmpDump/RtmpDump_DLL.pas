unit rtmpdump_dll;

interface

type
  TRtmpDumpDownloadProgressCallback = procedure(Tag: integer; DownloadedSize: integer; PercentDone: double; var DoAbort: integer); cdecl;

function RtmpDump_Init: boolean;
procedure RtmpDump_Done;

function RtmpDump_Download(
  Tag: integer; 
  Callback: TRtmpDumpDownloadProgressCallback; 
  LogFileName, FlvFileName, RtmpUrl, PlayPath: PChar
  ): integer;

implementation

uses
  Windows;

type
  TRtmpDumpMainFn = function(Tag: integer; Callback: TRtmpDumpDownloadProgressCallback; LogFileName, FlvFileName, RtmpUrl, PlayPath: PChar): integer; cdecl;

var LibHandle: THandle;
    RtmpDumpMain: TRtmpDumpMainFn;

function RtmpDump_Init: boolean;
begin
  if LibHandle = 0 then
    begin
    LibHandle := LoadLibrary('rtmpdump_dll.dll');
    if LibHandle <> 0 then
      begin
      RtmpDumpMain := GetProcAddress(LibHandle, 'RtmpDumpMain');
      end;
    end;
  Result := (LibHandle <> 0) and (@RtmpDumpMain <> nil);
end;

procedure RtmpDump_Done;
begin
  if LibHandle <> 0 then
    FreeLibrary(LibHandle);
  LibHandle := 0;
  RtmpDumpMain := nil;
end;

function RtmpDump_Download(Tag: integer; Callback: TRtmpDumpDownloadProgressCallback; LogFileName, FlvFileName, RtmpUrl, PlayPath: PChar): integer;
begin
  RtmpDump_Init;
  Result := RtmpDumpMain(Tag, Callback, LogFileName, FlvFileName, RtmpUrl, PlayPath);
end;

initialization
  LibHandle := 0;
  RtmpDumpMain := nil;

finalization
  RtmpDump_Done;

end.
