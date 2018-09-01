unit rtmpdump_dll;

interface

type
  TRtmpDumpOption = record
    ShortOption: AnsiChar;
    Argument: AnsiString;
    end;
  TRtmpDumpOptions = array of TRtmpDumpOption;

  TRtmpDumpDownloadProgressCallback = procedure(Tag: integer; DownloadedSize: integer; PercentDone: double; var DoAbort: integer); cdecl;

function RtmpDump_Init: boolean;
procedure RtmpDump_Done;

function RtmpDump_Download(
  Tag: integer; 
  Callback: TRtmpDumpDownloadProgressCallback; 
  LogFileName: PAnsiChar;
  const Options: TRtmpDumpOptions
  ): integer;

implementation

uses
  Windows;

type
  PInternalRtmpDumpOption = ^TInternalRtmpDumpOption;
  TInternalRtmpDumpOption = record
    ShortOption: integer;
    Argument: PAnsiChar;
    end;

type
  TRtmpDumpMainFn = function(Tag: integer; Callback: TRtmpDumpDownloadProgressCallback; OptionCount: integer; Options: PInternalRtmpDumpOption; LogFileName: PAnsiChar): integer; cdecl;

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

function RtmpDump_Download(Tag: integer; Callback: TRtmpDumpDownloadProgressCallback; LogFileName: PAnsiChar; const Options: TRtmpDumpOptions): integer;
var i, n: integer;
    RealOptionsPtr: PInternalRtmpDumpOption;
    RealOptions: array of TInternalRtmpDumpOption;
begin
  RtmpDump_Init;
  n := Length(Options);
  if n <= 0 then
    RealOptionsPtr := nil
  else
    begin
    SetLength(RealOptions, n);
    RealOptionsPtr := @RealOptions[0];
    for i := 0 to Pred(n) do
      begin
      RealOptions[i].ShortOption := Ord(Options[i].ShortOption);
      if Options[i].Argument = '' then
        RealOptions[i].Argument := nil
      else
        RealOptions[i].Argument := PAnsiChar(Options[i].Argument);
      end;
    end;
  Result := RtmpDumpMain(Tag, Callback, Length(Options), RealOptionsPtr, LogFileName);
end;

initialization
  LibHandle := 0;
  RtmpDumpMain := nil;

finalization
  RtmpDump_Done;

end.
