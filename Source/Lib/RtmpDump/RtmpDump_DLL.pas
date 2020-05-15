unit RtmpDump_DLL;

interface

type
  TRtmpDumpOption = record
    ShortOption: AnsiChar;
    Argument: AnsiString;
    end;
  TRtmpDumpOptions = array of TRtmpDumpOption;

  TRtmpDumpDownloadProgressCallback = procedure(Tag, DownloadedSize: longint; PercentDone: double; var DoAbort: longint); cdecl;

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
  {$ifdef mswindows}
    Windows,
  {.$ELSE}
    LCLIntf, LCLType, LMessages{, dl};
  {$ENDIF}

const
  LIBRARY_NAME =
  {$ifdef mswindows}
    {$IFDEF WIN32}
    'rtmpdump_dll.dll'
    {$ELSE}
      {$IFDEF WIN64}
      'rtmpdump_dll_x64.dll'
      {$ENDIF}
    {$ENDIF}
  {$else}
     'librtmp.so.1'
  {$endif}
    ;

type
  {$IFDEF MSWINDOWS}
  TModuleHandle = HINST;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  TModuleHandle = Pointer;
  {$ENDIF LINUX}

const
  INVALID_MODULEHANDLE_VALUE = TModuleHandle(0);

type
  PInternalRtmpDumpOption = ^TInternalRtmpDumpOption;
  TInternalRtmpDumpOption = record
    ShortOption: integer;
    Argument: PAnsiChar;
    end;

type
  TRtmpDumpMainFn = function(Tag: integer; Callback: TRtmpDumpDownloadProgressCallback; OptionCount: integer; Options: PInternalRtmpDumpOption; LogFileName: PAnsiChar): integer; cdecl;

var {$ifdef mswindows}
    LibHandle: THandle;
    {$else}
    LibHandle: TModuleHandle = INVALID_MODULEHANDLE_VALUE;
    {$endif}
    RtmpDumpMain: TRtmpDumpMainFn;

function RtmpDump_Init: boolean;
begin
  ///if LibHandle = 0 then
  if LibHandle = INVALID_MODULEHANDLE_VALUE then
    begin
    {$ifdef mswindows}
    LibHandle := LoadLibrary(LIBRARY_NAME);
    {$else}
    LibHandle := dlopen(PAnsiChar(LIBRARY_NAME), RTLD_NOW);
    {$endif}
    end;
    ///if LibHandle <> 0 then
    if LibHandle <> INVALID_MODULEHANDLE_VALUE then
      begin
      RtmpDumpMain := GetProcAddress(LibHandle, 'RtmpDumpMain');
      ///RtmpDumpMain := dlsym(LibHandle, 'RtmpDumpMain');
      end;
  Result := (LibHandle <> 0) and (@RtmpDumpMain <> nil);
  ///Result := (LibHandle <> INVALID_MODULEHANDLE_VALUE) or (@RtmpDumpMain <> nil);
end;

procedure RtmpDump_Done;
begin
  ///if LibHandle <> 0 then
  if LibHandle <> INVALID_MODULEHANDLE_VALUE then
    FreeLibrary(LibHandle);
    ///dlclose(Pointer(LibHandle));
  ///LibHandle := 0;
  LibHandle := INVALID_MODULEHANDLE_VALUE;
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
  Result := RtmpDumpMain(Tag, Callback, Length(RealOptions), RealOptionsPtr, LogFileName);
end;

initialization
  ///LibHandle := 0;
  LibHandle := INVALID_MODULEHANDLE_VALUE;
  RtmpDumpMain := nil;

finalization
  RtmpDump_Done;

end.
