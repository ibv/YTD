unit msdl_dll;

interface

type
  TMsdlOption = record
    ShortOption: AnsiChar;
    Argument: AnsiString;
    end;
  TMsdlOptions = array of TMsdlOption;

  TMsdlDownloadProgressCallback = procedure(Tag: integer; DownloadedSize, TotalSize: integer; var DoAbort: integer); cdecl;

const 
  MSDL_OPTION_URL = #0;

function Msdl_Init: boolean;
procedure Msdl_Done;

function Msdl_Download(
  Tag: integer; 
  Callback: TMsdlDownloadProgressCallback; 
  const Options: TMsdlOptions
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
    {$IFDEF WIN32}
    'msdl_dll.dll'
    {$ELSE}
      {$IFDEF WIN64}
      'msdl_dll_x64.dll'
      {$ENDIF}
    {$ENDIF}
    ///'libmsdl.so'
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
  PInternalMsdlOption = ^TInternalMsdlOption;
  TInternalMsdlOption = record
    ShortOption: integer;
    Argument: PAnsiChar;
    end;

type
  TMsdlMainFn = function(Tag: integer; Callback: TMsdlDownloadProgressCallback; OptionCount: integer; Options: PInternalMsdlOption): integer; cdecl;

var {$ifdef mswimdows}
    LibHandle: THandle;
    {$else}
    LibHandle: TModuleHandle = INVALID_MODULEHANDLE_VALUE;
    {$endif}

    MsdlMain: TMsdlMainFn;

function Msdl_Init: boolean;
begin
  ///if LibHandle = 0 then
  if LibHandle = INVALID_MODULEHANDLE_VALUE then
    begin
    LibHandle := LoadLibrary(LIBRARY_NAME);
    ///LibHandle := dlopen(PAnsiChar(LIBRARY_NAME), RTLD_NOW);
    ///if LibHandle <> 0 then
    if LibHandle <> INVALID_MODULEHANDLE_VALUE then
      begin
      MsdlMain := GetProcAddress(LibHandle, 'MsdlMain');
      ///MsdlMain := dlsym(LibHandle, 'MsdlMain');
      end;
    end;
  Result := (LibHandle <> INVALID_MODULEHANDLE_VALUE) and (@MsdlMain <> nil);
end;

procedure Msdl_Done;
begin
  if LibHandle <> INVALID_MODULEHANDLE_VALUE then
    FreeLibrary(LibHandle);
    ///dlclose(Pointer(LibHandle));
  LibHandle := INVALID_MODULEHANDLE_VALUE;
  MsdlMain := nil;
end;

function Msdl_Download(Tag: integer; Callback: TMsdlDownloadProgressCallback; const Options: TMsdlOptions): integer;
var i, n: integer;
    RealOptionsPtr: PInternalMsdlOption;
    RealOptions: array of TInternalMsdlOption;
begin
  Msdl_Init;
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
  Result := MsdlMain(Tag, Callback, Length(Options), RealOptionsPtr);
end;

initialization
  ///LibHandle := 0;
  LibHandle := INVALID_MODULEHANDLE_VALUE;
  MsdlMain := nil;

finalization
  Msdl_Done;

end.
