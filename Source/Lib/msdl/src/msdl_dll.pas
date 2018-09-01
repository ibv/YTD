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
  Windows;

type
  PInternalMsdlOption = ^TInternalMsdlOption;
  TInternalMsdlOption = record
    ShortOption: integer;
    Argument: PAnsiChar;
    end;

type
  TMsdlMainFn = function(Tag: integer; Callback: TMsdlDownloadProgressCallback; OptionCount: integer; Options: PInternalMsdlOption): integer; cdecl;

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
  LibHandle := 0;
  MsdlMain := nil;

finalization
  Msdl_Done;

end.
