unit uCompatibility;
{$INCLUDE 'jedi.inc'}

interface

{$IFNDEF DELPHI2009_UP}
uses
  SysUtils;

type
  TSysCharSet = set of Char;

type
  EOSError = EWin32Error;

type
  Utf8String = AnsiString;

type
  TSeekOrigin = Word;

const
  soBeginning = 0;
  soCurrent = 1;
  soEnd = 2;

const
  FILE_ATTRIBUTE_REPARSE_POINT = $400;

  LVS_EX_DOUBLEBUFFER     = $00010000;
  LVS_EX_LABELTIP         = $00004000; { listview unfolds partly hidden labels if it does not have infotip text }

  TBSTYLE_EX_MIXEDBUTTONS       = $00000008;
  TBSTYLE_EX_HIDECLIPPEDBUTTONS = $00000010;
  TBSTYLE_EX_DOUBLEBUFFER       = $00000080;

const
  HoursPerDay = 24;
  MinsPerHour = 60;
  MinsPerDay = HoursPerDay * MinsPerHour;

function CharInSet(C: Char; S: TSysCharSet): boolean;

function IncludeTrailingPathDelimiter(const Path: string): string;
function ExcludeTrailingPathDelimiter(const Path: string): string;
{$ENDIF}

{$IFNDEF DELPHI2010_UP}
const
  WM_XBUTTONDOWN          = $020B;

  XBUTTON1                = 1;
  XBUTTON2                = 2;
{$ENDIF}

implementation

{$IFNDEF DELPHI2009_UP}
function CharInSet(C: Char; S: TSysCharSet): boolean;
begin
  Result := C in S;
end;

function IncludeTrailingPathDelimiter(const Path: string): string;
begin
  Result := IncludeTrailingBackslash(Path);
end;

function ExcludeTrailingPathDelimiter(const Path: string): string;
begin
  Result := ExcludeTrailingBackslash(Path);
end;
{$ENDIF}

end.
