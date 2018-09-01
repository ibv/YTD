unit uCompatibility;
{$INCLUDE 'jedi.inc'}

interface

{$IFNDEF DELPHI2009_UP}
uses
  SysUtils;

type
  TSysCharSet = set of Char;
  Utf8String = AnsiString;

const
  FILE_ATTRIBUTE_REPARSE_POINT = $400;

function CharInSet(C: Char; S: TSysCharSet): boolean;
function IncludeTrailingPathDelimiter(const Path: string): string;
function ExcludeTrailingPathDelimiter(const Path: string): string;
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
