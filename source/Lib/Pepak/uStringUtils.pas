unit uStringUtils;

interface

uses
  SysUtils;

type
  Utf8String = AnsiString;

  EStringError = class(Exception);

function OemToAnsi(Value: Pointer; Length: integer): AnsiString; overload;
function OemToAnsi(const Value: AnsiString): AnsiString; overload;
function AnsiToOem(Value: Pointer; Length: integer): AnsiString; overload;
function AnsiToOem(const Value: AnsiString): AnsiString; overload;

function AnsiToWide(Value: Pointer; Length: integer): WideString; overload;
function AnsiToWide(const Value: AnsiString): WideString; overload;
function WideToAnsi(Value: Pointer; Length: integer): AnsiString; overload;
function WideToAnsi(const Value: WideString): AnsiString; overload;

function WideToUtf8(Value: Pointer; Length: integer): Utf8String; overload;
function WideToUtf8(const Value: WideString): Utf8String; overload;
function Utf8ToWide(Value: Pointer; Length: integer): WideString; overload;
function Utf8ToWide(const Value: Utf8String): WideString; overload;

function StrTr(const Kde, Co, Cim: string): string;

implementation

uses
  Windows;

function OemToAnsi(Value: Pointer; Length: integer): AnsiString;
begin
  if Length > 0 then
    begin
    SetLength(Result, Length);
    if not OemToCharBuff(Value, @(Result[1]), Length) then
      Raise EStringError.Create('Conversion failed.');
    end
  else
    Result := '';
end;

function OemToAnsi(const Value: AnsiString): AnsiString;
begin
  Result := OemToAnsi(@(Value[1]), Length(Value));
end;

function AnsiToOem(Value: Pointer; Length: integer): AnsiString;
begin
  if Length > 0 then
    begin
    SetLength(Result, Length);
    if not CharToOemBuff(Value, @(Result[1]), Length) then
      Raise EStringError.Create('Conversion failed.');
    end
  else
    Result := '';
end;

function AnsiToOem(const Value: AnsiString): AnsiString;
begin
  Result := AnsiToOem(@(Value[1]), Length(Value));
end;

function AnythingToWide(CodePage: integer; Value: Pointer; Length: integer): WideString;
var n: integer;
begin
  if Length > 0 then
    begin
    n := 2*Length;
    SetLength(Result, n);
    n := MultiByteToWideChar(CodePage, 0 (*8 {MB_ERR_INVALID_CHARS}*), Value, Length, @(Result[1]), n);
    if n > 0 then
      SetLength(Result, n)
    else
      Raise EStringError.Create('Conversion failed.');
    end
  else
    Result := '';
end;

function AnsiToWide(Value: Pointer; Length: integer): WideString;
begin
  Result := AnythingToWide(CP_ACP, Value, Length);
end;

function AnsiToWide(const Value: AnsiString): WideString;
begin
  Result := AnsiToWide(@(Value[1]), Length(Value));
end;

function WideToAnything(CodePage: integer; Value: Pointer; Length: integer): AnsiString;
var n: integer;
begin
  if Length > 0 then
    begin
    n := 2*Length;
    SetLength(Result, n);
    n := WideCharToMultiByte(CodePage, 0, Value, Length, @(Result[1]), n, nil, nil);
    if n > 0 then
      SetLength(Result, n)
    else
      Raise EStringError.Create('Conversion failed.');
    end
  else
    Result := '';
end;

function WideToAnsi(Value: Pointer; Length: integer): AnsiString;
begin
  Result := WideToAnything(CP_ACP, Value, Length);
end;

function WideToAnsi(const Value: WideString): AnsiString;
begin
  Result := WideToAnsi(@(Value[1]), Length(Value));
end;

function WideToUtf8(Value: Pointer; Length: integer): Utf8String;
begin
  Result := WideToAnything(CP_UTF8, Value, Length);
end;

function WideToUtf8(const Value: WideString): Utf8String;
begin
  Result := WideToUtf8(@(Value[1]), Length(Value));
end;

function Utf8ToWide(Value: Pointer; Length: integer): WideString;
begin
  Result := AnythingToWide(CP_UTF8, Value, Length);
end;

function Utf8ToWide(const Value: Utf8String): WideString;
begin
  Result := Utf8ToWide(@(Value[1]), Length(Value));
end;

function StrTr(const Kde, Co, Cim: string): string;
var i, j: integer;
begin
  Result := Kde;
  if Kde <> '' then
    for i := 1 to Length(Co) do
      repeat
        j := Pos(Co[i], Result);
        if j > 0 then
          Result[j] := Cim[i];
      until j <= 0;
end;

end.
