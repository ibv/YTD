(******************************************************************************

______________________________________________________________________________

libPepak                                                     (c) 2009-11 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2011, Pepak (http://www.pepak.net)
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Pepak nor the
      names of his contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL PEPAK BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

******************************************************************************)

unit uStringUtils;

interface

uses
  SysUtils,
  uCompatibility;

type
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

function Utf8ToString(const Value: Utf8String): string;
function StringToUtf8(const Value: string; BOM: boolean = False): Utf8String;

function StrTr(const Kde, Co, Cim: string): string;
function DeletePrefix(const Kde, Prefix: string): string;

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
  Result := Utf8String(WideToAnything(CP_UTF8, Value, Length));
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

function Utf8ToString(const Value: Utf8String): string;
begin
  {$IFDEF UNICODE}
  Result := Utf8ToWide(Value);
  {$ELSE}
  Result := WideToAnsi(Utf8ToWide(Value));
  {$ENDIF}
end;

function StringToUtf8(const Value: string; BOM: boolean): Utf8String;
begin
  {$IFDEF UNICODE}
  Result := WideToUtf8(Value);
  {$ELSE}
  Result := WideToUtf8(AnsiToWide(Value));
  {$ENDIF}
  if BOM then
    Result := #$ef#$bb#$bf + Result;
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

function DeletePrefix(const Kde, Prefix: string): string;
var n: integer;
begin
  n := Length(Prefix);
  if AnsiCompareText(Copy(Kde, 1, n), Prefix) = 0 then
    Result := Copy(Kde, Succ(n), MaxInt)
  else
    Result := Kde;
end;

end.
