(******************************************************************************

______________________________________________________________________________

YTD v1.00                                                    (c) 2009-12 Pepak
http://www.pepak.net/ytd                                  http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2009-12 Pepak (http://www.pepak.net)
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

unit uPCRE;
{$INCLUDE 'pepak.inc'}

interface

///{$UNDEF UPCRE_NATIVEIMPLEMENTATION}
{$DEFINE UPCRE_NATIVEIMPLEMENTATION}
{$IFDEF WIN64}
  {$DEFINE UPCRE_NATIVEIMPLEMENTATION}
{$ENDIF}

uses
  SysUtils, Classes,
   {$ifdef mswindows}
     Windows,
   {.$ELSE}
     LCLIntf, LCLType, LMessages,
   {$ENDIF}

  {$IFDEF UPCRE_NATIVEIMPLEMENTATION}
  PerlRegEx {RegularExpressionsCore}
  {$ELSE}
  PerlRegEx
  {$ENDIF}
  ;

type
  ///PCREString = {$IFDEF UPCRE_NATIVEIMPLEMENTATION} UTF8String {$ELSE} PerlRegEx.PCREString {$ENDIF};
  PCREString = {$IFNDEF UPCRE_NATIVEIMPLEMENTATION} UTF8String {$ELSE} PerlRegEx.PCREString {$ENDIF};

  TRegExp = class(TPerlRegEx)
    public
      class function Match(const Pattern, Subject, SubexpressionName: PCREString; out Value: PCREString): boolean; overload;
      function Match(const Subject: PCREString): boolean; overload;
      function SubexpressionByName(const Name: PCREString): PCREString; overload;
      function SubexpressionByName(const Name: PCREString; out Value: PCREString): boolean; overload;
      function SubexpressionByNameEx(const Name: PCREString; out Value: PCREString): boolean; overload;
      {$IFDEF UNICODE}
      class function Match(const Pattern, Subject, SubexpressionName: string; out Value: string): boolean; overload;
      function Match(const Subject: string): boolean; overload;
      function SubexpressionByName(const Name: string): string; overload;
      function SubexpressionByName(const Name: string; out Value: string): boolean; overload;
      function SubexpressionByNameEx(const Name: string; out Value: string): boolean; overload;
      {$ENDIF}
    end;

  TRegExpMatch = TRegExp;

  TRegExpOption = (rcoIgnoreCase, rcoMultiLine, rcoSingleLine, rcoIgnorePatternWhitespace, rcoAnchored, rcoUngreedy, rcoNoAutoCapture);
  TRegExpOptions = set of TRegExpOption;
  TRegExpNativeOptions = TPerlRegExOptions;

  TRegExpCache = class(TObject)
    private
      List: TList;
      function GetCount: integer;
      function GetItem(Index: integer): TRegExp;
    protected
      property Items[Index: integer]: TRegExp read GetItem; default;
      property Count: integer read GetCount;
      function Find(const Pattern: string; Options: TRegExpOptions; out Index: integer): boolean;
    public
      constructor Create;
      destructor Destroy; override;
      function GetRegExp(const Pattern: string; Options: TRegExpOptions): TRegExp; overload;
      function GetRegExp(const Pattern: string): TRegExp; overload;
    end;

function RegExCreate(const Pattern: PCREString; Options: TRegExpOptions): TRegExp; overload;
function RegExCreate(const Pattern: PCREString): TRegExp; overload;
{$IFDEF UNICODE}
function RegExCreate(const Pattern: string; Options: TRegExpOptions): TRegExp; overload;
function RegExCreate(const Pattern: string): TRegExp; overload;
{$ENDIF}
procedure RegExFree(RegExp: TRegExp);
procedure RegExFreeAndNil(var RegExp: TRegExp);

const
  REGEXP_DEFAULT_OPTIONS = [rcoIgnoreCase, rcoSingleLine];

implementation

function OptionsToNativeOptions(Options: TRegExpOptions): TRegExpNativeOptions;
begin
  Result := [];
  if rcoIgnoreCase              in Options then Result := Result + [preCaseLess];
  if rcoMultiLine               in Options then Result := Result + [preMultiLine];
  if rcoSingleLine              in Options then Result := Result + [preSingleLine];
  if rcoIgnorePatternWhitespace in Options then Result := Result + [preExtended];
  if rcoAnchored                in Options then Result := Result + [preAnchored];
  if rcoUngreedy                in Options then Result := Result + [preUnGreedy];
  if rcoNoAutoCapture           in Options then Result := Result + [preNoAutoCapture];
end;

{$HINTS OFF}
function RegExCreate(const Pattern: PCREString; Options: TRegExpOptions): TRegExp;
begin
  Result := TRegExp.Create  ;
  try
    Result.Options := OptionsToNativeOptions(Options);
    Result.RegEx := Pattern;
  except
    Result.Free;
    Result := nil;
    Raise;
    end;
end;
{$HINTS ON}

function RegExCreate(const Pattern: PCREString): TRegExp;
begin
  Result := RegExCreate(Pattern, REGEXP_DEFAULT_OPTIONS);
end;

{$IFDEF UNICODE}
function RegExCreate(const Pattern: string; Options: TRegExpOptions): TRegExp;
begin
  Result := RegExCreate(PCREString(Pattern), Options);
end;

function RegExCreate(const Pattern: string): TRegExp;
begin
  Result := RegExCreate(PCREString(Pattern));
end;
{$ENDIF}

procedure RegExFree(RegExp: TRegExp);
begin
  if RegExp <> nil then
    RegExp.Free;
end;

procedure RegExFreeAndNil(var RegExp: TRegExp);
begin
  RegExFree(RegExp);
  RegExp := nil;
end;

{ TRegExp }

class function TRegExp.Match(const Pattern, Subject, SubexpressionName: PCREString; out Value: PCREString): boolean;
var
  RE: TRegExp;
begin
  Result := False;
  RE := RegExCreate(Pattern);
  try
    if RE.Match(Subject) then
      Result := RE.SubexpressionByName(SubexpressionName, Value);
  finally
    RegExFreeAndNil(RE);
    end;
end;

{$IFDEF UNICODE}
class function TRegExp.Match(const Pattern, Subject, SubexpressionName: string; out Value: string): boolean;
var
  V: PCREString;
begin
  Result := Match(PCREString(Pattern), PCREString(Subject), PCREString(SubexpressionName), V);
  if Result then
    Value := string(V);
end;
{$ENDIF}

function TRegExp.Match(const Subject: PCREString): boolean;
begin
  Self.Subject := Subject;
  Result := Self.Match;
end;

function TRegExp.SubexpressionByName(const Name: PCREString): PCREString;
begin
  Self.SubexpressionByName(Name, Result);
end;

function TRegExp.SubexpressionByName(const Name: PCREString; out Value: PCREString): boolean;
var ix: integer;
begin
  ix := {$IFDEF UPCRE_NATIVEIMPLEMENTATION} Self.NamedGroup {$ELSE} Self.NamedSubExpression {$ENDIF} (Name);
  Result := ix >= 0;
  if Result then
    Value := {$IFDEF UPCRE_NATIVEIMPLEMENTATION} Self.Groups {$ELSE} Self.SubExpressions {$ENDIF} [ix]
  else
    Value := '';
end;

function TRegExp.SubexpressionByNameEx(const Name: PCREString; out Value: PCREString): boolean;
var ix: integer;
begin
  if Name = '' then begin
    if {$IFDEF UPCRE_NATIVEIMPLEMENTATION} Self.GroupCount {$ELSE} Self.SubexpressionCount {$ENDIF} > 0 then
      Value := {$IFDEF UPCRE_NATIVEIMPLEMENTATION} Self.Groups {$ELSE} Self.Subexpressions {$ENDIF} [1]
    else
      Value := {$IFDEF UPCRE_NATIVEIMPLEMENTATION} Self.MatchedText {$ELSE} Self.MatchedExpression {$ENDIF} ;
    Result := True;
  end
  else begin
    Result := SubExpressionByName(Name, Value);
    if not Result then begin
      ix := StrToIntDef( {$IFDEF UNICODE} string {$ENDIF} (Name), -1);
      if ix = 0 then
        Value := {$IFDEF UPCRE_NATIVEIMPLEMENTATION} Self.MatchedText {$ELSE} Self.MatchedExpression {$ENDIF}
      else if (ix > 0) and (ix <= {$IFDEF UPCRE_NATIVEIMPLEMENTATION} Self.GroupCount {$ELSE} Self.SubexpressionCount {$ENDIF} ) then begin
        Value := {$IFDEF UPCRE_NATIVEIMPLEMENTATION} Self.Groups {$ELSE} Self.SubExpressions {$ENDIF} [ix];
        Result := True;
      end;
    end;
  end;
end;

{$IFDEF UNICODE}
function TRegExp.Match(const Subject: string): boolean;
begin
  Result := Self.Match(PCREString(Subject));
end;

function TRegExp.SubexpressionByName(const Name: string): string;
begin
  Self.SubexpressionByName(Name, Result);
end;

function TRegExp.SubexpressionByName(const Name: string; out Value: string): boolean;
var s: PCREString;
begin
  Result := Self.SubexpressionByName(PCREString(Name), s);
  Value := string(s);
end;

function TRegExp.SubexpressionByNameEx(const Name: string; out Value: string): boolean;
var s: PCREString;
begin
  Result := Self.SubexpressionByNameEx(PCREString(Name), s);
  Value := string(s);
end;
{$ENDIF}

{ TRegExpCache }

constructor TRegExpCache.Create;
begin
  inherited Create;
  List := TList.Create;
end;

destructor TRegExpCache.Destroy;
var
  i: integer;
begin
  for i := 0 to Pred(Count) do
    Items[i].Free;
  List.Clear;
  FreeAndNil(List);
  inherited;
end;

function TRegExpCache.Find(const Pattern: string; Options: TRegExpOptions; out Index: integer): boolean;

  function CompareMemory(P1, P2: Pointer; Length: integer): integer;
    type
      PByte = ^Byte;
    begin
      Result := 0;
      while (Result = 0) and (Length > 0) do
        begin
        if PByte(P1)^ < PByte(P2)^ then
          Result := -1
        else if PByte(P1)^ > PByte(P2)^ then
          Result := 1;
        Dec(Length);
        end;
    end;

var
  Item: TRegExp;
  NativeOptions, ItemNativeOptions: TRegExpNativeOptions;
  L, H, I, C: Integer;
begin
  Result := False;
  NativeOptions := OptionsToNativeOptions(Options);
  L := 0;
  H := Pred(Count);
  while L <= H do
    begin
    I := (L + H) shr 1;
    Item := Items[I];
    C := AnsiCompareStr( {$IFDEF UNICODE} string {$ENDIF} (Item.RegEx), Pattern);
    if C = 0 then
      begin
      ItemNativeOptions := Item.Options;
      C := CompareMemory(@ItemNativeOptions, @NativeOptions, Sizeof(TRegExpNativeOptions));
      end;
    if C < 0
      then L := Succ(I)
    else
      begin
      H := Pred(I);
      if C = 0 then
        begin
        Result := True;
        L := I;
        end;
      end;
    end;
  Index := L;
end;

function TRegExpCache.GetCount: integer;
begin
  Result := List.Count;
end;

function TRegExpCache.GetItem(Index: integer): TRegExp;
begin
  Result := TRegExp(List[Index]);
end;

function TRegExpCache.GetRegExp(const Pattern: string): TRegExp;
begin
  Result := GetRegExp(Pattern, REGEXP_DEFAULT_OPTIONS);
end;

function TRegExpCache.GetRegExp(const Pattern: string; Options: TRegExpOptions): TRegExp;
var
  Index: integer;
begin
  if Find(Pattern, Options, Index) then
    Result := Items[Index]
  else
    begin
    Result := RegExCreate(Pattern, Options);
    List.Insert(Index, Result);
    end;
end;

end.
