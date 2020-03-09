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

unit uJSON;
{$INCLUDE 'pepak.inc'}

interface

uses
  SysUtils, 
  {$IFDEF DELPHI6_UP} Variants, {$ENDIF}
  ///{$IFDEF FPC} Variants, {$ENDIF}
  {$IFDEF JSON_SUPEROBJECT}
    SuperObject,
    SuperTypes
  {$ELSE}
    uLkJSON
  {$ENDIF}
  ;

type
  {$IFDEF JSON_SUPEROBJECT}
  TJSON = ISuperObject;
  TJSONNode = TJSON;
  TJSONString = SOString;
  {$ELSE}
  TJSON = TlkJSONbase;
  TJSONNode = TlkJSONbase;
  TJSONString = string;
  {$ENDIF}
  TJSONForEachCallback = procedure(const ElName: string; Elem: TJSONNode; UserData: pointer) of object;

function JSONCreate(const Source: TJSONString): TJSON; overload;
procedure JSONFree(JSON: TJSON);
procedure JSONFreeAndNil(var JSON: TJSON);

function JSONNodeByPath(JSON: TJSONNode; Path: string; out FoundNode: TJSONNode): boolean;
  // Note: numeric indexes are not supported for objects
function JSONValue(JSON: TJSON): string; overload;
function JSONValue(JSON: TJSON; const Path: string): string; overload;
function JSONValue(JSON: TJSON; const Path: string; out Value: string): boolean; overload;
procedure JSONForEach(JSON: TJSONNode; Callback: TJSONForEachCallback; UserData: Pointer);

implementation

function JSONCreate(const Source: TJSONString): TJSON;
begin
  {$IFDEF JSON_SUPEROBJECT}
  Result := SuperObject.SO(Source);
  {$ELSE}
  Result := TlkJSON.ParseText(Source);
  {$ENDIF}
end;

procedure JSONFree(JSON: TJSON);
begin
  {$IFDEF JSON_SUPEROBJECT}
  // Nothing to do, it's a ref-counted interface
  {$ELSE}
  if JSON <> nil then
    JSON.Free;
  {$ENDIF}
end;

procedure JSONFreeAndNil(var JSON: TJSON);
begin
  JSONFree(JSON);
  JSON := nil;
end;

function JSONNodeByPath(JSON: TJSONNode; Path: string; out FoundNode: TJSONNode): boolean;
var VarName: string;
    i: integer;
begin
  Result := False;
  FoundNode := nil;
  if JSON <> nil then
    if Path = '' then
      begin
      Result := True;
      FoundNode := JSON;
      end
    else
      begin
      i := Pos('/', Path);
      if i > 0 then
        begin
        VarName := Copy(Path, 1, Pred(i));
        System.Delete(Path, 1, i);
        end
      else
        begin
        VarName := Path;
        Path := '';
        end;
      {$IFDEF JSON_SUPEROBJECT}
      Result := JSONNodeByPath(JSON.O[VarName], Path, FoundNode);
      {$ELSE}
      if JSON is TlkJSONobject then
        Result := JSONNodeByPath(TlkJSONobject(JSON).Field[VarName], Path, FoundNode)
      else if JSON is TlkJSONcustomlist then
        begin
        i := StrToIntDef(VarName, -1);
        if (i >= 0) and (i < JSON.Count) then
          Result := JSONNodeByPath(JSON.Child[i], Path, FoundNode);
        end;
      {$ENDIF}
      end;
end;

function JSONValue(JSON: TJSON): string;
begin
  {$IFDEF JSON_SUPEROBJECT}
  case JSON.DataType of
    stDouble:
      Result := FloatToStr(JSON.AsDouble);
    stCurrency:
      Result := CurrToStr(JSON.AsCurrency);
    stString:
      Result := JSON.AsString;
    stBoolean:
      if JSON.AsBoolean then
        Result := '1'
      else
        Result := '0';
    stNull:
      Result := '';
    else
      Result := JSON.AsJSON;
  end;
  {$ELSE}
  if JSON is TlkJSONnumber then
    Result := FloatToStr(TlkJSONnumber(JSON).Value)
  else if JSON is TlkJSONstring then
    Result := VarToStr(JSON.Value)
  else if JSON is TlkJSONboolean then
    if JSON.Value = True then
      Result := '1'
    else
      Result := '0'
  else if JSON is TlkJSONnull then
    Result := ''
  else
    Result := TlkJSON.GenerateText(JSON);
  {$ENDIF}
end;

function JSONValue(JSON: TJSON; const Path: string; out Value: string): boolean;
var
  Node: TJSONNode;
begin
  Result := JSONNodeByPath(JSON, Path, Node);
  if Result then
    Value := JSONValue(Node);
end;

function JSONValue(JSON: TJSON; const Path: string): string;
begin
  if not JSONValue(JSON, Path, Result) then
    Result := '';
end;

{$IFNDEF JSON_SUPEROBJECT}
type
  TJsonCallbackInfo = class
    Callback: TJSONForEachCallback;
    procedure LkJsonCallback(ElName: string; Elem: TlkJSONbase; data: pointer; var Continue: Boolean);
  end;

procedure TJsonCallbackInfo.LkJsonCallback(ElName: string; Elem: TlkJSONbase; data: pointer; var Continue: Boolean);
begin
  Continue := True;
  try
    Self.Callback(ElName, Elem, data);
  except
    on EAbort do
      Continue := False;
  end;
end;
{$ENDIF}

procedure JSONForEach(JSON: TJSONNode; Callback: TJSONForEachCallback; UserData: Pointer);
{$IFDEF JSON_SUPEROBJECT}
var
  Item: TSuperObjectIter;
{$ELSE}
var
  Info: TJsonCallbackInfo;
{$ENDIF}
begin
  {$IFDEF JSON_SUPEROBJECT}
  if ObjectFindFirst(JSON, Item) then
    try
      repeat
        try
          Callback(Item.key, Item.val, UserData);
        except
          on EAbort do
            Break;
        end;
      until not ObjectFindNext(Item);
    finally
      ObjectFindClose(Item);
    end;
  {$ELSE}
  if JSON is TlkJSONcustomlist then begin
    Info := TJsonCallbackInfo.Create;
    try
      Info.Callback := Callback;
      TlkJSONcustomlist(JSON).ForEach(Info.LkJsonCallback, UserData);
    finally
      FreeAndNil(Info);
    end;
  end;
  {$ENDIF}
end;

end.
