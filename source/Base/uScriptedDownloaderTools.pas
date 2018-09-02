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

unit uScriptedDownloaderTools;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows,
  uPCRE, uXML, HttpSend, blcksock,
  uDownloader, uOptions;

type
  EScriptedDownloaderError = class(EDownloaderError);
  EScriptedDownloaderScriptError = class(EScriptedDownloaderError);

  TScriptVariable = class
    private
      fName: string;
      fValue: string;
    protected
    public
      constructor Create(const AName: string);
      destructor Destroy; override;
      property Name: string read fName;
      property Value: string read fValue write fValue;  
    end;

  TScriptVariables = class
    private
      fList: TList;
      function GetCount: integer;
      function GetValue(const Name: string): string;
      function GetVariable(Index: integer): TScriptVariable;
      procedure SetValue(const Name, Value: string);
      function GetExists(const Name: string): boolean;
    protected
    public
      constructor Create;
      destructor Destroy; override;
      procedure Clear;
      function Find(const Name: string; out Index: integer): boolean;
      property Count: integer read GetCount;
      property Variables[Index: integer]: TScriptVariable read GetVariable;
      property Values[const Name: string]: string read GetValue write SetValue; default;
      property Exists[const Name: string]: boolean read GetExists;
    end;

procedure ScriptError(const Msg: string; Node: TXmlNode);

implementation

uses
  uCompatibility,
  uMessages;

type
  THackXmlNode = class(TXmlNode);

procedure ScriptError(const Msg: string; Node: TXmlNode);
var
  Str: TMemoryStream;
  Tag: string;
begin
  Tag := '';
  if Node <> nil then
    begin
    Str := TMemoryStream.Create;
    try
      THackXmlNode(Node).WriteToStream(Str);
      Str.Position := 0;
      SetLength(Tag, Str.Size);
      if Str.Size > 0 then
        Str.Read(Tag[1], Str.Size);
    finally
      FreeAndNil(Str);
      end;
    end;
  if Tag = '' then
    Raise EScriptedDownloaderScriptError.Create(ERR_SCRIPTS_ERROR + Msg)
  else
    Raise EScriptedDownloaderScriptError.Create(ERR_SCRIPTS_ERROR + Msg + EOLN + {$IFDEF UNICODE} string {$ENDIF} (Tag));
end;

{ TScriptVariable }

constructor TScriptVariable.Create(const AName: string);
begin
  inherited Create;
  fName := AName;
  fValue := '';
end;

destructor TScriptVariable.Destroy;
begin
  inherited;
end;

{ TScriptVariables }

constructor TScriptVariables.Create;
begin
  inherited Create;
  fList := TList.Create;
end;

destructor TScriptVariables.Destroy;
begin
  Clear;
  FreeAndNil(fList);
  inherited;
end;

procedure TScriptVariables.Clear;
var
  i: integer;
begin
  for i := 0 to Pred(Count) do
    Variables[i].Free;
  fList.Clear;
end;

function TScriptVariables.GetCount: integer;
begin
  Result := fList.Count;
end;

function TScriptVariables.GetVariable(Index: integer): TScriptVariable;
begin
  Result := TScriptVariable(fList[Index]);
end;

function TScriptVariables.Find(const Name: string; out Index: integer): boolean;
var
  L, H, I, C: Integer;
  Item: TScriptVariable;
begin
  Result := False;
  L := 0;
  H := Pred(Count);
  while L <= H do
    begin
    I := (L + H) shr 1;
    Item := Variables[I];
    C := AnsiCompareStr(Item.Name, Name);
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

function TScriptVariables.GetValue(const Name: string): string;
var
  Index: integer;
begin
  if Find(Name, Index) then
    Result := Variables[Index].Value
  else
    Raise EScriptedDownloaderScriptError.CreateFmt(ERR_SCRIPTS_VARIABLE_NOT_FOUND, [Name]);
end;

procedure TScriptVariables.SetValue(const Name, Value: string);
var
  Index: integer;
  NewVar: TScriptVariable;
begin
  if Find(Name, Index) then
    Variables[Index].Value := Value
  else
    begin
    NewVar := TScriptVariable.Create(Name);
    NewVar.Value := Value;
    fList.Insert(Index, NewVar);
    end;
end;

function TScriptVariables.GetExists(const Name: string): boolean;
var
  Index: integer;
begin
  Result := Find(Name, Index);
end;

end.
