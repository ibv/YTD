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
  SysUtils, uLkJSON;

type
  TJSON = TlkJSONbase;
  TJSONNode = TlkJSONbase;
  TJSONString = string;

function JSONCreate(const Source: TJSONString): TJSON; overload;
procedure JSONFree(JSON: TJSON);
procedure JSONFreeAndNil(var JSON: TJSON);

function JSONNodeByPath(JSON: TJSONNode; Path: string; out FoundNode: TJSONNode): boolean;
  // Note: numeric indexes are not supported for objects

implementation

function JSONCreate(const Source: TJSONString): TJSON;
begin
  Result := TlkJSON.ParseText(Source);
end;

procedure JSONFree(JSON: TJSON);
begin
  if JSON <> nil then
    JSON.Free;
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
      if JSON is TlkJSONobject then
        Result := JSONNodeByPath(TlkJSONobject(JSON).Field[VarName], Path, FoundNode)
      else if JSON is TlkJSONcustomlist then
        begin
        i := StrToIntDef(VarName, -1);
        if (i >= 0) and (i < JSON.Count) then
          Result := JSONNodeByPath(JSON.Child[i], Path, FoundNode);
        end;
      end;
end;

end.
