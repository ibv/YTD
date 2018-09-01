unit uJSON;

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
