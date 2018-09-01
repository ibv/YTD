unit uApiGraphics;

interface
{$INCLUDE 'uApi.inc'}

uses
  SysUtils, Windows, Messages,
  uApiCommon;

function Color(Red, Green, Blue: byte): TColor;
procedure FreeGdiObject(var Obj: THandle);

implementation

function Color(Red, Green, Blue: byte): TColor;
begin
  Result := TColor(Red) or (TColor(Green) shl 8) or (TColor(Blue) shl 16);
end;

procedure FreeGdiObject(var Obj: THandle);
begin
  if Obj <> 0 then
    begin
    DeleteObject(Obj);
    Obj := 0;
    end;
end;

end.
