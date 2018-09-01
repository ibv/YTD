unit uApiGraphics;

interface

uses
  SysUtils, Windows, Messages;

type
  TColor = TColorRef;

function Color(Red, Green, Blue: byte): TColor;
procedure FreeGdiObject(var Obj: THandle);

const
  clBlack = TColor($000000);
  clMaroon = TColor($000080);
  clGreen = TColor($008000);
  clOlive = TColor($008080);
  clNavy = TColor($800000);
  clPurple = TColor($800080);
  clTeal = TColor($808000);
  clGray = TColor($808080);
  clSilver = TColor($C0C0C0);
  clRed = TColor($0000FF);
  clLime = TColor($00FF00);
  clYellow = TColor($00FFFF);
  clBlue = TColor($FF0000);
  clFuchsia = TColor($FF00FF);
  clAqua = TColor($FFFF00);
  clLtGray = TColor($C0C0C0);
  clDkGray = TColor($808080);
  clWhite = TColor($FFFFFF);
  clNone = TColor($1FFFFFFF);
  clDefault = TColor($20000000);

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
