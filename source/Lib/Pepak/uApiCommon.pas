unit uApiCommon;

interface
{$INCLUDE 'uApi.inc'}

uses
  Windows;

type
  TColor = TColorRef;

type
  TAlignment = (alLeft, alRight, alCenter);

{$IFDEF APIFORM_ANCHORS}
type
  TAnchorKind = (akTop, akLeft, akRight, akBottom);
  TAnchors = set of TAnchorKind;

  TAnchorDesc = class
    Handle: THandle;
    Anchors: TAnchors;
    Rect: TRect;
    Margins: TRect;
    end;
{$ENDIF}

type
  TMouseButton = (mbLeft, mbRight, mbMiddle, mbExtra1, mbExtra2);


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

end.
