unit Controls;

{
   LVCL - Very LIGHT VCL routines
   ------------------------------

   Tiny replacement for the standard VCL Controls.pas
   Just put the LVCL directory in your Project/Options/Path/SearchPath
   and your .EXE will shrink from 300KB to 30KB

   Notes:
   - implements TControl+TCustomControl+TGraphicControl+TWinControl
   - compatible with the standard .DFM files.
   - only use existing properties in your DFM, otherwise you'll get error on startup

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  License for the specific language governing rights and limitations
  under the License.

  The Initial Developer of the Original Code is Arnaud Bouchez.
  This work is Copyright (C) 2008 Arnaud Bouchez - http://bouchez.info
  Emulates the original Delphi/Kylix Cross-Platform Runtime Library
  (c)2000,2001 Borland Software Corporation
  Portions created by Paul Toth are Copyright (C) 2001 Paul Toth. http://tothpaul.free.fr
  All Rights Reserved.

}

interface

uses
  Windows, Messages, Classes, SysUtils, Graphics;

type
  TKeyPressEvent = procedure (Sender: TObject; var Key: Char) of object;

  TControl = class(TComponent)
  private
    EOnClick: TNotifyEvent;
  protected
    procedure ReadProperty(const Name:string; Reader: TReader); override;
    function FindMethod(Reader: TReader): TMethod;
  end;

  TWinControl = class;

  TCustomControl = class(TControl)
  private
    fParent: TWinControl;
    fCanvas: TCanvas;
    fFont: TFont;
    fColor: integer;
    fParentFont: boolean;
  protected
    EOnPaint: TNotifyEvent;
    EOnShow: TNotifyEvent;
    fCaption: string;
    fLeft, fTop: integer;
    fWidth, fHeight: integer;
    fVisible,
    fTransparent: boolean;
    procedure ReadProperty(const Name: string; Reader: TReader); override;
    function SubProperty(const Name: string): TPersistent; override;
    function GetFont: TFont;
    procedure SetFont(Value: TFont);
    function GetCanvas: TCanvas;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; virtual;
    procedure Invalidate;
    function ClientRect: TRect; virtual;
    property Font: TFont read GetFont write SetFont;
    property Canvas: TCanvas read GetCanvas;
    property Parent: TWinControl read fParent;
    property Color: integer read fColor write fColor;
    property Width: integer read fWidth;
    property Height: integer read fHeight;
    property Left: integer read fLeft;
    property Top: integer read fTop;
    property Transparent: boolean read fTransparent;
    property Caption: string read fCaption;
    property Visible: boolean read fVisible;
  end;

  TGraphicControl = class(TCustomControl)
  protected
    procedure SetParentComponent(Value: TComponent); override;
  public
  end;

  TControlStyle = set of (csAcceptsControl, csCaptureMouse, csClickEvents,
    csFramed, csSetCaption, csOpaque, cdDoubleClicks);
  TControlState = set of (csLButtonDown, csClicked, csPalette, csReadingState,
    csAlignmentNeeded, csFocusing, csCreating, csPaintCopy, csCustomPaint,
    csDestroyingHandle, csDocking);
  TBorderStyle = (bsNone,bsSingle);

  TWinControl = class(TCustomControl)
  private
    fGraphics: TList;
    procedure WMSize(var Msg: TWMSize); message wm_size;
    procedure WMLButtonDown(var msg: TWMLButtonDown); message wm_lbuttondown;
    procedure WMLButtonUp(var msg: TWMLButtonUp); message wm_lbuttonup;
    procedure WMEraseBkGnd(var msg: TWMEraseBkGnd); message wm_erasebkgnd;
    procedure WMDestroy(var Msg: TWMDestroy); message wm_destroy;
    procedure WMPaint(var Msg: TWMPaint); message wm_paint;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure SetCaption(const Value: string);
    function GetEnabled: boolean;
    procedure SetEnabled(const Value: boolean);
  public
    procedure DefaultHandler(var Message); override;
  protected
    EOnKeyPress: TKeyPressEvent;
    fHandle: integer;
    fOldProc: integer;
    fTabOrder: integer;
    fControlStyle: TControlStyle;
    fControlState: TControlState;
    fControls: TList;
    procedure ReadProperty(const Name: string; Reader: TReader); override;
    procedure SetParentComponent(Value: TComponent); override;
    function GetParentComponent: TComponent; override;
    procedure HandleNeeded;
    procedure CreateHandle; virtual; abstract;
    procedure SetHandle(Value: integer);
    procedure SetText(const Value: string); virtual;
    procedure AddChild(AChild: TWinControl);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ClientRect: TRect; override;
    procedure Paint; override;
    procedure Show; virtual;
    procedure Hide; virtual;
    procedure SetFocus;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer);
    property Handle: integer read fHandle write SetHandle;
    property Caption: string read fCaption write SetCaption;
    property Enabled: boolean read GetEnabled write SetEnabled;
  end;


implementation

function WndProc(Hwnd,Msg,wParam,lParam: integer): integer; stdcall;
var obj: TObject;
    dsp: TMessage;
begin
  obj := TObject(GetWindowLong(HWnd,GWL_USERDATA)); // faster than GetProp()
  if not Assigned(obj) then
    result := DefWindowProc(HWnd,Msg,wParam,lParam) else begin
    dsp.msg := msg;
    dsp.wParam := WParam;
    dsp.lParam := lParam;
    dsp.result := 0;
    obj.Dispatch(dsp);
    result := dsp.result;
  end;
end;

procedure TControl.ReadProperty(const Name: string; Reader:TReader);
const
  TControlProperties:array[0..0] of PChar=(
   'OnClick'
  );
begin
  case StringIndex(Name,TControlProperties) of
    0 : TMethod(EOnClick) := FindMethod(Reader);
    else inherited;
  end;
end;

function TControl.FindMethod(Reader: TReader): TMethod;
var AComponent: TComponent;
    Name: shortstring;
begin
  if Reader.ReadValueType in [vaString,vaIdent] then begin
    Name := Reader.ReadShortString;
    AComponent := self;
    while AComponent<>nil do begin
      result.Data := AComponent;
      result.Code := AComponent.MethodAddress(Name);
      if result.Code<>nil then
        exit;
      AComponent := AComponent.Owner;
    end;
  end;
  raise EClassesError.Create('method?');
end;

constructor TCustomControl.Create(AOwner: TComponent);
begin
  inherited;
  fParentFont := true; // default value
end;

destructor TCustomControl.Destroy;
begin
  fFont.Free;
  fCanvas.Free;
  inherited;
end;

function TCustomControl.GetCanvas: TCanvas;
begin
  if fCanvas=nil then
    fCanvas := TCanvas.Create;
  result := fCanvas;
end;

procedure TCustomControl.ReadProperty(const Name: string; Reader: TReader);
const
  TWinControlProperties: array[0..9] of PChar=(
   'Left','Top',
   'Width','Height',
   'Color',
   'Transparent',
   'Caption',
   'OnPaint',
   'ParentFont',
   'OnShow'
  );
begin
  case StringIndex(Name,TWinControlProperties) of
    0 : fLeft := Reader.IntegerProperty;
    1 : fTop := Reader.IntegerProperty;
    2 : fWidth := Reader.IntegerProperty;
    3 : fHeight := Reader.IntegerProperty;
    4 : fColor := Reader.ColorProperty;
    5 : fTransparent := Reader.BooleanProperty;
    6 : fCaption := Reader.StringProperty;
    7 : TMethod(EOnPaint) := FindMethod(Reader);
    8 : fParentFont := Reader.BooleanProperty;
    9 : TMethod(EOnShow) := FindMethod(Reader);
    else inherited;
  end;
end;

function TCustomControl.SubProperty(const Name:string): TPersistent;
const
  TControlSubProperties:array[0..0] of PChar=(
   'Font'
  );
begin
  case StringIndex(Name,TControlSubProperties) of
   0 : begin
     if fFont=nil then
       fFont := TFont.Create;
     result := fFont;
   end;
   else result := nil;
  end;
end;

function TCustomControl.GetFont: TFont;
begin
  if fFont=nil then begin
    if fParentFont and (Parent<>nil) then begin
      result := Parent.Font;
      exit;
    end;
    fFont := TFont.Create;
  end;
  result := fFont;
end;

procedure TCustomControl.SetFont(Value: TFont);
begin
  Font.Assign(Value);
end;

procedure TCustomControl.Paint;
begin
  if not fTransparent then
   with Canvas do
    if Assigned(EOnPaint) then EOnPaint(Self);
end;

function TCustomControl.ClientRect: TRect;
begin
  result.Left := fLeft;
  result.Right := result.Left+fWidth;
  result.Top := fTop;
  result.Bottom := result.Top+fHeight;
end;

procedure TGraphicControl.SetParentComponent(Value: TComponent);
begin
  while (Value<>nil) and not Value.InheritsFrom(TWinControl) do
    Value := Value.ParentComponent;
  if Value<>nil then begin
    fParent := TWinControl(Value);
    Font.Assign(fParent.Font);
    fParent.fGraphics.Add(Self);
  end;
end;

constructor TWinControl.Create(AOwner: TComponent);
begin
  inherited;
  fControls := TList.Create;
  fGraphics := TList.Create;
end;

destructor TWinControl.Destroy;
begin
  if fColor<>0 then
    DeleteObject(fColor);
  Handle := 0;
  fControls.Free;
  fGraphics.Free;
  inherited;
end;

procedure TWinControl.ReadProperty(const Name:string; Reader:TReader);
const
  TWinControlProperties:array[0..2] of PChar=(
   'Text',
   'TabOrder',
   'OnKeyPress'
  );
begin
  case StringIndex(Name,TWinControlProperties) of
    0 : SetText(Reader.StringProperty);
    1 : fTabOrder := Reader.IntegerProperty;
    2 : TMethod(EOnKeyPress) := FindMethod(Reader);
   else inherited;
  end;
end;

procedure TWinControl.SetParentComponent(Value:TComponent);
begin
  while (Value<>nil) and not(Value.InheritsFrom(TWinControl)) do
    Value := Value.ParentComponent;
  if Value<>nil then begin
    fParent := TWinControl(Value);
    Canvas.Font.Assign(fParent.Font);
    fParent.AddChild(Self);
  end;
end;

function TWinControl.GetParentComponent:TComponent;
begin
  result := fParent;
end;

procedure TWinControl.HandleNeeded;
begin
  if fParent<>nil then
    fParent.HandleNeeded;
  if fHandle=0 then
    CreateHandle;
end;

procedure TWinControl.SetHandle(Value: integer);
begin
  if fHandle<>0 then begin
    SetWindowLong(fHandle,GWL_WNDPROC,fOldProc);
    DestroyWindow(fHandle);
  end;
  fHandle := Value;
  if fHandle<>0 then begin
    fOldProc := GetWindowLong(fHandle,GWL_WNDPROC);
    SetWindowLong(fHandle,GWL_USERDATA,integer(self)); // faster than SetProp()
    SetWindowLong(fHandle,GWL_WNDPROC,integer(@WndProc));
    SendMessage(fHandle,WM_SETFONT,Font.Handle,0);
  end;
end;

procedure TWinControl.SetText(const Value:string);
begin
  fCaption := Value;
end;

procedure TWinControl.AddChild(AChild:TWinControl);
begin
  fControls.Add(AChild);
end;

procedure TWinControl.WMSize(var Msg: TWMSize);
begin
  inherited;
  fWidth := msg.Width;
  fHeight := msg.Height;
end;

procedure TWinControl.WMLButtonDown(var msg: TWMLButtonDown);
begin
  inherited;
  Include(fControlState,csClicked);
end;

procedure TWinControl.WMLButtonUp(var msg: TWMLButtonUp);
begin
  inherited;
  if csClicked in fControlState then begin
    if Assigned(EOnClick) then
      if (cardinal(msg.XPos)<=cardinal(fWidth)) and
         (cardinal(msg.YPos)<=cardinal(fHeight)) then
        EOnClick(Self);
    Exclude(fControlState,csClicked);
  end;
end;

procedure TWinControl.WMChar(var Msg: TWMChar);
var Key: Char;
begin
  Key := chr(Msg.CharCode);
  if Assigned(EOnKeyPress) then begin
    EOnKeyPress(Self,Key);
    if Key=#0 then exit;
  end;
  inherited;
end;

procedure TWinControl.WMEraseBkGnd(var msg: TWMEraseBkGnd);
begin
  msg.result := 1;
  if not fTransparent then
  with Canvas do begin
    Handle := Msg.DC;
    Brush.Color := Self.Color;
    FillRect(Rect(0,0,fWidth,fHeight));
  end;
end;

procedure TWinControl.WMPaint(var Msg: TWMPaint);
begin
  with Canvas do begin
    Handle := Msg.DC;
    if Handle=0 then
      Handle := GetDC(self.fHandle);
    Paint;
    if Msg.DC=0 then
      ReleaseDC(self.fHandle,Handle);
  end;
  inherited;
end;

procedure TWinControl.WMDestroy(var Msg: TWMDestroy);
begin
  inherited;
  PostQuitMessage(0);
end;

procedure TWinControl.DefaultHandler(var Message);
begin
  with TMessage(Message) do
    result := CallWindowProc(pointer(fOldProc),fHandle,Msg,wParam,lParam)
end;

procedure TWinControl.Paint;
var H, i: integer;
begin
  inherited;
  H := Self.Canvas.Handle;
  for i := 0 to fGraphics.Count-1 do
    with TGraphicControl(fGraphics.List[i]) do begin
      Canvas.Handle := H;
      Paint;
    end;
end;

procedure TWinControl.SetFocus;
begin
  Windows.SetFocus(fHandle);
end;

procedure TWinControl.SetCaption(const Value: string);
begin
  fCaption := Value;
  if fHandle<>0 then
    SendMessage(fHandle,WM_SETTEXT,0,integer(Value));
end;

procedure TWinControl.Hide;
var i: integer;
begin
  HandleNeeded;
  for i := 0 to fControls.Count-1 do
    with TWinControl(fControls.List[i]) do
      if fVisible then
        Hide;
  fVisible := false;
  ShowWindow(fHandle,SW_HIDE);
end;

procedure TWinControl.Show;
var i: integer;
begin
  HandleNeeded;
  for i := 0 to fControls.Count-1 do
    with TWinControl(fControls.List[i]) do
      if not fVisible then
        Show;
  fVisible := true;
  ShowWindow(fHandle,SW_SHOW);
  if Assigned(EOnShow) then
    EOnShow(Self);
end;

procedure TCustomControl.Invalidate;
var R: TRect;
begin
  if Parent=nil then
    exit;
  R := ClientRect;
  InvalidateRect(Parent.Handle,@R,false);
end;

function TWinControl.ClientRect: TRect;
begin
  GetClientRect(Handle,result);
end;

procedure TWinControl.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  MoveWindow(Handle,ALeft,Atop,AWidth,AHeight,false);
end;

function TWinControl.GetEnabled: boolean;
begin
  if fHandle=0 then
    result := true else
    result := IsWindowEnabled(fHandle);
end;

procedure TWinControl.SetEnabled(const Value: boolean);
begin
  if GetEnabled = Value then Exit;
  if fHandle <> 0 then
     EnableWindow(fHandle, Value);
  Invalidate; // necessary for Graphic controls
end;

end.
