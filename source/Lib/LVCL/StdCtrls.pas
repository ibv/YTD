unit StdCtrls;

{
   LVCL - Very LIGHT VCL routines
   ------------------------------

   Tiny replacement for the standard VCL StdCtrls.pas
   Just put the LVCL directory in your Project/Options/Path/SearchPath
   and your .EXE will shrink from 300KB to 30KB

   Notes:
   - implements TButton+TCheckBox+TEdit+TLabel+TMemo
   - for TMemo: use global Text property, as there's no Lines[] property;
     don't set anything in Lines property in IDE 
   - compatible with the standard .DFM files.
   - only use existing properties in your DFM, otherwise you'll get error on startup
     (no Anchor, e.g.)

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

{$WARNINGS OFF}
{$HINTS ON}

interface

uses
  Windows, Messages, Classes, Controls, SysUtils, Graphics;

type
  TLabel = class(TGraphicControl)
  protected
    procedure SetCaption(const Value: string);
  public
    procedure Paint; override;
    property Caption: string read fCaption write SetCaption;
  end;

  TButton = class(TWinControl)
  protected
    procedure CreateHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TEdit = class(TWinControl)
  private
    fText: string;
    fPassWordChar: char;
    fReadOnly: boolean;
  protected
    CreateFlags: cardinal;
    procedure ReadProperty(const Name: string; Reader: TReader); override;
    procedure CreateHandle; override;
    function GetText: string;
    procedure SetText(const Value: string); override;
  public
    procedure SetReadOnly(Value: Boolean);
    procedure SelectAll;
    property Text: string read GetText write SetText;
    property ReadOnly: boolean read fReadOnly write SetReadOnly;
  end;

  TMemo = class(TEdit)
  protected
    procedure CreateHandle; override;
  end;

  TCheckBox = class(TWinControl)
  private
    fChecked: boolean;
    procedure SetChecked(const Value: boolean);
    function GetChecked: boolean;
  protected
    procedure CreateHandle; override;
    procedure ReadProperty(const Name: string; Reader: TReader); override;
  public
    property Checked: boolean read GetChecked write SetChecked;
  end;



implementation


{ TButton }

constructor TButton.Create(AOwner:TComponent);
begin
  inherited;
  fTransparent := True; // do not use color
end;

procedure TButton.CreateHandle;
begin
  Handle := CreateWindow(
   'BUTTON',
   pointer(fCaption),
   WS_CHILD or WS_CLIPCHILDREN or WS_TABSTOP,
   fLeft,fTop,fWidth,fHeight,
   Parent.Handle,
   0,
   hInstance,
   nil
  );
  if not fVisible then
    ShowWindow(Handle,SW_HIDE);
end;


{ TEdit }

procedure TEdit.CreateHandle;
begin
  Color := clWhite;
  if CreateFlags=0 then begin
   CreateFlags := WS_CHILD or WS_CLIPCHILDREN or WS_TABSTOP
     or ES_AUTOHSCROLL or ES_AUTOVSCROLL;
   if fPassWordChar='*' then
     CreateFlags := CreateFlags or ES_PASSWORD;
  end;
  Handle := CreateWindowEx(
   WS_EX_CONTROLPARENT or WS_EX_CLIENTEDGE,
   'EDIT',
   pointer(fCaption),
   CreateFlags,
   fLeft,fTop,fWidth,fHeight,
   Parent.Handle,
   0,
   hInstance,
   nil
  );
  SendMessage(Handle,WM_SETTEXT,0,integer(fCaption));
  if not fVisible then
    ShowWindow(Handle,SW_HIDE);
  if fReadOnly then
    SendMessage(fHandle, EM_SETREADONLY, 1, 0);
end;

function TEdit.GetText: string;
var i: integer;
begin
  i := SendMessage(fHandle,WM_GETTEXTLENGTH,0,0);
  if i=0 then
    result := '' else begin
    SetLength(result,i);
    SetLength(result,SendMessage(fHandle,WM_GETTEXT,i+1,integer(result)));
  end;
end;

procedure TEdit.ReadProperty(const Name: string; Reader: TReader);
const Properties: array[0..1] of PChar=(
    'PasswordChar','ReadOnly');
var tmp: string;
begin
  case StringIndex(Name,Properties) of
  0: begin
    tmp := Reader.StringProperty;
    if tmp<>'' then
      fPassWordChar := tmp[1];
  end;
  1: fReadOnly := Reader.BooleanProperty;
  else inherited;
  end;
end;

procedure TEdit.SelectAll;
begin
  if fHandle<>0 then
    SendMessage(fHandle, EM_SETSEL, 0, -1);
end;

procedure TEdit.SetReadOnly(Value: Boolean);
begin
  if fHandle<>0 then
    SendMessage(fHandle, EM_SETREADONLY, Ord(Value), 0);
  fReadOnly := Value;
end;

procedure TEdit.SetText(const Value: string);
begin
  HandleNeeded;
  fText := Value;
  if fHandle<>0 then
    SendMessage(fHandle,WM_SETTEXT,0,integer(Value));
end;


{ TMemo }

procedure TMemo.CreateHandle;
begin
  CreateFlags := WS_VISIBLE or
    WS_CHILD or ES_MULTILINE or ES_WANTRETURN or ES_AUTOVSCROLL or WS_VSCROLL;
  inherited;
end;


{ TCheckBox }

procedure TCheckBox.CreateHandle;
begin
  inherited;
  Handle := CreateWindowEx(
   WS_EX_CONTROLPARENT,
   'BUTTON',
   pointer(fCaption),
   WS_VISIBLE or WS_CHILD or BS_AUTOCHECKBOX or WS_TABSTOP,
   fLeft,fTop,fWidth,fHeight,
   Parent.Handle,
   0,
   hInstance,
   nil
  );
  SendMessage(Handle, BM_SETCHECK, integer(fChecked), 0);
  if not fVisible then
    ShowWindow(Handle,SW_HIDE);
end;

function TCheckBox.GetChecked: boolean;
begin
  result := boolean(SendMessage(Handle,BM_GETCHECK,0,0));
end;

procedure TCheckBox.ReadProperty(const Name: string; Reader: TReader);
const Properties: array[0..0] of PChar=(
    'Checked');
//type TCheckBoxState = (cbUnchecked, cbChecked, cbGrayed);
begin
  case StringIndex(Name,Properties) of
  0: fChecked := Reader.BooleanProperty;
  //1: Reader.IdentProperty(fState,TypeInfo(TCheckBoxState));
    // no cbGrayed implementation yet
  else inherited;
  end;
end;

procedure TCheckBox.SetChecked(const Value: boolean);
begin
  SendMessage(Handle, BM_SETCHECK, integer(Value), 0);
end;


{ TLabel }

procedure TLabel.Paint;
var R: TRect;
begin
  R := ClientRect;
  with Canvas do begin
    Font.Assign(Self.Font);
    if fTransparent then
      Brush.Style := bsClear else begin
      Brush.Color := Parent.Color;
      FillRect(R);
    end;
    PrepareText;
    DrawText(Handle, pointer(fCaption), length(fCaption), R,
      DT_LEFT or DT_NOPREFIX or DT_WORDBREAK);
  end;
end;

procedure TLabel.SetCaption(const Value: string);
var Redraw: boolean;
begin
  Redraw := (fCaption<>Value);
  fCaption := Value;
  if Redraw then // avoid flicker
    Invalidate;
end;

const
  Classes: array[0..4] of TPersistentClass =
    (TLabel, TButton, TEdit, TCheckBox, TMemo);

initialization
  RegisterClasses(Classes);
end.
