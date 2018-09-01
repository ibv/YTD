(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                           (c) 2009-11 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2011, Pepak (http://www.pepak.net)
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

unit guiConverterWINAPI;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows, Messages, CommCtrl, ShellApi,
  uApiCommon, uApiFunctions, uApiForm, uApiGraphics,
  uLanguages, uMessages, uOptions, guiConsts, uDialogs;

type
  TFormSelectConverter = class(TApiForm)
    protected
    private
      fOptions: TYTDOptions;
      fConverterID: string;
      fTitle: string;
    private
      ComboConverter: THandle;
      procedure CreateObjects;
      procedure DestroyObjects;
    protected
      function DoInitDialog: boolean; override;
      function DoClose: boolean; override;
      function DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean; override;
    protected
      procedure LabelConverterClick;
    public
      constructor Create(AOwner: TApiForm; const ADialogResourceName: string); override;
      destructor Destroy; override;
      property Options: TYTDOptions read fOptions write fOptions;
      property ConverterID: string read fConverterID write fConverterID;
      property Title: string read fTitle write fTitle;
    end;

function SelectConverter(Options: TYTDOptions; var SelectedID: string; Owner: TApiForm = nil; const Caption: string = ''): boolean;
procedure PrepareConverterComboBox(Combo: THandle; Options: TYTDOptions; const SelectedID: string = '');
function DecodeConverterComboBox(Combo: THandle; Options: TYTDOptions; out SelectedID: string): boolean;

implementation

{$RESOURCE *.res}

function SelectConverter(Options: TYTDOptions; var SelectedID: string; Owner: TApiForm = nil; const Caption: string = ''): boolean;
var F: TFormSelectConverter;
begin
  Result := False;
  F := TFormSelectConverter.Create(Owner);
  try
    F.Options := Options;
    F.ConverterID := SelectedID;
    F.Title := Caption;
    if F.ShowModal = idOK then
      begin
      SelectedID := F.ConverterID;
      Result := True;
      end;
  finally
    F.Free;
    end;
end;

procedure PrepareConverterComboBox(Combo: THandle; Options: TYTDOptions; const SelectedID: string = '');
var L: TStringList;
    i, Index, SelectedIndex: integer;
    Converter: TConverter;
begin
  SendMessage(Combo, CB_RESETCONTENT, 0, 0);
  SendMessage(Combo, CB_ADDSTRING, 0, LPARAM(PChar(_(CONVERTERS_NOCONVERTER))));
  SelectedIndex := 0;
  L := TStringList.Create;
  try
    Options.ReadConverterIDList(L);
    for i := 0 to Pred(L.Count) do
      if Options.ReadConverter(L[i], Converter) then
        begin
        Index := SendMessage(Combo, CB_ADDSTRING, 0, LPARAM(PChar(_(Converter.Title))));
        if Index >= 0 then
          begin
          SendMessage(Combo, CB_SETITEMDATA, Index, i);
          if (SelectedIndex <= 0) and (SelectedID <> '') and (SelectedID = L[i]) then
            SelectedIndex := Index;
          end;
        end;
  finally
    L.Free;
    end;
  SendMessage(Combo, CB_SETCURSEL, SelectedIndex, 0);
end;

function DecodeConverterComboBox(Combo: THandle; Options: TYTDOptions; out SelectedID: string): boolean;
var L: TStringList;
    idx: integer;
begin
  Result := False;
  idx := SendMessage(Combo, CB_GETCURSEL, 0, 0);
  if (idx <> CB_ERR) then
    if idx = 0 then
      begin
      SelectedID := '';
      Result := True;
      end
    else
      begin
      idx := SendMessage(Combo, CB_GETITEMDATA, idx, 0);
      if idx <> CB_ERR then
        begin
        L := TStringList.Create;
        try
          Options.ReadConverterIDList(L);
          SelectedID := L[idx];
          Result := True;
        finally
          L.Free;
          end;
        end;
      end;
end;

// from resource.h
const
  IDC_BUTTON_OK = 1000;
  IDC_BUTTON_CANCEL = 1001;
  IDC_LABEL_CONVERTER = 1008;
  IDC_COMBO_CONVERTER = 1009;

const
  ACTION_OK = 40000;
  ACTION_CANCEL = 40001;

{ TFormSelectConverter }

constructor TFormSelectConverter.Create(AOwner: TApiForm; const ADialogResourceName: string);
begin
  inherited;
end;

destructor TFormSelectConverter.Destroy;
begin
  inherited;
end;

procedure TFormSelectConverter.CreateObjects;
begin
  ComboConverter := GetDlgItem(Self.Handle, IDC_COMBO_CONVERTER);
end;

procedure TFormSelectConverter.DestroyObjects;
begin
  ComboConverter := 0;
end;

function TFormSelectConverter.DoInitDialog: boolean;
begin
  Result := inherited DoInitDialog;
  CreateObjects;
  Self.Translate;
  // Caption
  if Title <> '' then
    SetWindowText(Self.Handle, PChar(Title));
  // ComboConverters
  PrepareConverterComboBox(ComboConverter, Options, ConverterID);
  // Make sure everything can be resized easily
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_COMBO_CONVERTER), [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_BUTTON_OK), [akBottom, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_BUTTON_CANCEL), [akBottom, akRight]);
  // Accelerators
  Accelerators := LoadAccelerators(hInstance, 'CONVERTER_ACTIONS');
end;

function TFormSelectConverter.DoClose: boolean;
var SelectedID: string;
begin
  Result := inherited DoClose;
  if Result then
    begin
    if ModalResult = idOK then
      if DecodeConverterComboBox(ComboConverter, Options, SelectedID) then
        ConverterID := SelectedID;
    DestroyObjects;
    end;
end;

function TFormSelectConverter.DoCommand(NotificationCode, Identifier: word; WindowHandle: THandle): boolean;
begin
  Result := False;
  case NotificationCode of
    1: // Accelerators
      case Identifier of
        ACTION_OK:
          begin
          Close(idOK);
          Result := True;
          end;
        ACTION_CANCEL:
          begin
          Close(idCancel);
          Result := True;
          end;
        end;
    STN_CLICKED {, BN_CLICKED} : // Click on a label, button etc.
      case Identifier of
        IDC_LABEL_CONVERTER:
          begin
          LabelConverterClick;
          Result := True;
          end;
        IDC_BUTTON_OK:
          begin
          Close(idOK);
          Result := True;
          end;
        IDC_BUTTON_CANCEL:
          begin
          Close(idCancel);
          Result := True;
          end;
        end;
    end;
  if not Result then
    Result := inherited DoCommand(NotificationCode, Identifier, WindowHandle);
end;

procedure TFormSelectConverter.LabelConverterClick;
begin
  SetFocus(ComboConverter);
end;

initialization
   InitCommonControls;

end.
