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

unit guiOptionsWINAPI_Downloads;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows, Messages, CommCtrl, ShellApi,
  uApiCommon, uApiFunctions, uApiForm, uApiGraphics,
  uLanguages, uMessages, uOptions, guiConsts, uDialogs,
  guiOptionsWINAPI;

type
  TFrameDownloadOptions = class(TFrameOptions)
    protected
    private
      fConverterIndex: integer;
    private
      ComboOverwriteMode: THandle;
      EditDownloadDir: THandle;
      ComboConverter: THandle;
      procedure CreateObjects;
      procedure DestroyObjects;
    protected
      function DoInitDialog: boolean; override;
      function DoClose: boolean; override;
      function DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean; override;
    protected
      procedure LabelOverwriteModeClick;
      procedure LabelDownloadDirClick;
      {$IFDEF CONVERTERS}
      procedure LabelConverterClick;
      procedure ComboConverterChange;
      {$ENDIF}
      procedure ButtonDownloadDirClick;
    public
      constructor Create(AOwner: TApiForm; const ADialogResourceName: string); override;
      destructor Destroy; override;
      procedure LoadFromOptions; override;
      procedure SaveToOptions; override;
    end;

implementation

{$RESOURCE *.res}

{$IFDEF CONVERTERS}
uses
  guiConverterWINAPI;
{$ENDIF}

// from resource.h
const
  IDC_CHECKBOX_AUTOSTARTDOWNLOADS = 1000;
  IDC_CHECKBOX_ENABLESUBTITLES = 1001;
  IDC_LABEL_OVERWRITEMODE = 1003;
  IDC_COMBO_OVERWRITEMODE = 1004;
  IDC_LABEL_DOWNLOADDIR = 1005;
  IDC_EDIT_DOWNLOADDIR = 1006;
  IDC_BUTTON_DOWNLOADDIR = 1007;
  IDC_LABEL_CONVERTER = 1008;
  IDC_COMBO_CONVERTER = 1009;
  IDC_CHECKBOX_AUTOTRYHTMLPARSER = 1010;

const
  ACTION_DOWNLOADDIR = 40001;

{ TFrameDownloadOptions }

constructor TFrameDownloadOptions.Create(AOwner: TApiForm; const ADialogResourceName: string);
begin
  inherited;
end;

destructor TFrameDownloadOptions.Destroy;
begin
  inherited;
end;

procedure TFrameDownloadOptions.CreateObjects;
begin
  ComboOverwriteMode := GetDlgItem(Self.Handle, IDC_COMBO_OVERWRITEMODE);
  EditDownloadDir := GetDlgItem(Self.Handle, IDC_EDIT_DOWNLOADDIR);
  ComboConverter := GetDlgItem(Self.Handle, IDC_COMBO_CONVERTER);
end;

procedure TFrameDownloadOptions.DestroyObjects;
begin
  ComboOverwriteMode := 0;
  EditDownloadDir := 0;
  ComboConverter := 0;
end;

function TFrameDownloadOptions.DoInitDialog: boolean;
begin
  Result := inherited DoInitDialog;
  CreateObjects;
  Self.Translate;
  LoadFromOptions;
  fConverterIndex := SendMessage(ComboConverter, CB_GETCURSEL, 0, 0);
  // Make sure everything can be resized easily
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_CHECKBOX_AUTOSTARTDOWNLOADS), [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_CHECKBOX_AUTOTRYHTMLPARSER), [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_CHECKBOX_ENABLESUBTITLES), [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_COMBO_OVERWRITEMODE), [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_EDIT_DOWNLOADDIR), [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_BUTTON_DOWNLOADDIR), [akTop, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_COMBO_CONVERTER), [akTop, akLeft, akRight]);
  // Accelerators
  Accelerators := LoadAccelerators(hInstance, 'OPTIONS_DOWNLOADS_ACTIONS');
end;

function TFrameDownloadOptions.DoClose: boolean;
begin
  Result := inherited DoClose;
  if Result then
    begin
    if ModalResult = idOK then
      SaveToOptions;
    DestroyObjects;
    end;
end;

function TFrameDownloadOptions.DoCommand(NotificationCode, Identifier: word; WindowHandle: THandle): boolean;
begin
  Result := False;
  case NotificationCode of
    1 {, CBN_SELCHANGE}: // Accelerators
      case Identifier of
        ACTION_DOWNLOADDIR:
          begin
          ButtonDownloadDirClick;
          Result := True;
          end;
        {$IFDEF CONVERTERS}
        IDC_COMBO_CONVERTER:
          begin
          ComboConverterChange;
          Result := True;
          end;
        {$ENDIF}
        end;
    STN_CLICKED {, BN_CLICKED, CBN_SELCHANGE} : // Click on a label, button etc.
      case Identifier of
        IDC_LABEL_OVERWRITEMODE:
          begin
          LabelOverwriteModeClick;
          Result := True;
          end;
        IDC_LABEL_DOWNLOADDIR:
          begin
          LabelDownloadDirClick;
          Result := True;
          end;
        {$IFDEF CONVERTERS}
        IDC_LABEL_CONVERTER:
          begin
          LabelConverterClick;
          Result := True;
          end;
        {$ENDIF}
        IDC_BUTTON_DOWNLOADDIR:
          begin
          ButtonDownloadDirClick;
          Result := True;
          end;
        end;
    end;
  if not Result then
    Result := inherited DoCommand(NotificationCode, Identifier, WindowHandle);
end;

procedure TFrameDownloadOptions.LoadFromOptions;
const CheckboxConsts: array[boolean] of DWORD = (BST_UNCHECKED, BST_CHECKED);
      OverwriteMode: array [TOverwriteMode] of integer = (2, 1, 3, 0);
begin
  // Auto Start Downloads
  CheckDlgButton(Self.Handle, IDC_CHECKBOX_AUTOSTARTDOWNLOADS, CheckboxConsts[Options.AutoStartDownloads]);
  // Auto Try HTML Parser
  CheckDlgButton(Self.Handle, IDC_CHECKBOX_AUTOTRYHTMLPARSER, CheckboxConsts[Options.AutoTryHtmlParser]);
  // Download Subtitles
  CheckDlgButton(Self.Handle, IDC_CHECKBOX_ENABLESUBTITLES, CheckboxConsts[Options.SubtitlesEnabled]);
  // Overwrite Mode
  SendMessage(ComboOverwriteMode, CB_RESETCONTENT, 0, 0);
  SendMessage(ComboOverwriteMode, CB_ADDSTRING, 0, LPARAM(PChar(_('Ask user'))));
  SendMessage(ComboOverwriteMode, CB_ADDSTRING, 0, LPARAM(PChar(_('Overwrite'))));
  SendMessage(ComboOverwriteMode, CB_ADDSTRING, 0, LPARAM(PChar(_('Skip'))));
  SendMessage(ComboOverwriteMode, CB_ADDSTRING, 0, LPARAM(PChar(_('Rename automatically'))));
  SendMessage(ComboOverwriteMode, CB_SETCURSEL, OverwriteMode[Options.OverwriteMode], 0);
  // Download Directory
  SetWindowText(EditDownloadDir, PChar(Options.DestinationPath));
  // Converter
  {$IFDEF CONVERTERS}
  PrepareConverterComboBox(ComboConverter, Options, Options.SelectedConverterID);
  {$ELSE}
  EnableWindow(ComboConverter, False);
  {$ENDIF}
end;

procedure TFrameDownloadOptions.SaveToOptions;
const OverwriteMode: array[0..3] of TOverwriteMode = (omAsk, omAlways, omNever, omRename);
var idx: integer;
{$IFDEF CONVERTERS}
    SelectedID: string;
{$ENDIF}
begin
  // Auto Start Downloads
  case IsDlgButtonChecked(Self.Handle, IDC_CHECKBOX_AUTOSTARTDOWNLOADS) of
    BST_CHECKED:
      Options.AutoStartDownloads := True;
    BST_UNCHECKED:
      Options.AutoStartDownloads := False;
    end;
  // Auto Try HTML Parser
  case IsDlgButtonChecked(Self.Handle, IDC_CHECKBOX_AUTOTRYHTMLPARSER) of
    BST_CHECKED:
      Options.AutoTryHtmlParser := True;
    BST_UNCHECKED:
      Options.AutoTryHtmlParser := False;
    end;
  // Download Subtitles
  case IsDlgButtonChecked(Self.Handle, IDC_CHECKBOX_ENABLESUBTITLES) of
    BST_CHECKED:
      Options.SubtitlesEnabled := True;
    BST_UNCHECKED:
      Options.SubtitlesEnabled := False;
    end;
  // Overwrite Mode
  idx := SendMessage(ComboOverwriteMode, CB_GETCURSEL, 0, 0);
  if (idx <> CB_ERR) and (idx >= 0) and (idx < Length(OverwriteMode)) then
    Options.OverwriteMode := OverwriteMode[idx];
  // Destination path
  Options.DestinationPath := GetWindowTextAsString(EditDownloadDir);
  {$IFDEF CONVERTERS}
  // Converter
  if DecodeConverterComboBox(ComboConverter, Options, SelectedID) then
    Options.SelectedConverterID := SelectedID;
  {$ENDIF}
end;

procedure TFrameDownloadOptions.LabelOverwriteModeClick;
begin
  SetFocus(ComboOverwriteMode);
end;

procedure TFrameDownloadOptions.LabelDownloadDirClick;
begin
  SetFocus(EditDownloadDir);
end;

{$IFDEF CONVERTERS}
procedure TFrameDownloadOptions.LabelConverterClick;
begin
  SetFocus(ComboConverter);
end;

procedure TFrameDownloadOptions.ComboConverterChange;
begin
  {$IFDEF CONVERTERSMUSTBEACTIVATED}
  if not Options.ConvertersActivated then
    begin
    ErrorMessageBox(_(CONVERTERS_INACTIVE_WARNING), APPLICATION_TITLE);
    SendMessage(ComboConverter, CB_SETCURSEL, fConverterIndex, 0);
    end;
  {$ENDIF}
end;
{$ENDIF}

procedure TFrameDownloadOptions.ButtonDownloadDirClick;
var Dir: string;
begin
  Dir := GetWindowTextAsString(EditDownloadDir);
  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
    SetWindowText(EditDownloadDir, PChar(Dir));
end;

initialization
   InitCommonControls;

end.
