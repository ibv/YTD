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

unit guiSetupWINAPI;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows, Messages, CommCtrl, ShlObj, ShellApi,
  uApiCommon, uApiFunctions, uApiForm, uApiGraphics,
  uLanguages, uMessages, uOptions, uFunctions, uCompatibility,
  guiConsts, uDialogs;

type
  TFormSetup = class(TApiForm)
    protected
    private
      fDestinationDir: string;
      fDesktopShortcut: boolean;
      fStartMenuShortcut: boolean;
    private
      EditDestinationDir: THandle;
      CheckDesktopShortcut: THandle;
      CheckStartMenuShortcut: THandle;
      procedure CreateObjects;
      procedure DestroyObjects;
    protected
      function DoInitDialog: boolean; override;
      function DoClose: boolean; override;
      function DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean; override;
    protected
      procedure ButtonDestinationDirClick;
      procedure LabelDestinationDirClick;
    public
      constructor Create(AOwner: TApiForm; const ADialogResourceName: string); override;
      destructor Destroy; override;
      property DestinationDir: string read fDestinationDir;
      property DesktopShortcut: boolean read fDesktopShortcut;
      property StartMenuShortcut: boolean read fStartMenuShortcut;
    end;

implementation

{$RESOURCE *.res}

// from resource.h
const
  IDC_LABEL_DESTINATIONDIR = 1001;
  IDC_EDIT_DESTINATIONDIR = 1002;
  IDC_BUTTON_DESTINATIONDIR = 1003;
  IDC_CHECK_DESKTOPSHORTCUT = 1004;
  IDC_CHECK_STARTMENUSHORTCUT = 1005;
  IDC_BUTTON_INSTALL = 1006;
  IDC_BUTTON_RUN = 1007;

const
  ACTION_DESTINATIONDIR = 40001;

{ TFormSetup }

constructor TFormSetup.Create(AOwner: TApiForm; const ADialogResourceName: string);
begin
  inherited;
end;

destructor TFormSetup.Destroy;
begin
  inherited;
end;

procedure TFormSetup.CreateObjects;
begin
  EditDestinationDir := GetDlgItem(Self.Handle, IDC_EDIT_DESTINATIONDIR);
  CheckDesktopShortcut := GetDlgItem(Self.Handle, IDC_CHECK_DESKTOPSHORTCUT);
  CheckStartMenuShortcut := GetDlgItem(Self.Handle, IDC_CHECK_STARTMENUSHORTCUT);
end;

procedure TFormSetup.DestroyObjects;
begin
  EditDestinationDir := 0;
  CheckDesktopShortcut := 0;
  CheckStartMenuShortcut := 0;
end;

function TFormSetup.DoInitDialog: boolean;
begin
  Result := inherited DoInitDialog;
  CreateObjects;
  Self.Translate;
  // Make sure everything can be resized easily
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_LABEL_DESTINATIONDIR), [akTop, akLeft, akRight]);
  SetControlAnchors(EditDestinationDir, [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_BUTTON_DESTINATIONDIR), [akTop, akRight]);
  SetControlAnchors(CheckDesktopShortcut, [akTop, akLeft, akRight]);
  SetControlAnchors(CheckStartMenuShortcut, [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_BUTTON_INSTALL), [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_BUTTON_RUN), [akTop, akLeft, akRight]);
  // Accelerators
  Accelerators := LoadAccelerators(hInstance, 'SETUP_ACTIONS');
  // Initial content
  SetWindowText(EditDestinationDir, PChar(GetSpecialFolder(CSIDL_PROGRAM_FILES) + '\' + APPLICATION_TITLE));
  CheckDlgButton(Self.Handle, IDC_CHECK_DESKTOPSHORTCUT, BST_CHECKED);
  CheckDlgButton(Self.Handle, IDC_CHECK_STARTMENUSHORTCUT, BST_CHECKED);
end;

function TFormSetup.DoClose: boolean;
begin
  Result := inherited DoClose;
  if Result then
    begin
    if ModalResult = idOK then
      begin
      fDestinationDir := GetWindowTextAsString(EditDestinationDir);
      case IsDlgButtonChecked(Self.Handle, IDC_CHECK_DESKTOPSHORTCUT) of
        BST_CHECKED:
          fDesktopShortcut := True;
        BST_UNCHECKED:
          fDesktopShortcut := False;
        end;
      case IsDlgButtonChecked(Self.Handle, IDC_CHECK_STARTMENUSHORTCUT) of
        BST_CHECKED:
          fStartMenuShortcut := True;
        BST_UNCHECKED:
          fStartMenuShortcut := False;
        end;
      end;
    DestroyObjects;
    end;
end;

function TFormSetup.DoCommand(NotificationCode, Identifier: word; WindowHandle: THandle): boolean;
begin
  Result := False;
  case NotificationCode of
    1: // Accelerators
      case Identifier of
        ACTION_DESTINATIONDIR:
          begin
          ButtonDestinationDirClick;
          Result := True;
          end;
        end;
    STN_CLICKED {, BN_CLICKED} : // Click on a label, button etc.
      case Identifier of
        IDC_LABEL_DESTINATIONDIR:
          begin
          LabelDestinationDirClick;
          Result := True;
          end;
        IDC_BUTTON_DESTINATIONDIR:
          begin
          ButtonDestinationDirClick;
          Result := True;
          end;
        IDC_BUTTON_INSTALL:
          begin
          Close(idOK);
          Result := True;
          end;
        IDC_BUTTON_RUN:
          begin
          Close(idIgnore);
          Result := True;
          end;
        end;
    end;
  if not Result then
    Result := inherited DoCommand(NotificationCode, Identifier, WindowHandle);
end;

procedure TFormSetup.ButtonDestinationDirClick;
var Dir: string;
begin
  Dir := GetWindowTextAsString(EditDestinationDir);
  if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0) then
    SetWindowText(EditDestinationDir, PChar(Dir));
end;

procedure TFormSetup.LabelDestinationDirClick;
begin
  SetFocus(EditDestinationDir);
end;

initialization
   InitCommonControls;

end.
