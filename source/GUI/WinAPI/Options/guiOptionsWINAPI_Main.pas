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

unit guiOptionsWINAPI_Main;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows, Messages, CommCtrl, ShellApi, ShlObj,
  uApiCommon, uApiFunctions, uApiForm, uApiGraphics,
  uLanguages, uMessages, uOptions, uDialogs, uFunctions,
  guiConsts, guiFunctions, guiOptionsWINAPI;

type
  TFrameMainOptions = class(TFrameOptions)
    protected
    private
      EditLanguage: THandle;
      procedure CreateObjects;
      procedure DestroyObjects;
    protected
      function DoInitDialog: boolean; override;
      function DoClose: boolean; override;
      function DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean; override;
    protected
      procedure LabelLanguageClick;
      procedure ButtonShortcutOnDesktopClick;
      procedure ButtonShortcutInStartMenuClick;
    public
      constructor Create(AOwner: TApiForm; const ADialogResourceName: string); override;
      destructor Destroy; override;
      procedure LoadFromOptions; override;
      procedure SaveToOptions; override;
    end;

implementation

{$RESOURCE *.res}

// from resource.h
const
  IDC_CHECKBOX_PORTABLEMODE = 1000;
  IDC_CHECKBOX_CHECKNEWVERSION = 1001;
  IDC_LABEL_LANGUAGE = 1002;
  IDC_EDIT_LANGUAGE = 1003;
  IDC_BUTTON_SHORTCUTONDESKTOP = 1004;
  IDC_BUTTON_SHORTCUTINSTARTMENU = 1005;
  IDC_CHECKBOX_MONITORCLIPBOARD = 1006;

{ TFrameMainOptions }

constructor TFrameMainOptions.Create(AOwner: TApiForm; const ADialogResourceName: string);
begin
  inherited;
end;

destructor TFrameMainOptions.Destroy;
begin
  inherited;
end;

procedure TFrameMainOptions.CreateObjects;
begin
  EditLanguage := GetDlgItem(Self.Handle, IDC_EDIT_LANGUAGE);
end;

procedure TFrameMainOptions.DestroyObjects;
begin
  EditLanguage := 0;
end;

function TFrameMainOptions.DoInitDialog: boolean;
begin
  Result := inherited DoInitDialog;
  CreateObjects;
  Self.Translate;
  LoadFromOptions;
  // Make sure everything can be resized easily
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_CHECKBOX_PORTABLEMODE), [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_CHECKBOX_CHECKNEWVERSION), [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_CHECKBOX_MONITORCLIPBOARD), [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_EDIT_LANGUAGE), [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_BUTTON_SHORTCUTONDESKTOP), [akBottom, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_BUTTON_SHORTCUTINSTARTMENU), [akBottom, akLeft, akRight]);
  // Accelerators
  Accelerators := LoadAccelerators(hInstance, 'OPTIONS_MAIN_ACTIONS');
end;

function TFrameMainOptions.DoClose: boolean;
begin
  Result := inherited DoClose;
  if Result then
    begin
    if ModalResult = idOK then
      SaveToOptions;
    DestroyObjects;
    end;
end;

function TFrameMainOptions.DoCommand(NotificationCode, Identifier: word; WindowHandle: THandle): boolean;
begin
  Result := False;
  case NotificationCode of
    STN_CLICKED {, BN_CLICKED, CBN_SELCHANGE} : // Click on a label, button etc.
      case Identifier of
        IDC_LABEL_LANGUAGE:
          begin
          LabelLanguageClick;
          Result := True;
          end;
        IDC_BUTTON_SHORTCUTONDESKTOP:
          begin
          ButtonShortcutOnDesktopClick;
          Result := True;
          end;
        IDC_BUTTON_SHORTCUTINSTARTMENU:
          begin
          ButtonShortcutInStartMenuClick;
          Result := True;
          end;
        end;
    end;
  if not Result then
    Result := inherited DoCommand(NotificationCode, Identifier, WindowHandle);
end;

procedure TFrameMainOptions.LoadFromOptions;
const CheckboxConsts: array[boolean] of DWORD = (BST_UNCHECKED, BST_CHECKED);
begin
  // Portable mode
  CheckDlgButton(Self.Handle, IDC_CHECKBOX_PORTABLEMODE, CheckboxConsts[Options.PortableMode]);
  // Check for new version automatically
  CheckDlgButton(Self.Handle, IDC_CHECKBOX_CHECKNEWVERSION, CheckboxConsts[Options.CheckForNewVersionOnStartup]);
  // Monitor clipboard
  CheckDlgButton(Self.Handle, IDC_CHECKBOX_MONITORCLIPBOARD, CheckboxConsts[Options.MonitorClipboard]);
  // Language
  SetWindowText(EditLanguage, PChar(Options.Language));
end;

procedure TFrameMainOptions.SaveToOptions;
begin
  // Portable mode
  case IsDlgButtonChecked(Self.Handle, IDC_CHECKBOX_PORTABLEMODE) of
    BST_CHECKED:
      Options.PortableMode := True;
    BST_UNCHECKED:
      Options.PortableMode := False;
    end;
  // Check for new version automatically
  case IsDlgButtonChecked(Self.Handle, IDC_CHECKBOX_CHECKNEWVERSION) of
    BST_CHECKED:
      Options.CheckForNewVersionOnStartup := True;
    BST_UNCHECKED:
      Options.CheckForNewVersionOnStartup := False;
    end;
  // Monitor clipboard
  case IsDlgButtonChecked(Self.Handle, IDC_CHECKBOX_MONITORCLIPBOARD) of
    BST_CHECKED:
      Options.MonitorClipboard := True;
    BST_UNCHECKED:
      Options.MonitorClipboard := False;
    end;
  // Language
  Options.Language := GetWindowTextAsString(EditLanguage);
end;

procedure TFrameMainOptions.LabelLanguageClick;
begin
  SetFocus(EditLanguage);
end;

procedure TFrameMainOptions.ButtonShortcutOnDesktopClick;
begin
  CreateShortcut(APPLICATION_SHORTCUT, '', CSIDL_DESKTOPDIRECTORY);
end;

procedure TFrameMainOptions.ButtonShortcutInStartMenuClick;
begin
  CreateShortcut(APPLICATION_SHORTCUT, '', CSIDL_PROGRAMS);
end;

initialization
   InitCommonControls;

end.
