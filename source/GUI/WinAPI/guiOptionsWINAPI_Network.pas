(******************************************************************************

______________________________________________________________________________

YouTube Downloader                                        (C) 2009, 2010 Pepak
http://www.pepak.net/download/youtube-downloader/         http://www.pepak.net
______________________________________________________________________________


Copyright (c) 2010, Pepak (http://www.pepak.net)
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

unit guiOptionsWINAPI_Network;
{$INCLUDE 'ytd.inc'}

interface

uses
  SysUtils, Classes, Windows, Messages, CommCtrl, ShellApi,
  uApiCommon, uApiFunctions, uApiForm, uApiGraphics,
  uLanguages, uMessages, uOptions, guiConsts, uDialogs,
  guiOptionsWINAPI;

type
  TFrameNetworkOptions = class(TFrameOptions)
    protected
    private
      EditProxyHost: THandle;
      EditProxyPort: THandle;
      EditProxyUser: THandle;
      EditProxyPass: THandle;
      procedure CreateObjects;
      procedure DestroyObjects;
    protected
      function DoInitDialog: boolean; override;
      function DoClose: boolean; override;
      function DoCommand(NotificationCode: word; Identifier: word; WindowHandle: THandle): boolean; override;
    protected
      procedure LabelProxyHostClick;
      procedure LabelProxyPortClick;
      procedure LabelProxyUserClick;
      procedure LabelProxyPassClick;
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
  IDC_CHECKBOX_USEPROXY = 1000;
  IDC_LABEL_PROXYHOST = 1001;
  IDC_EDIT_PROXYHOST = 1002;
  IDC_LABEL_PROXYPORT = 1003;
  IDC_EDIT_PROXYPORT = 1004;
  IDC_LABEL_PROXYUSER = 1005;
  IDC_EDIT_PROXYUSER = 1006;
  IDC_LABEL_PROXYPASS = 1007;
  IDC_EDIT_PROXYPASS = 1008;

{ TFrameNetworkOptions }

constructor TFrameNetworkOptions.Create(AOwner: TApiForm; const ADialogResourceName: string);
begin
  inherited;
end;

destructor TFrameNetworkOptions.Destroy;
begin
  inherited;
end;

procedure TFrameNetworkOptions.CreateObjects;
begin
  EditProxyHost := GetDlgItem(Self.Handle, IDC_EDIT_PROXYHOST);
  EditProxyPort := GetDlgItem(Self.Handle, IDC_EDIT_PROXYPORT);
  EditProxyUser := GetDlgItem(Self.Handle, IDC_EDIT_PROXYUSER);
  EditProxyPass := GetDlgItem(Self.Handle, IDC_EDIT_PROXYPASS);
end;

procedure TFrameNetworkOptions.DestroyObjects;
begin
  EditProxyHost := 0;
  EditProxyPort := 0;
  EditProxyUser := 0;
  EditProxyPass := 0;
end;

function TFrameNetworkOptions.DoInitDialog: boolean;
begin
  Result := inherited DoInitDialog;
  CreateObjects;
  Self.Translate;
  LoadFromOptions;
  // Make sure everything can be resized easily
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_CHECKBOX_USEPROXY), [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_EDIT_PROXYHOST), [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_EDIT_PROXYPORT), [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_EDIT_PROXYUSER), [akTop, akLeft, akRight]);
  SetControlAnchors(GetDlgItem(Self.Handle, IDC_EDIT_PROXYPASS), [akTop, akLeft, akRight]);
  // Accelerators
  Accelerators := LoadAccelerators(hInstance, 'OPTIONS_NETWORK_ACTIONS');
end;

function TFrameNetworkOptions.DoClose: boolean;
begin
  Result := inherited DoClose;
  if Result then
    begin
    if ModalResult = idOK then
      SaveToOptions;
    DestroyObjects;
    end;
end;

function TFrameNetworkOptions.DoCommand(NotificationCode, Identifier: word; WindowHandle: THandle): boolean;
begin
  Result := False;
  case NotificationCode of
    STN_CLICKED {, BN_CLICKED, CBN_SELCHANGE} : // Click on a label, button etc.
      case Identifier of
        IDC_LABEL_PROXYHOST:
          begin
          LabelProxyHostClick;
          Result := True;
          end;
        IDC_LABEL_PROXYPORT:
          begin
          LabelProxyPortClick;
          Result := True;
          end;
        IDC_LABEL_PROXYUSER:
          begin
          LabelProxyUserClick;
          Result := True;
          end;
        IDC_LABEL_PROXYPASS:
          begin
          LabelProxyPassClick;
          Result := True;
          end;
        end;
    end;
  if not Result then
    Result := inherited DoCommand(NotificationCode, Identifier, WindowHandle);
end;

procedure TFrameNetworkOptions.LoadFromOptions;
const CheckboxConsts: array[boolean] of DWORD = (BST_UNCHECKED, BST_CHECKED);
begin
  // Proxy settings
  CheckDlgButton(Self.Handle, IDC_CHECKBOX_USEPROXY, CheckboxConsts[Options.ProxyActive]);
  SendMessage(EditProxyHost, WM_SETTEXT, 0, LPARAM(PChar(Options.ProxyHost)));
  SendMessage(EditProxyPort, WM_SETTEXT, 0, LPARAM(PChar(Options.ProxyPort)));
  SendMessage(EditProxyUser, WM_SETTEXT, 0, LPARAM(PChar(Options.ProxyUser)));
  SendMessage(EditProxyPass, WM_SETTEXT, 0, LPARAM(PChar(Options.ProxyPassword)));
end;

procedure TFrameNetworkOptions.SaveToOptions;
begin
  // Proxy settings
  case IsDlgButtonChecked(Self.Handle, IDC_CHECKBOX_USEPROXY) of
    BST_CHECKED:
      Options.ProxyActive := True;
    BST_UNCHECKED:
      Options.ProxyActive := False;
    end;
  Options.ProxyHost := GetWindowTextAsString(EditProxyHost);
  Options.ProxyPort := GetWindowTextAsString(EditProxyPort);
  Options.ProxyUser := GetWindowTextAsString(EditProxyUser);
  Options.ProxyPassword := GetWindowTextAsString(EditProxyPass);
end;

procedure TFrameNetworkOptions.LabelProxyHostClick;
begin
  SetFocus(EditProxyHost);
end;

procedure TFrameNetworkOptions.LabelProxyPassClick;
begin
  SetFocus(EditProxyPort);
end;

procedure TFrameNetworkOptions.LabelProxyPortClick;
begin
  SetFocus(EditProxyUser);
end;

procedure TFrameNetworkOptions.LabelProxyUserClick;
begin
  SetFocus(EditProxyPass);
end;

initialization
   InitCommonControls;

end.
